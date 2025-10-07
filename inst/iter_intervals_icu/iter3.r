library(httr2)
library(here)
library(dplyr)
library(jsonlite)
library(ggplot2)
library(lubridate)

# Configuration
N_LAST <- 30  # Number of most recent Run activities to query
UPDATE <- FALSE  # If TRUE, overwrite existing sessions in database

# Load API credentials from secrets file
secrets_file <- here::here("inst", "iter_intervals_icu", "secrets", "api_key.txt")

if (!file.exists(secrets_file)) {
  stop("Secrets file not found. Please create api_key.txt with your credentials.")
}

# Read credentials
secrets_raw <- readLines(secrets_file)
secrets <- secrets_raw[!grepl("^#", secrets_raw) & nchar(trimws(secrets_raw)) > 0]

# Parse key-value pairs
credentials <- list()
for (line in secrets) {
  parts <- strsplit(line, "=")[[1]]
  if (length(parts) == 2) {
    credentials[[trimws(parts[1])]] <- trimws(parts[2])
  }
}

# Validate credentials
if (is.null(credentials$athlete_id) || credentials$athlete_id == "YOUR_ATHLETE_ID") {
  stop("Please set your athlete_id in secrets/api_key.txt")
}
if (is.null(credentials$api_key) || credentials$api_key == "YOUR_API_KEY") {
  stop("Please set your api_key in secrets/api_key.txt")
}

athlete_id <- credentials$athlete_id
api_key <- credentials$api_key

cat("Athlete ID:", athlete_id, "\n")
cat("API Key loaded successfully\n\n")

# Load existing database
db_file <- here::here("data", "iter_intervals_icu_db.RData")
if (file.exists(db_file)) {
  load(db_file)
  cat("✓ Loaded existing database with", nrow(run_streams_data), "rows\n")
  cat("  Activities in database:", length(unique(run_streams_data$activity_id)), "\n")
  existing_activities <- unique(run_streams_data$activity_id)
} else {
  cat("⚠ No existing database found, will create new one\n")
  run_streams_data <- data.frame()
  existing_activities <- character(0)
}

# Base URL for intervals.icu API
base_url <- "https://intervals.icu/api/v1"

# Create authenticated request function for JSON
intervals_request <- function(endpoint) {
  url <- paste0(base_url, endpoint)

  request(url) |>
    req_auth_basic(username = "API_KEY", password = api_key) |>
    req_perform() |>
    resp_body_json()
}

# Create authenticated request function for CSV
intervals_request_csv <- function(endpoint) {
  url <- paste0(base_url, endpoint)

  resp <- request(url) |>
    req_auth_basic(username = "API_KEY", password = api_key) |>
    req_perform()

  # Parse CSV response
  read.csv(text = resp_body_string(resp), stringsAsFactors = FALSE)
}

# Test connection: Get athlete info
cat("\nTesting API connection...\n")
tryCatch({
  athlete_info <- intervals_request(paste0("/athlete/", athlete_id))

  cat("✓ Connection successful!\n")
  cat("Athlete name:", athlete_info$name, "\n")

  # Get activities (CSV endpoint)
  cat("\nFetching recent activities...\n")
  activities_df <- intervals_request_csv(paste0("/athlete/", athlete_id, "/activities.csv"))

  cat("✓ Found", nrow(activities_df), "activities\n")

  if (nrow(activities_df) > 0) {
    cat("\nMost recent activities:\n")
    print(head(activities_df %>% select(1:8), 5))  # Show first 8 columns only
  }

  # Get last N_LAST Run activities
  cat(sprintf("\n\nFetching detailed streams for last %d Run activities...\n", N_LAST))
  run_activities <- activities_df %>%
    filter(type == "Run") %>%
    head(N_LAST)

  # Filter to activities we need to query
  if (UPDATE) {
    cat("UPDATE=TRUE: Will overwrite existing activities\n")
    activities_to_query <- run_activities
    # Remove old data for activities we're updating
    if (nrow(run_streams_data) > 0) {
      run_streams_data <- run_streams_data %>%
        filter(!activity_id %in% activities_to_query$id)
      cat("Removed old data for", sum(activities_to_query$id %in% existing_activities), "activities\n")
    }
  } else {
    activities_to_query <- run_activities %>%
      filter(!id %in% existing_activities)
    cat("Found", nrow(activities_to_query), "new activities to download\n")
    cat("Skipping", nrow(run_activities) - nrow(activities_to_query), "activities already in database\n")
  }

  if (nrow(activities_to_query) == 0) {
    cat("\n✓ Database is up to date, no new activities to fetch\n")
  } else {
    all_streams <- list()

    for (i in 1:nrow(activities_to_query)) {
      activity_id <- activities_to_query$id[i]
      activity_name <- activities_to_query$name[i]
      activity_date <- activities_to_query$start_date_local[i]

      cat(sprintf("\n[%d/%d] %s (%s)...\n", i, nrow(activities_to_query), activity_name, activity_date))

      tryCatch({
        # Get streams for this activity with resolution parameter
        # Try different resolutions: 0 = all data points
        streams <- intervals_request(paste0("/activity/", activity_id, "/streams.json?resolution=0"))

        if (length(streams) > 0) {
          # Extract stream types and data
          stream_types <- sapply(streams, function(x) x$type)
          cat("  Available streams:", paste(stream_types, collapse=", "), "\n")

          # Get the length from the first stream with data
          n_records <- length(streams[[1]]$data)

          # Create base data frame
          stream_df <- data.frame(
            activity_id = activity_id,
            activity_name = activity_name,
            start_date = activity_date,
            time_sec = 0:(n_records - 1)
          )

          # Add each stream as a column
          for (j in 1:length(streams)) {
            stream_type <- streams[[j]]$type
            raw_data <- streams[[j]]$data

            # For latlng, we have data and data2 (lat and lng)
            if (stream_type == "latlng") {
              lat_data <- sapply(raw_data, function(x) if (is.null(x)) NA else x)
              lng_data <- sapply(streams[[j]]$data2, function(x) if (is.null(x)) NA else x)
              stream_df$latitude <- lat_data
              stream_df$longitude <- lng_data
            } else {
              # Convert each element, handling NULLs and nested arrays
              stream_data <- sapply(raw_data, function(x) {
                if (is.null(x)) {
                  return(NA_character_)
                } else if (length(x) > 1) {
                  # For arrays (like HRV), convert to comma-separated string
                  return(as.character(paste(x, collapse=",")))
                } else {
                  return(as.character(x))
                }
              }, USE.NAMES = FALSE)

              # Store as character vector (can convert to numeric later if needed)
              stream_df[[stream_type]] <- as.character(stream_data)
            }
          }

          cat(sprintf("  ✓ %d records with %d streams\n", n_records, length(stream_types)))
          all_streams[[i]] <- stream_df
        } else {
          cat("  ✗ No streams returned\n")
        }

      }, error = function(e) {
        cat("  ✗ Failed:", conditionMessage(e), "\n")
      })
    }

    # Combine new streams
    if (length(all_streams) > 0) {
      cat("\n\nCombining new stream data...\n")
      new_streams <- bind_rows(all_streams)
      cat(sprintf("✓ New data: %d rows x %d columns\n", nrow(new_streams), ncol(new_streams)))

      # Append to existing database
      if (nrow(run_streams_data) > 0) {
        run_streams_data <- bind_rows(run_streams_data, new_streams)
      } else {
        run_streams_data <- new_streams
      }

      cat(sprintf("✓ Updated database: %d rows total\n", nrow(run_streams_data)))
      cat("  Activities in database:", length(unique(run_streams_data$activity_id)), "\n")

      # Save to RData file
      save(run_streams_data, file = db_file)
      cat("✓ Database saved to data/iter_intervals_icu_db.RData\n")

      # Also store in global environment
      assign("run_streams_data", run_streams_data, envir = .GlobalEnv)
    }
  }

}, error = function(e) {
  cat("✗ API connection failed:\n")
  cat(conditionMessage(e), "\n")
  cat("\nPlease check:\n")
  cat("1. Your athlete_id is correct\n")
  cat("2. Your API key is valid (get it from https://intervals.icu/settings)\n")
  cat("3. You have internet connection\n")
})


# ========================================================================
# Visualization (adapted from segmentation.r)
# ========================================================================

cat("\n\nCreating visualization...\n")

# Prepare data - convert character columns to numeric
data_all <- run_streams_data %>%
  mutate(
    hr = as.numeric(heartrate),
    speed = round(as.numeric(velocity_smooth) * 3.6, 1),  # Convert m/s to km/h and round to 0.1
    date = ymd_hms(start_date, tz = "UTC")  # Use lubridate to parse ISO8601 datetime
  ) %>%
  filter(!is.na(speed), !is.na(hr), !is.na(date))  # Remove NA values

# Extract year and ISO week
data_all$year <- year(data_all$date)
data_all$week <- isoweek(data_all$date)

# Define date range based on actual data
min_date <- data_all %>%
  .$date %>%
  min(na.rm = TRUE) %>%
  floor_date(unit = "week")

max_date <- data_all %>%
  .$date %>%
  max(na.rm = TRUE) %>%
  ceiling_date(unit = "week")

# Calculate weeks between min and max
wks <- difftime(max_date, min_date, units = "weeks") %>%
  as.numeric() %>%
  ceiling()

cat(sprintf("Date range: %s to %s (%d weeks)\n", min_date, max_date, wks))

# Calculate maximum width for normalization
grps <-
  data_all %>%
  group_by(year, week, speed) %>%
  summarise(n = n(), .groups = "drop")

mx_width <- grps$n %>% max()

# Use min_date as the starting point
date_first <- min_date

# Transform data for visualization
cat("Transforming data for histogram visualization...\n")
data_trf <- NULL

pb <- txtProgressBar(min = 1, max = wks, style = 3)
for (i in 1:wks) {
  data_chnk <-
    data_all %>%
    filter(date >= date_first + weeks(i - 1),
           date < date_first + weeks(i))

  if(nrow(data_chnk) == 0) {
    next
  }

  # Get min/max speeds and create bins
  speed_range <- data_chnk %>%
    filter(!is.na(speed)) %>%
    .$speed %>%
    range()

  if (any(is.infinite(speed_range))) next

  speed_bins <- seq(floor(speed_range[1] * 10) / 10,
                    ceiling(speed_range[2] * 10) / 10,
                    by = 0.1)

  for (j in speed_bins) {
    act_data <-
      data_chnk %>%
      filter(!is.na(speed), abs(speed - j) < 0.05)  # Bin width of 0.1 (±0.05)

    if (nrow(act_data) == 0) next

    act_out <- tibble(
      speed = j,
      wk = i - 1,
      hr = act_data %>%
        .$hr %>%
        { replace(., is.na(.), mean(., na.rm = TRUE)) } %>%
        sort(),
      act_x =
        seq(-nrow(act_data)/mx_width/2 + (i - 1),
            nrow(act_data)/mx_width/2 + (i - 1),
            length.out = nrow(act_data))
    )

    if(is.null(data_trf)) {
      data_trf <- act_out
    } else {
      data_trf <- bind_rows(data_trf, act_out)
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)

# Add human-readable week column
data_trf <- data_trf %>%
  mutate(wk_hmr = min_date + weeks(wk)) %>%
  filter(speed >= 0.2)  # Filter out very low speeds

# Downsample for faster rendering
MAX_POINTS_TARGET <- 10000
current_points <- nrow(data_trf)
cat(sprintf("\nCurrent points: %d\n", current_points))

if (current_points > MAX_POINTS_TARGET) {
  nth <- floor(current_points / MAX_POINTS_TARGET)
  cat(sprintf("Downsampling: selecting every %dth point for rendering\n", nth))

  data_trf <- data_trf %>%
    arrange(speed, hr) %>%
    slice(seq(1, n(), by = nth))

  cat(sprintf("Downsampled to %d points\n", nrow(data_trf)))
}

# Create plot
cat("\nGenerating plot...\n")
data_trf %>%
  ggplot(aes(x = act_x, y = speed,
             group = paste0(speed, wk), color = hr)) +
  geom_point(alpha = .2) +
  theme_minimal() +
  geom_hline(yintercept = 6, color = "salmon4") +
  geom_hline(yintercept = 7.1, color = "salmon4", linetype = "dashed") +
  scale_colour_gradient2(low = "blue", mid = "green", high = "red",
                         limits = c(120, 190),
                         midpoint = 157) +
  scale_y_continuous(breaks = c(5, 6, 7, 8, 9, 10, 12, 17)) +
  scale_x_continuous(breaks = seq(0, wks, length.out = 8),
                     labels = seq(min_date, max_date, length.out = 8)) +
  labs(x = "", title = "Speed vs Heart Rate Distribution by Week") +
  theme(legend.position = "bottom")
