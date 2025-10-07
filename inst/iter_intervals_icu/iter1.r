library(httr2)
library(here)
library(dplyr)
library(jsonlite)

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
cat("Testing API connection...\n")
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

  # Get last 5 Run activities
  cat("\n\nFetching detailed streams for last 5 Run activities...\n")
  run_activities <- activities_df %>%
    filter(type == "Run") %>%
    head(5)

  all_streams <- list()

  for (i in 1:nrow(run_activities)) {
    activity_id <- run_activities$id[i]
    activity_name <- run_activities$name[i]
    activity_date <- run_activities$start_date_local[i]

    cat(sprintf("\n[%d/%d] %s (%s)...\n", i, nrow(run_activities), activity_name, activity_date))

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

  # Combine all streams into one long table
  if (length(all_streams) > 0) {
    cat("\n\nCombining all stream data...\n")
    combined_streams <- bind_rows(all_streams)
    cat(sprintf("✓ Total: %d rows x %d columns\n", nrow(combined_streams), ncol(combined_streams)))
    cat("\nColumn names:\n")
    print(names(combined_streams))
    cat("\nFirst few rows:\n")
    print(head(combined_streams, 10))

    # Store in global environment for further analysis
    assign("run_streams_data", combined_streams, envir = .GlobalEnv)

    # Save to RData file
    save(run_streams_data, file = here::here("data", "iter_intervals_icu_db.RData"))
    cat("\n✓ Data saved to global variable 'run_streams_data'\n")
    cat("✓ Data saved to data/iter_intervals_icu_db.RData\n")
  }

}, error = function(e) {
  cat("✗ API connection failed:\n")
  cat(conditionMessage(e), "\n")
  cat("\nPlease check:\n")
  cat("1. Your athlete_id is correct\n")
  cat("2. Your API key is valid (get it from https://intervals.icu/settings)\n")
  cat("3. You have internet connection\n")
})


library(ggplot2)

ggplot(run_streams_data, aes(x = as.numeric(velocity_smooth), y = as.numeric(heartrate))) +
  geom_point(alpha = 0.3) +
  labs(x = "Velocity (m/s)", y = "Heart Rate (bpm)",
       title = "Heart Rate vs Velocity") +
  theme_minimal()

