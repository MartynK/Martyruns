library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(here)

# Load data on startup
db_file <- here::here("data", "iter_intervals_icu_db.RData")
if (!file.exists(db_file)) {
  stop("Database not found. Please run iter2.r first to create the database.")
}
load(db_file)

# Load only the first 30 most recent activities by default
initial_n_activities <- 30
initial_activity_ids <- run_streams_data %>%
  mutate(date = ymd_hms(start_date, tz = "UTC")) %>%
  group_by(activity_id) %>%
  summarise(max_date = max(date, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(max_date)) %>%
  head(initial_n_activities) %>%
  .$activity_id

run_streams_data <- run_streams_data %>%
  filter(activity_id %in% initial_activity_ids)

# Calculate date range for slider
all_dates <- run_streams_data %>%
  mutate(date = ymd_hms(start_date, tz = "UTC")) %>%
  group_by(activity_id) %>%
  summarise(max_date = max(date, na.rm = TRUE), .groups = "drop") %>%
  .$max_date

min_session_date <- min(all_dates, na.rm = TRUE) %>% as.Date() %>% floor_date(unit = "week", week_start = 1)
max_session_date <- max(all_dates, na.rm = TRUE) %>% as.Date() %>% ceiling_date(unit = "week", week_start = 1)

# Default: Monday of 4 weeks ago (or database min if more recent)
default_min_date <- (today() - weeks(4)) %>% floor_date(unit = "week", week_start = 1)
default_min_date <- max(default_min_date, min_session_date)

# UI
ui <- fluidPage(
  titlePanel("Running Data Visualization - Speed vs Heart Rate"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Period Settings"),
      numericInput("period_length",
                   "Period Length (days):",
                   value = 7,
                   min = 1,
                   max = 365),

      sliderInput("min_session_date",
                  "Minimum Session Date:",
                  min = as.Date(min_session_date),
                  max = as.Date(max_session_date),
                  value = as.Date(default_min_date),
                  step = 7,
                  timeFormat = "%Y-%m-%d"),

      h4("Speed Settings"),
      numericInput("min_speed",
                   "Minimum Speed (km/h):",
                   value = 4,
                   min = 0,
                   max = 30,
                   step = 0.5),

      numericInput("upper_cap",
                   "Speed Upper Cap (km/h):",
                   value = 13,
                   min = 1,
                   max = 50,
                   step = 0.5),

      selectInput("speed_units",
                  "Speed Units:",
                  choices = c("km/h", "m/s"),
                  selected = "km/h"),

      h4("Display Settings"),
      selectInput("y_variable",
                  "Y-axis Variable:",
                  choices = c("Speed" = "speed", "Heart Rate" = "hr"),
                  selected = "speed"),

      checkboxInput("reverse_y",
                    "Reverse Y-axis",
                    value = TRUE),

      checkboxInput("show_quantiles",
                    "Show Q1, Median, Q3 lines",
                    value = FALSE),

      numericInput("max_points",
                   "Max Points (for rendering):",
                   value = 10000,
                   min = 1000,
                   max = 100000,
                   step = 1000),

      hr(),

      h4("Data Management"),
      numericInput("n_activities",
                   "No. Activities to Fetch:",
                   value = 30,
                   min = 1,
                   max = 200,
                   step = 5),

      actionButton("update_db", "Update Database", class = "btn-warning"),

      hr(),

      h4("Data Info"),
      textOutput("data_info"),

      hr(),

      actionButton("refresh", "Refresh Plot", class = "btn-primary")
    ),

    mainPanel(
      width = 9,
      plotOutput("main_plot", height = "700px"),

      hr(),

      plotOutput("scatter_plot", height = "500px"),

      hr(),

      verbatimTextOutput("processing_info")
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive values for database
  db_data <- reactiveVal(run_streams_data)

  # Data info
  output$data_info <- renderText({
    data <- db_data()
    sprintf("Activities: %d\nTotal records: %s",
            length(unique(data$activity_id)),
            format(nrow(data), big.mark = ","))
  })

  # Update database
  observeEvent(input$update_db, {
    current_n_activities <- length(unique(db_data()$activity_id))

    showModal(modalDialog(
      title = "Updating Database",
      "Fetching new activities from intervals.icu...",
      footer = NULL
    ))

    tryCatch({
      # Run iter2.r as a separate R process to ensure it completes
      script_path <- here::here("inst", "iter_intervals_icu", "iter2.r")

      # Read iter2.r and modify N_LAST
      iter2_code <- readLines(script_path)

      # Replace N_LAST value
      iter2_code <- gsub("^N_LAST <- \\d+",
                        sprintf("N_LAST <- %d", input$n_activities),
                        iter2_code)

      # Write to temp file and source it
      temp_file <- tempfile(fileext = ".R")
      writeLines(iter2_code, temp_file)

      # Source in global environment (needed for library calls)
      source(temp_file, local = FALSE)

      # Clean up
      unlink(temp_file)

      # Reload the full database file (updated by iter2.r - keeps all fetched data)
      full_data <- local({
        load(db_file)
        run_streams_data
      })

      # Filter to N most recent activities for display (don't modify saved database)
      recent_activity_ids <- full_data %>%
        mutate(date = ymd_hms(start_date, tz = "UTC")) %>%
        group_by(activity_id) %>%
        summarise(max_date = max(date, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(max_date)) %>%
        head(input$n_activities) %>%
        .$activity_id

      filtered_data <- full_data %>%
        filter(activity_id %in% recent_activity_ids)

      # Update reactive value with filtered data (for display)
      db_data(filtered_data)

      # Update slider date range based on filtered data
      all_dates <- filtered_data %>%
        mutate(date = ymd_hms(start_date, tz = "UTC")) %>%
        group_by(activity_id) %>%
        summarise(max_date = max(date, na.rm = TRUE), .groups = "drop") %>%
        .$max_date

      new_min_date <- min(all_dates, na.rm = TRUE) %>% as.Date() %>% floor_date(unit = "week", week_start = 1)
      new_max_date <- max(all_dates, na.rm = TRUE) %>% as.Date() %>% ceiling_date(unit = "week", week_start = 1)

      # Update slider
      updateSliderInput(session, "min_session_date",
                       min = new_min_date,
                       max = new_max_date,
                       value = max((today() - weeks(4)) %>% floor_date(unit = "week", week_start = 1), new_min_date))

      removeModal()
      showModal(modalDialog(
        title = "Success",
        sprintf("Database updated! Now displaying %d activities.",
                length(unique(filtered_data$activity_id))),
        easyClose = TRUE
      ))

    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("Failed to update database:", e$message),
        easyClose = TRUE
      ))
    })
  })

  # Reactive data processing
  processed_data <- eventReactive(input$refresh, {

    withProgress(message = 'Processing data...', value = 0, {

      # Step 1: Prepare data
      incProgress(0.1, detail = "Preparing data")

      data_all <- db_data() %>%
        mutate(
          hr = as.numeric(heartrate),
          speed_ms = as.numeric(velocity_smooth),
          date = ymd_hms(start_date, tz = "UTC")
        ) %>%
        filter(!is.na(speed_ms), !is.na(hr), !is.na(date))

      # Filter by minimum session date
      min_date_filter <- ymd(input$min_session_date, tz = "UTC")
      data_all <- data_all %>%
        group_by(activity_id) %>%
        filter(any(date >= min_date_filter)) %>%
        ungroup()

      # Add noise to old watch data (before 2025-04-14)
      old_watch_cutoff <- ymd("2025-04-14", tz = "UTC")
      data_all <- data_all %>%
        mutate(
          speed_ms = ifelse(date < old_watch_cutoff,
                           speed_ms + rnorm(n(), 0, 0.15),
                           speed_ms)
        )

      # Convert speed to km/h
      data_all$speed_kmh <- data_all$speed_ms * 3.6

      # Apply upper cap
      data_all$speed_kmh <- pmin(data_all$speed_kmh, input$upper_cap)

      # Filter by minimum speed
      data_all <- data_all %>%
        filter(speed_kmh >= input$min_speed)

      # Select units
      if (input$speed_units == "m/s") {
        data_all$speed <- data_all$speed_ms
        data_all$speed <- pmin(data_all$speed, input$upper_cap / 3.6)
        data_all <- data_all %>% filter(speed >= input$min_speed / 3.6)
      } else {
        data_all$speed <- data_all$speed_kmh
      }

      # Round speed to 0.1 for binning
      data_all$speed <- round(data_all$speed, 1)

      incProgress(0.1, detail = "Calculating periods")

      # Define date range based on actual data
      min_date <- data_all %>%
        .$date %>%
        min(na.rm = TRUE) %>%
        floor_date(unit = "day")

      max_date <- data_all %>%
        .$date %>%
        max(na.rm = TRUE) %>%
        ceiling_date(unit = "day")

      # Calculate periods
      n_periods <- difftime(max_date, min_date, units = "days") %>%
        as.numeric() %>%
        { ceiling(. / input$period_length) }

      # Assign period to each observation
      data_all <- data_all %>%
        mutate(
          period = as.numeric(difftime(date, min_date, units = "days")) %/% input$period_length
        )

      incProgress(0.1, detail = "Calculating max width")

      # Determine which variable to bin and which to color
      if (input$y_variable == "speed") {
        bin_var <- "speed"
        color_var <- "hr"
        bin_width <- 0.1
        bin_tolerance <- 0.05
      } else {
        bin_var <- "hr"
        color_var <- "speed"
        bin_width <- 1  # 1 bpm bins for HR
        bin_tolerance <- 0.5
      }

      # Calculate maximum width for normalization
      grps <- data_all %>%
        group_by(period, !!sym(bin_var)) %>%
        summarise(n = n(), .groups = "drop")

      mx_width <- max(grps$n)

      incProgress(0.2, detail = "Transforming data")

      # Transform data for visualization
      data_trf <- NULL

      for (i in 0:(n_periods - 1)) {
        data_chnk <- data_all %>%
          filter(period == i)

        if (nrow(data_chnk) == 0) next

        # Get min/max for binning variable and create bins
        var_range <- data_chnk %>%
          filter(!is.na(!!sym(bin_var))) %>%
          pull(!!sym(bin_var)) %>%
          range()

        if (any(is.infinite(var_range))) next

        var_bins <- seq(floor(var_range[1] / bin_width) * bin_width,
                        ceiling(var_range[2] / bin_width) * bin_width,
                        by = bin_width)

        for (j in var_bins) {
          act_data <- data_chnk %>%
            filter(!is.na(!!sym(bin_var)), abs(!!sym(bin_var) - j) < bin_tolerance)

          if (nrow(act_data) == 0) next

          # Sort by the color variable
          sorted_color <- act_data %>%
            pull(!!sym(color_var)) %>%
            { replace(., is.na(.), mean(., na.rm = TRUE)) } %>%
            sort()

          act_out <- tibble(
            bin_value = j,
            period = i,
            color_value = sorted_color,
            act_x = seq(-nrow(act_data) / mx_width / 2 + i,
                        nrow(act_data) / mx_width / 2 + i,
                        length.out = nrow(act_data))
          )

          # Add named columns for speed and hr
          if (input$y_variable == "speed") {
            act_out$speed <- j
            act_out$hr <- sorted_color
          } else {
            act_out$hr <- j
            act_out$speed <- sorted_color
          }

          if (is.null(data_trf)) {
            data_trf <- act_out
          } else {
            data_trf <- bind_rows(data_trf, act_out)
          }
        }

        incProgress(0.5 / n_periods, detail = sprintf("Period %d/%d", i + 1, n_periods))
      }

      incProgress(0.1, detail = "Downsampling")

      # Downsample for faster rendering
      current_points <- nrow(data_trf)

      if (current_points > input$max_points) {
        nth <- floor(current_points / input$max_points)
        data_trf <- data_trf %>%
          arrange(speed, hr) %>%
          slice(seq(1, n(), by = nth))
      }

      incProgress(0.05, detail = "Calculating quantiles")

      # Calculate quantiles per period if requested
      quantiles_data <- NULL
      if (input$show_quantiles && !is.null(data_trf) && nrow(data_trf) > 0) {
        quant_var <- if (input$y_variable == "speed") "speed" else "hr"
        quantiles_data <- data_trf %>%
          group_by(period) %>%
          summarise(
            p10 = quantile(!!sym(quant_var), 0.10, na.rm = TRUE),
            q1 = quantile(!!sym(quant_var), 0.25, na.rm = TRUE),
            median = quantile(!!sym(quant_var), 0.5, na.rm = TRUE),
            q3 = quantile(!!sym(quant_var), 0.75, na.rm = TRUE),
            p90 = quantile(!!sym(quant_var), 0.90, na.rm = TRUE),
            .groups = "drop"
          )
      }

      incProgress(0.05, detail = "Done!")

      # Return list with data and metadata
      list(
        data = data_trf,
        quantiles = quantiles_data,
        min_date = min_date,
        max_date = max_date,
        n_periods = n_periods,
        n_points_before = current_points,
        n_points_after = nrow(data_trf)
      )
    })
  }, ignoreNULL = FALSE)  # Run on startup

  # Processing info
  output$processing_info <- renderText({
    req(processed_data())
    pd <- processed_data()
    sprintf("Date range: %s to %s\nPeriods: %d\nPoints before downsampling: %s\nPoints after downsampling: %s",
            pd$min_date, pd$max_date, pd$n_periods,
            format(pd$n_points_before, big.mark = ","),
            format(pd$n_points_after, big.mark = ","))
  })

  # Main plot
  output$main_plot <- renderPlot({
    req(processed_data())

    pd <- processed_data()
    data_trf <- pd$data

    # Determine plotting variables based on y_variable selection
    if (input$y_variable == "speed") {
      y_var <- "speed"
      color_var <- "hr"
      y_label <- if (input$speed_units == "m/s") "Speed (m/s)" else "Speed (km/h)"
      color_label <- "Heart Rate"
      color_limits <- c(120, 190)
      color_midpoint <- 157
      plot_title <- sprintf("Speed vs Heart Rate Distribution (%d-day periods)", input$period_length)
    } else {
      y_var <- "hr"
      color_var <- "speed"
      y_label <- "Heart Rate (bpm)"
      color_label <- if (input$speed_units == "m/s") "Speed (m/s)" else "Speed (km/h)"
      # Speed color scale: green at 7.5 km/h (or 7.5/3.6 m/s)
      speed_midpoint <- if (input$speed_units == "m/s") 7.5 / 3.6 else 7.5
      color_limits <- c(min(data_trf$speed, na.rm = TRUE), max(data_trf$speed, na.rm = TRUE))
      color_midpoint <- speed_midpoint
      plot_title <- sprintf("Heart Rate vs Speed Distribution (%d-day periods)", input$period_length)
    }

    # Create plot
    p <- ggplot(data_trf, aes(x = act_x, y = !!sym(y_var),
                              group = paste0(!!sym(y_var), period),
                              color = !!sym(color_var))) +
      geom_point(alpha = 0.33) +
      theme_minimal() +
      scale_colour_gradient2(low = "blue", mid = "green", high = "red",
                             limits = color_limits,
                             midpoint = color_midpoint) +
      scale_x_continuous(breaks = seq(0, pd$n_periods, length.out = 8),
                         labels = format(seq(pd$min_date, pd$max_date, length.out = 8), "%Y-%m-%d")) +
      labs(x = "", y = y_label,
           title = plot_title,
           color = color_label) +
      theme(legend.position = "bottom",
            text = element_text(size = 12))

    # Add quantile lines if requested
    if (input$show_quantiles && !is.null(pd$quantiles)) {
      p <- p +
        geom_line(data = pd$quantiles, aes(x = period, y = p10, group = 1),
                  color = "black", linetype = "dotted", size = 0.6, inherit.aes = FALSE) +
        geom_line(data = pd$quantiles, aes(x = period, y = q1, group = 1),
                  color = "black", linetype = "dashed", size = 0.8, inherit.aes = FALSE) +
        geom_line(data = pd$quantiles, aes(x = period, y = median, group = 1),
                  color = "black", linetype = "solid", size = 1, inherit.aes = FALSE) +
        geom_line(data = pd$quantiles, aes(x = period, y = q3, group = 1),
                  color = "black", linetype = "dashed", size = 0.8, inherit.aes = FALSE) +
        geom_line(data = pd$quantiles, aes(x = period, y = p90, group = 1),
                  color = "black", linetype = "dotted", size = 0.6, inherit.aes = FALSE)
    }

    # Reverse y-axis if requested
    if (input$reverse_y) {
      p <- p + scale_y_reverse()
    }

    p
  })

  # Scatter plot: Speed vs Heart Rate with smooth
  output$scatter_plot <- renderPlot({
    req(processed_data())

    pd <- processed_data()

    # Get raw data with speed caps applied
    scatter_data <- db_data() %>%
      mutate(
        hr = as.numeric(heartrate),
        speed_ms = as.numeric(velocity_smooth),
        date = ymd_hms(start_date, tz = "UTC")
      ) %>%
      filter(!is.na(speed_ms), !is.na(hr), !is.na(date))

    # Filter by minimum session date
    min_date_filter <- ymd(input$min_session_date, tz = "UTC")
    scatter_data <- scatter_data %>%
      group_by(activity_id) %>%
      filter(any(date >= min_date_filter)) %>%
      ungroup()

    # Add noise to old watch data (before 2025-04-14)
    old_watch_cutoff <- ymd("2025-04-14", tz = "UTC")
    scatter_data <- scatter_data %>%
      mutate(
        speed_ms = ifelse(date < old_watch_cutoff,
                         speed_ms + rnorm(n(), 0, 0.15),
                         speed_ms)
      )

    # Convert speed to km/h
    scatter_data$speed_kmh <- scatter_data$speed_ms * 3.6

    # Apply upper cap
    scatter_data$speed_kmh <- pmin(scatter_data$speed_kmh, input$upper_cap)

    # Filter by minimum speed
    scatter_data <- scatter_data %>%
      filter(speed_kmh >= input$min_speed)

    # Select units
    if (input$speed_units == "m/s") {
      scatter_data$speed <- scatter_data$speed_ms
      scatter_data$speed <- pmin(scatter_data$speed, input$upper_cap / 3.6)
      scatter_data <- scatter_data %>% filter(speed >= input$min_speed / 3.6)
    } else {
      scatter_data$speed <- scatter_data$speed_kmh
    }

    # Random downsample to max_points
    if (nrow(scatter_data) > input$max_points) {
      scatter_data <- scatter_data %>%
        slice_sample(n = input$max_points)
    }

    # X-axis label
    x_label <- if (input$speed_units == "m/s") "Speed (m/s)" else "Speed (km/h)"

    # Create scatter plot with date coloring
    ggplot(scatter_data, aes(x = speed, y = hr, color = date)) +
      geom_point(alpha = 0.3, size = 0.8) +
      geom_smooth(method = "loess", aes(group = 1), color = "red", se = TRUE, linewidth = 1.2) +
      geom_smooth(method = "lm", aes(group = 1), color = "blue", se = FALSE, linewidth = 1, linetype = "dashed") +
      scale_y_continuous(limits = c(100, 200), breaks = seq(100, 200, by = 10)) +
      scale_color_gradient(low = "gray80", high = "black") +
      theme_minimal() +
      labs(x = x_label, y = "Heart Rate (bpm)",
           title = "Speed vs Heart Rate (Scatter with Smooth)",
           color = "Date") +
      theme(text = element_text(size = 12),
            legend.position = "bottom")
  })
}

# Run app
shinyApp(ui = ui, server = server)
