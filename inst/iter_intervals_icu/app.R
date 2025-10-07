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
      checkboxInput("reverse_y",
                    "Reverse Y-axis",
                    value = TRUE),

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

      # Reload the database file (it was updated by iter2.r)
      new_data <- local({
        load(db_file)
        run_streams_data
      })

      # If user decreased n_activities, trim the database to only keep the most recent N
      if (input$n_activities < current_n_activities) {
        # Get the N most recent activities by date
        recent_activity_ids <- new_data %>%
          mutate(date = ymd_hms(start_date, tz = "UTC")) %>%
          group_by(activity_id) %>%
          summarise(max_date = max(date, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(max_date)) %>%
          head(input$n_activities) %>%
          .$activity_id

        new_data <- new_data %>%
          filter(activity_id %in% recent_activity_ids)

        # Save the trimmed database back
        run_streams_data <- new_data
        save(run_streams_data, file = db_file)
      }

      db_data(new_data)

      removeModal()
      showModal(modalDialog(
        title = "Success",
        sprintf("Database updated! Now has %d activities.",
                length(unique(new_data$activity_id))),
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

      # Calculate maximum width for normalization
      grps <- data_all %>%
        group_by(period, speed) %>%
        summarise(n = n(), .groups = "drop")

      mx_width <- max(grps$n)

      incProgress(0.2, detail = "Transforming data")

      # Transform data for visualization
      data_trf <- NULL

      for (i in 0:(n_periods - 1)) {
        data_chnk <- data_all %>%
          filter(period == i)

        if (nrow(data_chnk) == 0) next

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
          act_data <- data_chnk %>%
            filter(!is.na(speed), abs(speed - j) < 0.05)

          if (nrow(act_data) == 0) next

          act_out <- tibble(
            speed = j,
            period = i,
            hr = act_data %>%
              .$hr %>%
              { replace(., is.na(.), mean(., na.rm = TRUE)) } %>%
              sort(),
            act_x = seq(-nrow(act_data) / mx_width / 2 + i,
                        nrow(act_data) / mx_width / 2 + i,
                        length.out = nrow(act_data))
          )

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

      incProgress(0.1, detail = "Done!")

      # Return list with data and metadata
      list(
        data = data_trf,
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

    # Y-axis label
    y_label <- if (input$speed_units == "m/s") "Speed (m/s)" else "Speed (km/h)"

    # Create plot
    p <- ggplot(data_trf, aes(x = act_x, y = speed,
                              group = paste0(speed, period),
                              color = hr)) +
      geom_point(alpha = 0.2) +
      theme_minimal() +
      scale_colour_gradient2(low = "blue", mid = "green", high = "red",
                             limits = c(120, 190),
                             midpoint = 157) +
      scale_x_continuous(breaks = seq(0, pd$n_periods, length.out = 8),
                         labels = format(seq(pd$min_date, pd$max_date, length.out = 8), "%Y-%m-%d")) +
      labs(x = "", y = y_label,
           title = sprintf("Speed vs Heart Rate Distribution (%d-day periods)", input$period_length),
           color = "Heart Rate") +
      theme(legend.position = "bottom",
            text = element_text(size = 12))

    # Reverse y-axis if requested
    if (input$reverse_y) {
      p <- p + scale_y_reverse()
    }

    p
  })
}

# Run app
shinyApp(ui = ui, server = server)
