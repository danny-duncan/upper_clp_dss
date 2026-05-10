#' Plot Model Time Series Against Observed Data
#'
#' This function visualizes model-predicted and observed time series of water chemistry
#' (e.g., TOC) for a specified site and time window. It supports both ensemble and single-model
#' predictions.
#'
#' @param model_input_df A data frame containing input features used for model prediction.
#' Must include a `site` column and datetime column named `DT_round`.
#' @param water_chem_df A data frame of observed (measured) water chemistry data. Must contain
#' columns `site_code`, `DT_sample`, and the target variable (e.g., `TOC`).
#' @param model Either a single trained model object or a list of models (for ensemble mode).
#' Each model object should have accessible `$feature_names` and `$best_iteration`.
#' @param method Character. Either `"Ensemble"` (default) to compute mean and range across multiple
#' model folds, or another string (e.g., `"Single"`) for a single model.
#' @param site_sel Character. The lowercase site code to filter data for plotting.
#' @param site_title Character. A site label used in plot titles (e.g., `"Poudre at Rustic"`).
#' @param start_DT Character. Start datetime (e.g., `"2024-01-01 00:00"`) in `"MST"` timezone.
#' @param end_DT Character. End datetime (e.g., `"2024-12-31 23:00"`) in `"MST"` timezone.
#' @param summary_interval Character. Time interval for summarizing model input data.
#'  Must be compatible with `lubridate::round_date()` (default `"1 hour"`).
#' @param target_col Character. Column name of the target variable in `water_chem_df` (default `"TOC"`).
#' @param units Character. Units of the target variable (default `"mg/L"`). This is used for the plot X axis.
#' @param subtitle_arg Character. Subtitle text for the plot (default `"CLP Samples Only"`).
#' @param plot_model_input Logical. If `TRUE`, the function will return two plots: one for model input data and one for model predictions. Default is `FALSE`.
#'
#' @details
#' The function performs the following steps:
#' 1. Filters and averages model input data to the desired time period, interval, and site.
#' 2. Generates predictions using either a single model or an ensemble of models.
#' 3. If `method = "Ensemble"`, calculates mean, min, and max predictions across folds.
#' 4. Combines modeled and observed data for the selected site and time window.
#' 5. Plots the predicted time series alongside observed grab sample data.
#'
#' The plot includes:
#' - A pink line for model-predicted values (`Mean Model Estimate` or `Model Estimate`).
#' - A gray ribbon showing ensemble prediction range (min–max) if applicable (ie method = `Ensemble`).
#' - Blue points for observed grab sample values.
#' - A red dashed line if modeled TOC exceeds the maximum observed value.
#'
#' If `plot_model_input = TRUE`, the function will return two plots: one for the model input data and one for the model predictions.
#' The model input plot will show the summarized model input data over time for the selected site and time window.
#'
#' @return A `ggplot` object visualizing model predictions and observed data or two plots if `plot_model_input = TRUE`.
#'
#' @examples
#' \dontrun{
#'
#' plot_model_timeseries(
#'   model_input_df = sensor_data,
#'   water_chem_df = water_chem,
#'   model = map(full_model, "model"),
#'   method = "Ensemble",
#'   summary_interval = "1 hour",
#'   site_sel = "pbr",
#'   site_title = "Poudre Below Rustic",
#'   start_DT = "2025-04-01 00:00",
#'   end_DT = "2025-06-01 00:00"
#' )
#' }


plot_model_timeseries <- function( model_input_df, water_chem_df, model, method = "Ensemble",
                                   summary_interval = "1 hour", site_sel, site_title, start_DT, end_DT,
                                   target_col = "TOC", units = "mg/L", subtitle_arg = "CLP Samples Only",
                                   plot_model_input = F){
  #load theme
  source("src/setup_ross_theme.R")

  #convert datetimes
  start_DT <- ymd_hm(start_DT,tz = "MST")
  end_DT <- ymd_hm(end_DT,tz = "MST")

  # Water chemistry data for plotting
  plot_water_chem <- water_chem_df %>%
    mutate(site = tolower(site_code),
           DT_round = force_tz(DT_sample, "MST"))%>%
    filter(site == site_sel)%>%
    filter(between(DT_round, start_DT, end_DT))%>%
    mutate(DT_round = round_date(DT_round, unit = summary_interval )) #round date to match sensor data
  #process model input data
  # Summarize data to desired interval and trim to individual site
  summarized_data <- model_input_df%>%
    filter(site == site_sel)%>%
    mutate(DT_round = round_date(DT_round, unit = summary_interval )) %>% # ensure DT_round is rounded to the nearest
    group_by(site, DT_round) %>%
    summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

  if(method  %in% c("Ensemble", "ensemble")){
    features <- xgb.importance(model = model[[1]]) %>% pull(Feature)


    summarized_data <- imap_dfc(model, ~{
      # Get model features
      features <- xgb.importance(model = .x) %>% pull(Feature)
      best_iter <- as.numeric(xgb.attr(.x, "best_iteration"))

      feature_data <- summarized_data %>%
        select(all_of(features)) %>%
        mutate(across(everything(), as.numeric))
      #Check for missing values in features
      has_na <- rowSums(is.na(feature_data)) > 0

      # Make preds
      raw_preds <- feature_data %>%
        as.matrix() %>%
        predict(.x, ., iteration_range = c(1, best_iter), validate_features = TRUE) %>%
        round(2)

      # make preds NA where features had NA
      final_preds <- if_else(has_na, NA_real_, raw_preds)

      # Get predictions as tibble
      tibble(!!glue("{target_col}_guess_fold{.y}") := final_preds)

    }) %>%
      bind_cols(summarized_data, .)%>%
      # compute ensemble mean
      mutate(
        !!glue("{target_col}_guess_ensemble") := if_else(
          if_any(all_of(features), is.na),                # Check if ANY feature is NA
          NA_real_,                                                        # If true, set ensemble to NA
          round(rowMeans(across(matches(glue("{target_col}_guess_fold")))), 2) # Else, compute mean
        )
      )
    # Columns with fold predictions
    fold_cols <- grep(glue("{target_col}_guess_fold"), colnames(summarized_data), value = TRUE)

    plot_ts_data <- summarized_data %>%
      # Filter to desired time window first
      filter(between(DT_round, start_DT, end_DT)) %>%
      # Pad missing timestamps based on summary_interval
      pad(
        by = "DT_round",
        interval = summary_interval,   #  "1 hour", "1 day", etc.
        group = NULL
      ) %>%
      # Compute min/max across folds
      mutate(
        !!glue("{target_col}_guess_min") := pmin(!!!syms(fold_cols), na.rm = TRUE),
        !!glue("{target_col}_guess_max") := pmax(!!!syms(fold_cols), na.rm = TRUE),
        !!glue("{target_col}_guess_ensemble") := pmax(0, !!sym(glue("{target_col}_guess_ensemble"))),
        group = with(rle(!is.na(.data[[glue("{target_col}_guess_ensemble")]])), rep(seq_along(values), lengths))
      )
    plot_col <- glue("{target_col}_guess_ensemble")

    #get correct dates for plot title
    start_DT_title <- min(plot_ts_data$DT_round, na.rm = TRUE)
    end_DT_title <- max(plot_ts_data$DT_round, na.rm = TRUE)
    # find half way point between start and end DT
    halfway_DT <- start_DT + (end_DT - start_DT) / 2

    #create plot
    model_plot <- ggplot() +
      geom_ribbon(data = plot_ts_data,
                  aes(x = DT_round,
                      ymin = !!sym(glue("{target_col}_guess_min")),
                      ymax = !!sym(glue("{target_col}_guess_max")),,
                      fill = "Models Estimate Range"),
                  alpha = 0.5)+
      geom_line(
        data = plot_ts_data,
        aes(x = DT_round,
            y = .data[[plot_col]],
            group = group,
            color = "Mean Model Estimate"),
        linewidth = 1
      ) +
      geom_point(data = plot_water_chem,
                 aes(x = DT_round,
                     y = round(TOC,2),
                     color = "Sample Values",
                     shape = collector)) +

      labs(title = paste0("Model Estimated TOC at ", site_title," from ", as.Date(start_DT_title), " - ", as.Date(end_DT_title)),
           subtitle = subtitle_arg,
           x = "Date",
           y = "Model Estimated TOC (mg/L)",
           color = NULL,
           fill = NULL,
           shape = "Sample Collector") +
      scale_fill_manual(values = c("Models Estimate Range" = "grey")) +
      scale_color_manual(
        values = c(
          "Mean Model Estimate" = "#E70870",
          "Sample Values" = "#002EA3"
        )
      )+
      guides(
        fill = guide_legend(override.aes = list(alpha = 0.5)),
        color = guide_legend(override.aes = list(size = 1))  # keep consistent
      ) +
      # annotate("text",
      #          x = max(plot_ts_data$DT_round, na.rm = TRUE),
      #          y = Inf,
      #          label = "PRELIMINARY RESULTS",
      #          hjust = 1.1,
      #          vjust = 1.5,
      #          size = 7,
      #          fontface = "bold",
      #          color = "red") +
      ROSS_theme +
      theme(legend.position = "bottom")

  } else{
    #check that model is a single model
    if(length(model$feature_names) == 0){
      stop("For non-ensemble method, model must be a single trained model object.")
    }

    features <- xgb.importance(model = model) %>% pull(Feature)
    best_iter <- xgb.attr(model, "best_iteration")%>%as.numeric()

    preds <- summarized_data %>%
      select(all_of(features)) %>%
      mutate(across(everything(), as.numeric)) %>%
      as.matrix() %>%
      predict(model, ., iteration_range = c(1, best_iter), validate_parameters = T) %>%
      round(2)

    plot_col <- glue("{target_col}_guess")

    summarized_data <- summarized_data %>%
      mutate(!!sym(plot_col) := preds)

    plot_ts_data <- summarized_data %>%
      # Filter to desired time window first
      filter(between(DT_round, start_DT, end_DT)) %>%
      # Pad missing timestamps based on summary_interval
      pad(
        by = "DT_round",
        interval = summary_interval,   #  "1 hour", "1 day", etc.
        group = NULL
      ) %>%
      mutate(
        group = with(rle(!is.na(.data[[plot_col]])), rep(seq_along(values), lengths))
      )

    #get correct dates for plot title
    start_DT_title <- min(plot_ts_data$DT_round, na.rm = TRUE)
    end_DT_title <- max(plot_ts_data$DT_round, na.rm = TRUE)
    # find half way point between start and end DT
    halfway_DT <- start_DT + (end_DT - start_DT) / 2


    model_plot <- ggplot() +
      geom_line(
        data = plot_ts_data,
        aes(x = DT_round,
            y = .data[[plot_col]],
            group = group,
            color = "Model Estimate"),
        linewidth = 1
      ) +
      geom_point(data = plot_water_chem,
                 aes(x = DT_round,
                     y = round(TOC,2),
                     color = "Sample Values",
                     shape = collector)) +
      labs(title = paste0("Model Estimated TOC at ", site_title," from ", as.Date(start_DT_title), " - ", as.Date(end_DT_title)),
           subtitle = subtitle_arg,
           x = "Date",
           y = "Model Estimated TOC (mg/L)",
           color = NULL,
           fill = NULL,
           shape = "Sample Collector") +
      scale_fill_manual(values = c("Models Estimate Range" = "grey")) +
      scale_color_manual(
        values = c(
          "Model Estimate" = "#E70870",
          "Sample Values" = "#002EA3"
        )
      )+
      guides(
        fill = guide_legend(override.aes = list(alpha = 0.5)),
        color = guide_legend(override.aes = list(size = 1))  # keep consistent
      ) +
      annotate("text",
               x = max(plot_ts_data$DT_round, na.rm = TRUE),
               y = Inf,
               label = "PRELIMINARY RESULTS",
               hjust = 1.1,
               vjust = 1.5,
               size = 7,
               fontface = "bold",
               color = "red") +
      ROSS_theme +
      theme(legend.position = "bottom")
  }

  # combine grabs and sensor data for plotting to determine axes
  if(method  %in% c("Ensemble", "ensemble")){
    plot_range <- c(plot_ts_data[[glue("{target_col}_guess_ensemble")]], plot_ts_data[[glue("{target_col}_guess_max")]], plot_water_chem[[target_col]])
  } else{
    plot_range <- c(plot_ts_data[[plot_col]], plot_water_chem[[target_col]])
  }

  max_plot_val <- max(plot_range, na.rm = TRUE)
  min_plot_val <- min(plot_range, na.rm = TRUE)

  if(max_plot_val > max(water_chem_df[[target_col]], na.rm = T)){
    model_plot <- model_plot +
      geom_hline(yintercept = max_plot_val, linetype = "dashed", color = "red") +
      annotate("text", x = halfway_DT, y = max_plot_val - 0.2, label = "Max TOC Measured")
  }

  #add secondary plot of model input data if requested
  if(plot_model_input){
    if(method %in% c("Ensemble", "ensemble")){
      model <- model[[1]]  # Use the first model for input features
      features <- xgb.importance(model = model) %>% pull(Feature)
    }
    # Plot model input data
    model_input_long <- summarized_data %>%
      select(site, DT_round, all_of(features)) %>%
      # Filter to desired time window first
      filter(between(DT_round, start_DT, end_DT)) %>%
      # Pad missing timestamps based on summary_interval
      pad(
        by = "DT_round",
        interval = summary_interval,   #  "1 hour", "1 day", etc.
        group = NULL
      ) %>%
      pivot_longer(cols = -c(site, DT_round), names_to = "variable", values_to = "value") %>%
      #create groups based on consecutive NA values for line plotting
      mutate(
        group = with(rle(!is.na(.data$value)), rep(seq_along(values), lengths))
      )

    # compute per-parameter ranges and rescale grab TOC values to each sensor axis
    param_ranges <- model_input_long %>%
      group_by(variable) %>%
      summarise(y_min = min(value, na.rm = TRUE),
                y_max = max(value, na.rm = TRUE),
                .groups = "drop")

    grab_rescaled <- plot_water_chem %>%
      crossing(variable = unique(model_input_long$variable)) %>%
      left_join(param_ranges, by = "variable") %>%
      group_by(variable) %>%
      mutate(
        TOC_rescaled = rescale(
          TOC,
          to   = c(first(y_min), first(y_max)),
          from = range(TOC, na.rm = TRUE)
        )
      ) %>%
      ungroup()

    model_input_plot <- model_input_long %>%
      ggplot(aes(x = DT_round, y = value)) +
      geom_point(
        data = grab_rescaled,
        aes(x = DT_round,
            y = TOC_rescaled,
            color = "Sample Values",
            shape = collector)
      ) +
      geom_line(aes(group = group,color = "Input Variables"),linewidth = 1 ) +
      labs(title = paste0("Model Input Data at ", site_title," from ", as.Date(start_DT), " - ", as.Date(end_DT)),
           x = "Date",
           y = "Value",
           color = "Value",
           shape = "Collector") +
      scale_color_manual(
        values = c(
          "Input Variables" = "#E70870",
          "Sample Values" = "#002EA3"
        )
      )+
      ROSS_theme +
      theme(legend.position = "bottom")+
      facet_wrap(~variable, scales = "free_y")

    return(list(model_input_plot = model_input_plot, model_plot = model_plot))
  }else{
    return(model_plot)
  }


}
