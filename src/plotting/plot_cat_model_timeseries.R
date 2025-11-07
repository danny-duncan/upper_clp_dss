plot_cat_model_timeseries <- function( model_input_df, water_chem_df, target_col = "TOC_cat", models, voting_method = "soft",
                                       summary_interval = "1 hour", site_sel, site_title, start_DT, end_DT,
                                       subtitle_arg = "CLP Samples Only", title = "", units = "mg/L",
                                       cat_colors = c("0-2" = "#a6cee3", "2-4" = "#b2df8a", "4-8" = "orange", "8+" = "red2")){

  if(!models[[1]]$params$objective %in% c("multi:softmax", "multi:softprob")) {
    stop("Only multi-class classification objectives 'multi:softmax' and 'multi:softprob' are supported.")
  }
  #load theme
  source("src/setup_ross_theme.R")

  #convert datetimes
  start_DT <- ymd_hm(start_DT,tz = "MST")
  end_DT <- ymd_hm(end_DT,tz = "MST")
  #define prediction column name
  pred_col = paste0(target_col, "_guess")
  #define the number and levels of categories
  num_class <- length(cat_colors)
  class_levels <- names(cat_colors)

  # Water chemistry data for plotting
  plot_water_chem <- water_chem_df %>%
    mutate(site = tolower(site_code),
           DT_round = force_tz(DT_sample, "MST"))%>%
    filter(site == site_sel)%>%
    filter(between(DT_round, start_DT, end_DT))%>%
    mutate(DT_round = round_date(DT_round, unit = summary_interval ), #round date to match sensor data
           # Convert numeric classes (0,1,2,3) to factor with provided class levels
           category = factor(!!sym(target_col),
                             levels = seq_along(class_levels) - 1,
                             labels = class_levels
    ))
  #process model input data
  # Summarize data to desired interval and trim to individual site
  summarized_data <- model_input_df%>%
    filter(site == site_sel)%>%
    mutate(DT_round = round_date(DT_round, unit = summary_interval )) %>% # ensure DT_round is rounded to the nearest
    group_by(site, DT_round) %>%
    summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')%>%
    filter(between(DT_round, start_DT, end_DT))

  # Generate model predictions and calculate ensemble prediction
  pred_list <- map(models, ~{
    fold_model <- .x

    if (fold_model$params$objective == "multi:softprob") {
      # softprob → probabilities per class
      preds <- predict(fold_model, as.matrix(summarized_data[, fold_model$feature_names]))
      mat <- matrix(preds, ncol = num_class, byrow = TRUE)
      probs <- as_tibble(mat, .name_repair = ~paste0("class_", seq_len(num_class))) %>%
        mutate(pred_class = max.col(mat) - 1,
               max_prob = do.call(pmax, as.data.frame(mat)))

    } else {
      # softmax → direct class labels (no probabilities returned)
      pred_classes  <- predict(fold_model, as.matrix(test_df[, fold_model$feature_names]))
      # Convert to consistent tibble format (dummy probs = NA)
      probs  <- tibble(pred_class = pred_classes,  max_prob = NA_real_)
    }

    list(probs = probs)
  })

  # --- Combine model predictions depending on voting type ---
  combine_predictions <- function(df_list, voting, num_class) {
    if (voting == "soft" && "class_1" %in% names(df_list[[1]])) {
      # Average probabilities across models, then choose class with max mean prob
      prob_array <- array(
        unlist(map(df_list, ~select(.x, starts_with("class_")))),
        dim = c(nrow(df_list[[1]]), num_class, length(df_list))
      )
      mean_probs <- apply(prob_array, c(1, 2), mean)
      pred_class <- max.col(mean_probs) - 1
      tibble(
        pred_class = pred_class,
        max_prob = do.call(pmax, as.data.frame(mean_probs))
      )
    } else {
      # Hard voting — majority vote among predicted classes
      class_mat <- map(df_list, "pred_class") %>% bind_cols()
      pred_class <- apply(class_mat, 1, function(x) {
        as.integer(names(sort(table(x), decreasing = TRUE)[1]))
      })
      tibble(pred_class = pred_class, max_prob = NA_real_)
    }
  }

  # --- Combine across models ---
  ensemble_preds <- combine_predictions(map(pred_list, "probs"), voting_method, num_class)

  # --- Bind predictions back to data ---
  summarized_data <- summarized_data %>%
    mutate(!!pred_col := ensemble_preds$pred_class,
           !!paste0(pred_col, "_probs") := ensemble_preds$max_prob)

  plot_df <- summarized_data %>%
    mutate(pred_cat = factor(!!sym(pred_col),
                             levels = seq_along(class_levels) - 1,
                             labels = class_levels)
    )
  dummy_data <- expand.grid(
    DT_round = seq(from = start_DT, to = end_DT, by = summary_interval),
    pred_cat = factor(names(cat_colors), levels = class_levels)
  )
  # ---- Plot ----

  p <- ggplot() +
    # add dummy data to ensure all dates are shown but fill and color are transparent
    geom_tile(data = dummy_data, aes(x = DT_round, y = pred_cat), alpha = 0, height = 1) +
    geom_tile(data = plot_df, aes(x = DT_round, y = pred_cat, fill = pred_cat),color = "white", height = 1 ) +
    # custom fill colors for clarity
    scale_fill_manual(values = cat_colors,
                      limits = names(cat_colors),
                      drop = FALSE )+
    # overlay sample data as stars
    geom_point(
      data = plot_water_chem,
      aes(x = DT_round, y = category, shape = "Sample Data", color = "Sample Data"),
      size = 3,
      stroke = 1.1
    ) +
    scale_shape_manual(
      name = "",
      values = c("Sample Data" = 5)   # star shape (you can use 5 for diamond)
    ) +
    scale_color_manual(
      name = "",
      values = c("Sample Data" = "black")
    ) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      strip.text = element_text(face = "bold"),
    ) +
    labs(
      title = paste0("TOC Categorical Model Estimate for ", site_title, " from ", format(start_DT, "%Y/%m/%d"), "-", format(end_DT, "%Y/%m/%d")),
      subtitle = subtitle_arg,
      x = "Date",
      y = paste0("Category ", gsub("_cat", "", target_col), " (", units, ")"),
      fill = "Sample Category"
    )

  p

  # # ---- Plotly plot ----
  # plot_ly() |>
  #   # Add categorical tiles
  #   add_trace(
  #     data = df%>%filter(site == "Site_A"),
  #     x = ~datetime,
  #     y = ~category,
  #     type = "heatmap",
  #     z = ~as.numeric(category),
  #     colors = cat_colors,
  #     showscale = FALSE,
  #     hoverinfo = "text",
  #     text = ~paste("Site:", site,
  #                   "<br>Date:", datetime,
  #                   "<br>Category:", category)) |>
  #   # Add sample stars
  #   add_trace(
  #     data = sample_df%>% filter(site == "Site_A"),
  #     x = ~datetime,
  #     y = ~category,
  #     type = "scatter",
  #     mode = "markers",
  #     symbol = I("circle"),
  #     marker = list(size = 10, color = "black", line = list(color = "white", width = 1)),
  #     name = "Sample Data",
  #     hoverinfo = "text",
  #     text = ~paste("Site:", site,
  #                   "<br>Date:", datetime,
  #                   "<br>Sample Category:", category)
  #   ) |>
  #   layout(
  #     title = list(text = "TOC Categorical Model Predictions<br><sub>*synthetic data*</sub>"),
  #     xaxis = list(title = "Date"),
  #     yaxis = list(title = "Category", categoryorder = "array", categoryarray = categories),
  #     legend = list(title = list(text = ""), orientation = "v")
  #   )
}
