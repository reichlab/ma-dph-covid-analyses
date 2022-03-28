library(tidyverse)
library(covidHubUtils)
theme_set(theme_bw())

inc_hosp_targets <- paste(0:130, "day ahead inc hosp")

models <- list.dirs(
  "validation_forecasts",
  full.names = FALSE,
  recursive = FALSE)

forecasts <- load_forecasts(
#  models = "report_final_VAR",
  # models = models,
  # dates = "2021-03-08",
  # dates = '2020-12-07',
  date_window_size = 6,
  locations = c("25"),
  types = c("point", "quantile"),
  targets = inc_hosp_targets,
  source = "local_hub_repo",
  hub_repo_path = "validation_forecasts",
  data_processed_subpath = "",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)

hub_forecasts <- load_forecasts(
  models = c("COVIDhub-baseline", "COVIDhub-ensemble"),
  dates = unique(forecasts$forecast_date),
  date_window_size = 6,
  locations = c("25"),
  types = c("point", "quantile"),
  targets = inc_hosp_targets,
  source = "local_hub_repo",
  hub_repo_path = "../covid19-forecast-hub",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)
# combined_forecasts <- forecasts
combined_forecasts <- dplyr::bind_rows(forecasts, hub_forecasts)

combined_forecasts %>%
  dplyr::filter(type == "quantile") %>%
  dplyr::count(model) %>%
  as.data.frame()

# forecasts <- covidHubUtils::align_forecasts(forecasts)

# extract_variation <- function(model_name, var_name) {
#   ind <- regexpr(var_name, model_name) + nchar(var_name) + 1
#   model_substr <- substr(model_name, ind, nchar(model_name))
#   ind <- regexpr("_", model_substr)
#   substr(model_substr, 1, ifelse(ind == -1, nchar(model_substr), ind - 1))
# }

truth_data <- load_truth(
  truth_source = "HealthData",
  target_variable = "inc hosp",
  locations = c("25"),
  hub = "US"
)

truth_data <- truth_data %>%
  dplyr::filter(target_end_date >= "2020-10-01")

scores <- covidHubUtils::score_forecasts(
  combined_forecasts,
  truth_data %>% filter(target_end_date <= "2021-06-12"),
  use_median_as_point = TRUE)

mean_scores <- scores %>%
  filter(!(forecast_date %in% c("2020-12-07", "2020-12-14", "2021-02-01", "2021-03-15"))) %>%
  group_by(model) %>%
  summarize(wis = mean(wis), coverage_95 = mean(coverage_95)) %>%
  arrange(wis) %>%
  as.data.frame()
mean_scores

# plot the forecasts
models_to_plot <- mean_scores %>%
  filter(wis < 25) %>%
  pull(model)

forecasts_to_plot <- covidHubUtils::get_plot_forecast_data(
  forecast_data = combined_forecasts,
  truth_data = truth_data,
  # models_to_plot = unique(combined_forecasts$model),
  models_to_plot = models_to_plot,
  horizons_to_plot = 28,
  quantiles_to_plot = c(0.025, 0.5, 0.975),
  # quantiles_to_plot = c(0.025, 0.25, 0.5, 0.75, 0.975),
  target_variable_to_plot = "inc hosp",
  hub = "US")

forecasts_to_plot <- forecasts_to_plot %>%
  dplyr::filter(!grepl("Observed Data", model))

p <- ggplot() +
  # geom_ribbon(
  #   data = forecasts_to_plot %>%
  #     dplyr::filter(!is.na(`Prediction Interval`)),
  #   mapping = aes(
  #     x = target_end_date,
  #     ymin = lower,
  #     ymax = upper,
  #     alpha = `Prediction Interval`,
  #     group = paste0(model, forecast_date)
  #   ),
  #   fill = "cornflowerblue"
  # ) +
  # scale_alpha_manual(
  #   values = c("95%" = 0.5)
  # ) +
  geom_line(
    data = forecasts_to_plot %>%
      dplyr::filter(is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      y = point,
      group = paste0(model, forecast_date)
    ),
    color = "blue"
  ) +
  geom_line(
    data = truth_data %>% dplyr::select(-model) %>% dplyr::filter(target_end_date < "2021-07-01"),
    mapping = aes(x = target_end_date, y = value)
  ) +
  ylim(0, 500) +
  facet_wrap( ~ model, scales = "free_y", ncol = 2)
p


models_to_plot <- mean_scores %>%
  filter(wis < 22.8) %>%
  pull(model)

forecasts_to_plot <- covidHubUtils::get_plot_forecast_data(
  forecast_data = combined_forecasts,
  truth_data = truth_data,
  # models_to_plot = unique(combined_forecasts$model),
  models_to_plot = models_to_plot,
  horizons_to_plot = 28,
  quantiles_to_plot = c(0.025, 0.5, 0.975),
  # quantiles_to_plot = c(0.025, 0.25, 0.5, 0.75, 0.975),
  target_variable_to_plot = "inc hosp",
  hub = "US")

forecasts_to_plot <- forecasts_to_plot %>%
  dplyr::filter(!grepl("Observed Data", model))

p <- ggplot() +
  # geom_ribbon(
  #   data = forecasts_to_plot %>%
  #     dplyr::filter(!is.na(`Prediction Interval`)),
  #   mapping = aes(
  #     x = target_end_date,
  #     ymin = lower,
  #     ymax = upper,
  #     alpha = `Prediction Interval`,
  #     group = paste0(model, forecast_date)
  #   ),
  #   fill = "cornflowerblue"
  # ) +
  # scale_alpha_manual(
  #   values = c("95%" = 0.5)
  # ) +
  geom_line(
    data = truth_data %>% dplyr::select(-model) %>% dplyr::filter(target_end_date < "2021-07-01"),
    mapping = aes(x = target_end_date, y = value)
  ) +
  geom_line(
    data = forecasts_to_plot %>%
      dplyr::filter(is.na(`Prediction Interval`)),
    mapping = aes(
      x = target_end_date,
      y = point,
      color = model,
      group = paste0(model, forecast_date)
    ),
    size=1
  ) +
  # ylim(0, 500) +
  facet_wrap( ~ forecast_date, scales = "free_y")
p


# covidHubUtils::plot_forecasts(
#     forecast_data = stl_forecasts,
#     facet = formula(trend_w~location+trend_deg),
#     hub = "US",
#     truth_source = "HealthData",
#     subtitle = "none",
#     title = "none",
#     show_caption = FALSE,
#     plot = FALSE
#   ) +
#   scale_x_date(
#     breaks = "1 month",
#     date_labels = "%b-%y",
#     limits = as.Date(c(
#       reference_date - (7 * 32), reference_date + 28
#     ), format = "%b-%y")
#   ) +
#   theme_update(
#     legend.position = "bottom",
#     legend.direction = "vertical",
#     legend.text = element_text(size = 8),
#     legend.title = element_text(size = 8),
#     axis.text.x = element_text(angle = 90),
#     axis.title.x = element_blank()
#   ) +
#   ggforce::facet_wrap_paginate(
#     ~ location,
#     scales = "free",
#     ncol = 2,
#     nrow = 3,
#     page = 1
#   )

# n <- n_pages(p)
# pdf(
#   plot_path,
#   paper = 'A4',
#   width = 205 / 25,
#   height = 270 / 25
# )
# for (i in 1:n) {
#   suppressWarnings(print(
#     p + ggforce::facet_wrap_paginate(
#       ~ location,
#       scales = "free",
#       ncol = 2,
#       nrow = 3,
#       page = i
#     )
#   ))
# }
# dev.off()

