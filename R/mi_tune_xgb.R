globalVariables(".data")
globalVariables(".")
#' Tune the Xgboost model by hyperband.
#' @param data A tibble.All are numeric except the first column is a factor.
#' @param resampling R6/Resampling.
#' @param measure Model evaluation method.Use `mlr_measures` and `msr()` to view and choose metrics.
#' @param eta The percent parameter configurations discarded.
#' @importFrom dplyr slice mutate pull group_by
#' @importFrom mlr3tuning tnr tune
#' @importFrom paradox ps p_dbl p_int
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn rsmp
#' @importFrom ggplot2 geom_line geom_point guides scale_x_continuous scale_y_continuous
#' @return A list of tuning instance and stage plot.
#' @export
mi_tune_xgb <- function(data, resampling = rsmp("cv", folds = 5), measure = msr("classif.acc"), eta = 3) {
  #Construct ParamSets and limit parameter ranges in a succinct and readable way.
  search_space <- ps(
    max_depth = p_int(lower = 6, upper = 12),
    subsample = p_dbl(lower = 0.8, upper = 1),
    min_child_weight = p_dbl(lower = 0.8, upper = 1.2),
    colsample_bytree = p_dbl(lower = 0.8, upper = 1),
    eta = p_dbl(lower = 0, upper = 0.3),
    nrounds = p_int(lower = 1, upper = 40, tags = "budget")
  )
  #Repeat sampling.
  data %<>% slice(sample(nrow(.), nrow(.)))
  #Initialize the xgboost learner.
  learner <- lrn("classif.xgboost", nthread = 12)
  #Convert the results of the repeated sampling into a table, and later convert it into a classification task.
  task <- data %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -c("class"),id = "tune",store_backends = TRUE)
  #Tune the xgboost learner by hyperband.
  instance <- tune(
  	tuner = tnr("hyperband", eta = 3),
    task = task,
    learner = learner,
    resampling = resampling,
    measures = measure,
    search_space = search_space
  )
  #View the results.
  result <- instance$archive$data
  #Bind the "max_depth", "subsample", "min_child_weight" and "eta" columns in the result together by column, called "hyperband", and add one to each value of the "stage" column as a new column.
  hyperband_group <- result %<>% bind_cols(hyperband = str_c(result$max_depth, result$subsample, result$min_child_weight, result$eta)) %>% mutate("stage" = .data[["stage"]] + 1)
  #Convert "hyperband" to factor type.
  fct <- hyperband_group %>%
    pull("hyperband") %>%
    factor()
  #Divide into groups and reassemble.
  result <- hyperband_group %>% split(fct)
  #Plot with ggplot2.
  p <- ggplot(data = hyperband_group, mapping = aes(x = .data[["stage"]], y = .data[["classif.acc"]], group = .data[["hyperband"]], colour = factor(.data[["hyperband"]]))) +
    scale_x_continuous(limits = c(0.5, max(hyperband_group$stage) + 0.5)) +
    scale_y_continuous(limits = c(min(hyperband_group$classif.acc), max(hyperband_group$classif.acc))) +
    theme_bw() +
    guides(color = "none") +
    geom_point() +
    geom_line()
  #Output the tuning instance and stage plot as a list.
  return(list(instance, p))
}
