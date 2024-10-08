#' Compare classification models with small samples.
#' @param data A tibble.All are numeric except the first column is a factor.
#' @param row_num The number of samples used.
#' @param resamplings R6/Resampling.Resampling method.
#' @importFrom dplyr slice select across mutate
#' @importFrom tidyselect everything
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrns benchmark_grid set_threads benchmark msr rsmps
#' @export
#' @return  A list of R6 class of benchmark results and scores of test set.
#' examples
#' data(mi_data_procID)
#' mi_run_bmr(mi_data_procID)
mi_run_bmr <- function(data, row_num = 1000, resamplings = rsmps("cv", folds = 10)) {
  #All columns except the class column convert the data object to a numeric type.
  data <- data %>% mutate(across(.cols = -class, .fns = as.numeric))
  #If the overall number of data rows is less than the number of sampled samples, let the number of sampled samples be equal to the overall number of data rows.
  if (nrow(data) < row_num) {
    row_num <- nrow(data)
  }
  #Random sampling from the overall data, resampling at the corresponding number of rows after sampling is completed, converting the results into a table, and later converting it into a classification task.
  task <- data %>%
    slice(sample(nrow(data), row_num), preserve = TRUE) %>%
    as.data.table() %>%
    #Convert to a Classification Task
    as_task_classif(target = "class", feature = -c("class"), id = "bmr")
  #Initialize the learners and discriminate the probability of categorizing them as different learners.
  learners <- lrns(c("classif.naive_bayes", "classif.rpart", "classif.ranger", "classif.xgboost", "classif.kknn", "classif.multinom"),
    predict_type = "prob",
    predict_sets = c("train", "test")
  )
  #Compare multiple resampling of multiple learners for multiple tasks.
  bmr_g <- benchmark_grid(
    tasks = task,
    learners = learners,
    resamplings = resamplings
  )
  #Set the number of test threads per test process.
  set_threads(bmr_g)
  #Automatically perform resampling assessments for multiple learners and tasks.
  bmr <- benchmark(bmr_g)
  #Measure to compare true observed labels with predicted labels in multiclass classification tasks.
  measures <- msr("classif.acc")
  #A list of benchmark results and scores of test set.
  scores <- bmr$score(measures) %>%
    as.data.table() %>%
    select(ncol(.), "learner_id", everything())
  list(bmr, scores)
}
