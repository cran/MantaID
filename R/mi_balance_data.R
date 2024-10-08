#' Data balance.
#' Most classes adopt random undersampling, while a few classes adopt smote method to oversample to obtain relatively balanced data.
#' @param data A data frame. Except for class column, all are numeric types.
#' @param ratio Numeric between 0 and 1. The percent of the test set split from data.
#' @param parallel Logical.
#' @return A list contains a train set and a test set.
#' @export
#' @importFrom scutr SCUT_parallel SCUT oversample_smote resample_random
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif partition
#' @importFrom dplyr slice bind_rows
#' @importFrom magrittr set_names
mi_balance_data <- function(data, ratio = 0.3, parallel = FALSE) {
  system.time({
    #The multicategorical data were balanced by random sampling of the undersampled samples and the oversampling part was sampled by the smote method to obtain relatively balanced data. Speedup usually occurs only if there are many classes using one of the slower resampling techniques.
    if (parallel) {
      data_smtd <- SCUT_parallel(data, "class", oversample = oversample_smote, undersample = resample_random)
    } else {
      data_smtd <- SCUT(data, "class", oversample = oversample_smote, undersample = resample_random)
    }
  })
  #Difference between the balanced data set and the pre-balanced data set, as the training set.
  train_new <- setdiff(data_smtd, data)
  #Create a classification task.
  task <- data %>%
    as.data.table() %>%
    as_task_classif(target = "class", feature = -"class")
  #A portion of the original data set is also used as the training set.
  train_raw <- partition(task, ratio = 1 - ratio)$train %>% slice(data, .)
  #A split of the original data set plus a new sample obtained by sampling as the training set.
  train <- bind_rows(train_raw, train_new)
  #Test set data from a split of the original data set.
  test <- partition(task, ratio = 1 - ratio)$test %>% slice(data, .)
  #Return a list of training and test sets.
  return(list(train, test))
}
