#' Plot the bar plot for feature importance.
#'
#' @param data A table.
#' @importFrom tibble tibble
#' @importFrom ggplot2 scale_y_discrete geom_bar
#' @return A bar plot.
#' @export
mi_get_importance <- function(data){
	#Initialize the random forest learner and set the value of the hyperparameter.
	learner <- lrn("classif.ranger",
				   importance = "impurity"
	)
	#Convert the results of the repeated sampling into a table, and later convert it into a classification task.
	task <- data %>%
		as.data.table() %>%
		as_task_classif(target = "class", feature = -c("class"), id = "importance")
	#Divide the training set.
	train_set <- partition(task, ratio = 1)$train
	#Set the Number of Threads.
	set_threads(learner)
	#Train with a subset of data.
	learner$train(task, row_ids = train_set)
	#The $importance command is used to observe the importance of the independent variables, sorting the results in order from smallest to largest.
	importance_ = learner$importance() %>% .[order(.)]
	p <- ggplot(data = tibble(importance = importance_,feature= names(importance_))) +
		theme_bw(base_rect_size=1) +
		scale_y_discrete(factor(names(importance_),levels = names(importance_)),name = "Features")+
		geom_bar(
			stat = "identity",#it leaves the data without count and bin
			mapping = aes(x = importance, y=factor(feature,levels = feature)),
			show.legend = TRUE,
			# width = 0.9,
			fill = "#377EB8"
				# fill=hcl.colors(30, palette = "RdYlBu", alpha = NULL, rev = FALSE, fixup = TRUE) %>% rev
		) +
		theme(
			axis.text.x = element_text(
				angle = 45,
				vjust = 1, size = 41, hjust = 1,
				lineheight = 10
			),
			axis.text.y = element_text(size = 41,hjust = 0),
			strip.text.y = element_text(
				angle = 0,
				vjust = 0.5,
				hjust = -10,
				size = 13
			),
			axis.title = element_text(size = 63)
		)+
		xlab("Importance")+
		ylab("Features")+
		guides(color="none")
	return(p)
}
