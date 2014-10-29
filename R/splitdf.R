#' This will randomly split a data frame into training and test sets. I 
#'   borrowed it from \href{http://gettinggeneticsdone.blogspot.com/2011/02/split-data-frame-into-testing-and.html}{here}.
#'   The caret packages has a similar facility using its createDataPartition() 
#'   function. Perhaps even more appealing, there is sample.split() in caTools.
#'
#' @param dataframe This is the data frame you want to split.
#' @param weight The weights for the different classes.
#' @param seed An optional seed.
#' @return A list with two members; the training set and the test set.
splitdf <- function(
dataframe, 
weight = 2/3, 
seed=NULL
) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index) * weight))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}
