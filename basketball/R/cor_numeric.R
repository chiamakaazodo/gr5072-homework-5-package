#' Cor numeric
#'
#' This function selects all numeric variables in a data set and return the correlation plot of the variables
#' @param data a data set
#' @param year_value a year of interest
#' @return a correlation plot of all the numeric variables of a given data set
#' @export
#' @examples
#' cor_numeric()

cor_numeric <- function(data, year_value) {
  data <- select_if(data, is.numeric)
  data <- data[data$year == year_value,]
  output <- vector("double", length(data))
  cor_data <- cor(data, use="complete.obs")
  corrplot(cor_data, type = "upper")
}
