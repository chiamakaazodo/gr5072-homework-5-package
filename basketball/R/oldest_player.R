#' Oldest player
#'
#' This function  returns the oldest player for any given year, if there is a tie, then alphabetical order is used as a deciding factor.
#' @param data a data set
#' @param year_value a year of interest
#' @return oldest player for a given year
#' @export
#' @examples
#' oldest_player()

oldest_player <- function(data, year_value) {
  data %>%
    group_by(year) %>%
    select(player, age, year) %>%
    filter(age == max(age) & year == year_value) %>%
    arrange(player) %>%
    slice(1)

}
