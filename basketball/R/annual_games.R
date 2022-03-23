#' Annual games
#'
#' This function returns the total number of games played for a given year for the NBA player Giannis Antentokounmpo.
#' @param data a data set
#' @param year_value a year of interest
#' @return total number of games played for the given year
#' @export
#' @examples
#' annual_games()

annual_games <- function(data, year_value) {
  data %>%
    group_by(player) %>%
    select(g, year) %>%
    filter(player == "Giannis Antetokounmpo" & year == year_value)

}
