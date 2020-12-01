#' Discount prices
#'
#' This function discounts prices to the first observation by the risk free rate(inflation)
#'
#' @param p Price
#' @param r Inflation rate
#' @return discounted price
#' @export

discount <- function(p, r = 0.01) {
  p / (1 + r / 365)^(seq_along(p))
}
