#' Calculate the p value
#'
#' This function is used in GSADF to calculate the p value based on the simulated distribution
#' found from DF_distribution
#'
#' @param t_val T value from lm call
#' @param interval_length The length of the interval
#' @param distribution_tibble The result from DF_distribution
#' @return The p value as in P(x > X)
#' @export

p_val_fun <- function(t_val, interval_length, distribution_tibble) {
  if (interval_length %% 10 == 0) {
    sum(t_val > distribution_tibble[
      ,
      paste("T_val_", interval_length, sep = "")
      ]) /
      nrow(distribution_tibble)
  } else
    if (interval_length %% 10 != 0) {
      column <- paste("T_val_", plyr::round_any(interval_length, 10), sep = "")
      sum(t_val > distribution_tibble[, column]) / nrow(distribution_tibble)
    }
}
