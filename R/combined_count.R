#' Count amount of intervals each day is in
#'
#' This function counts the amount of intervals every day is in.
#'
#' @param u Result from GSADF with no drift or trend
#' @param d Result from GSADF with drift but no trend
#' @param d_t Result from GSADF with drift and trend
#' @param p_restrict Keep only observations with greater that p_restrict p value
#' @return Number of intervals a day is observed in
#' @export

combined_count <- function(u = NULL, d = NULL, d_t = NULL, p_restrict = 0.95) {
  t <- seq_along(u$stock$date)
  date <- u$stock$date

  modeltype <- dplyr::case_when(
    !is.null(u) & is.null(d) & is.null(d_t) ~ 1,
    !is.null(u) & !is.null(d) & is.null(d_t) ~ 2,
    !is.null(u) & !is.null(d) & !is.null(d_t) ~ 3
  )

  if (!is.null(u)) {
    plot_u <- u$result %>% dplyr::filter(p_val >= p_restrict)
  }
  if (!is.null(d)) {
    plot_d <- d$result %>% dplyr::filter(p_val >= p_restrict)
  }
  if (!is.null(d_t)) {
    plot_d_t <- d_t$result %>% dplyr::filter(p_val >= p_restrict)
  }

  combined_u <- c()
  combined_d <- c()
  combined_d_t <- c()

  for (i in seq_along(t)) {
    if (!is.null(u)) {
      combined_u[i] <-
        sum(i >= plot_u$start_day & i <= plot_u$end_day)
    }
    if (!is.null(d)) {
      combined_d[i] <-
        sum(i >= plot_d$start_day & i <= plot_d$end_day)
    }
    if (!is.null(d_t)) {
      combined_d_t[i] <-
        sum(i >= plot_d_t$start_day & i <= plot_d_t$end_day)
    }
  }

  if (modeltype == 1) {
    combined_df <- data.frame(x = date, u = combined_u)
  }
  if (modeltype == 2) {
    combined_df <- data.frame(x = date, u = combined_u,
                              d = combined_d)
  }
  if (modeltype == 3) {
    combined_df <- data.frame(x = date, u = combined_u,
                              d = combined_d,
                              d_t = combined_d_t)
  }
  return(combined_df %>% tidyr::pivot_longer(-1) %>%
           dplyr::mutate(name = factor(name, levels = c("u", "d", "d_t"))))
}
