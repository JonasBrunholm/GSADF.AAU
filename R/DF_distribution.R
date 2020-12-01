#' Simulate ADF distribution
#'
#' This function simulates the distribution for ADF
#'
#' @param T_val Sample size
#' @param seed Random seed
#' @param sd_rv Standard deviation for white noise
#' @param y_0 Starting value
#' @param n_monte Number of simulations, should be in the thousands
#' @param T_vals_only TRUE if only the T values are wanted, if FALSE more things are returned
#' @param drift FALSE if no drift
#' @param trend FALSE if no trend
#' @return ADF distribution for AR(1)
#' @export
DF_distribution <- function(T_val = 100, seed = 12345,
                      sd_rv = 1, y_0 = 1, n_monte = 10,
                      T_vals_only = T, drift = F, trend = F) {
  set.seed(seed)
  t_vector <- c()
  list_t_greater_0 <- list()

  t_val_model_nr <- dplyr::case_when(
    drift == F & trend == F ~ 1,
    drift == T & trend == F ~ 2,
    drift == F & trend == T ~ 2,
    drift == T & trend == T ~ 3
  )

  for (j in 1:n_monte) {
    y <- c()
    y[1] <- y_0
    y_fin <- c()

    for (i in 2:(T_val + 50)) {
      eps <- rnorm(1, mean = 0, sd = sd_rv)
      y[i] <- y[i - 1] + eps
    }
    y_fin <- y[(50 + 1):(T_val + 50)]
    data <- tibble::tibble(
      t = 1:T_val,
      y = y_fin
    )
    if (drift == F & trend == F) {
      model <- lm(diff(y) ~ lag(y)[2:T_val] + 0, data = data)
    }
    if (drift == T & trend == F) {
      model <- lm(diff(y) ~ na.omit(lag(y)), data = data)
    }
    if (drift == F & trend == T) {
      model <- lm(diff(y) ~ 0 + t[-1] + na.omit(dplyr::lag(y)), data = data)
    }
    if (drift == T & trend == T) {
      model <- lm(diff(y) ~ t[-1] + na.omit(dplyr::lag(y)), data = data)
    }

    t_vector[j] <- summary(model)$coefficients[t_val_model_nr, 3]
  }
  if (T_vals_only == F) {
    t_95 <- sort(t_vector, decreasing = T)[n_monte * 0.95]
    t_99 <- sort(t_vector, decreasing = T)[n_monte * 0.99]
  }

  if (T_vals_only == T) {
    list_return <- t_vector
  }
  if (T_vals_only == F) {
    list_return <- list(
      t_vector = t_vector,
      quantile_95 = t_95,
      quantile_99 = t_99,
      list_0 = list_t_greater_0
    )
  }
  return(list_return)
}
