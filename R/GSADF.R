#' GSADF
#'
#' This function is a implementation of GSADF as described by PSY
#'
#' @param ticker Ticker code from yahoo finance of the asset
#' @param x vector of prices if no ticker code is available
#' @param min_window Minimum size of the window to test for unit root
#' @param step_length The size that the window will move for every step
#' @param window_increase How much the window size should increase every iteration
#' @param date_from Date from
#' @param date_to Date to
#' @param drift TRUE if drift, FALSE if no drift
#' @param trend TRUE if trend, FALSE if no trend
#' @param risk_free_rate Numeric value giving the yearly inflation
#' @param own_df_distribution If Null, will use simulated distributions in package, can input your own, generated from DF_distribution
#' @return list with with "stock" containing the price of the asset and "result" containing
#' the result of the GSADF, meaning a a tibble containing interval lengths, estimates,
#'  p values and more
#' @export

GSADF <- function(ticker, x = NULL, min_window = 30, step_length = 5,
                  window_increase = 10, date_from = "1900-01-01", date_to = base::Sys.Date(),
                  drift = F, trend = F, risk_free_rate = 0.01, own_df_distribution = NULL) {
  if (base::is.null(x)) {
    stock_data <- tidyquant::tq_get(ticker,
      from = date_from,
      to = date_to
    ) %>%
      dplyr::select(date, adjusted) %>%
      stats::na.omit()
    x <- discount(p = stock_data$adjusted, r = risk_free_rate)
    date_x <- stock_data$date
  }
  nrow_x <- base::length(x)
  window_size <- min_window
  t_val_model_nr <- dplyr::case_when(drift == F & trend ==
    F ~ 1, drift == T & trend == F ~ 2, drift == T & trend ==
    T ~ 3)

  load("df_distri.rda")
  load("df_distri_drift.rda")
  load("df_distri_drift_trend.rda")

  if (base::is.null(own_df_distribution)) {
    distribution_tibble <- if (t_val_model_nr == 1) {
      df_distri
    }
    else if (t_val_model_nr == 2) {
      df_distri_drift
    }
    else if (t_val_model_nr == 3) {
      df_distri_drift_trend
    }
  }
  else {
    distribution_tibble <- own_df_distribution
  }
  result <- tibble::tibble()
  k_ind <- 1
  while (window_size < (nrow_x - step_length)) {
    max_i <- base::floor((nrow_x - window_size) / step_length)
    for (i in 0:max_i) {
      x_window <- x[(step_length * i):((step_length *
        i) + window_size)]
      plot_data <- tibble::tibble(
        t = base::seq_along(x_window),
        x_window
      )
      if (drift == F & trend == F) {
        model <- stats::lm(base::diff(x_window) ~ 0 +
          stats::na.omit(dplyr::lag(x_window)), data = plot_data)
      }
      if (drift == T & trend == F) {
        model <- stats::lm(base::diff(x_window) ~ stats::na.omit(dplyr::lag(x_window)),
          data = plot_data
        )
      }
      if (drift == T & trend == T) {
        model <- stats::lm(base::diff(x_window) ~ t[-1] +
          stats::na.omit(dplyr::lag(x_window)), data = plot_data)
      }
      coef_model <- stats::coefficients(base::summary(model))
      result[k_ind, "estimate"] <- coef_model[
        t_val_model_nr,
        1
      ]
      result[k_ind, "std_error"] <- coef_model[
        t_val_model_nr,
        2
      ]
      result[k_ind, "t_value"] <- coef_model[
        t_val_model_nr,
        3
      ]
      result[k_ind, "start_day"] <- step_length * i
      result[k_ind, "end_day"] <- ((step_length * i) +
        window_size)
      result[k_ind, "interval_length"] <- window_size
      k_ind <- k_ind + 1
    }
    window_size <- window_size + window_increase
  }
  result <- result %>%
    dplyr::rowwise() %>%
    dplyr::mutate(p_val = p_val_fun(
      t_val = t_value,
      interval_length = interval_length, distribution_tibble = distribution_tibble
    ))
  result_list <- base::list(stock = tibble::tibble(
    date = date_x,
    price = x
  ), result = result)
  base::return(result_list)
}
