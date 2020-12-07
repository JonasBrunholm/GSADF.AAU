#' Tabel GSADF
#'
#' This function summarises the largest p-values of the result of GSADF
#'
#' @param gsadf Output from GSADF
#'
#' @return Summary of largest p-value
#' @export



Tabel_GSADF <- function(gsadf) {
  max_data <- tibble::tibble(gsadf$result) %>% dplyr::filter(p_val ==
                                                               base::max(p_val))
  start_date <- gsadf$stock$date[1]
  count <- max_data %>% base::nrow()
  value <- max_data$p_val[1]
  interval_longest <- max_data %>% dplyr::slice_max(interval_length)
  if (interval_longest$start_day == 0) {
    interval_longest$start_day <- 1
  }
  interval_longest_values <- base::c(gsadf$stock$date[interval_longest$start_day],
                                     gsadf$stock$date[interval_longest$end_day])
  start_day_int_long <- interval_longest$start_day
  if (interval_longest$start_day == 0) {
    start_day_int_long <- 1
  }


  price_change_longest <- ((gsadf$stock$price[interval_longest$end_day] -
                              gsadf$stock$price[start_day_int_long]) / gsadf$stock$price[base::max(
                                interval_longest$start_day,
                                1
                              )]) * 100
  price_change_longest <- base::format(base::round(
    price_change_longest,
    2
  ), nsmall = 2)
  min_start_day_id <- max_data$start_day
  min_start_day_id <- dplyr::ifelse(min_start_day_id == 0, min_start_day_id + 1, min_start_day_id)
  min_startday <- gsadf$stock$date[base::min(min_start_day_id)]
  max_endday <- gsadf$stock$date[base::max(max_data$end_day)]
  output <- base::list(
    count = count, value = value, interval_longest = interval_longest_values,
    price_change_longest = base::paste(price_change_longest, "%",
                                       sep = " "
    ), min_startday = min_startday, max_endday = max_endday
  )

  base::return(output)
}
