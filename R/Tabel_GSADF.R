#' Tabel GSADF
#'
#' This function summarises the largest p-values of the result of GSADF
#'
#' @param gsadf Output from GSADF
#'
#' @return Summary of largest p-value
#' @export




Tabel_GSADF <- function(gsadf) {
max_data <- tibble::tibble(gsadf$result) %>%
  dplyr::filter(p_val == base::max(p_val))
start_date <- gsadf$stock$date[1]
count <- max_data %>% base::nrow()
value <- max_data$p_val[1]
interval_longest <- max_data %>% dplyr::slice_max(interval_length)
interval_longest_values <- c(interval_longest$start_day + lubridate::ymd(start_date),
                             interval_longest$end_day + lubridate::ymd(start_date))

price_change_longest <- ((gsadf$stock$price[interval_longest$end_day] -
                           gsadf$stock$price[base::max(interval_longest$start_day,1)]) /
  gsadf$stock$price[base::max(interval_longest$start_day,1)]) * 100

price_change_longest <- format(round(price_change_longest,2), nsmall = 2)

# first_date_data <- max_data %>% dplyr::slice_min(start_day)
# first_date_interval <- lubridate::ymd(start_date) +
#   base::c(first_date_data$start_day, first_date_data$end_day)
#
# last_date_data <- max_data %>% dplyr::slice_max(end_day)
# last_date_interval <- lubridate::ymd(start_date) +
#   base::c(last_date_data$start_day, last_date_data$end_day)


mean_startday <- base::mean(max_data$start_day) + lubridate::ymd(start_date)
mean_endday <- base::mean(max_data$end_day) + lubridate::ymd(start_date)

output <- base::list(count = count,
                     value = value,
                     interval_longest = interval_longest_values,
                     price_change_longest = paste(price_change_longest, "%", sep = " "),
                     # first_date_interval = first_date_interval,
                     # last_date_interval = last_date_interval,
                     mean_startday = mean_startday,
                     mean_endday = mean_endday)

base::return(output)
# hvor mange
# hvad den er
# interval på det længste
# price change i procent
# første dato helt interval
# sidste dato helt interval

}


