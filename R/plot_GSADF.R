#' Plot the result of GSADF
#'
#' This function takes the result of GSADF and returns a plot if the variable image_name is NULL
#' if it is not NULL then a pdf file will be created with the name given to image_name
#'
#' @param u Result from GSADF with drift and trend = FALSE
#' @param d Result from GSADF with drif = TRUE and trend = FALSE
#' @param d_t Result from GSADF with drift = TRUE and trend = TRUE
#' @param p_restrict Keep only intervals with p value more than
#' @param start_date_tq_get The date from from your GSADF call
#' @param image_name Name of resulting pdf file, given in character
#' @param valuta Currency of asset, given in character
#' @param aktie Name of asset, given in character
#' @return A plot if image_name = NULL, else create a pdf file with name image_name
#' @export

plot_GSADF <- function(u, d = NULL, d_t = NULL, p_restrict = 0.95, start_date_tq_get = "2020-01-01",
                       image_name = NULL, valuta = "valuta", aktie = "aktie") {

  model_nr <- dplyr::case_when(is.null(d_t) & is.null(d) ~ 1,
                        is.null(d_t) ~ 2,
                        !is.null(d_t) ~ 3)
  plot_data_combined <- combined_count(u = u, d_t = d_t, d = d, p_restrict = p_restrict)

  plot_data_rect <- u$result %>%
    dplyr::filter(p_val == max(u$result$p_val)) %>%
    dplyr::mutate(
      date_start = lubridate::ymd(start_date_tq_get) + start_day,
      date_end = lubridate::ymd(start_date_tq_get) + end_day
    )



  stock <- ggplot2::ggplot(data = u$stock, ggplot2::aes(x = date, y = price)) +
    ggplot2::geom_line() +
    ggplot2::geom_rect(
      data = plot_data_rect, aes(xmin = date_start, xmax = date_end),
      ymin = -Inf, ymax = Inf, alpha = 0.1, inherit.aes = F
    ) +
    ggplot2::scale_x_date(date_labels = "%Y %b", date_breaks = "2 month") +
    ggplot2::xlab("") +
    ggplot2::ylab(paste("Price in", valuta)) +
    ggplot2::labs(title = aktie) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1))




  bubble <- ggplot2::ggplot(data = plot_data_combined, aes(x = x, y = value, color = name)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_x_date(date_labels = "%Y %b", date_breaks = "2 month") +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) +
    ggplot2::xlab("") +
    ggplot2::ylab("Count")

  if (model_nr == 1) {
    bubble <- bubble + ggplot2::theme(legend.position = "none")
  }

  if (!is.null(image_name)) {
    file_name <- paste(image_name, ".pdf", sep = "")
    grDevices::pdf(file = file_name, height = 4,width = 8)
    gridExtra::grid.arrange(stock, bubble, nrow = 2)
    grDevices::dev.off()
    cat("image found in ", file_name)
  } else {
    gridExtra::grid.arrange(stock, bubble, nrow = 2) }
}
