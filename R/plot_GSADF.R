#' Plot function for result of GSADF
#'
#' @param u No drift no trend
#' @param d Drift
#' @param d_t Drift and Trend
#' @param p_restrict Only accepts p values larger than
#' @param start_date_tq_get Start date from GSADF
#' @param image_name If not null then makes a pdf with the name
#' @param valuta Currency
#' @param aktie Asset
#'
#' @return
#' @export
#'
plot_GSADF <- function(u, d = NULL, d_t = NULL, p_restrict = 0.95, start_date_tq_get = "2020-01-01",
                       image_name = NULL, valuta = "valuta", aktie = "aktie") {
  model_nr <- dplyr::case_when(is.null(d_t) & is.null(d) ~
  1, is.null(d_t) ~ 2, !is.null(d_t) ~ 3)

  gsadf_result <- if (model_nr == 1) {
    u
  } else if (model_nr == 2) {
    d
  } else if (mode_nr == 3) {
    d_t
  }
  stock_data <- gsadf_result$stock
  plot_data_combined <- combined_count(
    u = u, d_t = d_t, d = d,
    p_restrict = p_restrict
  )
  plot_data_rect <- tibble::tibble(u$result) %>%
    dplyr::filter(p_val ==
      max(p_val)) %>%
    dplyr::filter(interval_length == max(interval_length)) %>%
    dplyr::mutate(
      date_start = stock_data$date[start_day],
      date_end = stock_data$date[end_day]
    )
  stock <- ggplot2::ggplot(data = u$stock, ggplot2::aes(
    x = date,
    y = price
  )) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_rect(
      data = plot_data_rect,
      ggplot2::aes(xmin = date_start, xmax = date_end), ymin = -Inf,
      ymax = Inf, alpha = 0.7, inherit.aes = F
    ) +
    ggplot2::scale_x_date(
      date_labels = "%Y %b",
      date_breaks = "2 month"
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab(paste(
      "Price in",
      valuta
    )) +
    ggplot2::labs(title = aktie) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 60,
      hjust = 1
    ))
  bubble <- ggplot2::ggplot(data = plot_data_combined, ggplot2::aes(
    x = x,
    y = value, color = name
  )) +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_x_date(date_labels = "%Y %b", date_breaks = "2 month") +
    ggplot2::theme(legend.position = "bottom", axis.text.x = ggplot2::element_text(
      angle = 60,
      hjust = 1
    )) +
    ggplot2::xlab("") +
    ggplot2::ylab("Count")
  if (model_nr == 1) {
    bubble <- bubble + ggplot2::theme(legend.position = "none")
  }
  if (!is.null(image_name)) {
    file_name <- paste(image_name, ".pdf", sep = "")
    grDevices::pdf(file = file_name, height = 6, width = 8)
    gridExtra::grid.arrange(stock, bubble, nrow = 2, heights = c(
      3,
      2
    ))
    grDevices::dev.off()
    cat("image found in ", file_name)
  }
  else {
    gridExtra::grid.arrange(stock, bubble, nrow = 2, heights = c(
      3,
      2
    ))
  }
}
