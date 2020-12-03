plot_GSADF <- function(u, d = NULL, d_t = NULL, p_restrict = 0.95, start_date_tq_get = "2020-01-01",
                       image_name = NULL, valuta = "valuta", aktie = "aktie") {

  model_nr <- dplyr::case_when(is.null(d_t) & is.null(d) ~ 1,
                               is.null(d_t) ~ 2,
                               !is.null(d_t) ~ 3)
  plot_data_combined <- combined_count(u = u, d_t = d_t, d = d, p_restrict = p_restrict)

  plot_data_rect <- tibble(u$result) %>%
    dplyr::filter(p_val == max(p_val)) %>%
    dplyr::filter(interval_length == max(interval_length)) %>%
    dplyr::mutate(
      date_start = lubridate::ymd(start_date_tq_get) + start_day,
      date_end = lubridate::ymd(start_date_tq_get) + end_day
    )


  stock <- ggplot2::ggplot(data = u$stock, ggplot2::aes(x = date, y = price)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_rect(
      data = plot_data_rect, aes(xmin = date_start, xmax = date_end),
      ymin = -Inf, ymax = Inf, alpha = 0.7, inherit.aes = F
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
    gridExtra::grid.arrange(stock, bubble, nrow = 2, heights = c(3,2))
    grDevices::dev.off()
    cat("image found in ", file_name)
  } else {
    gridExtra::grid.arrange(stock, bubble, nrow = 2, heights = c(3,2))
  }
}
