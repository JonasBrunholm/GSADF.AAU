% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_GSADF.R
\name{plot_GSADF}
\alias{plot_GSADF}
\title{Plot function for result of GSADF}
\usage{
plot_GSADF(
  u = NULL,
  d = NULL,
  d_t = NULL,
  p_restrict = 0.95,
  start_date_tq_get = "2020-01-01",
  image_name = NULL,
  valuta = "valuta",
  aktie = "aktie"
)
}
\arguments{
\item{u}{No drift no trend}

\item{d}{Drift}

\item{d_t}{Drift and Trend}

\item{p_restrict}{Only accepts p values larger than}

\item{start_date_tq_get}{Start date from GSADF}

\item{image_name}{If not null then makes a pdf with the name}

\item{valuta}{Currency}

\item{aktie}{Asset}
}
\value{

}
\description{
Plot function for result of GSADF
}
