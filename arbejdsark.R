

gsadf_u <- GSADF(ticker = "btc-usd",
                            x = NULL,
                            min_window = 60,
                            step_length = 10,
                            window_increase = 10,
                            date_from = "2020-01-01",
                            date_to = "2020-12-01",
                            drift = F,
                            trend = F,
                            risk_free_rate = 0.01, own_df_distribution = NULL)


tabel <- tabel_gsadf(gsadf_u)
