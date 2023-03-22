get_period_date <- function(ref_date, num_periods, freq) {
  switch(
    freq,
    "annual" = ref_date + years(num_periods),
    "quarterly" = ref_date + months(num_periods * 3),
    "monthly" = ref_date + months(num_periods),
    "biweekly" = ref_date + weeks(num_periods * 2),
    "weekly" = ref_date + weeks(num_periods),
    "daily" = ref_date + days(num_periods)
  )
}

get_payments_per_year <- function(freq) {
  switch(
    freq,
    "annual" = 1,
    "quarterly" = 4,
    "monthly" = 12,
    "biweekly" = 26,
    "weekly" = 52,
    "daily" = 365
  )
}

calc_loan_pmt <- function(P, r, yrs, pmt_freq) {
  r <- r / get_payments_per_year(pmt_freq)
  n <- yrs * get_payments_per_year(pmt_freq)
  P * r * ((r + 1) ^ (n)) / (((r + 1) ^ n) - 1)
}

calc_npv <- function(P, r, num_periods) {
  r <- r / get_payments_per_year(pmt_freq)
  
}

calc_loan_term <- function(P, pmt, r, pmt_freq) {
  r <- r / get_payments_per_year(pmt_freq)
  ceiling(abs(log(1 - ((P * r) / pmt)) / log(1 + r)))
}

gen_amort_tbl <- function(pmt, P, r, yrs, start_date, pmt_freq) {
  n <- calc_loan_term(P, pmt, r, pmt_freq)
  r <- r / get_payments_per_year(pmt_freq)
  
  amort_tbl <- data.frame(
    period_num = seq(1, n),
    P = c(P, rep(NA, n - 1)),
    int_paid = rep(NA, n),
    prin_paid = rep(NA, n),
    int_npv = rep(NA, n),
    prin_npv = rep(NA, n)
  )
  
  for (i in seq(1, n - 1)) {
    int_paid <- amort_tbl[i, "P"] * r
    prin_paid <- pmt - int_paid
    rem_prin <- amort_tbl[i, "P"] - prin_paid
    
    amort_tbl[i, "int_paid"] <- int_paid
    amort_tbl[i, "prin_paid"] <- prin_paid
    amort_tbl[i, "int_npv"] <- amort_tbl[i, "int_paid"] /
      (1 + r) ** amort_tbl[i, "period_num"]
    amort_tbl[i, "prin_npv"] <- amort_tbl[i, "prin_paid"] /
      (1 + r) ** amort_tbl[i, "period_num"]
    amort_tbl[i + 1, "P"] <- rem_prin
  }
  
  amort_tbl[n, "int_paid"] <- amort_tbl[n, "P"] * r
  amort_tbl[n, "prin_paid"] <- amort_tbl[n, "P"]
  amort_tbl[n, "int_npv"] <- amort_tbl[n, "int_paid"] /
    (1 + r) ** amort_tbl[n, "period_num"]
  amort_tbl[n, "prin_npv"] <- amort_tbl[n, "prin_paid"] /
    (1 + r) ** amort_tbl[n, "period_num"]
  
  amort_tbl <- amort_tbl |>
    mutate(
      pmt_date = map_dbl(
        period_num,
        ~ get_period_date(.x, ref_date = start_date, freq = pmt_freq)
      ) |>
        as_date()
    ) |>
    unnest(pmt_date) |>
    filter(P >= 0.01)
  
  return(amort_tbl)
}

gen_invest_tbl <- function(coupon, P, r, num_periods, start_date, pmt_freq) {
  
  r <- r / get_payments_per_year(pmt_freq)
  
  invstmt_tbl <- data.frame(
    period_num = seq(1, num_periods),
    P = c(P, rep(NA, num_periods - 1)),
    int_earned = rep(NA, num_periods),
    investment_int_npv = rep(NA, num_periods)
  )
  
  for (i in seq(1, num_periods)) {
    invstmt_tbl[i, "int_earned"] <- (invstmt_tbl[i, "P"] * r) - coupon
    invstmt_tbl[i, "investment_int_npv"] <- invstmt_tbl[i, "int_earned"] /
      (1 + r) ** invstmt_tbl[i, "period_num"]
    invstmt_tbl[i + 1, "P"] <- invstmt_tbl[i, "P"] +
      invstmt_tbl[i, "int_earned"]
  }
  
  invstmt_tbl <- invstmt_tbl |>
    mutate(
      pmt_date = map_dbl(
        period_num,
        ~ get_period_date(.x, ref_date = start_date, freq = pmt_freq)
      ) |>
        as_date()
    ) |>
    unnest(pmt_date)
  
  return(invstmt_tbl)
}

gen_amort_plot <- function(amort_df, investment_df, summary_df) {
  area_plot_data <- amort_df |>
    pivot_longer(
      c(int_paid, prin_paid),
      names_to = "metric",
      values_to = "amount"
    ) |>
    select(scenario, pmt_date, metric, amount) |>
    mutate(
      metric = str_replace(metric, "int", "interest") |>
        str_replace("prin", "principal") |>
        str_replace_all("_", " ") |>
        str_to_title()
    )
  
  label_loc_data <- amort_df |>
    group_by(scenario) |>
    filter(prin_paid >= int_paid) |>
    slice_min(pmt_date) |>
    ungroup() |>
    select(scenario, pmt_date)
  
  finish_line_data <- summary_df |>
    select(scenario, scenario_end_date)
  
  text_height = max(area_plot_data$amount) * .85
  
  ggplot() +
    geom_area(
      data = area_plot_data,
      aes(
        x = pmt_date,
        y = amount,
        group = metric,
        fill = metric,
        color = metric
      ),
      alpha = 0.5, position = "identity"
    ) +
    geom_line(
      data = investment_df,
      aes(x = pmt_date, y = int_earned),
      color = "blue"
    ) +
    geom_vline(
      data = finish_line_data,
      aes(xintercept = scenario_end_date),
      linetype = "solid"
    ) +
    geom_text(
      data = summary_df,
      aes(
        x = text_date,
        y = text_height,
        label = str_glue(
          "Monthly payment: {scales::label_dollar()(loan_pmt)}",
          "Payoff date: {scales::label_date()(scenario_end_date)}",
          "Total interest paid (PV): {scales::label_dollar()(npv_int_paid)}",
          "Total interest earned: (PV) {scales::label_dollar()(investment_int_npv)}",
          "Net value (ex-house): {scales::label_dollar()(cash_npv)}",
          .sep = "\n"
        )
      ),
      size = 5
    ) +
    scale_y_continuous(labels = scales::label_dollar()) +
    scale_x_date(date_breaks = "3 years", date_labels = "%b %Y") +
    facet_wrap(~ scenario, ncol = 1, scales = "free") +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, vjust = 0.7),
      strip.text = element_text(size = 18),
      legend.text = element_text(size = 14),
      axis.text = element_text(size = 14),
      legend.title = element_blank()
    )
}
