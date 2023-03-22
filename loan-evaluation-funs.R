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

calc_loan_term <- function(P, pmt, r, pmt_freq) {
  r <- r / get_payments_per_year(pmt_freq)
  ceiling(abs(log(1 - ((P * r) / pmt)) / log(1 + r)))
}

gen_amort_tbl <- function(pmt, P, r, yrs, start_date, pmt_freq) {
  n <- calc_loan_term(P, pmt, r, pmt_freq)
  # n <- yrs * get_payments_per_year(pmt_freq)
  r <- r / get_payments_per_year(pmt_freq)
  
  amort_tbl <- data.frame(
    period_num = seq(0, n - 1),
    P = c(P, rep(NA, n - 1)),
    int_paid = rep(NA, n),
    prin_paid = rep(NA, n)
  )
  
  for (i in seq(1, n - 1)) {
    int_paid <- amort_tbl[i, "P"] * r
    prin_paid <- pmt - int_paid
    rem_prin <- amort_tbl[i, "P"] - prin_paid
    
    amort_tbl[i, "int_paid"] <- int_paid
    amort_tbl[i, "prin_paid"] <- prin_paid
    amort_tbl[i + 1, "P"] <- rem_prin
  }
  
  amort_tbl[n, "int_paid"] <- amort_tbl[n, "P"] * r
  amort_tbl[n, "prin_paid"] <- amort_tbl[n, "P"]
  
  amort_tbl <- amort_tbl |>
    mutate(
      pmt_date = map_dbl(
        period_num,
        ~ get_period_date(.x, ref_date = start_date, freq = pmt_freq)
      ) |>
        as_date()
    ) |>
    unnest(pmt_date) |>
    filter(P >= 0)
  
  return(amort_tbl)
}

gen_invest_tbl <- function(coupon, P, r, num_periods, start_date, pmt_freq) {
  
  r <- r / get_payments_per_year(pmt_freq)
  
  invstmt_tbl <- data.frame(
    period_num = seq(1, num_periods),
    P = c(P, rep(NA, num_periods - 1)),
    int_earned = rep(NA, num_periods)
  )
  
  for (i in seq(1, num_periods)) {
    invstmt_tbl[i, "int_earned"] <- (invstmt_tbl[i, "P"] * r) - coupon
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

gen_amort_plot <- function(amort_df, investment_df) {
  area_plot_data <- amort_df |>
    pivot_longer(
      c(int_paid, prin_paid),
      names_to = "metric",
      values_to = "amount"
    ) |>
    select(scenario, pmt_date, metric, amount)
  
  label_loc_data <- amort_df |>
    group_by(scenario) |>
    filter(prin_paid >= int_paid) |>
    slice_min(pmt_date) |>
    ungroup() |>
    select(scenario, pmt_date)
  
  finish_line_data <- amort_df |>
    group_by(scenario) |>
    summarise(
      total_prin_paid = sum(prin_paid, na.rm = TRUE),
      total_int_paid = sum(int_paid, na.rm = TRUE),
      payoff_date = max(pmt_date),
      .groups = "drop"
    ) |>
    select(scenario, payoff_date, total_prin_paid, total_int_paid)
  
  annotation_data <- investment_df |>
    group_by(scenario) |>
    slice_max(pmt_date) |>
    ungroup() |>
    left_join(finish_line_data, join_by(scenario)) |>
    left_join(
      select(label_loc_data, scenario, text_date = "pmt_date"),
      join_by(scenario)
    ) |>
    mutate(
      time_saved = interval(payoff_date, max(payoff_date)) |>
        as.period(unit = "days") |>
        slot("day") %>%
        `/`(360),
      net_cash_value = if_else(
        time_saved > 0,
        (P + int_earned) * ((1 + investment_interest_rate) ^ time_saved),
        P + int_earned
      ),
      net_value = net_cash_value - total_int_paid,
      across(
        c(loan_pmt, net_cash_value, total_int_paid, net_value, loan_amount),
        as.numeric
      )
    )
  
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
      aes(xintercept = payoff_date),
      linetype = "solid"
    ) +
    geom_text(
      data = annotation_data,
      aes(
        x = text_date,
        y = text_height,
        label = str_glue(
          "Monthly payment: {scales::label_dollar()(loan_pmt)}",
          "Principal : {scales::label_dollar()(loan_amount)}",
          "Payoff date: {scales::label_date()(payoff_date)}",
          "Interest rate: {scales::label_percent()(loan_interest_rate)}",
          "Total interest paid: {scales::label_dollar()(total_int_paid)}",
          "Net cash value: {scales::label_dollar()(net_cash_value)}",
          "Net value (ex-house): {scales::label_dollar()(net_value)}",
          .sep = "\n"
        )
      ),
      size = 3
    ) +
    scale_y_continuous(labels = scales::label_dollar()) +
    scale_x_date(date_breaks = "3 years", date_labels = "%b %Y") +
    facet_wrap(~ scenario, ncol = 2) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, vjust = 0.7)
    )
}
