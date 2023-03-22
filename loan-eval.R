library(tidyverse)

calc_prin_int <- function(P, r, n) {
  P * r * ((r + 1) ^ (n)) / (((r + 1) ^ n) - 1)
}

calc_loan_term <- function(P, pmt, r) {
  ceiling(abs(log(1 - ((P * r) / pmt)) / log(1 + r)))
}

gen_amort_tbl <- function(pmt, P, r, n, start_date, pmt_freq) {
  amort_tbl <- data.frame(
    period = seq(0, n - 1),
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
    pmt_date = case_when(
      pmt_freq %in% "monthly" ~ start_date + months(period),
      pmt_freq %in% "biweekly" ~ start_date + weeks(period * 2)
    )
  )
  
  return(amort_tbl)
}

gen_invest_tbl <- function(coupon, P, r, n, start_date, pmt_freq) {
  coupon_tbl <- data.frame(
    period = seq(1, n),
    P = c(P, rep(NA, n - 1)),
    int_earned = rep(NA, n)
  )
  
  for (i in seq(1, n)) {
    coupon_tbl[i, "int_earned"] <- coupon_tbl[i, "P"] * r
    coupon_tbl[i + 1, "P"] <- coupon_tbl[i, "P"] +
      coupon_tbl[i, "int_earned"] -
      coupon
  }
  
  coupon_tbl <- coupon_tbl |>
    mutate(
      pmt_date = case_when(
        pmt_freq %in% "monthly" ~ start_date + months(period),
        pmt_freq %in% "biweekly" ~ start_date + weeks(period * 2),
        pmt_freq %in% "annual" ~ start_date + years(period)
      )
    )
  
  return(coupon_tbl[1:n, ])
}

monthly_house_cost <- function(annual_pi, ann_tax, ann_hoa, ann_ins) {
  (annual_pi + ann_ins + ann_hoa + ann_tax) / 12
}

calc_prin_int(292929, 0.065/12, 360) %>% `*`(12) |>
  monthly_house_cost(292929 * 0.0281, 750, 1500)


calc_prin_int(292929, 0.065/12, 360) |>
  gen_amort_tbl(292929, 0.065/12, 360, ymd("2023-06-01"), "biweekly") |>
  View()

home_price <- 292929
funding_fee <- 6342
available_cash <- 100000
incentive <- 15000
bond_int <- 0.04

loan_scenarios <- tibble(
  scenario_index = 1:6,
  scenario = c(
    "Base loan",
    "Apply incentive to principal",
    "Apply incentive and all cash to principal, and extra payment",
    "Apply incentive to the funding fee and the rest to buy points",
    "Use full incentive for points",
    "Use full incentive for points, and add extra payment"
  ),
  principal = c(
    home_price + funding_fee,
    home_price + funding_fee - incentive,
    home_price + funding_fee - incentive - available_cash,
    home_price,
    home_price + funding_fee,
    home_price + funding_fee
  ),
  liquid_capital = c(100000, 100000, 0, 100000, 100000, 100000),
  mort_int = c(
    0.0674,
    0.0674,
    0.0674,
    0.0674 - ((incentive - funding_fee) / (home_price * 0.01) * .0025),
    0.0674 - ((incentive) / ((home_price + funding_fee) * 0.01) * .0025),
    0.0674 - ((incentive) / ((home_price + funding_fee) * 0.01) * .0025)
  ) /
    12,
  extra_pmt = c(0, 0, 500, 0, 0, 200),
  coupon = c(0, 0, 0, 0, 0, 200),
  loan_term = rep(360, 6),
  # payment_frequency = rep("monthly", 6)
) |>
  mutate(
    monthly_pmt = pmap_dbl(
      list(principal, mort_int, loan_term),
      calc_prin_int
    ),
    monthly_pmt = monthly_pmt + extra_pmt,
    loan_periods = pmap(list(principal, monthly_pmt, mort_int), calc_loan_term),
    amortization = pmap(
      list(monthly_pmt, principal, mort_int, loan_periods),
      gen_amort_tbl,
      start_date = ymd("2023-06-01"),
      pmt_freq = "monthly"
    ),
    interest_earnings = pmap(
      list(coupon, liquid_capital, loan_periods),
      gen_coupon_tbl,
      r = bond_int / 12,
      start_date = ymd("2023-06-01"),
      pmt_freq = "monthly"
    ),
    scenario = fct_reorder(scenario, scenario_index)
  )

loan_scenarios <- tibble(
  scenario_index = 1:3,
  scenario = c(
    "First Choice",
    "First Choice - Pay Extra Points",
    "Mr. Cooper"
  ),
  principal = c(rep(home_price + funding_fee, 3)),
  liquid_capital = c(
    100000,
    100000 - (home_price + funding_fee) * 0.0194782,
    100000 - (home_price + funding_fee) * 0.03
  ),
  mort_int = c(
    0.0674 - ((incentive) / ((home_price + funding_fee) * 0.01) * .0025),
    0.0674 - ((incentive) / ((home_price + funding_fee) * 0.01) * .0025) -
      (1.94782 * 0.0025),
    0.0575 - (3 * 0.0025)
  ) /
    12,
  extra_pmt = c(0, 0, 0),
  coupon = c(0, 0, 0),
  loan_term = rep(360, 3),
  # payment_frequency = rep("monthly", 6)
) |>
  mutate(
    monthly_pmt = pmap_dbl(
      list(principal, mort_int, loan_term),
      calc_prin_int
    ),
    monthly_pmt = monthly_pmt + extra_pmt,
    loan_periods = pmap(list(principal, monthly_pmt, mort_int), calc_loan_term),
    amortization = pmap(
      list(monthly_pmt, principal, mort_int, loan_periods),
      gen_amort_tbl,
      start_date = ymd("2023-06-01"),
      pmt_freq = "monthly"
    ),
    interest_earnings = pmap(
      list(coupon, liquid_capital, loan_periods),
      gen_coupon_tbl,
      r = bond_int / 12,
      start_date = ymd("2023-06-01"),
      pmt_freq = "monthly"
    ),
    scenario = fct_reorder(scenario, scenario_index)
  )

earnings_data <- loan_scenarios |>
  select(scenario, interest_earnings) |>
  unnest(interest_earnings) |>
  group_by(scenario) |>
  mutate(sum_earnings = cumsum(int_earned)) |>
  ungroup()

payment_plot_data <- loan_scenarios |>
  unnest(amortization) |>
  group_by(scenario) |>
  mutate(
    sum_prin_paid = cumsum(prin_paid),
    sum_int_paid = cumsum(int_paid)
  ) |>
  ungroup()

area_plot_data <- payment_plot_data |>
  pivot_longer(
    c(int_paid, prin_paid),
    names_to = "metric",
    values_to = "amount"
  ) %>%
  select(scenario, pmt_date, metric, amount)

prin_int_xover_data <- payment_plot_data |>
  group_by(scenario) |>
  filter(prin_paid >= int_paid) |>
  slice_min(pmt_date) |>
  ungroup() |>
  select(scenario, pmt_date)

finish_line_data <- payment_plot_data |>
  group_by(scenario) |>
  slice_max(pmt_date) |>
  ungroup() |>
  select(scenario, pmt_date, sum_prin_paid, sum_int_paid)

annotation_data <- earnings_data |>
  group_by(scenario) |>
  slice_max(pmt_date) |>
  ungroup() |>
  left_join(
    rename(finish_line_data, payoff_date = "pmt_date"),
    join_by(scenario)
  ) |>
  left_join(
    select(loan_scenarios, scenario, monthly_pmt, principal, mort_int) |>
      mutate(mort_int = mort_int * 12),
    join_by(scenario)
  ) |>
  left_join(
    select(prin_int_xover_data, scenario, text_date = "pmt_date"),
    join_by(scenario)
  ) |>
  mutate(
    time_saved = interval(pmt_date, max(pmt_date)) |>
      as.period(unit = "days") |>
      slot("day") %>%
      `/`(360),
    net_cash_value = if_else(
      time_saved > 0,
      (P + int_earned) * ((1 + bond_int) ^ time_saved),
      P + int_earned
    ),
    net_value = net_cash_value - sum_int_paid,
    across(
      c(monthly_pmt, net_cash_value, sum_int_paid, net_value, principal),
      as.numeric
    )
  )

ggplot() +
  geom_area(
    data = area_plot_data,
    aes(x = pmt_date, y = amount, group = metric, fill = metric, color = metric),
    alpha = 0.5, position = "identity"
  ) +
  geom_line(
    data = earnings_data,
    aes(x = pmt_date, y = int_earned),
    color = "blue"
  ) +
  geom_vline(
    data = finish_line_data,
    aes(xintercept = pmt_date),
    linetype = "solid"
  ) +
  geom_text(
    data = annotation_data,
    aes(
      x = text_date,
      y = 1500,
      label = str_glue(
        "Monthly payment: {scales::label_dollar()(monthly_pmt)}",
        "Principal : {scales::label_dollar()(principal)}",
        "Payoff date: {scales::label_date()(payoff_date)}",
        "Interest rate: {scales::label_percent()(mort_int)}",
        "Total interest paid: {scales::label_dollar()(sum_int_paid)}",
        "Net cash value: {scales::label_dollar()(net_cash_value)}",
        "Net value (ex-house): {scales::label_dollar()(net_value)}",
        .sep = "\n"
      )
    ),
    size = 2.3
  ) +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_x_date(date_breaks = "3 years", date_labels = "%b %Y") +
  facet_wrap(~ scenario, nrow = 2) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, vjust = 0.7)
  )

