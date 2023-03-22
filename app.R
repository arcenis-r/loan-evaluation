library(shiny)
library(shinyWidgets)
library(tidyverse)
library(rhandsontable)

source("loan-evaluation-funs.R")

periodicities <- c(
  "Annual",
  "Quarterly",
  "Monthly",
  "Biweekly",
  "Weekly",
  "Daily"
)

ui <- fluidPage(
  titlePanel("Loan Evaluation"),
  
  # Increase the visibility of horizontal rules
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  sidebarLayout(
    
    # Inputs -------------------------------------------------------------------
    sidebarPanel(
      
      # Panel width
      width = 2,
      
      h3("Loan Inputs"),
      br(),
      
      # Scenario title
      textInput(
        "scenario_title",
        label = "Scenario Title",
        value = "",
        width = "100%",
        placeholder = "Scenario Title"
      ),
      
      # Home loan amount
      autonumericInput(
        "loan_amount",
        label = "Loan Amount",
        value = 200000,
        minimumValue = 0,
        currencySymbol = "$",
        currencySymbolPlacement = "p",
        width = "100%"
      ),
      
      # Loan APR
      autonumericInput(
        "loan_apr",
        label = "Loan APR (%)",
        value = 0.05,
        decimalCharacter = ".",
        decimalPlaces = 4,
        digitGroupSeparator = ",",
        minimumValue = 0,
        maximumValue = 100,
        rawValueDivisor = 100
      ),
      
      # Loan Term
      numericInput(
        "loan_term",
        label = "Loan Term (in Years)",
        value = 30,
        min = 0,
        max = 100,
        step = 1,
      ),
      
      # Extra payment
      autonumericInput(
        "extra_pmt",
        label = "Extra Payment per Period",
        value = 0,
        min = 0,
        currencySymbol = "$",
        currencySymbolPlacement = "p",
        width = "100%"
      ),
      
      # Payment frequency
      selectInput(
        "pmt_freq",
        "Payment Frequency",
        choices = periodicities[1:5],
        selected = "Monthly",
        width = "100%"
      ),
      
      hr(),
      
      h3("Investment Inputs"),
      
      # Investment principal
      autonumericInput(
        "investment_principal",
        label = "Investment Principal",
        value = 1000,
        minimumValue = 0,
        currencySymbol = "$",
        currencySymbolPlacement = "p",
        width = "100%"
      ),
      
      # Investment APR
      autonumericInput(
        "investment_apr",
        label = "Investment APR (%)",
        value = 0.03,
        decimalCharacter = ".",
        decimalPlaces = 4,
        digitGroupSeparator = ",",
        minimumValue = 0,
        maximumValue = 100,
        rawValueDivisor = 100
      ),
      
      # Investment term
      numericInput(
        "investment_term",
        label = "Investment Term (in Years)",
        value = 30,
        min = 0,
        max = 100,
        step = 1,
      ),
      
      # Coupon payment
      autonumericInput(
        "coupon",
        label = "Coupon per Period",
        value = 0,
        min = 0,
        currencySymbol = "$",
        currencySymbolPlacement = "p",
        width = "100%"
      ),
      
      # Scenario start date
      airDatepickerInput(
        "start_date",
        "Scenario Start Date",
        value = today(),
        addon = "right"
      ),
      
      # Add scenario button
      actionBttn(
        "add_scenario",
        label = "Add Scenario",
        style = "pill",
        color = "primary"
      )
    ), # End sidbarPanel
    
    # Tabs for display in the main panel ---------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        # Scenario tables
        tabPanel("Scenarios", br(), rHandsontableOutput("scenario_table")),
        
        # Plots
        tabPanel("Plots", plotOutput("amort_plot")),
        
        # Amortization table
        tabPanel("Amortization", dataTableOutput("amort_table")),
        
        # Investment interest table
        tabPanel("Investment Interest", dataTableOutput("investment_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Store tables for calculations ----------------------------------------------
  
  rvals <- reactiveValues(
    input_tbl = NULL
  )
  
  # Store table of inputs
  observeEvent(
    input$add_scenario,
    {
      rvals$input_tbl <- bind_rows(
        rvals$input_tbl,
        tibble(
          viz_include = TRUE,
          scenario = input$scenario_title,
          loan_amount = input$loan_amount,
          loan_interest_rate = input$loan_apr,
          loan_term = input$loan_term,
          payment_freq = input$pmt_freq,
          extra_payment = input$extra_pmt,
          investment_principal = input$investment_principal,
          investment_interest_rate = input$investment_apr,
          investment_coupon = input$coupon,
          scenario_start_date = input$start_date
        ) |>
          mutate(
            loan_pmt = pmap_dbl(
              list(
                loan_amount,
                loan_interest_rate,
                loan_term,
                str_to_lower(payment_freq)
              ),
              calc_loan_pmt
            ),
            loan_periods = pmap_dbl(
              list(
                loan_amount,
                loan_pmt + extra_payment,
                loan_interest_rate,
                str_to_lower(payment_freq)
              ),
              calc_loan_term
            ),
            scenario_end_date = pmap_dbl(
              list(
                scenario_start_date,
                loan_periods,
                str_to_lower(payment_freq)
              ),
              get_period_date
            ) |>
              as_date()
          ) |>
          unnest(scenario_end_date)
      )
    }
  )
  
  observe({
    if (!is.null(input$scenario_table))
      rvals$input_tbl <- hot_to_r(input$scenario_table)
  })
  
  # Run amortization and appreciation tables
  amort_data <- reactive({
    if (!is.null(rvals$input_tbl)) {
      rvals$input_tbl |>
        select(-viz_include) |>
        mutate(
          loan_pmt = loan_pmt + extra_payment,
          loan_periods = pmap_dbl(
            list(
              loan_amount,
              loan_pmt,
              loan_interest_rate,
              str_to_lower(payment_freq)
            ),
            calc_loan_term
          ),
          amortization = pmap(
            list(
              loan_pmt,
              loan_amount,
              loan_interest_rate,
              loan_term,
              scenario_start_date,
              str_to_lower(payment_freq)
            ),
            gen_amort_tbl
          )
        ) |>
        unnest(amortization)
    }
  })
  
  investment_data <- reactive({
    if (!is.null(rvals$input_tbl)) {
      rvals$input_tbl |>
        select(-viz_include) |>
        mutate(
          loan_pmt = loan_pmt + extra_payment,
          loan_periods = pmap_dbl(
            list(
              loan_amount,
              loan_pmt,
              loan_interest_rate,
              str_to_lower(payment_freq)
            ),
            calc_loan_term
          ),
          interest_earnings = pmap(
            list(
              investment_coupon,
              investment_principal,
              investment_interest_rate,
              loan_periods,
              scenario_start_date,
              str_to_lower(payment_freq)
            ),
            gen_invest_tbl
          )
        ) |>
        unnest(interest_earnings)
    }
  })
  
  
  # Create dataframes for plotting ---------------------------------------------
  
  # Amortization data
  # area_plot_data <- reactive({
  #   amort_data() |>
  #     pivot_longer(
  #       c(int_paid, prin_paid),
  #       names_to = "metric",
  #       values_to = "amount"
  #     ) |>
  #     select(scenario, pmt_date, metric, amount)
  # })
  
  # Appreciation data
  
  # Create display objects -----------------------------------------------------
  
  scenarios_to_view <- reactive({
    rvals$input_tbl |>
      filter(viz_include) |>
      pull(scenario)
  })
  
  # Scenario table
  output$scenario_table <- renderRHandsontable({
    if (!is.null(rvals$input_tbl)) {
      rhandsontable(rvals$input_tbl, overflow = "visible") |>
        hot_col("loan_interest_rate", format = "0.00%") |>
        hot_col("investment_interest_rate", format = "0.00%") |>
        hot_col("loan_amount", format = "$0,0.00") |>
        hot_col("extra_payment", format = "$0,0.00") |>
        hot_col("investment_principal", format = "$0,0.00") |>
        hot_col("investment_coupon", format = "$0,0.00") |>
        hot_col(
          "payment_freq",
          type = "dropdown",
          source = periodicities[1:5]
        ) |>
        hot_cols(columnSorting = TRUE)
    }
  })
  
  # Amortization plot
  # output$amort_plot <- renderPlot({
  #   ggplot() +
  #     geom_area(
  #       data = area_plot_data() |> filter(scenario %in% scenarios_to_view()),
  #       aes(
  #         x = pmt_date,
  #         y = amount,
  #         group = metric,
  #         fill = metric,
  #         color = metric
  #       ),
  #       alpha = 0.5, position = "identity"
  #     ) +
  #     geom_line(
  #       data = investment_data() |> filter(scenario %in% scenarios_to_view()),
  #       aes(x = pmt_date, y = int_earned),
  #       color = "blue"
  #     ) +
  #     facet_wrap(~ scenario)
  # })
  
  output$amort_plot <- renderPlot({
    gen_amort_plot(amort_data(), investment_data())
  })
  
  # Amortization table
  output$amort_table <- renderDataTable({
    amort_data() |>
      filter(scenario %in% scenarios_to_view()) |>
      select(scenario, principal = "P", int_paid, prin_paid, pmt_date) |>
      mutate(pmt_date = as.character(pmt_date))
  })
  
  # Investment table
  output$investment_table <- renderDataTable({
    investment_data() |>
      filter(scenario %in% scenarios_to_view()) |>
      select(scenario, principal = "P", int_earned, pmt_date) |>
      mutate(pmt_date = as.character(pmt_date))
  })
}

shinyApp(ui = ui, server = server)