library(shiny)
library(shinyWidgets)
library(tidyverse)
library(rhandsontable)
library(DT)

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
        tabPanel("Investment Interest", dataTableOutput("investment_table")),
        
        # Summary table
        tabPanel("Scenario Summary Table", DTOutput("summary_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Store tables for calculations ----------------------------------------------
  
  rvals <- reactiveValues(
    input_tbl = NULL,
    summary_tbl = NULL,
    best_scenario = NULL,
    plot_height = NULL
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
        )
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
              loan_amount + extra_payment,
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
  
  # Summary data
  rvals$summary_tbl <- reactive({
    left_join(
      amort_data() |>
        group_by(scenario) |>
        summarise(
          scenario_start_date = min(pmt_date, na.rm = TRUE),
          scenario_end_date = max(pmt_date, na.rm = TRUE),
          text_date = min(pmt_date[prin_paid > int_paid], na.rm = TRUE),
          num_periods = n(),
          loan_pmt = mean(loan_pmt + extra_payment, na.rm = TRUE),
          total_int_paid = sum(int_paid, na.rm = TRUE),
          total_prin_paid = sum(prin_paid, na.rm = TRUE),
          npv_int_paid = sum(int_npv, na.rm = TRUE),
          npv_prin_paid = sum(prin_npv, na.rm = TRUE),
          .groups = "drop"
        ),
      investment_data() |>
        group_by(scenario) |>
        summarise(
          investment_int_npv = sum(investment_int_npv, na.rm = TRUE),
          int_earned = sum(int_earned, na.rm = TRUE),
          investment_principal = mean(investment_principal, na.rm = TRUE)
        ),
      join_by(scenario)
    ) |>
      mutate(
        cash_npv = investment_principal + investment_int_npv - npv_int_paid
      )
  })
  
  
  # Create display objects -----------------------------------------------------
  
  scenarios_to_view <- reactive({
    rvals$input_tbl |>
      filter(viz_include) |>
      pull(scenario)
  })
  
  # Select best scenario
  rvals$best_scenario <- reactive({
    if (!is.null(rvals$summary_tbl)) {
      rvals$summary_tbl() |>
        filter(scenario %in% scenarios_to_view()) |>
        slice_max(cash_npv, with_ties = FALSE) |>
        pull(scenario)
    }
  })
  
  # Set the plot height
  rvals$plot_height <- reactive({
    500 * length(scenarios_to_view())
  })
  
  # Scenario table
  output$scenario_table <- renderRHandsontable({
    if (!is.null(rvals$input_tbl)) {
      rhandsontable(
        rvals$input_tbl,
        overflow = "visible"
        # row_highlights = rvals$best_scenario
      ) |>
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
  
  # Amortization plots
  output$amort_plot <- renderPlot(
    {
      gen_amort_plot(
        amort_data() |> filter(scenario %in% scenarios_to_view()),
        investment_data() |> filter(scenario %in% scenarios_to_view()),
        rvals$summary_tbl() |> filter(scenario %in% scenarios_to_view())
      )
    },
    height = isolate(rvals$plot_height)
  )
  
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
  
  # Summary table
  output$summary_table <- renderDT({
    rvals$summary_tbl() |>
      select(-text_date) |>
      filter(scenario %in% scenarios_to_view()) |>
      datatable(rownames = FALSE) |>
      formatStyle(
        "scenario",
        target = "row",
        backgroundColor = styleEqual(rvals$best_scenario(), "lightgreen")
      ) |>
      formatCurrency(
        c(
          "loan_pmt", "total_int_paid", "total_prin_paid", "npv_int_paid",
          "npv_prin_paid", "investment_int_npv", "int_earned",
          "investment_principal", "cash_npv"
        )
      )
  })
}

shinyApp(ui = ui, server = server)