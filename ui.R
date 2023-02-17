library(shiny)

# Define the UI
ui <- fluidPage(
  # Define the side panel
  sidebarLayout(
    sidebarPanel(
      # Status
      textInput("status", "Status", ""),
      # Amount 
      numericInput("loan_amount", "Amount", 1000000),
      #Recalculation
      selectInput("Recalculation", "Recalculation", 
                  c('NO','YES')),
      # Rate/Margin
      numericInput("interest", "Rate/Margin", 12),
      #tenor_unit
      selectInput("tenor_unit", "tenor_unit", 
                  c('month','day')),
      # Tenor
      numericInput("tenor", "Tenor", 12),
      
      # Accrual Basis
      selectInput("AccrualBasis", "Accrual Basis", 
                  c('30/360' ,'30/365','n/360' ,'n/365' ,'actual/actual')),
      # Combo box 1
      selectInput("amortization_type", "Amortization Type", 
                  c('constant_due_amount1','constant_due_amount2','constant_amortization' ,'degressive' , 'infine' ,'progressive')),
      
      
      
      # coupon frequency
      selectInput("coupon_frequency", "Coupon Frequency", 
                  c("monthly","quarterly","semi annual","infine","bimonthly","termly","annual","weekly","biweekly","inadvance","daily_1","daily_2","daily_3","daily_4","daily_5","daily_6","begin","end","one_time")),
      
      # Amortization Frequency
      selectInput("amortization_frequency", "Amortization Frequency", 
                  c("monthly","quarterly","semi annual","annual")),
      # start_date 
      dateInput("start_date", "Start Date", value = Sys.Date()),
      # first_payment_date 
      dateInput("first_payment_date", "First Payment Date", value = Sys.Date()),
      # end_date
      dateInput("end_date", "End Date", value = Sys.Date() ),
      
      # Downpayment Mode
      selectInput("downpayment_mode", "Downpayment Mode", 
                  c("Option 1", "Option 2", "Option 3")),
      # Downpayment 
      numericInput("downpayment", "Downpayment",0 ),
      # base rate
      selectInput("base_rate", "Base rate", 
                  c('fixed' , 'euribor')),
      
      # Grace Period Unit
      selectInput("grace_period_unit", "Grace Period Unit", 
                  c("Moratorium", "Interest", "Simple", "Total")),
      # Grace Period
      numericInput("grace_period", "Grace Period", 0),
      # Grace Calculation Methode
      selectInput("grace-calculation_methode", "Grace Calculation Methode", 
                  c("Option 1", "Option 2", "Option 3")),
      # Taxe
      selectInput("taxe", "Taxe", 
                  c("Option 1", "Option 2", "Option 3")),
      
      downloadButton("downloadData", "Download data"),
      
      actionButton("do", "Generate!"),
      width = 3
    ),
    
    # Main panel
    # Show a table of investment using the Server's output called "schedule"
    mainPanel(
      h1("Main Panel"),
      splitLayout( tableOutput("schedule")
      ),splitLayout( tableOutput("evaluation")
      ),
      width = 9
    )
  )
)





