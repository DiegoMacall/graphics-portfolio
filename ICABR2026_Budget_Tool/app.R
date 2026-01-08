#
# ICABR 2026 Budget App 
# 

# Libraries
library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(ggplot2)

# 0. ICABR Orange Palette
orange_theme <- bs_theme(
  version = 5,
  primary = "#FF7A00",     
  secondary = "#FFB266",   
  success = "#FFA64D",     
  base_font = font_google("Inter")
)

# 1. User Interface 

ui <- fluidPage(
  theme = orange_theme,
  style = "max-width: 1600px; margin: auto;",
  
  div(
    style = "text-align:center; margin-top:20px;",
    img(src = "logo.jpg", height = "120px")
  ),
  
  titlePanel("ICABR 2026 Budget Tool"),
  
  tabsetPanel(
    
# -------------------------------
# 2. TAB 1: Income & Revenue Report
# -------------------------------

    tabPanel("1. Registrations & Revenue",
             
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 
                 h4("Registration-Based Revenue"),
                 
                 sliderInput("full_regs_early", "Early Bird Full Registrations:",
                             min = 0, max = 200, value = 0),
                 
                 sliderInput("full_regs", "Full Registrations:",
                             min = 0, max = 200, value = 0),
                 
                 sliderInput("student_regs_early", "Early Bird Student Registrations:",
                             min = 0, max = 200, value = 0),
                 
                 sliderInput("student_regs", "Student Registrations:",
                             min = 0, max = 200, value = 0),
                 
                 sliderInput("accompanying", "Accompanying Person:",
                             min = 0, max = 200, value = 0),
                 
                 hr(),
                 h4("Additional Income"),
                 
                 textInput("income_name", "Income Source:", ""),
                 numericInput("income_value", "Income Amount (€):", value = 0, min = 0),
                 
                 actionButton("add_income", "Add Income", class = "btn-success"),
                 br(), br(),
                 actionButton("delete_income", "Delete Selected Income", class = "btn-danger")
               ),
               
               mainPanel(
                 width = 8,
                 
                 h3("Revenue Summary"),
                 verbatimTextOutput("summary"),
                 
                 h3("Additional Income Table"),
                 DTOutput("income_table")
               )
             )
    ),
    
# -------------------------------
# 3. TAB 2: Expenses (Fixed & Variable)
# -------------------------------

    tabPanel("2. Conference Costs",
             
             sidebarLayout(
               sidebarPanel(
                 width = 4,
                 
                 textInput("expense_name", "Expense Name:", ""),
                 
                 selectInput(
                   "expense_type",
                   "Cost Type:",
                   choices = c("Fixed", "Variable")
                 ),
                 
                 # Fixed cost
                 conditionalPanel(
                   condition = "input.expense_type == 'Fixed'",
                   numericInput(
                     "expense_value",
                     "Fixed Cost Amount (€):",
                     value = 0,
                     min = 0
                   )
                 ),
                 
                 # Variable cost
                 conditionalPanel(
                   condition = "input.expense_type == 'Variable'",
                   numericInput(
                     "expense_units",
                     "Units (e.g., # of coffee breaks per participant):",
                     value = 1,
                     min = 1
                   ),
                   numericInput(
                     "expense_per_person",
                     "Cost per participant (€):",
                     value = 0,
                     min = 0
                   )
                 ),
                 
                 actionButton("add_expense", "Add Expense", class = "btn-primary"),
                 br(), br(),
                 actionButton("delete_expense", "Delete Selected Expense", class = "btn-danger")
               ),
               
               mainPanel(
                 width = 8,
                 h3("Expenses Table"),
                 DTOutput("expense_table")
               )
             )
    ),
    
# -------------------------------
# 4. TAB 3: Summary & Charts
# -------------------------------

    tabPanel("3. Summary & Charts",
             
             h3("Overall Summary"),
             verbatimTextOutput("final_summary"),
             
             downloadButton("download_summary", "Download Budget Summary (CSV)", class = "btn-primary"),
             hr(),
             
             h3("Donut Chart: Income vs Expenses"),
             plotOutput("donut_chart", height = "350px"),
             hr(),
             
             h3("Bar Chart: Fixed vs Variable Costs"),
             plotOutput("bar_chart", height = "350px"),
             hr(),
             
             h3("Cost Per Participant"),
             plotOutput("per_participant_chart", height = "350px")
    )
  )
)

server <- function(input, output, session) {
  
  # 5. Reactive data frame for expenses
  expenses <- reactiveVal(
    data.frame(
      Name = character(),
      Type = character(),
      Amount = numeric(),
      Units = numeric(),
      PerPerson = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  # 6. Reactive data frame for additional income
  income_items <- reactiveVal(
    data.frame(
      Name = character(),
      Amount = numeric(),
      stringsAsFactors = FALSE
    )
  )
  
  # 7. Total participants
  total_participants <- reactive({
    input$full_regs_early +
      input$full_regs +
      input$student_regs_early +
      input$student_regs +
      input$accompanying
  })
  
  # 8. Add new expense
  observeEvent(input$add_expense, {
    req(input$expense_name)
    
    if (input$expense_type == "Fixed") {
      new_row <- data.frame(
        Name = input$expense_name,
        Type = "Fixed",
        Amount = input$expense_value,
        Units = NA,
        PerPerson = NA
      )
    } else {
      new_row <- data.frame(
        Name = input$expense_name,
        Type = "Variable",
        Amount = NA,
        Units = input$expense_units,
        PerPerson = input$expense_per_person
      )
    }
    
    expenses(bind_rows(expenses(), new_row))
  })
  
  # 9. Add new income
  observeEvent(input$add_income, {
    req(input$income_name, input$income_value)
    
    new_row <- data.frame(
      Name = input$income_name,
      Amount = input$income_value
    )
    
    income_items(bind_rows(income_items(), new_row))
  })
  
  # 10. Delete selected expense
  observeEvent(input$delete_expense, {
    sel <- input$expense_table_rows_selected
    if (length(sel) == 0) return(NULL)
    df <- expenses()
    df <- df[-sel, , drop = FALSE]
    expenses(df)
  })
  
  # 11. Delete selected income
  observeEvent(input$delete_income, {
    sel <- input$income_table_rows_selected
    if (length(sel) == 0) return(NULL)
    df <- income_items()
    df <- df[-sel, , drop = FALSE]
    income_items(df)
  })
  
  # 12. Show income table (DT)
  output$income_table <- renderDT({
    df <- income_items()
    datatable(
      df,
      selection = "single",
      options = list(pageLength = 5),
      rownames = FALSE
    )
  })
  
  # 13. Show expenses table with computed amounts (DT)
  output$expense_table <- renderDT({
    df <- expenses()
    tp <- total_participants()
    
    if (nrow(df) == 0) {
      return(datatable(df, selection = "single", rownames = FALSE))
    }
    
    df$ComputedAmount <- ifelse(
      df$Type == "Fixed",
      df$Amount,
      df$Units * df$PerPerson * tp
    )
    
    datatable(
      df,
      selection = "single",
      options = list(pageLength = 5),
      rownames = FALSE
    )
  })
  
  # 14. Registration revenue
  reg_income_reactive <- reactive({
    input$full_regs_early    * 600 +
      input$full_regs        * 750 +
      input$student_regs_early * 300 +
      input$student_regs       * 400 +
      input$accompanying       * 200
  })
  
  # 15. Summary for Tab 1
  output$summary <- renderPrint({
    df <- expenses()
    tp <- total_participants()
    
    fixed_costs <- sum(df$Amount[df$Type == "Fixed"], na.rm = TRUE)
    variable_costs <- sum(df$Units[df$Type == "Variable"] *
                            df$PerPerson[df$Type == "Variable"] *
                            tp, na.rm = TRUE)
    
    total_expenses <- fixed_costs + variable_costs
    extra_income <- sum(income_items()$Amount)
    
    reg_income <- reg_income_reactive()
    total_income <- reg_income + extra_income
    remaining <- total_income - total_expenses
    
    list(
      Total_Participants = tp,
      Fixed_Costs = fixed_costs,
      Variable_Costs = variable_costs,
      Total_Expenses = total_expenses,
      Registration_Revenue = reg_income,
      Additional_Income = extra_income,
      Total_Income = total_income,
      Remaining = remaining
    )
  })
  
  # 16. Summary data frame for CSV and Tab 3
  summary_data <- reactive({
    df <- expenses()
    tp <- total_participants()
    
    fixed_costs <- sum(df$Amount[df$Type == "Fixed"], na.rm = TRUE)
    variable_costs <- sum(df$Units[df$Type == "Variable"] *
                            df$PerPerson[df$Type == "Variable"] *
                            tp, na.rm = TRUE)
    
    total_expenses <- fixed_costs + variable_costs
    extra_income <- sum(income_items()$Amount)
    reg_income <- reg_income_reactive()
    total_income <- reg_income + extra_income
    remaining <- total_income - total_expenses
    
    data.frame(
      Item = c(
        "Total Participants",
        "Fixed Costs",
        "Variable Costs",
        "Total Expenses",
        "Registration Revenue",
        "Additional Income",
        "Total Income",
        "Remaining Balance"
      ),
      Value = c(
        tp,
        fixed_costs,
        variable_costs,
        total_expenses,
        reg_income,
        extra_income,
        total_income,
        remaining
      )
    )
  })
  
  # 17. Download handler
  output$download_summary <- downloadHandler(
    filename = function() {
      paste0("ICABR_budget_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(summary_data(), file, row.names = FALSE)
    }
  )
  
  # 18. Final summary (Tab 3)
  output$final_summary <- renderPrint({
    summary_data()
  })
  
  # -------------------------------
  # 19. Donut Chart
  # -------------------------------
  output$donut_chart <- renderPlot({
    df <- summary_data()
    
    plot_df <- data.frame(
      Category = df$Item[c(2, 3, 7)],   # Fixed, Variable, Total Income
      Value = df$Value[c(2, 3, 7)]
    )
    
    ggplot(plot_df, aes(x = 2, y = Value, fill = Category)) +
      geom_col(width = 1, color = "white") +
      coord_polar(theta = "y") +
      xlim(0.5, 2.5) +
      theme_void() +
      theme(legend.position = "right") +
      scale_fill_manual(values = c("#FF7A00", "#FFA64D", "#FFB266"))
  })
  
  # -------------------------------
  # 20. Bar Chart
  # -------------------------------
  output$bar_chart <- renderPlot({
    df <- summary_data()
    
    plot_df <- data.frame(
      Category = c("Fixed Costs", "Variable Costs"),
      Value = df$Value[c(2, 3)]
    )
    
    ggplot(plot_df, aes(x = Category, y = Value, fill = Category)) +
      geom_col(width = 0.6) +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("#FF7A00", "#FFA64D")) +
      labs(x = "", y = "€", title = "Fixed vs Variable Costs")
  })
  
  # -------------------------------
  # 21. Cost Per Participant Chart
  # -------------------------------
  output$per_participant_chart <- renderPlot({
    df <- summary_data()
    
    total_participants <- df$Value[1]
    total_expenses <- df$Value[4]
    total_income <- df$Value[7]
    
    plot_df <- data.frame(
      Metric = c("Cost per Participant", "Income per Participant", "Net Margin per Participant"),
      Value = c(
        total_expenses / total_participants,
        total_income / total_participants,
        (total_income - total_expenses) / total_participants
      )
    )
    
    ggplot(plot_df, aes(x = Metric, y = Value, fill = Metric)) +
      geom_col(width = 0.6) +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("#FFA64D", "#FF7A00", "#FFB266")) +
      labs(x = "", y = "€", title = "Participant‑Scaled Financials")
  })
}

shinyApp(ui, server)
