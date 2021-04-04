# Start with the ui and only the ui - get that working correctly.

# To run R code, you can highlight it, and use 
# command + return (Mac)
# control + return (Windows)

# Load libraries - for now I only need shiny & tidyverse

library(shiny)       # for app
library(tidyverse)   # graphing/wrangling

# I'm loading this because the training dataset is in this output.
# And I want to use the training data.
lending_mod <- readRDS("../lending_stack.rds")

# Find unique states and put them in alphabetical order:
states <- 
  lending_mod$train  %>% 
  select(addr_state) %>% 
  distinct(addr_state) %>% 
  arrange(addr_state) %>% 
  pull(addr_state)

# Fix employment length

emp_len <- 
  lending_mod$train %>% 
  select(emp_length) %>% 
  distinct() %>% 
  mutate(emp_length = fct_relevel(emp_length,
                                  "emp_ge_10", 
                                  after = 10)) %>% 
  arrange(emp_length) %>% 
  pull(emp_length)


# Find min's, max's, and median's for quantitative vars:

stats_num <-
  lending_mod$train  %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(),
               names_to = "variable", 
               values_to = "value") %>% 
  group_by(variable) %>% 
  summarize(min_val = min(value),
            max_val = max(value),
            med_val = median(value))

# Define UI 
# Add *****ONE******* input at a time! 
# And run app right after that to assure it works. 
# After you've added a few, publish to shinyapps.io to make sure it works at an early stage.

# NOTE: I haven't made all the labels look nice - I should.

ui <- fluidPage(
  
  # Application title
  titlePanel("Ceteris Perabus Profile"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "acc_now_delinq",
                  label = "Number of accounts delinquent:",
                  min = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "all_util",
                  label = "Balance to credit limit:",
                  min = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(min_val) %>% 
                    round(digits = -1),
                  max = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(max_val) %>% 
                    round(digits = -1),
                  value = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(med_val) %>% 
                    round(digits = -1), 
                  step = 10),
      sliderInput(inputId = "annual_inc",
                  label = "Annual income:",
                  min = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(min_val) %>% 
                    round(digits = -4),
                  max = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(max_val) %>% 
                    round(digits = -4),
                  value = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(med_val) %>% 
                    round(digits = -4), 
                  step = 10000),
      sliderInput(inputId = "delinq_2yrs",
                  label = "# of 30+ days past-due incideces for past 2 yrs:",
                  min = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "funded_amnt",
                  label = "Funded amount:",
                  min = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(min_val) %>% 
                    round(digits = -3),
                  max = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(max_val) %>% 
                    round(digits = -3),
                  value = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(med_val) %>% 
                    round(digits = -3), 
                  step = 1000),
      sliderInput(inputId = "inq_fi",
                  label = "inq_fi:",
                  min = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "inq_last_12m",
                  label = "inq_last_12m:",
                  min = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "inq_last_6mths",
                  label = "inq_last_6mths:",
                  min = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "num_il_tl",
                  label = "num_il_tl:",
                  min = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "open_il_12m",
                  label = "open_il_12m:",
                  min = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "open_il_24m",
                  label = "open_il_24m:",
                  min = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "open_il_6m",
                  label = "open_il_6m:",
                  min = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      sliderInput(inputId = "revol_util",
                  label = "Revolving line utilization rate:",
                  min = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(med_val), 
                  step = 2, 
                  round = TRUE),
      sliderInput(inputId = "int_rate",
                  label = "Interest rate:",
                  min = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = .1, 
                  round = TRUE),
      sliderInput(inputId = "total_bal_il",
                  label = "Total current balance of installment accts:",
                  min = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(min_val) %>% 
                    round(digits = -4),
                  max = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(max_val) %>% 
                    round(digits = -4),
                  value = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(med_val) %>% 
                    round(digits = -4), 
                  step = 10000),
      sliderInput(inputId = "total_il_high_credit_limit",
                  label = "Total installments high credit/credit limit:",
                  min = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(min_val) %>% 
                    round(digits = -4),
                  max = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(max_val) %>% 
                    round(digits = -4),
                  value = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(med_val) %>% 
                    round(digits = -4), 
                  step = 10000),
      selectInput(inputId = "addr_state", 
                  label = "State:", 
                  choices = states),
      selectInput(inputId = "emp_length", 
                  label = "Employment length:", 
                  choices = emp_len),
      selectInput(inputId = "sub_grade", 
                  label = "LC assigned loan subgrade:", 
                  choices = lending_mod$train %>% 
                    select(sub_grade) %>% 
                    distinct() %>% 
                    arrange(sub_grade) %>% 
                    pull(sub_grade)),
      selectInput(inputId = "term", 
                  label = "Term:", 
                  choices = lending_mod$train %>% 
                    select(term) %>% 
                    distinct() %>% 
                    arrange(term) %>% 
                    pull(term)),
      selectInput(inputId = "verification_status", 
                  label = "Verification Status:", 
                  choices = lending_mod$train %>% 
                    select(verification_status) %>% 
                    distinct() %>% 
                    arrange(verification_status) %>% 
                    pull(verification_status)),
      selectInput(inputId = "var",
                  label = "Variable to vary in the plot:",
                  choices = list(Income = "annual_inc",
                                 `Interest rate` =  "int_rate",
                                 `Funded amount` = "funded_amnt")),
      submitButton(text = "Create the CP profile"),
    ),
    
    # Show a cp plot for this observation ... eventually
    mainPanel(
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
