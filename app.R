
library(shiny)       # for app
#library(tidyverse)  # all the libs we need are in tidymodels
library(tidymodels)  # for modeling
library(stacks)      # for stacking
library(ranger)      # for random forest
library(glmnet)      # for lasso
library(rpart)       # for decision tree

lending_mod <- readRDS("lending_stack.rds")

states <- 
    lending_mod$train  %>% 
    select(addr_state) %>% 
    distinct(addr_state) %>% 
    arrange(addr_state) %>% 
    pull(addr_state)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Ceteris Perabus Profile"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            sliderInput("acc_now_delinq",
                        "Number of accounts delinquent:",
                        min = 0,
                        max = 3,
                        value = 0, 
                        step = 1, 
                        round = TRUE),
            sliderInput("all_util",
                        "Balance to credit limit:",
                        min = 0,
                        max = 200,
                        value = 0, 
                        step = 10),
            sliderInput("anual_inc",
                        "Annual income:",
                        min = 0,
                        max = 1000000,
                        value = 0, 
                        step = 10000),
            selectInput("state", 
                        "State:", 
                        choices = states),
            selectInput("var",
                        "Variable to vary:",
                        choices = list(Income = "annual_inc",
                                       `Interest rate` =  "int_rate",
                                       `Funded amount` = "funded_amnt")),
            submitButton(text = "Create the CP profile"),
        ),

        # Show a cp plot for this observation
        mainPanel(
           plotOutput("cp_plot")
        )
    )
)

# Define server logic required for the cp_plot
server <- function(input, output) {
    output$cp_plot <- renderPlot({
        
        min_var <- 0
        max_var <- 1000000
        
        obs_many <- lending_mod$train %>% 
            slice(4) %>% #replace later with obs from ui
            sample_n(size = 50, replace = TRUE) %>% 
            select(-annual_inc) %>% 
            mutate(annual_inc = seq(min_var, max_var, length.out = 50))
        
        obs_many %>% 
            select(annual_inc) %>% 
            bind_cols(
                predict(lending_mod,
                        new_data = obs_many, 
                        type = "prob")
            ) %>% 
            ggplot(aes(x = annual_inc,
                       y = .pred_good)) +
            geom_line() +
            labs(title = "Predicted probability of loan fully paid back \nor currently on-time",
                 y = NULL) +
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
