library(shiny)
library(ggplot2)
library(dplyr)
library(palmerpenguins)
library(pokemon)

#data(mtcars)
#data(penguins)
data(pokemon)


# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-size: 12px; }
      .form-control { font-size: 11px; }
      h4 { font-size: 14px; }
      h5 { font-size: 12px; }
      .btn { font-size: 11px; padding: 4px 8px; }
      .well { padding: 15px; }
      table { font-size: 10px; }
    "))
  ),
  titlePanel("Sampling and Regression Analysis"),
  
  fluidRow(
    # Left panel - Controls
    column(2,
           wellPanel(
             
             # Variable selection dropdowns
             h4("Variable Selection"),
             selectInput("y_var", 
                         "Y Variable:",
                         choices = names(select_if(pokemon, is.numeric)),
                         selected = "defense"),
             
             selectInput("x_var", 
                         "X Variable:",
                         choices = names(select_if(pokemon, is.numeric)),
                         selected = "attack"),
             
             hr(), # Add a horizontal line separator
             
             h4("Sample Parameters"),
             
             # Sample size slider
             sliderInput("sample_size", 
                         "Sample Size:",
                         min = 10, 
                         max = nrow(pokemon),
                         value = 50,
                         step = 1),
             
             # Number of samples buttons
             h5("Number of Samples:"),
             div(style = "margin-bottom: 10px;",
                 actionButton("btn_1", "+1", class = "btn-primary", style = "margin-right: 5px;"),
                 actionButton("btn_10", "+10", class = "btn-primary", style = "margin-right: 5px;"),
                 actionButton("btn_100", "+100", class = "btn-primary")
             ),
             
             # Display selected number of samples
             div(style = "margin-bottom: 15px;",
                 textOutput("selected_samples")
             ),
             
             # Go and Reset buttons
             div(
               actionButton("go_btn", "Go", class = "btn-success", style = "margin-right: 10px;"),
               actionButton("reset_btn", "Reset", class = "btn-warning")
             )
           )
    ),
    
    # Middle panel - Plot
    column(4,
           wellPanel(
             h4("Regression Lines Plot"),
             plotOutput("regression_plot", height = "400px")
           )
    ),
    
    # Right panel - Results table
    column(3,
           wellPanel(
             h4("Coefficient Estimates"),
             div(style = "max-height: 400px; overflow-y: auto;",
                 tableOutput("results_table")
             )
           )
    ),
    
    # Fourth panel - Theoretical sampling distribution
    column(3,
           wellPanel(
             h4("Estimated Sampling Distribution of Slope Parameter"),
             plotOutput("theoretical_dist_plot", height = "400px")
           )
    )
  )
)