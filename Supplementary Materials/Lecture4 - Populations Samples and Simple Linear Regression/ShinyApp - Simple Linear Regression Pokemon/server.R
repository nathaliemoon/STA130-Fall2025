library(palmerpenguins)
#data(penguins)
library(pokemon)
data(pokemon)


# Define server logic
server <- function(input, output, session) {
  
  # Create reactive data1 based on user selections
  data1 <- reactive({
    # Remove rows with missing values for the selected variables
    pokemon_clean <- pokemon[complete.cases(pokemon[c(input$x_var, input$y_var)]), ]
    data.frame(
      x1 = pokemon_clean[[input$x_var]],
      y1 = pokemon_clean[[input$y_var]]
    )
  })
  
  # Calculate theoretical sampling distribution parameters reactively
  theoretical_params <- reactive({
    full_model <- lm(y1 ~ x1, data = data1())
    pop_slope <- coef(full_model)[2]
    sigma_squared <- summary(full_model)$sigma^2
    x1_centered <- data1()$x1 - mean(data1()$x1)
    slope_se_factor <- sqrt(sigma_squared / sum(x1_centered^2))
    
    list(
      pop_slope = pop_slope,
      slope_se_factor = slope_se_factor
    )
  })
  
  # Update slider max based on selected data
  observe({
    updateSliderInput(session, "sample_size",
                      max = nrow(data1()),
                      value = min(50, nrow(data1())))
  })
  
  # Reactive values to store state
  values <- reactiveValues(
    nsim = 1,
    results = NULL,
    plot_data = NULL,
    show_theoretical = FALSE,
    slope_estimates = NULL,
    intercept_estimates = NULL
  )
  
  # Update number of samples based on button clicks
  observeEvent(input$btn_1, {
    values$nsim <- 1
  })
  
  observeEvent(input$btn_10, {
    values$nsim <- 10
  })
  
  observeEvent(input$btn_100, {
    values$nsim <- 100
  })
  
  # Display selected number of samples
  output$selected_samples <- renderText({
    paste("Selected samples:", values$nsim)
  })
  
  # Reset button
  observeEvent(input$reset_btn, {
    values$results <- NULL
    values$plot_data <- NULL
    values$show_theoretical <- FALSE
    values$slope_estimates <- NULL
    values$intercept_estimates <- NULL
    values$nsim <- 1
  })
  
  # Go button - main computation
  observeEvent(input$go_btn, {
    
    # Show theoretical distribution after first Go click
    values$show_theoretical <- TRUE
    
    # Determine starting sample number
    start_sample <- if(is.null(values$results)) 1 else nrow(values$results) + 1
    
    # Initialize new results tibble
    new_results <- tibble(
      sample = integer(),
      intercept = numeric(),
      slope = numeric()
    )
    
    # Initialize new plot data storage
    new_plot_lines <- list()
    
    # Generate samples and fit models
    for(i in 1:values$nsim) {
      
      # Draw sample without replacement
      sample_indices <- sample(nrow(data1()), size = input$sample_size, replace = FALSE)
      sample_data <- data1()[sample_indices, ]
      
      # Fit linear model
      model <- lm(y1 ~ x1, data = sample_data)
      
      # Store coefficients
      intercept_val <- coef(model)[1]
      slope_val <- coef(model)[2]
      
      sample_num <- start_sample + i - 1
      
      new_results <- new_results %>%
        add_row(
          sample = as.integer(sample_num),
          intercept = round(intercept_val, 4),
          slope = round(slope_val, 4)
        )
      
      # Store line data for plotting
      x_range <- range(data1()$x1)
      line_data <- data.frame(
        x = x_range,
        y = intercept_val + slope_val * x_range,
        sample = sample_num
      )
      new_plot_lines[[i]] <- line_data
    }
    
    # Combine new line data
    new_plot_data <- do.call(rbind, new_plot_lines)
    
    # Add to existing results
    if(is.null(values$results)) {
      values$results <- new_results
      values$plot_data <- new_plot_data
      values$slope_estimates <- new_results$slope
      values$intercept_estimates <- new_results$intercept
    } else {
      values$results <- bind_rows(values$results, new_results)
      values$plot_data <- bind_rows(values$plot_data, new_plot_data)
      values$slope_estimates <- c(values$slope_estimates, new_results$slope)
      values$intercept_estimates <- c(values$intercept_estimates, new_results$intercept)
    }
  })
  
  # Render plot
  output$regression_plot <- renderPlot({
    
    # Base plot with original data
    p <- ggplot(data1(), aes(x = x1, y = y1)) +
      geom_point(alpha = 0.3, color = "gray60") +
      labs(x = input$x_var, y = input$y_var, title = "Original Data with Fitted Regression Lines") +
      theme_minimal()
    
    # Add regression lines if they exist
    if (!is.null(values$plot_data)) {
      p <- p + geom_line(data = values$plot_data, 
                         aes(x = x, y = y, group = sample), 
                         color = "blue", 
                         size = 0.8,
                         alpha = 0.2)
      
      # Add annotation with total number of samples
      total_samples <- length(values$slope_estimates)
      p <- p + annotate("text", 
                        x = Inf, y = Inf, 
                        label = paste("Total samples:", total_samples),
                        hjust = 1.1, vjust = 1.5,
                        size = 4,
                        color = "black",
                        fill = "white",
                        fontface = "bold")
    }
    
    return(p)
  })
  
  # Render results table
  output$results_table <- renderTable({
    if (!is.null(values$results)) {
      values$results
    }
  }, striped = TRUE, hover = TRUE)
  
  # Render theoretical sampling distribution
  output$theoretical_dist_plot <- renderPlot({
    
    if (!values$show_theoretical) {
      # Empty plot before Go is clicked
      ggplot() + 
        theme_void() +
        labs(title = "Click 'Go' to see estimated sampling distribution")
    } else {
      # Calculate theoretical parameters for current sample size
      n <- input$sample_size
      slope_se <- theoretical_params()$slope_se_factor * sqrt(nrow(data1()) / n)  # Adjust SE for sample size
      
      # Create x range for theoretical curve
      x_range <- theoretical_params()$pop_slope + c(-4, 4) * slope_se
      x_seq <- seq(x_range[1], x_range[2], length.out = 200)
      
      # Theoretical normal curve
      theoretical_curve <- data.frame(
        x = x_seq,
        y = dnorm(x_seq, mean = theoretical_params()$pop_slope, sd = slope_se)
      )
      print(theoretical_curve)
      
      # First, let's get the y-axis range of your theoretical curve
      y_range <- range(theoretical_curve$y)
      plot_height <- diff(y_range)
      
      # Calculate how tall the dotplot would be with default settings
      n_points <- length(values$slope_estimates)
      max_stack_height <- max(table(cut(values$slope_estimates, breaks = 30)))  # Approximate max stack
      
      # Default dotplot settings
      default_dotsize <- 1
      default_scale <- 1
      
      # Only scale down if the dots would exceed the plot height
      # Estimate dotplot height (this is approximate)
      estimated_dotplot_height <- max_stack_height * default_dotsize * default_scale * 0.1
      
      # Only apply scaling if dots would go beyond the plot bounds
      scale_factor <- ifelse(estimated_dotplot_height > plot_height, 
                             sqrt(plot_height / estimated_dotplot_height),  # Less aggressive
                             1)
      #scale_factor <- ifelse(estimated_dotplot_height > plot_height, 
      #                       plot_height / estimated_dotplot_height, 
      #                       1)
      
      # Apply the scaling factor only when needed
      final_dotsize <- default_dotsize * scale_factor
      final_scale <- default_scale * scale_factor
      
      
      
        # Plot theoretical curve
      ggplot(theoretical_curve, aes(x = x, y = y)) +
        geom_line(color = "red", size = 1, linetype = "dashed", alpha = 0.5) +
        geom_dotplot(data = data.frame(b = values$slope_estimates), 
                     aes(x = b), 
                     fill = "blue", 
                     color = "black",
                     binwidth = diff(range(theoretical_curve$x))/30,  # Adaptive binwidth
                     dotsize = 0.8,  # Fixed reasonable size
                     stackdir = "up",
                     inherit.aes = FALSE) +
        labs(x = "Slope Estimate", y = "Density", 
             title = paste("Estimated Sampling Dist. (n = ", n, ")", sep="")) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),      # Remove y-axis labels
          axis.ticks.y = element_blank(),     # Remove y-axis tick marks
          axis.title.y = element_blank()      # Remove y-axis title
        )
      
      
    }
  })
}