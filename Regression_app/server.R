# Load required packages
library(shiny)
library(tidymodels)
library(DBI)
library(RMySQL)
library(tidyverse)

# Set the maximum purchase value for normalization
Purchase_max <- 21399

# Connect to the MySQL database
con <- dbConnect(MySQL(), user = "root", password = "3012001", 
                 host = "localhost", dbname = "fridayblack")

# Enable loading data from local files into MySQL database
dbSendQuery(con, "SET GLOBAL local_infile = true;")

# Load the trained models
final_rf_model <- readRDS("../models/rf_model.rds")
tree <- readRDS("../models/tree_model.rds")
fitting <- readRDS("../models/lr_model.rds")

# Function to generate the model comparison plot
generate_plot <- function(num_points = 1, model, Purchase_max = 21399) {
  
  # Retrieve the specified number of data points from the database
  if (is.na(num_points)) {
    query <- "SELECT Product_Category_1, Product_Category_2, Product_Category_3, Product_ID, Purchase FROM black_friday_cleaned_table LIMIT 1"
    black_friday <- dbGetQuery(con, query)
  } else {
    query <- paste0("SELECT Product_Category_1, Product_Category_2, Product_Category_3, Product_ID, Purchase FROM black_friday_cleaned_table LIMIT ", num_points)
    black_friday <- dbGetQuery(con, query)
  }
  
  # Convert categorical variables to factors
  black_friday[, 1:4] <- lapply(black_friday[, 1:4], as.factor)
  
  # Make predictions for the test data
  predictions <- predict(model, new_data = black_friday) * Purchase_max
  names(predictions) <- "predicted"
  
  # Combine the predictions and observed values into a data frame
  results <- data.frame(observed = black_friday$Purchase, predicted = predictions)
  
  # Create the scatter plot
  ggplot(results, aes(x = observed, y = predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + 
    labs(x = "Observed Purchase value", y = "Predicted Purchase value", title = "Observed vs. Predicted Purchase Values") +
    scale_x_continuous(limits = c(0, 25000)) + 
    scale_y_continuous(limits = c(0, 25000))
}

# Define the server logic
server <- function(input, output) {
  
  # Define reactive values for the model comparison plot
  values_points <- reactiveValues(num_points = 1, model = "Random forest")
  
  # Update `values_points` when the "Update plot" button is clicked
  observeEvent(input$update_plot, {
    values_points$num_points <- input$num_points
    values_points$model <- input$model
  })
  
  # Render the model comparison plot
  output$Model_Comparison <- renderPlot({
    generate_plot(
      values_points$num_points, 
      if (values_points$model == "Random forest") {
        model = final_rf_model
      } else if (values_points$model == "Decision tree") {   
        model = tree
      } else {   
        model = fitting 
      }    
    )
  }, height = 650, width = 800)
  
  # Define reactive values for the data table
  data_rows <- reactiveValues(query_num_points = 1, max_num_points = 30) # set the maximum limit to 30
  
  # Update `data_rows` when the "Update data" button is clicked
  observeEvent(input$update_data, {
    # Validate the input
    if (input$query_num_points > data_rows$max_num_points) {
      data_rows$query_num_points <- data_rows$max_num_points
    } else if (input$query_num_points < 1) {
      data_rows$query_num_points <- 1
    } else {
      data_rows$query_num_points <- input$query_num_points
    }
  })
  
  # Render the data table
  output$selectedData <- renderTable({
    query <- paste0("SELECT Product_Category_1, Product_Category_2, Product_Category_3, Product_ID, Purchase FROM black_friday_cleaned_table LIMIT ", data_rows$query_num_points)
    dbGetQuery(con, query)
  })
  
  # Define reactive values for the model test
  values <- reactiveValues(model2 = 'Random forest', cat1 = "3", cat2 = "4", cat3 = "12", cat4 = "P00069042")
  
  # Update `values` when the "Submit" button is clicked
  observeEvent(input$testModel, {
    values$model <- input$model2
    values$cat1 <- input$cat1
    values$cat2 <- input$cat2
    values$cat3 <- input$cat3
    values$cat4 <- input$cat4
  })
  
  # Test the model and display the prediction
  observeEvent(input$testModel, {
    newdata <- data.frame(Product_Category_1 = as.factor(values$cat1),
                          Product_Category_2 = as.factor(values$cat2),
                          Product_Category_3 = as.factor(values$cat3),
                          Product_ID = as.factor(values$cat4))
    
    if (values$model == "Random forest") {
      pred <- predict(final_rf_model, newdata) * Purchase_max
    } else if (values$model == "Decision tree") {
      pred <- predict(tree, newdata) * Purchase_max
    } else {
      pred <- predict(fitting, newdata %>% select('Product_Category_1')) * Purchase_max
    }
    
    # Display the prediction
    output$testOutput <- renderText({
      paste("Prediction: $", round(pred,2))
    })
  })
}