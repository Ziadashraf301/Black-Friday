# Load the shiny library
library(shiny)

# Define the user interface (UI)
ui <- fluidPage(
  # Create a tabsetPanel with three tabs
  tabsetPanel(
    # First tab: Model Comparison
    tabPanel("Model Comparison",
             # Create a sidebarLayout with a sidebarPanel and mainPanel
             sidebarLayout(
               # Sidebar panel with a selectInput widget for choosing a model, a numericInput widget for selecting the number of data points to display, and an actionButton for updating the plot
               sidebarPanel(
                 selectInput("model", "Select model:", choices = c("Random forest", "Decision tree", "Linear regression")),
                 numericInput("num_points", "Number of data points:", value = 1,min = 1, max = 3000),
                 actionButton("update_plot", "Update plot")
               ),
               # Main panel with a plotOutput widget for displaying the plot
               mainPanel(
                 plotOutput("Model_Comparison")
               )
             )
    ),
    # Second tab: View Data
    tabPanel("View Data",
             # Create a sidebarLayout with a sidebarPanel and mainPanel
             sidebarLayout(
               # Sidebar panel with a numericInput widget for selecting the number of rows to display in the table and an actionButton for updating the table
               sidebarPanel(
                 numericInput("query_num_points", "Select data size:", value = 1, min = 1, max = 30),
                 actionButton("update_data", "Update data")
               ),
               # Main panel with a tableOutput widget for displaying the table
               mainPanel(
                 tableOutput("selectedData")
               )
             )
    ),
    # Third tab: Test Model
    tabPanel("Test Model",
             # Create a sidebarLayout with a sidebarPanel and mainPanel
             sidebarLayout(
               # Sidebar panel with selectInput widget for choosing a model and textInput widgets for entering values of four features, and an actionButton for testing the model
               sidebarPanel(
                 selectInput("model2", "Select Model:", choices = c("Random forest", "Decision tree", "Linear regression")),
                 textInput("cat1", "Product_Category_1:", value = "3"),
                 textInput("cat2", "Product_Category_2:", value = "4"),
                 textInput("cat3", "Product_Category_3:", value = "12"),
                 textInput("cat4", "Product_ID:", value = "P00069042"),
                 actionButton("testModel", "Test Model")
               ),
               # Main panel with a verbatimTextOutput widget for displaying the output of the model test
               mainPanel(
                 verbatimTextOutput("testOutput")
               )
             )
    )
  )
)