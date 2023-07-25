# Black Friday Sales Prediction Web Application

This web application uses Shiny to provide a simple user interface for visualizing and comparing the results of three regression models trained on Black Friday sales data: random forest, decision tree, and linear regression. The trained models are deployed in the app to make predictions on new data.

## Functionality

The app has the following features:

- **Model Comparison:** The predicted-observed scatter plot is generated for the selected model, allowing users to compare the performance of different models.

- **Data Retrieval:** Users can retrieve a specified number of records from the Black Friday sales data stored in a MySQL database to examine the nature of the data and its suitability for the models.

- **Model Testing:** Users can test the trained models by entering the values of the predictor variables (product category 1, product category 2, product category 3, and product ID) for a new data point, and the app will make a prediction of the purchase value for that data point.

## App Structure

The app consists of two main components: the user interface (UI) and the server logic.

### UI

The UI is defined in the `ui.R` file and includes the following elements:

- **Model Comparison Panel:** This panel allows users to select the model to visualize and the number of records to use for the comparison plot.

- **Data Retrieval Panel:** This panel allows users to specify the number of records to retrieve from the database.

- **Model Testing Panel:** This panel allows users to enter the predictor variable values for a new data point and test the selected model.

### Server

The server logic is defined in the `server.R` file and includes the following components:

- **Model Comparison:** The selected model is used to generate the predicted-observed scatter plot for the specified number of records.

- **Data Retrieval:** The specified number of records is retrieved from the database and displayed in a data table.

- **Model Testing:** The entered predictor variable values are used to make a purchase value prediction for the selected model, which is displayed as text output.

## Deployment

The app can be deployed locally or on a web server that supports Shiny apps, such as shinyapps.io or RStudio Connect. To run the app locally, simply open the `app.R` file in RStudio and click the "Run App" button. The app will open in a web browser.

## Dependencies

The app requires the following R packages to be installed:

- `shiny`
- `tidymodels`
- `DBI`
- `RMySQL`
- `tidyverse`

## Demo


https://github.com/Ziadashraf301/Black-Friday/assets/111798631/ea902a6c-44ce-4710-99c9-8a85a7f32835





