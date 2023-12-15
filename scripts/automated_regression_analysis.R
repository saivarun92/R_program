# Load necessary libraries
library(DBI)
library(odbc)
library(dplyr)

# Function to extract data from Snowflake using a SQL script
extract_data_from_snowflake <- function(connection_string, sql_script_path) {
  con <- dbConnect(odbc::odbc(), connection_string)
  
  # Read SQL script
  query <- readLines(sql_script_path, warn = FALSE)
  
  # Execute the query
  result <- dbGetQuery(con, paste(query, collapse = " "))
  
  dbDisconnect(con)
  
  return(result)
}

# Function to run linear regression and save coefficients and fitted values to CSV
run_regression_and_analyze <- function(data, dependent_var, independent_vars) {
  # Run linear regression
  model <- lm(formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + "))), data = data)
  
  # Save coefficients to CSV
  coef_df <- as.data.frame(summary(model)$coefficients)
  write.csv(coef_df, "coef.csv", row.names = FALSE)
  
  # Save fitted values to CSV
  fittest_df <- data.frame(FittedValues = fitted(model))
  write.csv(fittest_df, "fittest.csv", row.names = FALSE)
}

# Function to create SQL statement and CSV for updating data in CredReadiness_Factors
create_update_sql_and_csv <- function(coef_df, keys) {
  # Combine coefficients with keys
  coef_with_keys <- cbind(coef_df, Keys = keys)
  
  # Save to CSV
  write.csv(coef_with_keys, "coefwithkeys.csv", row.names = FALSE)
  
  # Generate SQL statements
  insert_sql <- paste("INSERT INTO CredReadiness_Factors (KeyColumn, CoefColumn) VALUES ('", coef_with_keys$Keys, "', ", coef_with_keys$Estimate, ");", sep = "")
  
  # Save SQL statements to a file
  cat(insert_sql, file = "insertcoefwithkeys.sql", sep = "\n")
}

# Main script

# Connection string for Snowflake
snowflake_connection_string <- "YourSnowflakeDSN"

# Path to the SQL script for data extraction
sql_script_path <- "get_regression.sql"

# Extract data from Snowflake
data <- extract_data_from_snowflake(snowflake_connection_string, sql_script_path)

# Save the extracted data to a CSV file
write.csv(data, "Regressiondata.csv", row.names = FALSE)

# Set column parameters for linear regression
dependent_variable <- "ResponseVariable"
independent_variables <- c("PredictorVariable1", "PredictorVariable2")

# Run linear regression and analyze results
run_regression_and_analyze(data, dependent_variable, independent_variables)

# Create SQL statement and CSV for updating data in CredReadiness_Factors
keys <- c("Key1", "Key2", "Key3")  # Replace with actual keys
create_update_sql_and_csv(coef_df, keys)
