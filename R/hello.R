#' Scatterplot for correlation
#'
#' This function creates a scatter plot to show the correlation between two variables.
#'
#' @param data A dataset as a data frame
#' @param x_var Name of the independent variable
#' @param y_var Name of the dependent variable
#'
#' @return A ggplot object with the correlation plot
#'
#' @import ggplot2
#'
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' plot_correlation(data, "x", "y")
#'
#' @export
plot_correlation <- function(data, x_var, y_var) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = x_var, y = y_var, title = paste("Correlation between", x_var, "and", y_var))
}

#' Plot a Variable Over Time
#'
#' This function creates a plot to visualize the temporal evolution of a variable
#' from a given dataset.
#'
#' @param data A data frame containing the data.
#' @param date_column The name of the column representing the date or time.
#' @param variable_column The name of the column representing the variable to plot.
#' @param title The title of the plot (optional).
#' @return A ggplot object representing the plot.
#'
#' @import ggplot2
#' @examples
#' data <- data.frame(
#'   date_column = as.Date("2024-01-01") + 0:9,
#'   variable_column = cumsum(rnorm(10))
#' )
#' plot_variable_over_time(data, "date_column", "variable_column", "My Plot Title")
#' @export
plot_variable_over_time <- function(data, date_column, variable_column, title = "Variable over Time") {
  library(ggplot2)
  ggplot(data, aes_string(x = date_column, y = variable_column)) +
    geom_line() +
    labs(title = title, x = "Date", y = "Value") +
    theme_minimal()
}

#' Add Binary Column to Data Frame
#'
#' This function adds a binary column to a data frame based on the value of another column.
#'
#' @param df_name The name of the data frame as a character string.
#' @param column_name The name of the column to check.
#' @param value The value to check for.
#' @return No return value. Modifies the data frame in the global environment.
#' @export
#' @examples
#' df <- data.frame(observation = c("bird", "dog", "cat", "dog"))
#' add_binary_column("df", "observation", "dog")
#' print(df)
add_binary_column <- function(df_name, column_name, value) {
  # Retrieve the data frame from the global environment
  df <- get(df_name, envir = .GlobalEnv)

  # Add the binary column
  binary_column_name <- paste0(value, "_binary")
  df[[binary_column_name]] <- ifelse(df[[column_name]] == value, 1, 0)

  # Assign the modified data frame back to the global environment
  assign(df_name, df, envir = .GlobalEnv)
}

#' lazyleonpackage: A package for data manipulation and visualization
#'
#' The `lazyleonpackage` package provides functions for easy data manipulation and plot creation.
#'
#' @section Functions:
#' - \code{\link{plot_variable_over_time}}: Visualizes the development of a variable over time using ggplot2.
#'
#' - \code{\link{plot_correlation}}: Creates a scatter plot showing the correlation between two variables.
#'
#' - \code{\link{add_binary_column}}: Adds a binary column to a data frame based on a specified value in another column.
#'
#' @docType package
#' @name lazyleonpackage
#' @import ggplot2
NULL
