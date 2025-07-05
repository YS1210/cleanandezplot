#' Quickly generate common ggplot2 plots
#'
#' @param data A data frame.
#' @param x Name of x-axis variable (character).
#' @param y Name of y-axis variable (character). Not needed for histograms or count-based bar plots.
#' @param type Type of plot: "hist", "line", "bar", "scatter".
#' @param bins Number of histogram bins (default = 30).
#' @param xlab Label for x-axis (optional).
#' @param ylab Label for y-axis (optional).
#' @param title Title for the plot (optional).
#'
#' @return A ggplot2 plot object.
#' @examples
#' quick_plot(mtcars, x = "mpg", type = "hist", xlab = "MPG", title = "Histogram of MPG")
#' quick_plot(mtcars, x = "hp", y = "mpg", type = "scatter", xlab = "Horsepower", ylab = "Miles per Gallon", title = "Horsepower vs Miles per Gallon")
#' quick_plot(mtcars, x = "hp", y = "mpg", type = "line")
#'quick_plot(mtcars, x = "gear", y = "mpg", type = "bar")
#'
#' @export
quick_plot <- function(data, x, y = NULL, type, bins = 30, xlab = NULL, ylab = NULL, title = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required.")
  }

  library(ggplot2)

  if (!x %in% names(data)) stop("x variable not found in data")
  if (!is.null(y) && !y %in% names(data)) stop("y variable not found in data")

  xlab <- if (is.null(xlab)) x else xlab
  ylab <- if (is.null(ylab)) (if (!is.null(y)) y else "Count") else ylab

  if (type == "hist") {
    p <- ggplot(data, aes_string(x = x)) +
      geom_histogram(bins = bins, fill = "steelblue", color = "white")
  } else if (type == "line") {
    p <- ggplot(data, aes_string(x = x, y = y)) +
      geom_line(color = "steelblue")
  } else if (type == "bar") {
    if (is.null(y)) {
      p <- ggplot(data, aes_string(x = x)) +
        geom_bar(fill = "steelblue")
    } else {
      p <- ggplot(data, aes_string(x = x, y = y)) +
        geom_col(fill = "steelblue")
    }
  } else if (type == "scatter") {
    p <- ggplot(data, aes_string(x = x, y = y)) +
      geom_point(color = "steelblue")
  } else {
    stop("Invalid type. Choose one of 'hist', 'line', 'bar', or 'scatter'.")
  }

  p + theme_minimal() +
    labs(x = xlab, y = ylab, title = title)
}
