#' #' Check and clean missing values in a data frame
#'
#' Automatically checks for missing values. If found, either removes rows or fills values depending on the method.
#' If no missing values are found, returns the original data with a message.
#'
#' @param data A data frame.
#' @param method How to handle missing values: "remove" (default) or "fill".
#' @param fill_value A numeric value used to fill missing numeric values (only used if method = "fill").
#'
#' @return A cleaned data frame.
#' @examples
#' clean_if_missing(iris)
#' clean_if_missing(mtcars, method = "fill", fill_value = -99)
#' clean_if_missing(mtcars, method = "remove")
#'
#' @export
clean_if_missing <- function(data, method = "remove", fill_value = 0) {
  if (!is.data.frame(data)) stop("Input must be a data frame")
  if (!method %in% c("fill", "remove")) stop("method must be either 'fill' or 'remove'")

  total_na <- sum(is.na(data))

  if (total_na == 0) {
    message("No missing values detected. Returning original data.")
    return(data)
  }

  message("Missing values detected: ", total_na, " in total.")
  message("Processing missing values using method: '", method, "'")

  if (method == "remove") {
    message("Removing all rows with any missing values...")
    return(na.omit(data))
  }

  if (method == "fill") {
    message("Filling missing numeric values with: ", fill_value)
    for (col in names(data)) {
      if (is.numeric(data[[col]])) {
        data[[col]][is.na(data[[col]])] <- fill_value
      }
    }
    return(data)
  }
}
