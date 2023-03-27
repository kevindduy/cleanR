#' Remove duplicate rows
#' This function removes duplicate rows from the input dataframe.
distinctMe <- function(data) {
  data <- distinct(data)
  return(data)
}
#___________________________________________________________
#' Convert date strings to date objects
#' This function converts columns containing date strings to date objects.
dateMe <- function(data, cols, format) {
  for (col in cols) {
    data[[col]] <- ymd(data[[col]], format=format)
  }
  return(data)
}
#___________________________________________________________
#' Remove columns from dataframe
#' This function removes columns from the input dataframe.
removeCol<- function(data, cols) {
  data <- data[, !colnames(data) %in% cols]
  return(data)
}
#___________________________________________________________
replaceMe <- function(data, old_values, new_values, cols) {
  for (col in cols) {
    for (i in 1:length(old_values)) {
      data[[col]] <- gsub(old_values[i], new_values[i], data[[col]])
    }
  }
  return(data)
}
#___________________________________________________________
#' Replace commas with periods in numeric columns
#' This function replaces commas with periods in columns containing numeric data. This is useful when reading in CSV files that use commas as decimal separators.
No_comma <- function(data, cols) {
  numeric_cols <- sapply(data, is.numeric)
  numeric_cols[cols] <- TRUE
  data[, numeric_cols] <- select_if(data[, numeric_cols], is.numeric) %>%
    mutate(across(cols, ~str_replace_all(.x, ",", ".")))
  return(data)
}

#___________________________________________________________
#' Remove rows with missing values
#' This function removes rows containing missing values from the input dataframe.
BatmanTheme <- function(data) {
  data <- na.omit(data)
  return(data)
}



