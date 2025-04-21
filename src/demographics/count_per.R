count_per <- function(column, target_string) {
  proportion <- mean(column == target_string, na.rm = TRUE)
  percentage <- round(proportion * 100, 1)
  count <- sum(column == target_string, na.rm = TRUE)
  paste(percentage, "% (", count, ")", sep = "")
}