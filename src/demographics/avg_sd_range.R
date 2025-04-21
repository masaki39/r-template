avg_sd_range <- function(column) {
  avg <- mean(column, na.rm = TRUE)
  sd_val <- sd(column, na.rm = TRUE)
  min_val <- min(column, na.rm = TRUE)
  max_val <- max(column, na.rm = TRUE)
  paste(round(avg, 1), "±", round(sd_val, 1), " (", round(min_val, 1), "-", round(max_val, 1), ")")
}