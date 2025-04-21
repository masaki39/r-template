# 指定された列のNAを含む行を除外
remove_na <- function(df, ...) {
  # 全ての列がNAの行を除外
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  # 全ての行がNAの列を除外
  df <- df[, colSums(is.na(df)) != nrow(df)]
  # 指定された列のNAを含む行を除外
  df[complete.cases(df[...]), ]
}