check_na <- function(df) {
  # 各列の欠損率を計算
  na_rates <- sapply(df, function(x) {
    sum(is.na(x)) / length(x) * 100
  })
  
  # 結果をデータフレームに変換
  result <- data.frame(
    column = names(na_rates),
    na_rate = na_rates,
    row.names = NULL
  )
  
  # 欠損率の降順でソート
  result <- result[order(result$na_rate, decreasing = TRUE), ]
  
  return(result)
}
    