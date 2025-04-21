# リストを受け取り、そのリストの要素をfactorに変換
convert_to_factor <- function(column, factor1, factor2) {
  result <- as.factor(ifelse(column == factor1, 1,
                             ifelse(column == factor2, 0, NA)))
  return(result)
}
