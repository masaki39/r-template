# 列名にスペースがある場合、スペースをアンダースコアに変換する

replace_space_of_colname <- function(df) {
  colnames(df) <- gsub(" ", "_", colnames(df))
  return(df)
}


