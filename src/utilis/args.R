#' 引数をリストに変換
#' @param ... 引数
#' @return リスト
process_args <- function(...) {
    args <- list(...)
    if (length(args) == 1) {
        if (is.data.frame(args[[1]])) list(args[[1]])
        else if (is.list(args[[1]])) args[[1]]
        else args
    } else args
}

#' 結果を適切な形式で返す
#' @param result 結果のリスト
#' @return 単一の場合は要素、複数の場合はリスト
simplify_result <- function(result) {
    if (length(result) == 1) result[[1]] else result
} 