preprocess <- function(df) {
    # 全ての列がNAの行を除外
    df <- df[rowSums(is.na(df)) != ncol(df), ]
    
    # 全ての行がNAの列を除外
    df <- df[, colSums(is.na(df)) != nrow(df)]

    return(df)
}