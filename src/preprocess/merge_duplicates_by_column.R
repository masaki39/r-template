#  dplyrが必要
# この関数は、指定された列で重複する行をマージし、各列の最初の非NA値を選択します。
# 引数:
#   - df: データフレーム
#   - col_name: 重複をマージする基準となる列名
# 戻り値:
#   - 重複をマージしたデータフレーム

merge_duplicates_by_column <- function(df, col_name) {
  # 指定された列でグループ化
  df <- df %>%
    group_by(across(all_of(col_name))) %>%
    # 各列に対して、最初の非NA値を選択
    summarise(across(everything(), ~ {
      # グループ内のすべての値を取得
      values <- .x[!is.na(.x)]
      if (length(values) > 0) {
        # 値がある場合は最初の値を選択
        return(values[1])
      } else {
        # すべてNAの場合はNAを返す
        return(NA)
      }
    })) %>%
    ungroup()
  
  return(df)
}
