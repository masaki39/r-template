# ODI（Oswestry Disability Index）スコアを計算する関数
# 
# この関数は、ODIの各セクションのスコアから総合的なODIスコアを計算します。
# ODIは腰痛患者の機能障害を評価するための指標で、0-100点で評価されます。
# 
# 引数:
#   - df: データフレーム
#   - item_list: 各セクションの項目名を含むリスト
# 
# 戻り値:
#   - ODIスコアが追加されたデータフレーム
# 
# 注意点:
#   - 各項目は0-5点で評価
#   - 未回答のセクションは除外して計算
#   - 最終スコアは0-100点に変換
#   - すべての項目がNAの場合はNAを返す


calculate_ODI <- function(df, item_list) {
  # データを数値に変換（数値でない場合はNA）
  df_numeric <- as.data.frame(lapply(df, function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  }))
  
  # 0-5点の範囲外の値をNAに変換
  df_numeric[df_numeric < 0 | df_numeric > 5] <- NA
  
  # すべての項目がNAの行を特定
  all_na_rows <- apply(df_numeric, 1, function(x) all(is.na(x)))
  
  # 各セクションの合計得点を計算
  section_scores <- sapply(item_list, function(items) {
    # セクション内のすべての項目がNAかどうかをチェック
    section_all_na <- apply(df_numeric[, items, drop = FALSE], 1, function(x) all(is.na(x)))
    # 合計を計算（すべてNAの場合はNAを返す）
    scores <- ifelse(section_all_na, NA, rowSums(df_numeric[, items, drop = FALSE], na.rm = TRUE))
    return(scores)
  })
  
  # 未回答のセクション数をカウント
  missing_sections <- rowSums(is.na(section_scores))
  
  # ODIスコアを計算
  total_scores <- rowSums(section_scores, na.rm = TRUE)
  denominators <- 50 - (missing_sections * 5)
  
  # ODIスコアを計算（0-100の範囲）
  df$ODI <- (total_scores / denominators) * 100
  
  # すべての項目がNAの場合はNAを返す
  df$ODI[all_na_rows] <- NA
  
  # 結果を返す
  return(df)
}