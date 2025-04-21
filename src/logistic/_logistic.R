#' ロジスティック回帰分析を実行する関数
#'
#' @param df データフレーム
#' @param target 目的変数（factor型）
#' @param predictors 説明変数（文字列ベクトル）
#' @return ロジスティック回帰分析の結果（リスト）
#' @export
#'
#' @examples
#' # 使用例
#' # result <- logistic(df = iris, target = "Species", predictors = c("Sepal.Length", "Sepal.Width"))
logistic <- function(df, target, predictors) {
  # フォーミュラの作成
  formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # ロジスティック回帰モデルの構築
  model <- glm(formula_obj, data = df, family = binomial())
  
  # モデルのサマリー
  model_summary <- summary(model)
  
  # オッズ比とその信頼区間の計算
  odds_ratio <- exp(coef(model))
  conf_int <- exp(confint(model))
  
  # 結果のリスト化
  result <- list(
    model = model,
    summary = model_summary,
    odds_ratio = odds_ratio,
    conf_int = conf_int
  )
  
  return(result)
}
