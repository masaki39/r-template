multivariate <- function(objective, explanatory, df) {
  # モデルの構築
  formula <- as.formula(paste(objective, "~", paste(explanatory, collapse = " + ")))
  model <- lm(formula, data = df)
  summary_model <- summary(model)
  
  # 多重共線性のチェック（相関行列）
  numeric_cols <- sapply(df[, explanatory], is.numeric)
  cor_matrix <- cor(df[, explanatory[numeric_cols]], use = "complete.obs")
  
  # 残差プロット
  par(mfrow = c(2, 2))
  plot(model)

  return(list(summary_model, cor_matrix))
}
