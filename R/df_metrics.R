#' Data Frame Metrics
#'
#' This function outputs relevant metrics for a data frame in column vector and/or matrix form
#'
#' @param df input data frame from which metrics are calculated
#'
#' @return printed output of the various data frame metrics (sample mean array, covariance matrix, and correlation matrix)
#' @export
#'
#' @examples
#' df <- Table_1_5
#' df_metrics(df)
df_metrics <- function (df) {
  mean_vec = colMeans(df)

  cov_mat = cov(df)

  cor_mat <- cor(df)

  cat("Sample Mean Array\n")
  print(round(mean_vec,4))
  cat("Sample Covariance/Variance Matrix\n")
  print(round(cov_mat,4))
  cat("Sample Correlation Matrix\n")
  print(round(cor_mat,4))
}
