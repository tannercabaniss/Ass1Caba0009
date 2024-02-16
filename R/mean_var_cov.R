#' Mean, Variance, Covariance, and Correlation for Two Vectors
#'
#' This function calculates common metrics between two vectors including sample mean, sample variance, covariance,
#' and correlation (if requested) between the vectors.
#'
#' @param x1 input vector 1
#' @param x2 input vector 2
#' @param cor allows user to select if they want to calculate the correlation coefficient
#'
#' @return prints the sample means, sample variances, covariance, and correlation (if requested) cleanly
#' @export
#'
#' @examples
#' x1 <- c(1,2,3,4,5,6)
#' x2 <- c(4,5,3,2,4,6)
#' mean_var_cov(x1, x2, cor=TRUE)
mean_var_cov <- function(x1, x2, cor=FALSE) {
  mean_x1 <- mean(x1)
  mean_x2 <- mean(x2)

  var_x1 <- var(x1)
  var_x2 <- var(x2)

  cov_x1x2 <- cov(x1, x2)

  cor_x1x2 <- cor(x1, x2)

  cat("Vector x1:", x1, "\n")
  cat("Vector x2:", x2, "\n")
  cat("Mean x1:", mean_x1, "\n")
  cat("Mean x2:", mean_x2, "\n")
  cat("Variance x1:", var_x1, "\n")
  cat("Variance x2:", var_x2, "\n")
  cat("Covariance x1,x2:", cov_x1x2)

  if(cor==TRUE) {
    cat("\nCorrelation x1,x2:", cor_x1x2)
  }
}
