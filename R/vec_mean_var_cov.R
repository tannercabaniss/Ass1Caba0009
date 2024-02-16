#' Mean, Variance, Covariance, and Correlation Vectors for Sample Two Vectors
#'
#' This function calculates common metrics between two vectors including sample mean, sample variance, covariance,
#' and correlation and returns them in matrix form.
#'
#' @param x1 input vector 1
#' @param x2 input vector 2
#' @param x3 option input vector 3
#'
#' @return prints the sample means, sample variances, covariance, and correlation in matrix form
#' @export
#'
#' @examples
#' x1 <- c(1,2,3,4,5,6)
#' x2 <- c(4,5,3,2,4,6)
#' vec_mean_var_cov(x1, x2)
vec_mean_var_cov <- function(x1, x2, x3=NULL) {
  if (length(x3)==0) {
    data <- data.frame(x1,x2)
  }
  else if (length(x3)!=0) {
    data <- data.frame(x1,x2,x3)
  }

  mean_vec = colMeans(data)

  cov_mat = cov(data)

  cor_mat <- cor(data)

  cat("Sample Mean Array\n")
  print(mean_vec)
  cat("Sample Covariance/Variance Matrix\n")
  print(cov_mat)
  cat("Sample Correlation Matrix\n")
  print(cor_mat)
}
