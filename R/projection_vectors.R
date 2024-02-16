#' Projection Vectors
#'
#' This function serves to project a vector y onto a vector x where the call format (x,y) is used
#'
#' @param vec1 the input vector on which the other vector is projected onto
#' @param vec2 the input vector which is projected onto the first vector
#'
#' @return the projection of vector 2 onto vector 1
#' @export
#'
#' @examples
#' vec1 <- c(1,2,3)
#' vec2 <- c(2,1,4)
#' projection_vectors(vec1,vec2)
projection_vectors <- function (vec1,vec2) {
  dot_prod <- sum(vec1 * vec2)
  mag1_sqrd <- sum(vec1^2)
  projection <- (dot_prod / mag1_sqrd) * vec1

  return(projection)
}
