#' Angle Vectors
#'
#' This function finds the angle between two vectors.
#'
#' @param vec1 input vector 1
#' @param vec2 input vector 2
#'
#' @return a named list of the angle between the vectors in degrees and radians
#' @export
#'
#' @examples
#' vec1 <- c(1,2,3)
#' vec2 <- c(2,1,5)
#' angle_vectors(vec1,vec2)
angle_vectors <- function (vec1,vec2) {
  dot_product <- sum(vec1 * vec2)
  mag1 <- sqrt(sum(vec1^2))
  mag2 <- sqrt(sum(vec2^2))

  angle_rad <- acos(dot_product / (mag1 * mag2))
  angle_deg <- angle_rad * (180/pi)

  angle_list <- list("Radians"=angle_rad, "Degrees"=angle_deg)
  return(angle_list)
}
