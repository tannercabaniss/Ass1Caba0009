#' Graph Vectors
#'
#' This function graphs two 3D vectors originating from the origin
#'
#' @param vec1 input vector 1
#' @param vec2 input vector 2
#'
#' @return a figure of the 3D graph of the two vectors
#' @export
#'
#' @examples
#' vec1 <- c(1,2,3)
#' vec2 <- c(-1,-2,-3)
#' graph_vectors(vec1,vec2)
graph_vectors <- function (vec1,vec2) {

  plot3D::arrows3D(0,0,0,vec1[1],vec1[2],vec1[3], col="blue")
  plot3D::arrows3D(0,0,0,vec2[1],vec2[2],vec2[3], col="red", add=TRUE)

  plot3D::text3D(vec1[1], vec1[2], vec1[3], labels = "x'", col = "blue", adj = 0, add=TRUE)
  plot3D::text3D(vec2[1], vec2[2], vec2[3], labels = "y'", col = "red", adj = 0, add=TRUE)
}
