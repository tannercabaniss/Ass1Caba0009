#' Scatter Plot with Marginal Dot Diagrams
#'
#' This function will take in two vectors of equal length and create a scatter plot as well as marginal diagrams.
#'
#' @param x1 input vector 1
#' @param x2 input vector 2
#' @param xlab label for x axis
#' @param ylab label for y axis
#' @param dotsize allows the user to set the dotsize while calling the function
#'
#' @return a scatter plot with marginal dot diagrams for both axes
#' @export
#'
#' @examples
#' x1 <- c(1,2,3,4,5,6)
#' x2 <- c(7,5,2,6,8,10)
#' scatter_marginal(x1,x2)
scatter_marginal <- function(x1, x2, xlab="x1", ylab="x2", dotsize=3) {
  if (length(x1)==length(x2)) {
    data <- data.frame(x1,x2)
    scatter_plot <- ggplot2::ggplot(data, ggplot2::aes(x = x1, y = x2)) +
      ggplot2::geom_point(size=dotsize) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x=xlab, y=ylab)
    marginal_included <- ggExtra::ggMarginal(scatter_plot, type="histogram", margins="both")
    return(marginal_included)

    # Attempt do make dot plots instead of histograms for marginal plots
    # empty_plot <- ggplot2::ggplot(data) + ggplot2::theme(panel.background = ggplot2::element_rect(fill="white"))
    #
    # marginal_x1 <- ggplot2::ggplot(data, ggplot2::aes(x = x1)) +
    #   ggplot2::geom_dotplot(binwidth = 0.5, fill = "blue", dotsize=0.5) +
    #   ggplot2::theme_minimal() +
    #   ggplot2::theme(axis.title.y = ggplot2::element_blank(),
    #         axis.text.y = ggplot2::element_blank(),
    #         axis.ticks.y = ggplot2::element_blank(),
    #         axis.text.x = ggplot2::element_blank(),
    #         axis.ticks.x = ggplot2::element_blank()) +
    #   ggplot2::labs(x=NULL, y=NULL) +
    #   ggExtra::removeGrid()
    #
    # marginal_x2 <- ggplot2::ggplot(data, ggplot2::aes(x = x2)) +
    #   ggplot2::geom_dotplot(binwidth = 0.5, fill = "red", dotsize=1) +
    #   ggplot2::theme_minimal() +
    #   ggplot2::theme(axis.title.y = ggplot2::element_blank(),
    #           axis.text.y = ggplot2::element_blank(),
    #           axis.ticks.y = ggplot2::element_blank(),
    #           axis.text.x = ggplot2::element_blank(),
    #           axis.ticks.x = ggplot2::element_blank()) +
    #   ggplot2::coord_flip() +
    #   ggplot2::labs(x=NULL, y=NULL) +
    #   ggExtra::removeGrid()
    #
    # final_plot <- cowplot::plot_grid(
    #   marginal_x1 + ggplot2::theme(legend.position = "none"),
    #   empty_plot,
    #   scatter_plot + ggplot2::theme(legend.position = "none"),
    #   marginal_x2 + ggplot2::theme(legend.position = "none"),
    #   nrow = 2, align = "v", rel_widths = c(4,4,4,1), rel_heights = c(1,1,4,2)
    # )
    # print(final_plot)
  }
  else {
    cat("Error: Vectors are not of equal length.\n")
    cat("Vector 1 length:", length(x1), "\nVector 2 length:", length(x2))
  }
}
