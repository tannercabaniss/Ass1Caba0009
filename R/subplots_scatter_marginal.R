#' Subplots Scatter Marginal
#'
#' This function serves to create a figure layout where subplots are organized into columns and rows. The creation of
#'     each subplot is handled by the scatter_marginal function.
#'
#' @param df The data frame input for the figure (must have accurate col names already applied)
#'
#' @return a figure with multiple subplots
#' @export
#'
#' @examples
#' df <- Table_1_5
#' subplots_scatter_marginal(df)
subplots_scatter_marginal <- function(df) {
  plots_list <- list()
  for (i in 1:(ncol(df) - 1)) {
    for (j in (i + 1):ncol(df)) {
      plots_list[[length(plots_list) + 1]] <- scatter_marginal(df[, i], df[, j], colnames(df)[i], colnames(df)[j], 2)
    }
  }

  # Arrange the plots in a grid
  final_plot <- cowplot::plot_grid(plotlist = plots_list, nrow = 7, ncol = 3)

  # Print the final plot
  print(final_plot)
}
