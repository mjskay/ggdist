# sample_draws
#
# Author: mjskay
###############################################################################

# sample_draws ---------------------------------------------------

#' Sample draws from a tidy-format data frame of draws
#'
#' Given a tidy-format data frame of draws with a column indexing each draw, subsample the data frame to a given size.
#'
#' @param data Data frame to sample from
#' @param n The number of draws to select
#' @param draw The name of the column indexing the draws
#' @author Matthew Kay
#' @keywords manip
#' @importFrom dplyr filter
#' @export
sample_draws = function(data, n, draw = ".draw") {
  .draw = as.name(draw)

  draw_full = data[[draw]]

  draw_sample = sample(unique(draw_full), n)

  filter(data, !!.draw %in% !!draw_sample)
}
