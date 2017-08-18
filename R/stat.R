# Helper methods for creating stats
#
# Author: mjskay
###############################################################################

# from ggstance:::generate
generate <- function(fn) {
  get(fn, -1, environment(ggplot_build))
}

# from ggstance:::uniquecols
uniquecols <- generate("uniquecols")

# from ggstance:::summarise_by_y
summarise_by_y <- function(data, summary, ...) {
  summary <- plyr::ddply(data, c("group", "y"), summary, ...)
  unique <- plyr::ddply(data, c("group", "y"), uniquecols)
  unique$x <- NULL

  merge(summary, unique, by = c("y", "group"), sort = FALSE)
}

summarise_by_x <- function(data, summary, ...) {
  summary <- plyr::ddply(data, c("group", "x"), summary, ...)
  unique <- plyr::ddply(data, c("group", "x"), uniquecols)
  unique$y <- NULL

  merge(summary, unique, by = c("x", "group"), sort = FALSE)
}
