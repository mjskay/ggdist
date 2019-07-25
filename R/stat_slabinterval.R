# A stat_summary with a geom_pointinterval
#
# Author: mjskay
###############################################################################


# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c("...width.."))


#' Compute slab functions and interval functions (ggplot stat)
#'
#' A combination of \code{\link{stat_summary}} / \code{\link{stat_summaryh}} and
#' \code{\link{geom_pointinterval}} / \code{\link{geom_pointintervalh}} with sensible defaults.
#' While the corresponding \code{geom}s are intended for use on
#' data frames that have already been summarized using a \code{\link{point_interval}}
#' function, these \code{stat}s are intended for use directly on data frames of draws, and
#' will perform the summarization using a \code{\link{point_interval}} function.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param geom Use to override the default connection between
#' \code{geom_pointinterval}/\code{geom_pointintervalh} and \code{stat_pointinterval}/\code{stat_pointintervalh}.
#' @param position The position adjustment to use for overlapping points on this layer.
#' @param ...  Other arguments passed to \code{\link{layer}}. They may also be arguments to the paired geom.
#' @param point_interval A function that when given a vector should
#'   return a data frame with variables \code{y}, \code{ymin}, \code{ymax}, and \code{.width}; or
#'   \code{x}, \code{xmin}, \code{xmax}, and \code{.width}. \strong{Either is acceptable}: output
#'   will be converted into the \code{y}-based aesthetics for \code{stat_pointinterval} and the
#'   \code{x}-based aesthetics for \code{stat_pointintervalh}. See the \code{point_interval} family of functions.
#' @param fun.data Similar to \code{point_interval}, for compatibility with \code{stat_summary}.
#'   Note: if the summary function is passed using \code{fun.data}, the \code{x} and \code{y}-based aesthetics
#'   are not converted to the correct form automatically.
#' @param .width The \code{.width} argument passed to \code{point_interval}.
#' @param .prob Deprecated. Use \code{.width} instead.
#' @param fun.args Other optional arguments passed to \code{fun.data}.
#' @param na.rm	If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing
#' values are silently removed.
#' @param show.legend Should this layer be included in the legends? Default is \code{c(size = FALSE)}, unlike most geoms,
#' to match its common use cases. \code{FALSE} hides all legends, \code{TRUE} shows all legends, and \code{NA} shows only
#' those that are mapped (the default for most geoms).
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them. This is
#' most useful for helper functions that define both data and aesthetics and shouldn't inherit behavior from the
#' default plot specification, e.g. borders.
#' @seealso See \code{\link{geom_pointinterval}} / \code{\link{geom_pointintervalh}} for the geom versions, intended
#' for use on points and intervals that have already been summarized using a \code{\link{point_interval}} function.
#' See \code{\link{stat_interval}} / \code{\link{stat_intervalh}} for a similar stat intended for intervals without
#' point summaries.
#' @examples
#'
#' #TODO
#'
#' @importFrom rlang as_function
#' @export
stat_slabinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  orientation = c("horizontal", "vertical"),

  slab_function = NULL,
  slab_args = list(),
  limits = NULL,
  n = 101,

  interval_function = NULL,
  interval_args = list(),
  point_interval = NULL,
  .width = c(.66, .95),

  na.rm = FALSE,

  show.legend = NA,
  inherit.aes = TRUE
) {
  orientation = match.arg(orientation)

  layer(
    data = data,
    mapping = mapping,
    stat = StatSlabinterval,
    geom = geom,
    position = position,

    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      slab_function = slab_function,
      slab_args = slab_args,
      limits = limits,
      n = n,

      interval_function = interval_function,
      interval_args = interval_args,
      point_interval = point_interval,
      .width = .width,

      na.rm = na.rm,
      ...
    )
  )
}

StatSlabinterval <- ggproto("StatSlabinterval", Stat,
  default_aes = aes(
    datatype = "slab"
  ),

  compute_panel = function(data, scales,
    orientation = "horizontal",

    slab_function = NULL,
    slab_args = list(),
    limits = NULL,
    n = 101,

    interval_function = NULL,
    interval_args = list(),
    point_interval = NULL,
    .width = c(.66, .95),

    na.rm = FALSE
  ) {
    define_orientation_variables(orientation)

    # do the slab
    s_data = NULL


    # do the interval
    interval_args = modifyList(list(.width = .width), interval_args)

    draw_interval = FALSE
    if (is.null(interval_function)) {
      if (!is.null(point_interval)) {
        # function is a point_interval, we need to make the version that
        # can take in a data frame
        fix_orientation_aes = switch(orientation,
          horizontal = horizontal_aes,
          vertical = vertical_aes
        )
        point_interval = fix_orientation_aes(as_function(point_interval))

        fun = function(df) {
          do.call(point_interval, c(list(df[[x]]), interval_args))
        }
        draw_interval = TRUE
      }
    } else {
      interval_function = as_function(interval_function)

      fun = function(df) {
        do.call(interval_function, c(list(df), interval_args))
      }
      draw_interval = TRUE
    }

    i_data = NULL
    if (draw_interval) {
      i_data = summarise_by(data, c("group", y), fun)
      i_data$level = forcats::fct_rev(ordered(i_data$.width))
      i_data$datatype = "interval"
    }

    rbind(s_data, i_data)
  }
)

