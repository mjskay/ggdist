# A stat designed for use with geom_slabinterval
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
#' @param point_interval A function from the \code{\link{point_interval}} family (e.g., \code{median_qi}, \code{mean_qi}, etc).
#'   This function should obey the
#'   \code{.width} and \code{.simple_names} parameters of \code{\link{point_interval}} functions, such that when given
#'   a vector with \code{.simple_names = TRUE} should return a data frame with variables \code{.value}, \code{.lower},
#'   \code{.upper}, and \code{.width}. Output will be converted to the appropriate \code{x}- or \code{y}-based aesthetics
#'   depending on the value of \code{orientation}. See the \code{\link{point_interval}} family of functions for
#'   more information.
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
#' @importFrom dplyr bind_rows
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
    interval_args = modifyList(
      list(.width = .width),
      interval_args
    )

    draw_interval = FALSE
    if (is.null(interval_function)) {
      if (!is.null(point_interval)) {
        # need to set .simple_names here to get .value, .lower, and .upper
        interval_args$.simple_names = TRUE
        point_interval = as_function(point_interval)

        # function is a point_interval, we need to make the version that
        # can take in a data frame
        fun = function(df) {
          do.call(point_interval, c(list(quote(df[[x]])), interval_args))
        }
        draw_interval = TRUE
      }
    } else {
      interval_function = as_function(interval_function)

      fun = function(df) {
        do.call(interval_function, c(list(quote(df)), interval_args))
      }
      draw_interval = TRUE
    }

    i_data = NULL
    if (draw_interval) {
      i_data = summarise_by(data, c("group", y), fun)
      names(i_data)[names(i_data) == ".value"] = x
      names(i_data)[names(i_data) == ".lower"] = xmin
      names(i_data)[names(i_data) == ".upper"] = xmax
      i_data$level = forcats::fct_rev(ordered(i_data$.width))
      i_data$datatype = "interval"
    }

    bind_rows(s_data, i_data)
  }
)

