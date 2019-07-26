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

  limits_function = NULL,
  limits_args = list(),
  limits = NULL,

  slab_function = NULL,
  slab_args = list(),
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
      orientation = orientation,

      limits_function = limits_function,
      limits_args = limits_args,
      limits = limits,

      slab_function = slab_function,
      slab_args = slab_args,
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

  compute_panel = function(self, data, scales,
    orientation = "horizontal",

    limits_function = NULL,
    limits_args = list(),
    limits = NULL,

    slab_function = NULL,
    slab_args = list(),
    n = 101,

    interval_function = NULL,
    interval_args = list(),
    point_interval = NULL,
    .width = c(.66, .95),

    na.rm = FALSE
  ) {
    define_orientation_variables(orientation)

    # figure out coordinate transformation
    x_trans = if (is.null(scales[[x]]) || scales[[x]]$is_discrete()) {
      scales::identity_trans()
    } else {
      scales[[x]]$trans
    }

    # SLAB
    s_data = NULL
    if (!is.null(slab_function)) {

      # determine limits of the slab function
      # we do this first so we can figure out the overall limits
      # based on the min/max limits over the entire input data

      # manually-defined limits we want to obey as maximums
      # (the limits are *at most* these)
      max_limits = limits
      if (is.null(max_limits)) {
        if (is.null(scales[[x]]$limits)) {
          max_limits = c(NA, NA)
        } else{
          max_limits = x_trans$inverse(scales[[x]]$limits)
        }
      }

      # data-defined limits we want to obey as minimums
      # (the limits are *at least* these, unless the
      # max_limits are more narrow)
      min_limits = if (is.null(scales[[x]])) {
        c(NA, NA)
      } else {
        x_trans$inverse(scales[[x]]$dimension())
      }

      # for making versions of min/max that ignore NAs but also
      # return NA if there are no values / no non-NA values
      na_ = function(m_, ...) {
        values = na.omit(c(...))
        if (length(values) == 0) NA
        else m_(values)
      }

      # if a limits function was provided, we also want to account
      # for the limits suggested by that function based on the data
      # these will adjust min_limits
      if (!is.null(limits_function)) {
        limits_function = as_function(limits_function)
        limits_fun = function(df) do.call(limits_function, c(list(quote(df)), limits_args))
        l_data = summarise_by(data, c("group", y), limits_fun)
        min_limits = c(
          na_(min, l_data$.lower, min_limits[[1]]),
          na_(max, l_data$.upper, min_limits[[2]])
        )
      }

      limits = c(
        na_(max, min_limits[[1]], max_limits[[1]]),
        na_(min, min_limits[[2]], max_limits[[2]])
      )
      #default to 0 (min) and 1 (max) for unknown limits
      limits = ifelse(is.na(limits), c(0,1), limits)


      # now, figure out the points at which the slab functions should be evaluated
      # we set up the grid in the transformed space
      input = x_trans$inverse(seq(x_trans$transform(limits[[1]]), x_trans$transform(limits[[2]]), length.out = n))
      slab_args[["input"]] = input
      slab_args[["n"]] = n

      # evaluate the slab function
      slab_function = as_function(slab_function)
      slab_fun = function(df) do.call(slab_function, c(list(quote(df)), slab_args))
      s_data = summarise_by(data, c("group", y), slab_fun)

      names(s_data)[names(s_data) == ".value"] = "thickness"
      s_data[[x]] = x_trans$transform(s_data$.input)
      s_data$.input = NULL

      s_data$datatype = "slab"
    }


    # INTERVAL
    interval_args = modifyList(
      list(.width = .width),
      interval_args
    )

    draw_interval = FALSE
    if (is.null(interval_function)) {
      if (!is.null(point_interval)) {
        # need to set .simple_names here to get .value, .lower, and .upper
        interval_args$.simple_names = TRUE

        # function is a point_interval, we need to make the version that
        # can take in a data frame
        point_interval = as_function(point_interval)
        interval_fun = function(df) do.call(point_interval, c(list(quote(df[[x]])), interval_args))
        draw_interval = TRUE
      }
    } else {
      interval_function = as_function(interval_function)
      interval_fun = function(df) do.call(interval_function, c(list(quote(df)), interval_args))
      draw_interval = TRUE
    }

    i_data = NULL
    if (draw_interval) {
      i_data = summarise_by(data, c("group", y), interval_fun)

      i_data[[x]] = x_trans$transform(i_data$.value)
      i_data$.value = NULL
      i_data[[xmin]] = x_trans$transform(i_data$.lower)
      i_data$.lower = NULL
      i_data[[xmax]] = x_trans$transform(i_data$.upper)
      i_data$.upper = NULL

      i_data$level = forcats::fct_rev(ordered(i_data$.width))
      i_data$datatype = "interval"
    }

    bind_rows(s_data, i_data)
  }
)


# translate arguments of the form `arg1` ... `arg9` (or from a list column, args) into a single list of arguments
args_from_aes = function(args = list(), ...) {
  dot_args = list(...)
  args_from_dots = list()
  for (i in 1:9) {
    arg_name = paste0("arg", i)
    if (arg_name %in% names(dot_args)) {
      args_from_dots[[i]] = dot_args[[arg_name]]
    }
  }

  c(args_from_dots, args)
}


dist_interval_function = function(df, .width, ...) {
  pmap_dfr(df, function(dist, ...) {
    args = args_from_aes(...)
    quantile_fun = match.fun(paste0("q", dist))

    intervals = map_dfr(.width, function(w) {
      quantile_args = c(list(c(0.5, (1 - w)/2, (1 + w)/2)), args)
      quantiles = do.call(quantile_fun, quantile_args)
      data.frame(
        .value = quantiles[[1]],
        .lower = quantiles[[2]],
        .upper = quantiles[[3]],
        .width = w
      )
    })
  })
}

dist_function = function(
  df, input, type = "pdf", limits = NULL, n = 201, ...
) {
  pmap_dfr(df, function(dist, ...) {
    args = args_from_aes(...)
    dist_fun = switch(type,
      pdf = match.fun(paste0("d", dist)),
      cdf = match.fun(paste0("p", dist)),
      ccdf = {
        cdf = match.fun(paste0("p", dist));
        function (...) 1 - cdf(...)
      }
    )
    quantile_fun = match.fun(paste0("q", dist))

    data.frame(
      .input = input,
      .value = do.call(dist_fun, c(list(quote(input)), args))
    )
  })
}

dist_limits_function = function(df, p_limits = c(.001, .999)) {
  pmap_dfr(df, function(dist, ...) {
    args = args_from_aes(...)
    quantile_fun = match.fun(paste0("q", dist))
    limits = do.call(quantile_fun, c(list(quote(p_limits)), args))

    data.frame(
      .lower = limits[[1]],
      .upper = limits[[2]]
    )
  })
}
