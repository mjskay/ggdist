# A stat designed for use with geom_slabinterval
#
# Author: mjskay
###############################################################################


#' Meta-stat for computing slab functions and interval functions (ggplot stat)
#'
#' A meta-stat for computing slab and interval functions for use with \code{\link{geom_slabinterval}}
#' and its derivatives. Generally speaking not intended to be used directly: The API for
#' this stat is \strong{experimental and subject to change}. This is used as the basis
#' for several other more directly useful stats whose APIs are more stable; it is recommended
#' to use those instead.
#'
#' @inheritParams geom_slabinterval
#' @param geom Use to override the default connection between
#' \code{stat_slabinterval} and \code{\link{geom_slabinterval}}
#' @param ...  Other arguments passed to \code{\link{layer}}. They may also be arguments to the paired geom
#' (e.g., \code{\link{geom_pointinterval}})
#' @param limits_function A function that takes a data frame of aesthetics and returns a data frame with
#' columns \code{.lower} and \code{.upper} indicating the limits of the input for the slab function for that data frame.
#' @param limits_args Additional arguments passed to \code{limits_function}
#' @param limits Limits for \code{slab_function}, as a vector of length two. These limits are combined with those
#' computed by the \code{limits_function} as well as the limits defined by the scales of the plot to determine the
#' limits used to draw the slab functions: these limits specify the maximal limits; i.e., if specified, the limits
#' will not be wider than these (but may be narrower).
#' @param slab_function A function that takes a data frame of aesthetics and an \code{input} parameter (a vector
#' of function inputs), and returns a data frame with
#' columns \code{.input} (from the \code{input} vector) and \code{.value} (result of applying the function to
#' each value of input). Given the results of \code{slab_function}, \code{.value} will be translated into the
#' \code{thickness} aesthetic and \code{input} will be translated into either the \code{x} or \code{y} aesthetic
#' automatically depending on the value of \code{orientation}.
#' @param slab_args Additional arguments passed to \code{limits_function}
#' @param n Number of points at which to evaluate \code{slab_function}
#' @param interval_function A function that takes a data frame of aesthetics and a \code{.width} parameter (a vector
#' of interval widths), and returns a data frame with
#' columns \code{.width} (from the \code{.width} vector), \code{.value} (point summary) and \code{.lower} and \code{.upper}
#' (endpoints of the inverals, given the \code{.width}). Output will be converted to the appropriate \code{x}- or
#' \code{y}-based aesthetics depending on the value of \code{orientation}. If \code{interval_function} is \code{NULL},
#' \code{point_interval} is used instead.
#' @param interval_args Additional arguments passed to \code{interval_function} or \code{point_interval}.
#' @param point_interval A function from the \code{\link{point_interval}} family (e.g., \code{median_qi},
#'   \code{mean_qi}, etc). This function should take in a vector of value, and should obey the
#'   \code{.width} and \code{.simple_names} parameters of \code{\link{point_interval}} functions, such that when given
#'   a vector with \code{.simple_names = TRUE} should return a data frame with variables \code{.value}, \code{.lower},
#'   \code{.upper}, and \code{.width}. Output will be converted to the appropriate \code{x}- or \code{y}-based aesthetics
#'   depending on the value of \code{orientation}. See the \code{\link{point_interval}} family of functions for
#'   more information.
#' @param .width The \code{.width} argument passed to \code{interval_function} or \code{point_interval}.
#' @seealso See \code{\link{geom_slabinterval}} for the geom version, intended
#' for use on data that has already been translated into funciton evaluations, points, and intervals.
#' @examples
#'
#' #TODO
#'
#' @importFrom rlang as_function
#' @importFrom dplyr bind_rows
#' @keywords internal
#' @export
stat_slabinterval = function(
  mapping = NULL,
  data = NULL,
  geom = "slabinterval",
  position = "identity",
  ...,

  orientation = c("vertical", "horizontal"),

  limits_function = NULL,
  limits_args = list(),
  limits = NULL,

  slab_function = NULL,
  slab_args = list(),
  n = 501,

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
    orientation = "vertical",

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



# limits, slab, and interval functions for distributions -------------------------

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

dist_limits_function = function(df, p_limits = c(.001, .999), ...) {
  pmap_dfr(df, function(dist, ...) {
    if (is.na(dist)) {
      return(data.frame(.lower = NA, .upper = NA))
    }

    args = args_from_aes(...)
    quantile_fun = match.fun(paste0("q", dist))
    limits = do.call(quantile_fun, c(list(quote(p_limits)), args))

    data.frame(
      .lower = limits[[1]],
      .upper = limits[[2]]
    )
  })
}

dist_slab_function = function(
  df, input, type = "pdf", limits = NULL, n = 201, ...
) {
  pmap_dfr(df, function(dist, ...) {
    if (is.na(dist)) {
      return(data.frame(.input = NA, .value = NA))
    }

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

dist_interval_function = function(df, .width, ...) {
  pmap_dfr(df, function(dist, ...) {
    if (is.na(dist)) {
      return(data.frame(.value = NA, .lower = NA, .upper = NA, .width = .width))
    }

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
