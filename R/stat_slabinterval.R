# A stat designed for use with geom_slabinterval
#
# Author: mjskay
###############################################################################


#' Meta-stat for computing slab functions and interval functions (ggplot stat)
#'
#' A meta-stat for computing slab and interval functions for use with [geom_slabinterval()]
#' and its derivatives. Generally speaking not intended to be used directly: The API for
#' this stat is **experimental and subject to change**. This is used as the basis
#' for several other more directly useful stats whose APIs are more stable; it is recommended
#' to use those instead.
#'
#' @eval rd_slabinterval_aesthetics(stat = StatSlabinterval)
#' @inheritParams geom_slabinterval
#' @param geom Use to override the default connection between
#' `stat_slabinterval` and [geom_slabinterval()]
#' @param ...  Other arguments passed to [layer()]. They may also be arguments to the paired geom
#' (e.g., [geom_pointinterval()])
#' @param limits Manually-specified limits for the slab, as a vector of length two. These limits are combined with those
#' computed automatically for the slab as well as the limits defined by the scales of the plot to determine the
#' limits used to draw the slab functions: these limits specify the maximal limits; i.e., if specified, the limits
#' will not be wider than these (but may be narrower). Use `NA` to leave a limit alone; e.g.
#' `limits = c(0, NA)` will ensure that the lower limit does not go below 0.
#' @param n Number of points at which to evaluate the function that defines the slab.
#' @param interval_function Custom function for generating intervals (for most common use cases the `point_interval`
#' argument will be easier to use). This function takes a data frame of aesthetics and a `.width` parameter (a vector
#' of interval widths), and returns a data frame with
#' columns `.width` (from the `.width` vector), `.value` (point summary) and `.lower` and `.upper`
#' (endpoints of the intervals, given the `.width`). Output will be converted to the appropriate `x`- or
#' `y`-based aesthetics depending on the value of `orientation`. If `interval_function` is `NULL`,
#' `point_interval` is used instead.
#' @param interval_args Additional arguments passed to `interval_function` or `point_interval`.
#' @param point_interval A function from the [point_interval()] family (e.g., `median_qi`,
#'   `mean_qi`, etc). This function should take in a vector of value, and should obey the
#'   `.width` and `.simple_names` parameters of [point_interval()] functions, such that when given
#'   a vector with `.simple_names = TRUE` should return a data frame with variables `.value`, `.lower`,
#'   `.upper`, and `.width`. Output will be converted to the appropriate `x`- or `y`-based aesthetics
#'   depending on the value of `orientation`. See the [point_interval()] family of functions for
#'   more information.
#' @param .width The `.width` argument passed to `interval_function` or `point_interval`.
#' @param show.legend Should this layer be included in the legends? Default is `c(size = FALSE)`, unlike most geoms,
#' to match its common use cases. `FALSE` hides all legends, `TRUE` shows all legends, and `NA` shows only
#' those that are mapped (the default for most geoms).
#' @seealso See [geom_slabinterval()] for the geom version, intended
#' for use on data that has already been translated into function evaluations, points, and intervals.
#' See [stat_sample_slabinterval()] and [stat_dist_slabinterval()] for families of stats
#' built on top of this stat for common use cases (like `stat_halfeye`).
#' See `vignette("slabinterval")` for a variety of examples of use.
#' @examples
#'
#' # stat_slabinterval() is typically not that useful on its own.
#' # See vignette("slabinterval") for a variety of examples of the use of its
#' # shortcut geoms and stats, which are more useful than using
#' # stat_slabinterval() directly.
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

  orientation = NA,
  limits = NULL,
  n = 501,

  interval_function = NULL,
  interval_args = list(),
  point_interval = NULL,
  .width = c(.66, .95),

  show_slab = TRUE,
  show_interval = TRUE,

  na.rm = FALSE,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE
) {
  .Deprecated_arguments(c("limits_function", "limits_args", "slab_function", "slab_args"), ...)

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
      limits = limits,
      n = n,

      interval_function = interval_function,
      interval_args = interval_args,
      point_interval = point_interval,
      .width = .width,

      show_slab = show_slab,
      show_interval = show_interval,

      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSlabinterval = ggproto("StatSlabinterval", Stat,
  default_aes = aes(
    datatype = "slab",
    thickness = stat(f),
    size = stat(-.width),
    x = NULL,
    y = NULL
  ),

  default_params = list(
    orientation = NA,
    limits = NULL,
    n = 501,
    interval_function = NULL,
    interval_args = list(),
    point_interval = NULL,
    .width = c(.66, .95),

    show_slab = TRUE,
    show_interval = TRUE,

    na.rm = FALSE
  ),

  setup_params = function(self, data, params) {
    params = defaults(params, self$default_params)

    # detect orientation
    params$flipped_aes = get_flipped_aes(data, params,
      main_is_orthogonal = TRUE, range_is_orthogonal = TRUE, group_has_equal = TRUE, main_is_optional = TRUE
    )
    params$orientation = get_orientation(params$flipped_aes)

    params
  },

  setup_data = function(self, data, params) {
    #set up orientation
    data$flipped_aes = params$flipped_aes
    define_orientation_variables(params$orientation)

    # when we are missing a main aesthetic (e.g. the y aes in a horizontal orientation),
    # fill it in with 0 so that we can still draw stuff
    data[[y]] = data[[y]] %||% 0

    data
  },

  # A function that takes a data frame of aesthetics and returns a data frame with
  # columns `.lower` and `.upper` indicating the limits of the input for the slab
  # function for that data frame
  # @param data The data frame of aesthetic values
  # @param trans the scale transformation object applied to the coordinate space
  # @param ... other stat parameters created by children of stat_slabinterval
  compute_limits = function(self, data, trans, ...) {
    data.frame(.lower = NA, .upper = NA)
  },

  # Compute the function that defines the slab. That takes a data frame of
  # aesthetic values and a vector of function inputs and returns a data frame
  # with columns `.input` (from the `input` vector) and `.value` (the result of
  # applying the function to each value of input).
  # @param data The data frame of aesthetic values
  # @param input Input values for the function (may be ignored in some cases
  # where compute_slab() needs to determine its own input values)
  # @param trans the scale transformation object applied to the coordinate space
  # @param ... other stat parameters created by children of stat_slabinterval
  compute_slab = function(self, data, input, trans, ...) {
    data.frame()
  },

  compute_panel = function(self, data, scales,
    orientation = self$default_params$orientation,
    limits = self$default_params$limits,
    n = self$default_params$n,

    interval_function = self$default_params$interval_function,
    interval_args = self$default_params$interval_args,
    point_interval = self$default_params$point_interval,
    .width = self$default_params$.width,

    show_slab = self$default_params$show_slab,
    show_interval = self$default_params$show_interval,

    na.rm = self$default_params$na.rm,
    ...
  ) {
    define_orientation_variables(orientation)

    # remove missing values
    data = ggplot2::remove_missing(data, na.rm, c(x, y), name = "stat_slabinterval")

    # figure out coordinate transformation
    trans = if (is.null(scales[[x]]) || scales[[x]]$is_discrete()) {
      scales::identity_trans()
    } else {
      scales[[x]]$trans
    }

    # SLABS
    s_data = if (show_slab) {
      compute_slabs_(self, data, scales, trans,
        orientation = orientation, limits = limits, n = n,
        na.rm = na.rm,
        ...
      )
    }

    # INTERVALS
    i_data = if (show_interval) {
      compute_intervals_(self, data, scales, trans, na.rm,
        orientation, interval_function, interval_args, point_interval, .width
      )
    }

    results = bind_rows(s_data, i_data)
    # must ensure there's an f and a .width aesthetic produced even if we don't draw
    # the slab or the interval, otherwise the default aesthetic mappings can break.
    if (nrow(results) > 0) {
      results$f = results[["f"]] %||% NA
      results$.width = results[[".width"]] %||% NA
    }
    results
  },

  parameters = function(self, extra = TRUE) {
    # Based on ggplot2::Stat$parameters, except we always take parameters
    # from compute_panel(), because compute_group() is not used by this stat,
    # and we do take extra_params by default
    panel_args = names(ggproto_formals(self$compute_panel))

    # Remove arguments of defaults
    args <- setdiff(panel_args, c(names(ggproto_formals(Stat$compute_group)), "..."))

    if (extra) {
      args <- union(args, self$extra_params)
    }
    args
  }
)



# stat computation functions ----------------------------------------------

# for making versions of min/max that ignore NAs but also
# return NA if there are no values / no non-NA values
# (in compute_slab)
#' @importFrom stats na.omit
na_ = function(m_, ...) {
  values = c(...)
  if (all(is.na(values))) NA
  else m_(values, na.rm = TRUE)
}


compute_slabs_ = function(self, data, scales, trans,
  orientation, limits, n,
  ...
) {
  define_orientation_variables(orientation)

  # LIMITS
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
      max_limits = trans$inverse(scales[[x]]$limits)
    }
  }

  # data-defined limits we want to obey as minimums
  # (the limits are *at least* these, unless the
  # max_limits are more narrow)
  min_limits = if (is.null(scales[[x]])) {
    c(NA, NA)
  } else {
    trans$inverse(scales[[x]]$dimension())
  }

  # we also want to account for the limits suggested by compute_limits()
  # based on the data; these will adjust min_limits
  l_data = summarise_by(data, c("group", y), self$compute_limits,
    trans = trans, ...
  )
  min_limits = c(
    na_(min, l_data$.lower, min_limits[[1]]),
    na_(max, l_data$.upper, min_limits[[2]])
  )

  limits = c(
    na_(max, min_limits[[1]], max_limits[[1]]),
    na_(min, min_limits[[2]], max_limits[[2]])
  )
  #default to 0 (min) and 1 (max) for unknown limits
  limits = ifelse(is.na(limits), c(0,1), limits)


  # SLABS
  # now, figure out the points at which the slab functions should be evaluated
  # we set up the grid in the transformed space
  input = trans$inverse(seq(trans$transform(limits[[1]]), trans$transform(limits[[2]]), length.out = n))

  # evaluate the slab function
  s_data = summarise_by(data, c("group", y), self$compute_slab,
    input = input, trans = trans,
    limits = limits, n = n, orientation = orientation,
    ...
  )

  names(s_data)[names(s_data) == ".value"] = "f"
  s_data[[x]] = trans$transform(s_data$.input)
  s_data$.input = NULL

  if (nrow(s_data) > 0) s_data$datatype = "slab"
  s_data
}


compute_intervals_ = function(self, data, scales, trans, na.rm,
  orientation, interval_function, interval_args, point_interval, .width
) {
  define_orientation_variables(orientation)

  interval_args[[".width"]] = .width

  if (is.null(interval_function)) {
    if (!is.null(point_interval)) {
      # need to set .simple_names here to get .value, .lower, and .upper
      interval_args$.simple_names = TRUE

      # function is a point_interval, we need to make the version that
      # can take in a data frame
      point_interval = as_function(point_interval)
      interval_fun = function(df) {
        do.call(point_interval, c(list(quote(df[[x]])), interval_args))
      }
    } else {
      # no value for interval_function or pointinterval => no interval to draw
      return(NULL)
    }
  } else {
    interval_args[["orientation"]] = orientation
    interval_args[["trans"]] = trans
    interval_function = as_function(interval_function)
    interval_fun = function(df) do.call(interval_function, c(list(quote(df)), interval_args))
  }

  i_data = summarise_by(data, c("group", y), interval_fun)

  i_data[[x]] = i_data$.value
  i_data$.value = NULL
  i_data[[xmin]] = i_data$.lower
  i_data$.lower = NULL
  i_data[[xmax]] = i_data$.upper
  i_data$.upper = NULL

  i_data$level = fct_rev_(ordered(i_data$.width))
  i_data$datatype = "interval"
  i_data
}
