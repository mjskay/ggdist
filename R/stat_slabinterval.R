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
#' @param ...  Other arguments passed to [layer()]. These are often aesthetics, used to set an aesthetic
#' to a fixed value, like `colour = "red"` or `size = 3` (see **Aesthetics**, below). They may also be
#' parameters to the paired geom/stat (e.g. `geom_slabinterval()`).
#' @param limits Manually-specified limits for the slab, as a vector of length two. These limits are combined with those
#' computed automatically for the slab as well as the limits defined by the scales of the plot to determine the
#' limits used to draw the slab functions: these limits specify the maximal limits; i.e., if specified, the limits
#' will not be wider than these (but may be narrower). Use `NA` to leave a limit alone; e.g.
#' `limits = c(0, NA)` will ensure that the lower limit does not go below 0.
#' @param n Number of points at which to evaluate the function that defines the slab.
#' @param point_interval A function from the [point_interval()] family (e.g., `median_qi`,
#'   `mean_qi`, `mode_hdi`, etc), or a string giving the name of a function from that family
#'   (e.g., `"median_qi"`, `"mean_qi"`, `"mode_hdi"`, etc). This function determines the point summary
#'   (typically mean, median, or mode) and interval type (quantile interval, `qi`;
#'   highest-density interval, `hdi`; or highest-density continuous interval, `hdci`). Output will
#'   be converted to the appropriate `x`- or `y`-based aesthetics depending on the value of `orientation`.
#'   See the [point_interval()] family of functions for more information.
#' @param .width The `.width` argument passed to `point_interval`: a vector of probabilities to use
#' that determine the widths of the resulting intervals. If multiple probabilities are provided,
#' multiple intervals per group are generated, each with a different probability interval (and
#' value of the corresponding `.width` and `level` generated variables).
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
#' @name stat_slabinterval
NULL


#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSlabinterval = ggproto("StatSlabinterval", AbstractStat,
  default_aes = aes(
    datatype = "slab",
    thickness = stat(f),
    size = stat(-.width),
    x = NULL,
    y = NULL
  ),

  default_params = defaults(list(
    limits = NULL,
    n = 501,

    point_interval = NULL,
    .width = c(.66, .95),

    show_slab = TRUE,
    show_point = TRUE,
    show_interval = TRUE
  ), AbstractStat$default_params),

  deprecated_params = union(c(
    ".prob",
    "limits_function", "limits_args",
    "slab_function", "slab_args",
    "interval_function", "fun.data", "interval_args", "fun.args"
  ), AbstractStat$deprecated_params),

  hidden_params = union(c(
    "show_slab", "show_point", "show_interval"
  ), AbstractStat$hidden_params),

  layer_args = defaults(list(
    show.legend = c(size = FALSE)
  ), AbstractStat$layer_args),

  orientation_options = defaults(list(
    main_is_orthogonal = TRUE, range_is_orthogonal = TRUE, group_has_equal = TRUE, main_is_optional = TRUE
  ), AbstractStat$orientation_options),

  setup_data = function(self, data, params) {
    data = ggproto_parent(AbstractStat, self)$setup_data(data, params)
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
  compute_slab = function(self, data, trans, input, ...) {
    data.frame()
  },

  # Compute interval(s). Takes a data frame of aesthetics and a `.width`
  # parameter (a vector of interval widths) and returns a data frame with
  # columns `.width` (from the `.width` vector), `.value` (point summary) and
  #`.lower` and `.upper` (endpoints of the intervals, given the `.width`).
  # Default implementation uses the `point_interval` parameter (a
  # `point_interval()` function) to compute summaries and intervals.
  # @param data The data frame of aesthetic values
  # @param trans the scale transformation object applied to the coordinate space
  # @param ... other stat parameters created by children of stat_slabinterval
  compute_interval = function(
    self, data, trans,
    orientation, point_interval,
    .width, na.rm,
    ...
  ) {
    if (is.null(point_interval)) return(data.frame())

    define_orientation_variables(orientation)

    point_interval(data[[x]], .simple_names = TRUE, .width = .width, na.rm = na.rm)
  },

  compute_panel = function(self, data, scales,
    orientation = self$default_params$orientation,
    show_slab = self$default_params$show_slab,
    show_point = self$default_params$show_point,
    show_interval = self$default_params$show_interval,
    na.rm = self$default_params$na.rm,
    ...
  ) {
    define_orientation_variables(orientation)

    # remove missing values
    data = ggplot2::remove_missing(data, na.rm, c(x, y), name = "stat_slabinterval")

    # figure out coordinate transformation
    trans = scales[[x]]$trans %||% scales::identity_trans()

    # SLABS
    s_data = if (show_slab) {
      compute_panel_slabs(self, data, scales, trans,
        orientation = orientation,
        na.rm = na.rm,
        ...
      )
    }

    # INTERVALS
    i_data = if (show_interval) {
      compute_panel_intervals(self, data, scales, trans,
        orientation = orientation,
        show_point = show_point,
        na.rm = na.rm,
        ...
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
  }
)

#' @rdname stat_slabinterval
#' @export
stat_slabinterval = make_stat(StatSlabinterval, geom = "slabinterval")


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


#' @param ... stat parameters
#' @noRd
compute_panel_slabs = function(
  self, data, scales, trans,
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
    trans = trans, orientation = orientation, ...
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
  # now, figure out the points at which values the slab functions should be evaluated
  # we set up the grid in the transformed space
  input = trans$inverse(seq(trans$transform(limits[[1]]), trans$transform(limits[[2]]), length.out = n))

  # evaluate the slab function
  s_data = summarise_by(data, c("group", y), self$compute_slab,
    trans = trans, input = input,
    orientation = orientation, limits = limits, n = n,
    ...
  )

  names(s_data)[names(s_data) == ".value"] = "f"
  s_data[[x]] = trans$transform(s_data$.input)
  s_data$.input = NULL

  if (nrow(s_data) > 0) s_data$datatype = "slab"
  s_data
}

#' @param ... stat parameters
#' @noRd
compute_panel_intervals = function(
  self, data, scales, trans,
  orientation, point_interval,
  ...
) {
  define_orientation_variables(orientation)

  if (!is.null(point_interval)) {
    point_interval = as_function(point_interval)
  }

  i_data = summarise_by(data, c("group", y), self$compute_interval,
    trans = trans,
    orientation = orientation, point_interval = point_interval,
    ...
  )

  i_data[[x]] = i_data$.value
  i_data$.value = NULL
  i_data[[xmin]] = i_data$.lower
  i_data$.lower = NULL
  i_data[[xmax]] = i_data$.upper
  i_data$.upper = NULL

  i_data$level = fct_rev_(ordered(i_data$.width))
  if (nrow(i_data) > 0) i_data$datatype = "interval"
  i_data
}
