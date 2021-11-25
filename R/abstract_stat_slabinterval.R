# A stat designed for use with geom_slabinterval
#
# Author: mjskay
###############################################################################



#' @importFrom dplyr bind_rows
#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
AbstractStatSlabinterval = ggproto("AbstractStatSlabinterval", AbstractStat,
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

#' @importFrom rlang as_function
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
