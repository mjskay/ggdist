# A stat designed for use with geom_slabinterval
#
# Author: mjskay
###############################################################################



#' @importFrom dplyr bind_rows
#' @importFrom rlang as_function
#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
AbstractStatSlabinterval = ggproto("AbstractStatSlabinterval", AbstractStat,
  default_aes = aes(
    datatype = "slab",
    thickness = after_stat(f),
    size = after_stat(-.width),
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
  # with columns `.input` (from the `input` vector) and `f` (the result of
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
  # [point_interval()] function) to compute summaries and intervals.
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
    limits = self$default_params$limits,
    n = self$default_params$n,
    point_interval = self$default_params$point_interval,
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


    # SLAB FUNCTION PRE-CALCULATIONS
    # determine limits of the slab function
    limits = compute_panel_limits(self, data, scales, trans,
      orientation = orientation, limits = limits,
      ...
    )

    # figure out the points at which values the slab functions should be evaluated
    # we set up the grid in the transformed space
    input = trans$inverse(seq(trans$transform(limits[[1]]), trans$transform(limits[[2]]), length.out = n))


    # POINT/INTERVAL PRE-CALCULATIONS
    if (!is.null(point_interval)) {
      point_interval = if (is.character(point_interval)) {
        # ensure we always search the ggdist namespace for point_interval
        # functions in case ggdist is not in the caller's search path
        get0(point_interval, mode = "function") %||%
          get(point_interval, mode = "function", envir = getNamespace("ggdist"))
      } else {
        as_function(point_interval)
      }
    }


    results = summarise_by(data, c("group", y), function(d) {
      dist = check_one_dist(d$dist)

      # we compute *both* the slab functions and intervals first (even if one
      # or the other will not be shown), since even if a component is not shown,
      # its values are still available in the other component
      #
      # TODO: currently we skip s_data if ggdist.experimental.slab_data_in_intervals
      # is FALSE and we aren't showing the slab because otherwise we can get a
      # bunch of spurious warnings when visualizing only intervals on a dist
      # where the slab functions can't be reliably computed (see e.g. the
      # logit dotplot example at the end of vignette("dotsinterval")); eventually
      # I'd like this to be reliable enough that we can compute it for that
      # example without warnings and remove this guard.
      s_data = if (getOption("ggdist.experimental.slab_data_in_intervals", FALSE) || show_slab) {
        self$compute_slab(d,
          trans = trans, input = input,
          orientation = orientation, limits = limits, n = n,
          na.rm = na.rm,
          ...
        )
      } else {
        data.frame()
      }
      i_data = self$compute_interval(d,
        trans = trans,
        orientation = orientation, point_interval = point_interval,
        show_point = show_point,
        na.rm = na.rm,
        ...
      )

      # SLABS
      s_data[[x]] = trans$transform(s_data$.input)
      s_data$.input = NULL
      if (nrow(s_data) > 0) s_data$datatype = "slab"


      # INTERVALS
      i_data[[x]] = i_data$.value
      i_data$.value = NULL
      i_data[[xmin]] = i_data$.lower
      i_data$.lower = NULL
      i_data[[xmax]] = i_data$.upper
      i_data$.upper = NULL

      i_data$level = fct_rev_(ordered(i_data$.width))
      if (nrow(i_data) > 0) i_data$datatype = "interval"

      # INTERVAL INFO ADDED TO SLAB COMPONENT
      if (show_slab) {
        # fill in relevant data from the interval component
        # this is expensive, so we only do it if we are actually showing the interval

        # find the smallest interval that contains each x value
        contains_x = outer(i_data[[xmin]], s_data[[x]], `<=`) & outer(i_data[[xmax]], s_data[[x]], `>=`)
        # need a fake interval guaranteed to contain all points (for NAs, points out of range...)
        contains_x = rbind(TRUE, contains_x)
        width = c(Inf, i_data$.width)
        smallest_interval = apply(ifelse(contains_x, width, NA), 2, which.min)

        # fill in the .width and level of the smallest interval containing x
        # (or NA if no such interval)
        s_data$.width = c(NA_real_, i_data$.width)[smallest_interval]
        s_data$level = vctrs::vec_c(NA, i_data$level)[smallest_interval]
      }


      # SLAB INFO ADDED TO INTERVAL COMPONENT
      if (
        getOption("ggdist.experimental.slab_data_in_intervals", FALSE) &&
        show_interval && nrow(s_data) - sum(is.na(s_data$pdf) | is.na(s_data$cdf)) >= 2
      ) {
        # fill in relevant data from the slab component
        # this is expensive, so we only do it if we are actually showing the interval
        pdf_fun = if (distr_is_constant(dist)) {
          dist_value = distr_quantile(dist)(0.5)
          function(x) ifelse(x == dist_value, Inf, 0)
        } else {
          approxfun(s_data[[x]], s_data$pdf, yleft = 0, yright = 0, ties = max)
        }
        i_data$pdf = pdf_fun(i_data[[x]])
        i_data$pdf_min = pdf_fun(i_data[[xmin]])
        i_data$pdf_max = pdf_fun(i_data[[xmax]])

        cdf_fun = if (distr_is_constant(dist)) {
          dist_value = distr_quantile(dist)(0.5)
          function(x) ifelse(x >= dist_value, 1, 0)
        } else {
          approxfun(s_data[[x]], s_data$cdf, yleft = 0, yright = 1, method = "constant", ties = max)
        }
        i_data$cdf = cdf_fun(i_data[[x]])
        i_data$cdf_min = cdf_fun(i_data[[xmin]])
        i_data$cdf_max = cdf_fun(i_data[[xmax]])
      }


      bind_rows(
        if (show_slab) s_data,
        if (show_interval) i_data
      )
    })

    # must ensure there's an f and a .width aesthetic produced even if we don't draw
    # the slab or the interval, otherwise the default aesthetic mappings can break.
    if (nrow(results) > 0) {
      results$f = results[["f"]] %||% NA_real_
      results$.width = results[[".width"]] %||% NA_real_
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

#' Compute the limits and the input x values for slab functions
#' @param ... stat parameters
#' @return length 2 vector giving the limits
#' @noRd
compute_panel_limits = function(
  self, data, scales, trans,
  orientation, limits,
  ...
) {
  define_orientation_variables(orientation)

  # determine limits of the slab function
  # we do this first so we can figure out the overall limits
  # based on the min/max limits over the entire input data

  # manually-defined limits we want to obey as maximums
  # (the limits are *at most* these)
  max_limits = limits
  if (is.null(max_limits)) {
    if (is.null(scales[[x]]$limits) || scales[[x]]$is_discrete()) {
      max_limits = c(NA_real_, NA_real_)
    } else {
      max_limits = trans$inverse(scales[[x]]$get_limits())
    }
  }

  # data-defined limits we want to obey as minimums
  # (the limits are *at least* these, unless the
  # max_limits are more narrow)
  min_limits = if (is.null(scales[[x]]) || scales[[x]]$is_empty()) {
    c(NA_real_, NA_real_)
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

  limits
}


# helpers -----------------------------------------------------------------

check_one_dist = function(dist) {
  if (length(dist) > 1) {
    stop0(glue('
      {length(dist)} distributions in `dist` were associated with the same
      combination of other aesthetics.
      - Distributions passed to the `dist` aesthetic must be uniquely associated
        with a combination of levels of the `group` and some other aesthethics
        (like x, y, color, fill, etc) so that unique intervals, densities, etc.
        of those distributions are well defined.
      - Try checking whether you need to adjust the `group` aesthetic or provide
        some other aesthetic mapping (like color or fill) to differentiate
        distributions.
      '
    ))
  }
  dist
}
