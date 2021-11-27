# Deprecated functions and arguments
#
# Author: mjskay
###############################################################################



# ggdist-deprecated ----------------------------------------------------

#' Deprecated functions and arguments in ggdist
#'
#' Deprecated functions and arguments and their alternatives are listed below.
#'
#' @section Deprecated Arguments:
#'
#' Parameters for `stat_slabinterval()` and family deprecated as of ggdist 3.1 are:
#'
#' - The `.prob` argument, which is a long-deprecated alias for `.width`, was
#' removed in ggdist 3.1.
#' - The `limits_function` argument: this was a parameter for determining the
#' function to compute limits of the slab in `stat_slabinterval()` and its
#' derived stats. This function is really an internal function only needed by
#' subclasses of the base class, yet added a lot of noise to the documentation,
#' so it was replaced with `AbstractStatSlabInterval$compute_limits()`.
#' - The `limits_args` argument: extra stat parameters are now passed through to
#' the `...` arguments to `AbstractStatSlabInterval$compute_limits()`; use
#' these instead.
#' - The `slab_function` argument: this was a parameter for determining the
#' function to compute slabs in `stat_slabinterval()` and its
#' derived stats. This function is really an internal function only needed by
#' subclasses of the base class, yet added a lot of noise to the documentation,
#' so it was replaced with `AbstractStatSlabInterval$compute_slab()`.
#' - The `slab_args` argument: extra stat parameters are now passed through to
#' the `...` arguments to `AbstractStatSlabInterval$compute_slab()`; use
#' these instead.
#' - The `interval_function` and `fun.data` arguments: these were parameters for determining the
#' function to compute intervals in `stat_slabinterval()` and its
#' derived stats. This function is really an internal function only needed by
#' subclasses of the base class, yet added a lot of noise to the documentation,
#' so it was replaced with `AbstractStatSlabInterval$compute_interval()`.
#' - The `interval_args` and `fun.args` arguments: to pass extra arguments to
#' a `point_interval` replace the value of the `point_interval` argument with
#' a simple wrapper; e.g. `stat_halfeye(point_interval = \(...) point_interval(..., extra_arg = XXX))`
#'
#' Parameters for `geom_slabinterval()` and family deprecated as of ggdist 3.1 are:
#'
#' - The `size_domain` and `size_range` arguments, which are long-deprecated aliases
#' for `interval_size_domain` and `interval_size_range`, were removed in ggdist 3.1.
#'
#' @format NULL
#' @usage NULL
#' @author Matthew Kay
#' @name ggdist-deprecated
NULL


# stat_dist_... -----------------------------------------------------------

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_halfeye = stat_halfeye

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_eye = stat_eye

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_ccdfinterval = stat_ccdfinterval

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_cdfinterval = stat_cdfinterval

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_gradientinterval = stat_gradientinterval

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_slab = stat_slab

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_pointinterval = stat_pointinterval

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_interval = stat_interval

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_lineribbon = stat_lineribbon

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_dotsinterval = stat_dotsinterval

#' @rdname ggdist-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_dots = stat_dots
