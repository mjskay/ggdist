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
#' Arguments to `stat_slabinterval()` deprecated in ggdist 3.1 are:
#'
#' - The `limits_function` argument: this was a parameter for determining the
#' function to compute limits of the slab in `stat_slabinterval()` and its
#' derived stats. This function is really an internal function only needed by
#' subclasses of the base class, yet added a lot of noise to the documentation,
#' so it was replaced with `AbstractStatSlabInterval$compute_limits()`.
#' - The `limits_args` argument: extra stat parameters are now passed through to
#' the `...` arguments to `AbstractStatSlabInterval$compute_limits()`, use
#' these instead.
#'
#' @format NULL
#' @usage NULL
#' @author Matthew Kay
#' @name ggdist-deprecated
NULL
