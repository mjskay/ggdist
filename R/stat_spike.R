# spike stat
#
# Author: mjskay
###############################################################################


#' Spike plot (ggplot2 stat)
#'
#' Stat for drawing "spikes" (optionally with points on them) at specific points
#' on a distribution (numerical or determined as a function of the distribution),
#' intended for annotating [stat_slabinterval()] geometries.
#'
#' @details
#' This stat computes slab values at specified locations on a distribution, as
#' determined by the `at` aesthetic. `at` may be [numeric], a list of [numeric]s,
#' or a list of [function]s giving the points on the distribution to evaluate the
#' density and cumulative distribution functions at. See the **Aesthetics**
#' section, below.
#' @inheritParams stat_slab
#' @inheritParams geom_spike
#' @eval rd_layer_params("spike", StatSpike, as_dots = TRUE)
#' @param geom Use to override the default connection between [stat_spike()] and [geom_spike()]
#' @template details-x-y-xdist-ydist
#' @return A [ggplot2::Stat] representing a spike geometry which can be added to a [ggplot()] object.
#' @eval rd_spike_aesthetics("spike", StatSpike)
#' @eval rd_slabinterval_computed_variables(StatSpike)
#' @seealso See [geom_spike()] for the geom underlying this stat.
#'   See [stat_slabinterval()] for the stat this shortcut is based on.
#' @family slabinterval stats
#' @examples
#' library(ggplot2)
#' library(distributional)
#' library(dplyr)
#'
#' df = tibble(
#'   d = c(dist_normal(1), dist_gamma(2,2)), g = c("a", "b")
#' )
#'
#' # annotate the density at the mode of a distribution
#' df %>%
#'   ggplot(aes(y = g, xdist = d)) +
#'   stat_slab(aes(xdist = d)) +
#'   stat_spike(aes(at = "Mode")) +
#'   # need shared thickness scale so that stat_slab and geom_spike line up
#'   scale_thickness_shared()
#'
#' # annotate the endpoints of intervals of a distribution
#' # here we'll use an arrow instead of a point by setting size = 0
#' arrow_spec = arrow(angle = 45, type = "closed", length = unit(4, "pt"))
#' df %>%
#'   ggplot(aes(y = g, xdist = d)) +
#'   stat_halfeye(point_interval = mode_hdci) +
#'   stat_spike(
#'     aes(at = c(function(x) hdci(x, .width = .66))),
#'     size = 0, arrow = arrow_spec, color = "blue", linewidth = 0.75
#'   ) +
#'   scale_thickness_shared()
#'
#' @name stat_spike
NULL


# compute_slab ------------------------------------------------------------

#' StatSpike$compute_slab()
#' @noRd
compute_slab_spike = function(
  self, data, scales, trans, input, orientation,
  slab_type,
  ...
) {
  define_orientation_variables(orientation)

  # calculate slab functions
  s_data = ggproto_parent(StatSlab, self)$compute_slab(
    data, scales, trans, input, orientation,
    slab_type = slab_type,
    ...
  )
  dist = data$dist
  pdf_fun = approx_pdf(dist, s_data$.input, s_data$pdf)
  cdf_fun = approx_cdf(dist, s_data$.input, s_data$cdf)

  # determine evaluation points
  at = data$at
  if (is.list(at)) {
    stopifnot("Cannot have more than one set of evaluation points per distribution" = length(at) == 1)
    at = at[[1]]
  }
  if (!is.numeric(at)) {
    at = match_function(at)
    at = at(dist)
  }
  # needs to be a vector (e.g. in cases of interval functions
  # like qi() which return matrices)
  at = as.vector(unlist(at, use.names = FALSE))

  # evaluate functions
  pdf = pdf_fun(at)
  cdf = cdf_fun(at)

  f = switch(slab_type,
    histogram = ,
    pdf = pdf,
    cdf = cdf,
    ccdf = 1 - cdf,
    stop0("Unknown `slab_type`: ", deparse0(slab_type), '. Must be "histogram", "pdf", "cdf", or "ccdf"')
  )

  data.frame(
    .input = at,
    f = pdf,
    pdf = pdf,
    cdf = cdf,
    n = s_data$n[[1]]
  )
}


# stat_spike --------------------------------------------------------------

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
StatSpike = ggproto("StatSpike", StatSlab,
  aes_docs = defaults(list(
    at = glue_doc('The points at which to evaluate the PDF and CDF of the distribution.
      One of: \\itemize{
       \\item [numeric] vector: points to evaluate the PDF and CDF of the distributions at.
       \\item list of [numeric] vectors: points to evaluate the PDF and CDF of the distributions
         at, where each distribution\'s functions may be evaluated at multiple points.
       \\item list of functions or [character] vector: functions (or names of functions) which,
         when applied on a distribution-like object (e.g. a \\pkg{distributional} object or a
         [posterior::rvar]), return a vector of values to evaluate the distribution functions at.
      }')
  ), StatSlab$aes_docs),

  default_aes = defaults(aes(
    at = "median",
  ), StatSlab$default_aes),

  # workaround (#84)
  compute_slab = function(self, ...) compute_slab_spike(self, ...)
)
#' @rdname stat_spike
#' @export
stat_spike = make_stat(StatSpike, geom = "spike")
