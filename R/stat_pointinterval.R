# A stat_summary with a geom_pointinterval
#
# Author: mjskay
###############################################################################


#' Point summary + multiple uncertainty interval plots (ggplot stat)
#'
#' A combination of [stat_sample_slabinterval()] and
#' [geom_slabinterval()] with sensible defaults.
#' While the corresponding `geom`s are intended for use on
#' data frames that have already been summarized using a [point_interval()]
#' function, these `stat`s are intended for use directly on data frames of draws, and
#' will perform the summarization using a [point_interval()] function.
#'
#' @eval rd_slabinterval_computed_variables()
#' @eval rd_slabinterval_aesthetics(geom = GeomPointinterval, geom_name = "geom_pointinterval", stat = StatPointinterval)
#' @inheritParams stat_sample_slabinterval
#' @inheritParams geom_slabinterval
#' @return A [ggplot2::Stat] representing a point+multiple uncertainty interval geometry which can
#' be added to a [ggplot()] object.
#' @seealso See [geom_pointinterval()] for the geom versions, intended
#' for use on points and intervals that have already been summarized using a [point_interval()] function.
#' See [stat_interval()] for a similar stat intended for intervals without
#' point summaries. See [stat_sample_slabinterval()] for a variety of other
#' stats that combine intervals with densities and CDFs.
#' @seealso See [geom_pointinterval()] for the geom versions, intended
#' for use on points and intervals that have already been summarized using a [point_interval()] function.
#' See [stat_interval()] for a similar stat intended for intervals without
#' point summaries.
#' See [stat_sample_slabinterval()] for a variety of other
#' stats that combine intervals with densities and CDFs.
#' See [geom_slabinterval()] for the geom that these geoms wrap. All parameters of that geom are
#' available to these geoms.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' data(RankCorr_u_tau, package = "ggdist")
#'
#' RankCorr_u_tau %>%
#'   ggplot(aes(y = factor(i), x = u_tau)) +
#'   stat_pointinterval(.width = c(.66, .95))
#'
#' RankCorr_u_tau %>%
#'   ggplot(aes(x = factor(i), y = u_tau)) +
#'   stat_pointinterval(.width = c(.66, .95))
#'
#' @name stat_pointinterval
NULL

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPointinterval = ggproto("StatPointinterval", StatSampleSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval"
  ), StatSampleSlabinterval$default_aes),

  default_params = defaults(list(
    show_slab = FALSE
  ), StatSampleSlabinterval$default_params),

  hidden_params = union(c(
    "slab_type", "adjust", "trim", "breaks", "outline_bars", "limits", "n",
    "show_slab", "show_point", "show_interval"
  ), StatSlabinterval$hidden_params)
)

#' @rdname stat_pointinterval
#' @export
stat_pointinterval = make_stat(StatPointinterval, geom = "pointinterval")
