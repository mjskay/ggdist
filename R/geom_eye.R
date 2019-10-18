# geom_eye for eye plots with intervals
#
# Author: mjskay
###############################################################################



#' Eye plots of densities with point and interval summaries (Deprecated spellings)
#'
#' These are deprecated spellings of [stat_eye()], [stat_eyeh()],
#' and [stat_halfeyeh()]. Use those functions instead.
#'
#' @inheritParams stat_sample_slabinterval
#' @inheritParams geom_slabinterval
#' @param ... Arguments passed to [stat_sample_slabinterval()]
#' @param relative_scale Deprecated. Use `scale` instead.
#' @param .prob Deprecated. Use `.width` instead.
#' @author Matthew Kay
#' @seealso See [stat_sample_slabinterval()] and the shortcut stats documented there for the
#' preferred way to generate these geoms.
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#'
#' data(RankCorr, package = "tidybayes")
#'
#' RankCorr %>%
#'   spread_draws(u_tau[i]) %>%
#'   ggplot(aes(y = i, x = u_tau)) +
#'   geom_eyeh()
#'
#' RankCorr %>%
#'   spread_draws(u_tau[i]) %>%
#'   ggplot(aes(x = i, y = u_tau)) +
#'   geom_eye()
#'
#' @importFrom utils modifyList
#' @importFrom purrr discard
#' @importFrom rlang %||%
#' @import ggplot2
#' @export
geom_eye = function(
  ...,
  scale = 0.9,
  .width = c(.66, .95),

  #deprecated arguments
  relative_scale,
  .prob
) {
  .width = .Deprecated_argument_alias(.width, .prob)
  scale = .Deprecated_argument_alias(scale, relative_scale)

  stat_eye(..., .width = .width, scale = scale)
}

#' @rdname geom_eye
#' @export
geom_eyeh = function(
  ...,
  scale = 0.9,
  .width = c(.66, .95),

  #deprecated arguments
  relative_scale,
  .prob
) {
  .width = .Deprecated_argument_alias(.width, .prob)
  scale = .Deprecated_argument_alias(scale, relative_scale)

  stat_eyeh(..., .width = .width, scale = scale)
}

#' @rdname geom_eye
#' @export
geom_halfeyeh = function(
  ...,
  scale = 0.9,
  .width = c(.66, .95),

  #deprecated arguments
  relative_scale,
  .prob
) {
  .width = .Deprecated_argument_alias(.width, .prob)
  scale = .Deprecated_argument_alias(scale, relative_scale)

  stat_halfeyeh(..., .width = .width, scale = scale)
}
