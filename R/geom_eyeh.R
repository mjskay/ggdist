# geom_eyeh for eye plots with intervals
#
# Author: mjskay
###############################################################################


#' @rdname geom_eye
#' @importFrom utils modifyList
#' @importFrom purrr discard
#' @import ggplot2
#' @export
geom_eyeh = function(
  #shared properties
  mapping = NULL, data = NULL,

  #violin properties
  position = "identity", trim = TRUE, scale = "area", relative_scale = 1, fill = NULL, violin.color = NA,

  ...,

  #stat_summaryh properties
  point_interval = median_qi,
  fun.data = NULL,
  fun.args = list(),
  .width = c(.66, .95),
  .prob,
  color = NULL, size = NULL, size_domain = NULL, size_range = NULL, fatten_point = NULL
) {
  .width = .Deprecated_argument_alias(.width, .prob)

  fun.data = fun.data %||% horizontal_aes(point_interval)

  #build violin plot
  violin.args = list(
      mapping = mapping, data = data, position = position, trim = trim, scale = scale,
      relative_scale = relative_scale, fill = fill, color = violin.color
    ) %>%
    discard(is.null)
  violin = do.call(geom_grouped_violinh, violin.args)

  #build interval annotations
  interval.args =
    list(mapping = mapping, position = position, data = data, fun.data = fun.data, fill = NA, .width = .width, fun.args = fun.args) %>%
    {if (!is.null(color)) modifyList(., list(color = color)) else .} %>%
    {if (!is.null(size)) modifyList(., list(size = size)) else .} %>%
    {if (!is.null(size_domain)) modifyList(., list(size_domain = size_domain)) else .} %>%
    {if (!is.null(size_range)) modifyList(., list(size_range = size_range)) else .} %>%
    {if (!is.null(fatten_point)) modifyList(., list(fatten_point = fatten_point)) else .}

  interval = do.call(stat_pointintervalh, interval.args)

  # we return a list of geoms that can be added to a ggplot object, as in
  # > ggplot(...) + list(geom_a(), geom_b())
  # which is equivalent to
  # > ggplot(...) + geom_a() + geom_b()
  list(violin, interval)
}
