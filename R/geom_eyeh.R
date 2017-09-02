# geom_eyeh for eye plots with intervals
#
# Author: mjskay
###############################################################################


#' @rdname geom_eye
#' @importFrom utils modifyList
#' @importFrom ggstance geom_violinh
#' @import ggplot2
#' @export
geom_eyeh = function(
  #shared properties
  mapping = NULL, data = NULL,

  #violin properties
  stat = "xdensity", position = "dodgev", trim = TRUE, scale = "area", fill = "gray65",

  ...,

  #stat_summaryh properties
  point.interval = mean_qih,
  fun.data = point.interval,
  fun.args = list(),
  .prob = c(.95, .66),
  color = NULL, size = NULL, fatten.interval = NULL, fatten.point = NULL
) {

  #build violin plot
  violin.args =
    list(mapping = mapping, data = data, stat = stat, position = position, trim = trim,
      scale = scale, color = NA, fill = fill, size = 0.5, alpha = 1.0, linetype = "solid"
    ) %>%
    {if (!is.null(fill)) modifyList(., list(fill = fill)) else .}

  violin = do.call(geom_violinh, violin.args)

  #build interval annotations
  interval.args =
    list(mapping = mapping, data = data, fun.data = fun.data, fill = NA, .prob = .prob, fun.args = fun.args) %>%
    {if (!is.null(color)) modifyList(., list(color = color)) else .} %>%
    {if (!is.null(size)) modifyList(., list(size = size)) else .} %>%
    {if (!is.null(fatten.interval)) modifyList(., list(fatten.interval = fatten.interval)) else .} %>%
    {if (!is.null(fatten.point)) modifyList(., list(fatten.point = fatten.point)) else .}

  interval = do.call(stat_pointintervalh, interval.args)

  # we return a list of geoms that can be added to a ggplot object, as in
  # > ggplot(...) + list(geom_a(), geom_b())
  # which is equivalent to
  # > ggplot(...) + geom_a() + geom_b()
  list(violin, interval)
}
