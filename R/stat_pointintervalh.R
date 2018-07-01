# A stat_summary with a geom_pointinterval
#
# Author: mjskay
###############################################################################


#' @rdname stat_pointinterval
#' @export
stat_pointintervalh <- function(mapping = NULL, data = NULL,
  geom = "pointintervalh", position = "identity",
  ...,
  point_interval = median_qi,
  fun.data = NULL,
  .prob = c(.66, .95),
  fun.args = list(),
  na.rm = FALSE,
  show.legend = FALSE,
  inherit.aes = TRUE
) {

  fun.data = fun.data %||% horizontal_aes(point_interval)

  l = layer(
    data = data,
    mapping = mapping,
    stat = StatPointintervalh,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun.data = fun.data,
      .prob = .prob,
      fun.args = fun.args,
      na.rm = na.rm,
      ...
    )
  )

  #provide some default computed aesthetics
  default_computed_aesthetics = aes(size = -...prob..)  # nolint

  compute_aesthetics = l$compute_aesthetics
  l$compute_aesthetics = function(self, data, plot) {
    apply_default_computed_aesthetics(self, plot, default_computed_aesthetics)
    compute_aesthetics(data, plot)
  }

  map_statistic = l$map_statistic
  l$map_statistic = function(self, data, plot) {
    apply_default_computed_aesthetics(self, plot, default_computed_aesthetics)
    map_statistic(data, plot)
  }

  l
}

#' @importFrom plyr defaults
StatPointintervalh <- ggproto("StatPointintervalh", StatSummary,
  compute_panel = function(data, scales, fun.data = median_qih, .prob = c(.66, .95),
    fun.args = list(), na.rm = FALSE
  ) {

    fun.args = modifyList(list(.prob = .prob), fun.args)

    # Function that takes complete data frame as input
    fun.data = match.fun(fun.data)
    fun = function(df) {
      do.call(fun.data, c(list(quote(df$x)), fun.args))
    }

    summarise_by_y(data, fun)
  }
)
