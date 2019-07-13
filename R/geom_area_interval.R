# Meta-geom for intervals, densities, and their combinations
#
# Author: mjskay
###############################################################################

#' Area function + point + interval meta-geom
#'
#' This meta-geom supports drawing combinations of functions (as areas), points, and intervals. It acts as a meta-geom
#' for implementing eye plots, half-eye plots, CCDF barplots, and point+multiple interval plots with horizontal or
#' vertical orientations, and with appropriate support for dodging.
#'
#' \code{geom_area_interval} is a flexible meta-geom that you can use directly, though in most cases you will want to
#' use shortcut geoms that combine appropriate stats with this geom to create more useful primitives, such as eye plots,
#' halfeye plots, point+interval plots, or CCDF barplots.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#' \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set at the
#' layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#' the plot defaults.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position The position adjustment to use for overlapping points on this layer.
#' @param ...  Other arguments passed to \code{\link{layer}}.
#' @param na.rm	If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing
#' values are silently removed.
#' @param show.legend Should this layer be included in the legends? Default is \code{c(size = FALSE)}, unlike most geoms,
#' to match its common use cases. \code{FALSE} hides all legends, \code{TRUE} shows all legends, and \code{NA} shows only
#' those that are mapped (the default for most geoms).
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them. This is
#' most useful for helper functions that define both data and aesthetics and shouldn't inherit behavior from the
#' default plot specification, e.g. borders.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_lineribbon}} for a similar geom designed for curves plus probability bands. See
#' \code{\link{geom_pointrange}} and \code{\link{geom_pointrangeh}} for the geoms these are based on.
#' @keywords manip
#' @examples
#'
#' library(magrittr)
#' library(ggplot2)
#'
#' data(RankCorr, package = "tidybayes")
#'
#' RankCorr %>%
#'   spread_draws(u_tau[i]) %>%
#'   median_qi(.width = c(.8, .95)) %>%
#'   ggplot(aes(y = i, x = u_tau)) +
#'   geom_pointintervalh()
#'
#' RankCorr %>%
#'   spread_draws(u_tau[i]) %>%
#'   median_qi(.width = c(.8, .95)) %>%
#'   ggplot(aes(x = i, y = u_tau)) +
#'   geom_pointinterval()
#'
#' @importFrom ggplot2 GeomSegment GeomPolygon
#' @importFrom plyr dlply
#' @importFrom rlang %||%
#' @export
geom_area_interval = function(
  mapping = NULL, data = NULL,
  stat = "identity", position = "identity",
  ...,

  scale = 0.9,
  side = c("topright", "top", "right", "bottomleft", "bottom", "left", "both"),
  orientation = c("horizontal", "vertical"),
  na.rm = FALSE,

  show.legend = NA,
  inherit.aes = TRUE
) {

  side = match.arg(side)
  orientation = match.arg(orientation)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAreaInterval,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      scale = scale,
      side = side,
      orientation = orientation,
      na.rm = na.rm,
      ...
    )
  )
}

GeomAreaInterval = ggproto("GeomAreaInterval", Geom,
  default_aes = aes(
    shape = 19,
    colour = "black",
    fill = "gray65",
    alpha = NA,
    size = 1,
    stroke = 0.5,
    linetype = "solid"
  ),

  extra_params = c("side", "scale", "orientation", "na.rm"),

  setup_data = function(self, data, params) {
    define_orientation_variables(params$orientation)

    # rescale functions according to how we want to scale them
    # current approach is normalize so max height across all is 1
    # this preserves areas across groups
    finite_f = data$f[is.finite(data$f)]
    if (length(finite_f) > 0) {
      data$f = data$f / max(finite_f)
    }

    # figure out the bounding rectangles for each group
    # this is necessary so that the bounding box is correct for
    # positions to work (e.g. position_dodge, etc)
    data[[height]] = data[[height]] %||% params[[height]] %||%
      resolution(data[[y]], FALSE)

    switch_side(params$side,
      top = {
        data[[ymin]] = data[[y]]
        data[[ymax]] = data[[y]] + data[[height]]
      },
      bottom = {
        data[[ymin]] = data[[y]] - data[[height]]
        data[[ymax]] = data[[y]]
      },
      both = {
        data[[ymin]] = data[[y]] - data[[height]] / 2
        data[[ymax]] = data[[y]] + data[[height]] / 2
      }
    )

    data
  },

  draw_group = function(self, data, panel_params, coord, ..., side, scale, orientation) {
    define_orientation_variables(orientation)

    density_grobs = list()
    if (!is.null(data$f)) {
      # function values were provided, draw them

      # function data is any of the data with finite function values
      f_data = data[is.finite(data$f),]

      if (nrow(f_data) > 0) {
        # rescale the data to be within the confines of the bounding box
        # we do this *again* here (rather than in setup_data) because
        # position_dodge doesn't work if we only do it up there
        f_scale = scale * (f_data[[ymax]] - f_data[[ymin]])
        switch_side(side,
          top = {
            f_data[[ymin]] = f_data[[y]]
            f_data[[ymax]] = f_data[[y]] + f_data$f * f_scale
          },
          bottom = {
            f_data[[ymin]] = f_data[[y]] - f_data$f * f_scale
            f_data[[ymax]] = f_data[[y]]
          },
          both = {
            f_data[[ymin]] = f_data[[y]] - f_data$f * f_scale / 2
            f_data[[ymax]] = f_data[[y]] + f_data$f * f_scale / 2
          }
        )

        # density grob color defaults to NA
        # TODO: make this something else
        f_data$colour = NA

        # build grobs to display the densities
        density_grobs = dlply(f_data, y, function(d) {
          data_order = order(d[[x]])
          density_data_top = d[data_order,]
          density_data_top[[y]] = density_data_top[[ymax]]

          density_data_bottom = d[rev(data_order),]
          density_data_bottom[[y]] = density_data_bottom[[ymin]]

          GeomPolygon$draw_panel(rbind(density_data_top, density_data_bottom), panel_params, coord, ...)
        })
      }
    }

    interval_grobs = list()
    point_grobs = list()
    if (!is.null(data[[xmin]]) && !is.null(data[[xmax]])) {
      # intervals were provided, draw them

      # interval data is any of the data with non-missing interval values
      i_data = data[!is.na(data[[xmin]]) & !is.na(data[[xmax]]),]

      if (nrow(i_data) > 0) {
        # reorder by interval width so largest intervals are drawn first
        i_data = i_data[order(abs(i_data[[xmax]] - i_data[[xmin]]), decreasing = TRUE),]

        p_data = i_data
        # TODO: make this something else
        p_data$colour = "red"
        point_grobs = list(GeomPoint$draw_panel(p_data, panel_params, coord, ...))

        i_data[[x]] = i_data[[xmin]]
        i_data[[xend]] = i_data[[xmax]]
        i_data[[yend]] = i_data[[y]]
        interval_grobs = list(GeomSegment$draw_panel(i_data, panel_params, coord, ...))
      }
    }

    ggname("geom_area_interval",
      gTree(children = do.call(gList, c(density_grobs, interval_grobs, point_grobs)))
    )
  }
)

# defines "orientation" variables in the environment of the calling
# function (for convenience): these are variables (typically aesthetics)
# that differ depending on whether the geom is horizontal or vertical.
# They are named assuming a horizontal orientation.
define_orientation_variables = function(orientation) {
  f = parent.frame()

  if (orientation == "horizontal") {
    f$height = "height"

    f$y = "y"
    f$ymin = "ymin"
    f$ymax = "ymax"
    f$yend = "yend"

    f$x = "x"
    f$xmin = "xmin"
    f$xmax = "xmax"
    f$xend = "xend"
  } else if (orientation == "vertical") {
    f$height = "width"

    f$y = "x"
    f$ymin = "xmin"
    f$ymax = "xmax"
    f$yend = "xend"

    f$x = "y"
    f$xmin = "ymin"
    f$xmax = "ymax"
    f$xend = "yend"
  } else {
    stop("Unknown orientation: `", orientation, "`")
  }
}

switch_side = function(side, top, bottom, both) {
  switch(side,
    top = ,
    topright = ,
    right = top,
    bottom = ,
    left = ,
    bottomleft = bottom,
    both = both,
    stop("Invalid side: `", side, "`")
  )
}

