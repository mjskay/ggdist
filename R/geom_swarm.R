# shortcut geoms for beeswarm-like plots
#
# Author: mjskay
###############################################################################

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomSwarm = ggproto("GeomSwarm", GeomDots,
  default_aes = defaults(aes(
    side = "both"
  ), GeomDots$default_aes),

  default_params = defaults(list(
    overflow = "compress",
    binwidth = quote(unit(1.5, "mm")),
    layout = "swarm"
  ), GeomDots$default_params)
)

#' @eval rd_dotsinterval_shortcut_geom("swarm", "beeswarm")
#' @export
geom_swarm = make_geom(GeomSwarm)


#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomWeave = ggproto("GeomWeave", GeomSwarm,
  default_params = defaults(list(
    layout = "weave"
  ), GeomSwarm$default_params)
)

#' @eval rd_dotsinterval_shortcut_geom("weave", "dot-weave")
#' @export
geom_weave = make_geom(GeomWeave)
