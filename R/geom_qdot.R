# Geom for quick / quantile dotplots
#
# Author: mjskay
###############################################################################



# drawing functions -------------------------------------------------------

#' @importFrom ggplot2 .stroke .pt
qdot_grob = function(d, max_height, x, y,
  name=NULL, gp=NULL, vp=NULL
) {
  gTree(
    d = d, max_height = max_height, x = x, y = y,
    name = name, gp = gp, vp = vp, cl = "qdot_grob"
  )
}

makeContent.qdot_grob = function(gr) {
  d = gr$d
  max_height = gr$max_height
  x = gr$x
  y = gr$y

  size_ratio = 1.2
  stack_ratio = 1/size_ratio

  xrange = range(d[[x]])
  xspread = xrange[[2]] - xrange[[1]]

  heap_spec = function(nbins) {
    nbins = floor(nbins)
    # this ensures that datasets with an even (resp odd) number of items have an
    # even (resp odd) number of bins so that symmetrical distributions look symmetrical.
    nbins = nbins + (nbins %% 2 != nrow(d) %% 2)
    max_width = xspread / nbins
    breaks = seq(xrange[[1]], xrange[[2]], by = max_width)
    bins = cut(d[[x]], breaks, include.lowest = TRUE)
    d$bins = bins
    max_bin_size = max(unlist(dlply(d, "bins", nrow)))
    dot_size = convertUnit(unit(max_width, "native"),
      "native", axisFrom = x, axisTo = "y", typeFrom = "dimension", valueOnly = TRUE) *
      size_ratio
    y_spacing = convertUnit(unit(dot_size, "native"),
      "native", axisFrom = "y", axisTo = y, typeFrom = "dimension", valueOnly = TRUE) *
      stack_ratio

    as.list(environment())
  }
  is_valid_heap_spec = function(h) {
    h$max_bin_size * h$y_spacing <= max_height
  }

  # figure out a reasonable minimum number of bins based on histogram binning
  min_h = heap_spec(nclass.Sturges(d[[x]]))

  if (!is_valid_heap_spec(min_h)) {
    # figure out a maxiumum number of bins based on data resolution
    max_h = heap_spec(xspread / resolution(d[[x]]))

    # N.B. we search in increments of 2 instead of 1 here because heap_spec
    # guarantees that datasets with an even (resp odd) number of items have
    # an even (resp odd) number of bins, which both guarantees symmetrical
    # distributions look symmetrical and cuts our search space in half.
    if (max_h$nbins <= min_h$nbins) {
      # even at data resolution there aren't enough bins, not much we can do...
      h = min_h
    } else if (max_h$nbins == min_h$nbins + 2) {
      # nowhere to search, use maximum number of bins
      h = max_h
    } else {
      # use binary search to find a reasonable number of bins
      repeat {
        h = heap_spec((min_h$nbins + max_h$nbins) / 2)
        if (is_valid_heap_spec(h)) {
          # heap spec is valid, search downwards
          if (h$nbins - 2 <= min_h$nbins) {
            # found it, we're done
            break
          }
          max_h = h
        } else {
          # heap spec is not valid, search upwards
          if (h$nbins + 2 >= max_h$nbins) {
            # found it, we're done
            h = max_h
            break
          }
          min_h = h
        }
      }
    }
  } else {
    h = min_h
  }

  d$bins = h$bins
  dot_points = max(
    convertY(unit(h$dot_size, "native"), "points", valueOnly = TRUE) - max(d$stroke) * .stroke/2,
    0.5
  )
  children = do.call(gList, dlply(d, "bins", function (bin_df) {
    bin_df[[x]] = mean(bin_df[[x]])
    bin_df[[y]] = bin_df[[y]] + h$y_spacing/stack_ratio/size_ratio/size_ratio * .5 +
      seq(0, h$y_spacing * (nrow(bin_df) - 1), length.out = nrow(bin_df))

    pointsGrob(bin_df$x, bin_df$y, pch = bin_df$shape,
      gp = gpar(col = alpha(bin_df$colour, bin_df$alpha),
        fill = alpha(bin_df$fill, bin_df$alpha),
        fontsize = dot_points, lwd = bin_df$stroke * .stroke/2))
  }))

  setChildren(gr, children)
}


qdot_grob_old = function(d, max_height, x, y) {


  nbins = nclass.Sturges(d[[x]])
  xrange = range(d[[x]])
  max_width = (xrange[[2]] - xrange[[1]])/nbins
  breaks = seq(xrange[[1]], xrange[[2]], by = max_width)
  d$bins = cut(d[[x]], breaks, include.lowest = TRUE)
  max_bin_size = max(unlist(dlply(d, "bins", nrow)))
  y_spacing = if (max_bin_size <= 1) 0 else 1/(max_bin_size - 1)
  dlply(d, "bins", function (bin_df) {
    bin_df[[x]] = mean(bin_df[[x]])
    bin_df[[y]] = bin_df[[y]] + max_height *
      seq(0, y_spacing * (nrow(bin_df) - 1), length.out = nrow(bin_df))

    pointsGrob(bin_df$x, bin_df$y, pch = bin_df$shape,
      gp = gpar(col = alpha(bin_df$colour, bin_df$alpha), fill = alpha(bin_df$fill,
        bin_df$alpha), fontsize = bin_df$size * .pt + bin_df$stroke *
          .stroke/2, lwd = bin_df$stroke * .stroke/2))
  })
}



draw_slabs_qdot = function(self, s_data, panel_params, coord, side, scale, orientation, justification, normalize) {
  define_orientation_variables(orientation)

  # slab thickness is fixed to 1 for dotplots
  s_data$thickness = 1
  s_data = override_slab_aesthetics(rescale_slab_thickness(
    s_data, side, scale, orientation, justification, normalize, height, y, ymin, ymax
  ))

  if (!coord$is_linear()) {
    stop("geom_qdot does not work properly with non-linear coordinates.")
  }
  # Swap axes if using coord_flip
  if (inherits(coord, "CoordFlip")) {
    orientation = ifelse(orientation == "horizontal", "vertical", "horizontal")
    define_orientation_variables(orientation)
  }
  s_data = coord$transform(s_data, panel_params)




  # build groups for the slabs
  # must group within both group and y for the polygon and path drawing functions to work
  slab_grobs = dlply(s_data, c("group", y), function(d) {
    max_height = max(d[[ymax]] - d[[ymin]])
    qdot_grob(d, max_height, x, y)
  })

  # when side = "top", need to invert draw order so that overlaps happen in a sensible way
  # (only bother doing this when scale > 1 since that's the only time it will matter)
  if (side == "top" && scale > 1) {
    rev(slab_grobs)
  } else {
    slab_grobs
  }
}


# geom_qdot ---------------------------------------------------------------

#' Quick / Quantile dotplots (ggplot geom)
#'
#' Geom for quick / quantile dotplots
#'
#' @inheritParams ggplot2::layer
#' @param ...  Other arguments passed to \code{\link{layer}}.
#' @param side Which side to draw the dots on. \code{"topright"}, \code{"top"}, and \code{"right"} are synonyms
#' which cause the slab to be drawn on the top or the right depending on if \code{orientation} is \code{"horizontal"}
#' or \code{"vertical"}. \code{"bottomleft"}, \code{"bottom"}, and \code{"left"} are synonyms which cause the slab
#' to be drawn on the bottom of the left depending on if \code{orientation} is \code{"horizontal"} or
#' \code{"vertical"}. \code{"both"} draws the slab mirrored on both sides (as in a violin plot).
#' @param scale What proportion of the region allocated to this geom to use to draw the dots. If \code{scale = 1},
#' slabs that use the maximum range will just touch each other. Default is \code{0.9} to leave some space.
#' @param orientation Whether this geom is drawn horizontally (\code{"horizontal"}) or
#' vertically (\code{"vertical"}). When horizontal (resp. vertical), the geom uses the \code{y} (resp. \code{x})
#' aesthetic to identify different groups, then for each group uses the \code{x} (resp. \code{y}) aesthetic and the
#' \code{thickness} aesthetic to draw a function as an slab, and draws points and intervals horizontally
#' (resp. vertically) using the \code{xmin}, \code{x}, and \code{xmax} (resp. \code{ymin}, \code{y}, and \code{ymax})
#' aesthetics.
#' @param justification Justification of the interval relative to the slab, where \code{0} indicates bottom/left
#' justification and \code{1} indicates top/right justification (depending on \code{orientation}). If \code{justification}
#' is \code{NULL} (the default), then it is set automatically based on the value of \code{side}: when \code{side} is
#' \code{"top"}/\code{"right"} \code{justification} is set to \code{0}, when \code{side} is \code{"bottom"}/\code{"left"}
#' \code{justification} is set to \code{1}, and when \code{side} is \code{"both"} \code{justification} is set to
#' \code{0.5}.
#' @param interval_size_domain The minimum and maximum of the values of the size aesthetic that will be translated into actual
#' sizes for intervals drawn according to \code{interval_size_range} (see the documentation for that argument.)
#' @param interval_size_range This geom scales the raw size aesthetic values when drawing interval and point sizes, as
#' they tend to be too thick when using the default settings of \code{\link{scale_size_continuous}}, which give sizes
#' with a range of \code{c(1, 6)}. The \code{interval_size_domain} value indicates the input domain of raw size values
#' (typically this should be equal to the value of the \code{range} argument of the \code{\link{scale_size_continuous}}
#' function), and \code{interval_size_range} indicates the desired output range of the size values (the min and max of
#' the actual sizes used to draw intervals).
#' @param fatten_point A multiplicative factor used to adjust the size of the point relative to the size of the
#' thickest interval line. If you wish to specify point sizes directly, you can also use the \code{point_size}
#' aesthetic and \code{\link{scale_point_size_continuous}} or \code{\link{scale_point_size_discrete}}; sizes
#' specified with that aesthetic will not be adjusted using \code{fatten_point}.
#' @param show_dots Should the slab portion of the geom be drawn? Default \code{TRUE}.
#' @param show_point Should the point portion of the geom be drawn? Default \code{TRUE}.
#' @param show_interval Should the interval portion of the geom be drawn? Default \code{TRUE}.
#' @param na.rm	If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing
#' values are silently removed.
#' @author Matthew Kay
#' @seealso See \code{\link{geom_lineribbon}} for a combination geom designed for fit curves plus probability bands.
#' See \code{\link{stat_sample_slabinterval}} and \code{\link{stat_dist_slabinterval}} for families of stats
#' built on top of this geom for common use cases (like \code{stat_halfeyeh}).
#' See \code{vignette("slabinterval")} for a variety of examples of use.
#' @examples
#'
#' # geom_slabinterval() is typically not that useful on its own.
#' # See vignette("slabinterval") for a variety of examples of the use of its
#' # shortcut geoms and stats, which are more useful than using
#' # geom_slabinterval() directly.
#'
#' @importFrom ggplot2 GeomSegment GeomPolygon
#' @importFrom plyr dlply
#' @importFrom rlang %||%
#' @export
geom_qdot = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layer_geom_slabinterval(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    geom = GeomQdot,

    normalize = "none",
    ...,

    show.legend = show.legend,
    inherit.aes = inherit.aes,
  )
}


GeomQdot = ggproto("GeomQdot", GeomSlabinterval,
  default_aes = defaults(aes(
    slab_shape = NULL,
    slab_stroke = NULL
  ), GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    slab_shape = 21,
    slab_stroke = 0.75
  ), GeomSlabinterval$default_key_aes),

  default_params = defaults(list(
    normalize = "none"
  ), GeomSlabinterval$default_params),

  setup_data = function(self, data, params) {
    data = ggproto_parent(GeomSlabinterval, self)$setup_data(data, params)

    # override any thickness values --- all thicknesses must be == 1 since we
    # don't actually show a function (for this geom it is just used ot determine positioning)
    data$thickness = 1

    data
  },

  draw_slabs = draw_slabs_qdot
)
