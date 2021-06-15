#Geom for quick / quantile dotplots
#
# Author: mjskay
###############################################################################



# dots_grob ---------------------------------------------------------------

#' @importFrom ggplot2 .stroke .pt
#' @importFrom dplyr %>% arrange_at group_by_at group_split
dots_grob = function(data, max_height, x, y,
  name = NULL, gp = gpar(), vp = NULL,
  dotsize = 1, stackratio = 1, binwidth = NA, layout = "bin",
  side = "topright", orientation = "vertical"
) {
  datas = data %>%
    arrange_at(x) %>%
    group_by_at(c("group", y)) %>%
    group_split()

  gTree(
    datas = datas, max_height = max_height, x_ = x, y_ = y,
    dotsize = dotsize, stackratio = stackratio, binwidth = binwidth, layout = layout,
    side = side, orientation = orientation,
    name = name, gp = gp, vp = vp, cl = "dots_grob"
  )
}


#' @importFrom grDevices nclass.Sturges nclass.FD nclass.scott
#' @export
makeContent.dots_grob = function(x) {
  grob_ = x
  datas = grob_$datas
  max_height = grob_$max_height
  x = grob_$x_
  y = grob_$y_
  side = grob_$side
  orientation = grob_$orientation

  point_size_ratio = 1.43  # manual fudge factor for point size in ggplot
  stackratio = 1.07 * grob_$stackratio
  dotsize = grob_$dotsize
  bin_width = grob_$binwidth
  layout = grob_$layout

  # if bin width was specified as a grid::unit, convert it
  if (is.unit(bin_width)) {
    bin_width = convertUnit(bin_width, "native", axisFrom = x, typeFrom = "dimension", valueOnly = TRUE)
  }

  # if bin width has length 2, it specifies a desired bin width range
  if (length(bin_width) == 2) {
    user_min_bin_width = min(bin_width)
    user_max_bin_width = max(bin_width)
    # set to NA so that a prospective bin width is found dynamically first
    # before user-specified constraints are applied
    bin_width = NA
  } else {
    user_min_bin_width = 0
    user_max_bin_width = Inf
  }

  # ratio between width of the bins (bin_width)
  # and the vertical spacing of dots (y_spacing)
  yratio = convertUnit(unit(dotsize * stackratio, "native"),
    "native", axisFrom = x, axisTo = y, typeFrom = "dimension", valueOnly = TRUE)

  # create a specification for a heap of dots, which includes things like
  # what the bins are, what the dot widths are, etc.
  heap_spec = function(d, nbins = NULL, bin_width = NULL) {
    xrange = range(d[[x]])
    xspread = xrange[[2]] - xrange[[1]]
    if (xspread == 0) xspread = 1
    if (is.null(bin_width)) {
      nbins = floor(nbins)
      bin_width = xspread / nbins
    } else {
      nbins = max(floor(xspread / bin_width), 1)
    }
    binning = automatic_bin(d[[x]], bin_width)
    max_bin_count = max(tabulate(binning$bins))

    y_spacing = bin_width * yratio

    if (nbins == 1) {
      # if there's only 1 bin, we can scale it to be as large as we want as long as it fits, so
      # let's back out a max bin size based on that...
      max_y_spacing = max_height / max_bin_count
      max_bin_width = max_y_spacing / yratio
    } else {
      # if there's more than 1 bin, the provided nbins or bin width determines the max bin width
      max_y_spacing = y_spacing
      max_bin_width = bin_width
    }

    as.list(environment())
  }
  is_valid_heap_spec = function(h) {
    isTRUE(h$max_bin_count * h$max_y_spacing <= max_height)
  }

  if (is.na(bin_width)) {
    # find the best bin widths across all the heaps we are going to draw
    max_bin_widths = map_dbl(datas, function(d) {
      # figure out a reasonable minimum number of bins based on histogram binning
      if (nrow(d) <= 1) {
        min_h = heap_spec(d, nbins = 1)
      } else {
        min_h = heap_spec(d, nbins = min(nclass.scott(d[[x]]), nclass.FD(d[[x]]), nclass.Sturges(d[[x]])))
      }

      if (!is_valid_heap_spec(min_h)) {
        # figure out a maximum number of bins based on data resolution
        max_h = heap_spec(d, bin_width = resolution(d[[x]]))

        if (max_h$nbins <= min_h$nbins) {
          # even at data resolution there aren't enough bins, not much we can do...
          h = min_h
        } else if (max_h$nbins == min_h$nbins + 1) {
          # nowhere to search, use maximum number of bins
          h = max_h
        } else {
          # use binary search to find a reasonable number of bins
          repeat {
            h = heap_spec(d, (min_h$nbins + max_h$nbins) / 2)
            if (is_valid_heap_spec(h)) {
              # heap spec is valid, search downwards
              if (h$nbins - 1 <= min_h$nbins) {
                # found it, we're done
                break
              }
              max_h = h
            } else {
              # heap spec is not valid, search upwards
              if (h$nbins + 1 >= max_h$nbins) {
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

      # check if the selected heap spec is valid....
      if (!is_valid_heap_spec(h)) {
        # ... if it isn't, this means we've ended up with some bin that's too
        # tall, probably because we have discrete data --- we'll just
        # conservatively shrink things down so they fit by backing out a bin
        # width that works with the tallest bin
        y_spacing = max_height / h$max_bin_count
        y_spacing / yratio
      } else {
        h$max_bin_width
      }
    })

    bin_width = max(min(max_bin_widths, user_max_bin_width), user_min_bin_width)
  }

  # now, draw all the heaps using the same bin width
  children = do.call(gList, unlist(recursive = FALSE, lapply(datas, function(d) {
    h = heap_spec(d, bin_width = bin_width)
    h$binning$bin_midpoints = nudge_bins(h$binning$bin_midpoints, bin_width)
    d$bins = h$binning$bins
    d$midpoint = h$binning$bin_midpoints[h$binning$bins]
    point_size = convertUnit(unit(h$bin_width * dotsize, "native"),
      "native", axisFrom = x, axisTo = "y", typeFrom = "dimension", valueOnly = TRUE)
    # the dot size in native units (point_size) doesn't translate directly into
    # font size (point_fontsize); need a fudge factor based on how big the circle
    # glyph is as a ratio of font size (point_size_ratio) plus need to account
    # for stroke width)
    point_fontsize = max(
      convertHeight(unit(point_size * point_size_ratio, "native"), "points", valueOnly = TRUE) -
        max(d$size, 0, na.rm = TRUE) * .stroke/2,
      0.5
    )

    # determine x positions (for bin/weave) / x and y positions (for swarm)
    y_start = switch_side(side, orientation,
      topright = h$y_spacing / 2,
      bottomleft = - h$y_spacing / 2,
      both = 0
    )
    switch(layout,
      bin = {
        d[[x]] = d$midpoint
      },
      weave = {
        # keep original x positions, but re-order within bins so that overlaps
        # across bins are less likely
        d = ddply_(d, "bins", function(bin_df) {
          seq_fun = if (side == "both") seq_interleaved_centered else seq_interleaved
          bin_df = bin_df[seq_fun(nrow(bin_df)),]
          bin_df$rows = seq_len(nrow(bin_df))
          bin_df
        })

        # nudge values within each row to ensure there are no overlaps
        # (rows are not well-defined in side = "both" for this to work,
        # so we skip this step in that case)
        if (side != "both") {
          d = ddply_(d, "rows", function(row_df) {
            row_df[[x]] = nudge_bins(row_df[[x]], bin_width)
            row_df
          })
        }
      },
      swarm = {
        if (!requireNamespace("beeswarm", quietly = TRUE)) {
          stop('Using layout = "swarm" with the dots geom requires the `beeswarm` package to be installed.')
        }

        swarm_xy = beeswarm::swarmy(d[[x]], d[[y]],
          xsize = h$bin_width, ysize = h$y_spacing,
          log = "", cex = 1,
          side = switch_side(side, orientation, topright = 1, bottomleft = -1, both = 0),
          # priority = "density",
          compact = TRUE
        )

        d[[x]] = swarm_xy[["x"]]
        d[[y]] = swarm_xy[["y"]] + y_start
      },
      stop("Unknown layout type for dots: ", deparse0(layout))
    )

    # determine y positions (for bin/weave)
    if (layout %in% c("bin", "weave")) {
      d = ddply_(d, "bins", function(bin_df) {
        y_offset = seq(0, h$y_spacing * (nrow(bin_df) - 1), length.out = nrow(bin_df))
        switch_side(side, orientation,
          topright = {},
          bottomleft = {
            y_offset = - y_offset
          },
          both = {
            y_offset = y_offset - h$y_spacing * (nrow(bin_df) - 1) / 2
          }
        )
        bin_df[[y]] = bin_df[[y]] + y_start + y_offset
        bin_df
      })
    }

    # generate grobs
    dlply_(d, "bins", function(bin_df) {
      pointsGrob(bin_df$x, bin_df$y, pch = bin_df$shape,
        gp = gpar(
          col = alpha(bin_df$colour, bin_df$alpha),
          fill = alpha(bin_df$fill, bin_df$alpha),
          fontsize = point_fontsize,
          lwd = bin_df$size * .stroke/2,
          lty = bin_df$linetype
        ))
    })
  })))

  setChildren(grob_, children)
}


# panel drawing function -------------------------------------------------------

draw_slabs_dots = function(self, s_data, panel_params, coord,
  side, scale, orientation, justification, normalize, fill_type, na.rm,
  child_params
) {
  define_orientation_variables(orientation)

  # remove missing values
  s_data = ggplot2::remove_missing(s_data, na.rm, c(x, y), name = "geom_dotsinterval", finite = TRUE)
  if (nrow(s_data) == 0) return(gList())

  # slab thickness is fixed to 1 for dotplots
  s_data$thickness = 1
  s_data = self$override_slab_aesthetics(rescale_slab_thickness(
    s_data, side, scale, orientation, justification, normalize, height, y, ymin, ymax
  ))

  if (!coord$is_linear()) {
    stop("geom_dotsinterval does not work properly with non-linear coordinates.")
  }
  # Swap axes if using coord_flip
  if (inherits(coord, "CoordFlip")) {
    orientation = switch(orientation,
      y = ,
      horizontal = "x",
      x = ,
      vertical = "y"
    )
    define_orientation_variables(orientation)
  }
  s_data = coord$transform(s_data, panel_params)

  if (!isTRUE(is.na(child_params$binwidth)) && !is.unit(child_params$binwidth)) {
    #binwidth is expressed in terms of data coordinates, need to translate into standardized space
    child_params$binwidth = child_params$binwidth / (max(panel_params[[x.range]]) - min(panel_params[[x.range]]))
  }

  # draw the dots grob (which will draw dotplots for all the slabs)
  max_height = max(s_data[[ymax]] - s_data[[ymin]])
  slab_grobs = list(dots_grob(s_data, max_height, x, y,
      dotsize = child_params$dotsize,
      stackratio = child_params$stackratio,
      binwidth = child_params$binwidth,
      layout = child_params$layout,
      side = side,
      orientation = orientation
    ))
}


# geom_dotsinterval ---------------------------------------------------------------

#' Automatic dotplots, dots + intervals, and quantile dotplots (ggplot geom)
#'
#' Geoms and stats for creating dotplots that automatically determines a bin width that
#' ensures the plot fits within the available space. Also ensures dots do not overlap, and allows
#' generation of quantile dotplots using the `quantiles` argument to `stat_dotsinterval`/`stat_dots`
#' and `stat_dist_dotsinterval`/`stat_dist_dots`. Generally follows the naming scheme and
#' arguments of the [geom_slabinterval()] and [stat_slabinterval()] family of
#' geoms and stats.
#'
#' The dots geoms are similar to [geom_dotplot()] but with a number of differences:
#'
#' \itemize{
#'   \item Dots geoms act like slabs in [geom_slabinterval()] and can be given x positions (or y positions when
#'   in a horizontal orientation).
#'   \item Given the available space to lay out dots, the dots geoms will automatically determine how many bins to
#'   use to fit the available space.
#'   \item Dots geoms use a dynamic layout algorithm that lays out dots from the center out if the input data are
#'   symmetrical, guaranteeing that symmetrical data results in a symmetrical plot. The layout algorithm also prevents
#'   dots from overlapping each other.
#'   \item The shape of the dots in a in these geoms can be changed using the `slab_shape` aesthetic (when using the
#'   `dotsinterval` family) or the `shape` or `slab_shape` aesthetic (when using the `dots` family)
#' }
#'
#' The `stat_...` and `stat_dist_...` versions of the stats when used with the `quantiles` argument
#' are particularly useful for constructing quantile dotplots, which can be an effective way to communicate uncertainty
#' using a frequency framing that may be easier for laypeople to understand (Kay et al. 2016, Fernandes et al. 2018).
#'
#' @eval rd_slabinterval_aesthetics(geom = GeomDotsinterval, geom_name = "geom_dotsinterval", stat = StatDotsinterval)
#' @inheritParams geom_slabinterval
#' @inheritParams stat_slabinterval
#' @inheritDotParams geom_slabinterval
#' @param ...  Other arguments passed to [layer()].
#' @author Matthew Kay
#' @param dotsize The size of the dots relative to the bin width. The default, `1`, makes dots be just about as
#' wide as the bin width.
#' @param stackratio The distance between the center of the dots in the same stack relative to the bin height. The
#' default, `1`, makes dots in the same stack just touch each other.
#' @param binwidth The bin width to use for drawing the dotplots. One of:
#'   - `NA` (the default): Dynamically select the bin width based on the
#'     size of the plot when drawn.
#'   - A length-1 (scalar) numeric or [unit] object giving the exact bin width.
#'   - A length-2 (vector) numeric or [unit] object giving the minimum and maximum
#'     desired bin width. The bin width will be dynamically selected within
#'     these bounds.
#'
#' If the value is numeric, it is assumed to be in units of data. The bin width
#' (or its bounds) can also be specified using `unit()`, which may be useful if
#' it is desired that the dots be a certain point size or a certain percentage of
#' the width/height of the viewport. For example, `unit(0.1, "npc")` would make
#' dots that are *exactly* 10% of the viewport size along whichever dimension the
#' dotplot is drawn; `unit(c(0, 0.1), "npc")` would make dots that are *at most*
#' 10% of the viewport size.
#' @param layout The layout method used for the dots:
#'  - `"bin"` (default): places dots on the off-axis at the midpoint of their bins as in the classic Wilkinson dotplot.
#'    This maintains the alignment of rows and columns in the dotplot.
#'  - `"weave"`: places dots in the off-axis at their actual positions (modulo overlaps, which are nudged out of
#'    the way). Maintains the alignment of rows but does not align dots within columns. Does not work well when
#'    `side = "both"`.
#'  - `"swarm"`: uses the `"compactswarm"` layout from `beeswarm::beeswarm()`. Does not maintain alignment of rows or
#'    columns, but can be more compact and neat looking, especially for sample data (as opposed to quantile
#'    dotplots of theoretical distributions, which may look better with `"bin"` or `"weave"`).
#' @param quantiles For the `stat_` and `stat_dist_` stats, setting this to a value other than `NA`
#' will produce a quantile dotplot: that is, a dotplot of quantiles from the sample (for `stat_`) or a dotplot
#' of quantiles from the distribution (for `stat_dist_`). The value of `quantiles` determines the number
#' of quantiles to plot. See Kay et al. (2016) and Fernandes et al. (2018) for more information on quantile dotplots.
#' @return A [ggplot2::Geom] or [ggplot2::Stat] representing a dotplot or combined dotplot+interval geometry which can
#' be added to a [ggplot()] object.
#' @references
#'   Kay, M., Kola, T., Hullman, J. R., & Munson, S. A. (2016). When (ish) is My Bus? User-centered Visualizations
#'   of Uncertainty in Everyday, Mobile Predictive Systems. *Conference on Human Factors
#'   in Computing Systems - CHI '16*, 5092--5103. \doi{10.1145/2858036.2858558}.
#'
#'   Fernandes, M., Walls, L., Munson, S., Hullman, J., & Kay, M. (2018). Uncertainty Displays Using Quantile Dotplots
#'   or CDFs Improve Transit Decision-Making. *Conference on Human Factors in Computing Systems - CHI '18*.
#'   \doi{10.1145/3173574.3173718}.
#' @seealso See [stat_sample_slabinterval()] and [stat_dist_slabinterval()] for families of other
#' stats built on top of [geom_slabinterval()].
#' See `vignette("slabinterval")` for a variety of examples of use.
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' data(RankCorr_u_tau, package = "ggdist")
#'
#' # orientation is detected automatically based on
#' # which axis is discrete
#'
#' RankCorr_u_tau %>%
#'   ggplot(aes(x = u_tau)) +
#'   geom_dots()
#'
#' RankCorr_u_tau %>%
#'   ggplot(aes(y = u_tau)) +
#'   geom_dots()
#'
#' # stat_dots can summarize quantiles, creating quantile dotplots
#'
#' RankCorr_u_tau %>%
#'   ggplot(aes(x = u_tau, y = factor(i))) +
#'   stat_dots(quantiles = 100)
#'
#' # color and fill aesthetics can be mapped within the geom
#' # dotsinterval adds an interval
#'
#' RankCorr_u_tau %>%
#'   ggplot(aes(x = u_tau, y = factor(i), fill = stat(x > 6))) +
#'   stat_dotsinterval(quantiles = 100)
#'
#' @importFrom rlang %||%
#' @import grid
#' @export
geom_dotsinterval = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",

  ...,
  dotsize = 1,
  stackratio = 1,
  binwidth = NA,
  layout = c("bin", "weave", "swarm"),

  na.rm = FALSE,

  show.legend = NA,
  inherit.aes = TRUE
) {
  layout = match.arg(layout)

  layer(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    geom = GeomDotsinterval,
    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      normalize = "none",

      dotsize = dotsize,
      stackratio = stackratio,
      binwidth = binwidth,
      layout = layout,

      na.rm = na.rm,
      ...
    )
  )
}
#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomDotsinterval = ggproto("GeomDotsinterval", GeomSlabinterval,
  default_aes = defaults(aes(
    slab_shape = NULL
  ), GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    slab_shape = 21,
    slab_size = 0.75,
    slab_colour = "gray65"
  ), GeomSlabinterval$default_key_aes),

  override_slab_aesthetics = function(self, s_data) {
    s_data = ggproto_parent(GeomSlabinterval, self)$override_slab_aesthetics(s_data)
    s_data$shape = s_data$slab_shape
    s_data
  },

  extra_params = c(GeomSlabinterval$extra_params,
    "dotsize",
    "stackratio",
    "binwidth",
    "layout"
  ),

  default_params = defaults(list(
    normalize = "none",
    dotsize = 1,
    stackratio = 1,
    binwidth = NA,
    layout = "bin"
  ), GeomSlabinterval$default_params),

  draw_panel = function(self, data, panel_params, coord,
    side = self$default_params$side,
    scale = self$default_params$scale,
    orientation = self$default_params$orientation,
    justification = self$default_params$justification,
    normalize = self$default_params$normalize,
    interval_size_domain = self$default_params$interval_size_domain,
    interval_size_range = self$default_params$interval_size_range,
    fatten_point = self$default_params$fatten_point,
    show_slab = self$default_params$show_slab,
    show_point = self$default_params$show_point,
    show_interval = self$default_params$show_interval,
    na.rm = self$default_params$na.rm,

    dotsize = self$default_params$dotsize,
    stackratio = self$default_params$stackratio,
    binwidth = self$default_params$binwidth,
    layout = self$default_params$layout,

    child_params = list()
  ) {
    ggproto_parent(GeomSlabinterval, self)$draw_panel(data, panel_params, coord,
      side = side,
      scale = scale,
      orientation = orientation,
      justification = justification,
      normalize = normalize,
      interval_size_domain = interval_size_domain,
      interval_size_range = interval_size_range,
      fatten_point = fatten_point,
      show_slab = show_slab,
      show_point = show_point,
      show_interval = show_interval,
      na.rm = na.rm,

      child_params = list(
        dotsize = dotsize,
        stackratio = stackratio,
        binwidth = binwidth,
        layout = layout
      )
    )
  },

  setup_data = function(self, data, params) {
    data = ggproto_parent(GeomSlabinterval, self)$setup_data(data, params)

    # override any thickness values --- all thicknesses must be == 1 since we
    # don't actually show a function (for this geom it is just used to determine positioning)
    data$thickness = 1

    data
  },

  draw_slabs = draw_slabs_dots,

  draw_key_slab = function(self, data, key_data, params, size) {
    # slab key is different from usual - it's actually a point!
    # size is not in this list because if size it set but colour is not then there's nothing to draw,
    # so size can only occur in cases where colour is also set (so we can just check colour)
    if (
      params$show_slab &&
      any(!is.na(data[,c(
        "fill","alpha","slab_fill","slab_colour","slab_size",
        "slab_linetype","slab_alpha","slab_shape"
      )]))
    ) {
      s_key_data = self$override_slab_aesthetics(key_data)

      # what point calls "stroke" is what we call "size", since "size" is determined automatically
      s_key_data$stroke = s_key_data$size
      s_key_data$size = 2
      draw_key_point(s_key_data, params, size)
    }
  }
)


# shortcut geoms ----------------------------------------------------------
#' @export
#' @rdname geom_dotsinterval
geom_dots = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",

  ...,

  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  layer(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    geom = GeomDots,
    show.legend = show.legend,
    inherit.aes = inherit.aes,

    params = list(
      normalize = "none",
      show_point = FALSE,
      show_interval = FALSE,

      na.rm = na.rm,
      ...
    )
  )
}
#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomDots = ggproto("GeomDots", GeomDotsinterval,
  # override these from GeomSlabinterval instead of GeomDotsinterval
  # because we want to directly change the base versions, which in geom_dots
  # correspond to shape/size/colour of the dots in the geom but in
  # geom_dotsinterval do not
  default_key_aes = defaults(aes(
    shape = 21,
    size = 0.75,
    colour = "gray65"
  ), GeomSlabinterval$default_key_aes),

  override_slab_aesthetics = function(self, s_data) {
    # we define these differently from geom_dotsinterval to make this easier to use on its own
    s_data$colour = s_data$slab_colour %||% s_data$colour
    s_data$colour = apply_colour_ramp(s_data$colour, s_data$colour_ramp)
    s_data$fill = s_data$slab_fill %||% s_data$fill
    s_data$fill = apply_colour_ramp(s_data$fill, s_data$fill_ramp)
    s_data$alpha = s_data$slab_alpha %||% s_data$alpha
    s_data$size = s_data$slab_size %||% s_data$size
    s_data$shape = s_data$slab_shape %||% s_data$shape
    s_data
  },

  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), GeomDotsinterval$default_params),

  draw_key_slab = function(self, data, key_data, params, size) {
    # can drop all the complicated checks from this key since it's just one geom
    s_key_data = self$override_slab_aesthetics(key_data)

    # what point calls "stroke" is what we call "size", since "size" is determined automatically
    s_key_data$stroke = s_key_data$size
    s_key_data$size = 2
    draw_key_point(s_key_data, params, size)
  }
)
# have to unset these here because defaults() does not treat NULLs as unsetting values
GeomDots$default_key_aes$slab_colour = NULL
GeomDots$default_key_aes$slab_size = NULL
