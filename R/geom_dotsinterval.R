#Geom for quick / quantile dotplots
#
# Author: mjskay
###############################################################################



# dots_grob ---------------------------------------------------------------

#' @importFrom ggplot2 .stroke .pt
#' @importFrom dplyr %>% arrange_at group_by_at group_split
dots_grob = function(data, x, y, xscale = 1,
  name = NULL, gp = gpar(), vp = NULL,
  dotsize = 1.07, stackratio = 1, binwidth = NA, layout = "bin",
  overlaps = "nudge", overflow = "keep",
  subguide = "none",
  verbose = FALSE,
  orientation = "vertical"
) {
  datas = data %>%
    group_by_at(c("group", y)) %>%
    group_split()

  gTree(
    datas = datas,
    xscale = xscale,
    dotsize = dotsize, stackratio = stackratio, binwidth = binwidth, layout = layout,
    overlaps = overlaps, overflow = overflow,
    subguide = subguide,
    verbose = verbose,
    orientation = orientation,
    name = name, gp = gp, vp = vp, cl = "dots_grob"
  )
}


#' @export
makeContent.dots_grob = function(x) {
  grob_ = x
  datas = grob_$datas
  xscale = grob_$xscale
  verbose = grob_$verbose
  orientation = grob_$orientation
  dotsize = grob_$dotsize
  binwidth = grob_$binwidth
  layout = grob_$layout
  overlaps = grob_$overlaps
  overflow = grob_$overflow
  subguide = grob_$subguide

  define_orientation_variables(orientation)

  dot_size_ratio = 1.07                  # historical fudge factor based on old stackratio
  font_size_ratio = 1.43/dot_size_ratio  # manual fudge factor for point size in ggplot
  stackratio = grob_$stackratio

  # ratio between width of the bins (binwidth)
  # and the vertical spacing of dots (y_spacing)
  # this is a bit different from a raw stackratio since we want to account
  # for the dotsize
  heightratio = convertUnit(unit(dotsize * stackratio, "native"),
    "native", axisFrom = x, axisTo = y, typeFrom = "dimension", valueOnly = TRUE)

  # if bin width was specified as a grid::unit, convert it to native units
  if (is.unit(binwidth)) {
    binwidth = convertUnit(binwidth, "native", axisFrom = x, typeFrom = "dimension", valueOnly = TRUE)
  }

  if (length(binwidth) == 2) {
    # if bin width has length 2, it specifies a desired bin width range
    user_min_binwidth = min(binwidth)
    user_max_binwidth = max(binwidth)
    # set to NA so that a prospective bin width is found dynamically first
    # before user-specified constraints are applied
    binwidth = NA
  } else if (isTRUE(is.na(binwidth))) {
    user_min_binwidth = 0
    user_max_binwidth = Inf
  } else {
    user_min_binwidth = binwidth
    user_max_binwidth = binwidth
  }

  if (isTRUE(is.na(binwidth)) || overflow == "compress") {
    # find the best bin widths across all the dotplots we are going to draw
    binwidths = map_dbl_(datas, function(d) {
      maxheight = max(d[[ymax]] - d[[ymin]])
      find_dotplot_binwidth(d[[x]], maxheight, heightratio, stackratio, layout = layout)
    })

    binwidth = min(binwidths, user_max_binwidth)
    if (binwidth < user_min_binwidth) {
      switch(overflow,
        compress = {
          s = user_min_binwidth / binwidth
          dotsize = dotsize * s
          stackratio = stackratio / s
        },
        keep = {
          binwidth = user_min_binwidth
        }
      )
    }
  }

  if (isTRUE(verbose)) {
    binwidth_npc = convertUnit(unit(binwidth, "native"), "npc", axisFrom = x, typeFrom = "dimension", valueOnly = TRUE)
    message(
      "geom_dots() binwidth = ", binwidth * xscale, " data units\n",
      "                     = unit(", binwidth_npc, ', "npc")'
    )
  }

  # now, draw all the dotplots using the same bin width
  dot_grobs = lapply(datas, function(d) {
    # bin the dots
    dot_positions = bin_dots(
      d$x, d$y,
      binwidth = binwidth, heightratio = heightratio, stackratio = stackratio,
      overlaps = overlaps,
      layout = layout, side = d$side[[1]], orientation = orientation
    )

    # ensure a consistent spatial drawing order, so that which dot overlaps which
    # is determined by their relative positions in space, not in the data
    dot_order = order(dot_positions[[x]], dot_positions[[y]])
    d = d[dot_order, ]
    dot_positions = dot_positions[dot_order, ]

    # determine size of the dots as a font size
    # the dot size in points (dot_pointsize) doesn't translate directly into
    # the font size in points needed to draw that dot (dot_fontsize); need a fudge
    # factor based on how big the circle glyph is as a ratio of font size
    # (font_size_ratio) plus need to account for stroke width
    lwd = d$linewidth * .stroke/2
    lwd[is.na(lwd) | is.na(d$colour)] = 0
    dot_pointsize = convertUnit(unit(binwidth * dotsize, "native"),
      "points", axisFrom = x, axisTo = "y", typeFrom = "dimension", valueOnly = TRUE)
    dot_fontsize = max(
      dot_pointsize * font_size_ratio - lwd,
      0.5
    )

    # generate grob for this dotplot
    pointsGrob(
      dot_positions$x, dot_positions$y, pch = d$shape,
      gp = gpar(
        col = alpha(d$colour, d$alpha),
        fill = alpha(d$fill, d$alpha),
        fontfamily = d$family,
        fontsize = dot_fontsize,
        lwd = lwd,
        lty = d$linetype
      )
    )
  })

  # generate subguide if requested
  subguide_grobs = if (identical(subguide, "none")) {
    # quick exit, also avoid errors for multiple non-equal axes when not drawing them
    list()
  } else {
    subguide_fun = match_function(subguide, "subguide_")
    subguide_params = bind_rows(lapply(datas, `[`, i = 1, j = , drop = FALSE))
    dlply_(
      subguide_params[, c(y, ymin, ymax, "side", "justification", "scale")],
      c(y, "side", "justification", "scale"),
      function(d) {
        if (nrow(unique(d)) > 1) {
          cli_abort(
            "Cannot draw a subguide for the dot count axis when multiple dots
             geometries with different parameters are drawn on the same axis.",
            class = "ggdist_incompatible_subguides"
          )
        }
        d = d[1, ]

        dot_height = binwidth * heightratio / stackratio
        guide_height = max(d[[ymax]] - d[[y]], d[[y]] - d[[ymin]])
        direction = switch_side(d$side, orientation, topright = 1, bottomleft = -1, both = 1)
        both_adjust = if (d$side == "both") 2 else 1
        not_both = if (d$side == "both") 0 else 1
        max_count = guide_height / binwidth / heightratio * both_adjust + 1 - 1/stackratio

        # construct a viewport such that the guide drawn in this viewport
        # will have its data values at the correct locations
        vp = viewport(just = c(0,0))
        vp[[x]] = unit(0, "native")
        vp[[y]] = unit(d[[y]] + dot_height / 2 * not_both * direction, "native")
        vp[[width.]] = unit(1, "npc")
        vp[[height]] = unit(guide_height - dot_height / both_adjust, "native") * direction


        grobTree(
          subguide_fun(c(1, max_count), orientation = orientation),
          vp = vp
        )
      })
  }

  setChildren(grob_, do.call(gList, c(dot_grobs, subguide_grobs)))
}


# panel drawing function -------------------------------------------------------

draw_slabs_dots = function(self, s_data, panel_params, coord,
  orientation, normalize, fill_type, na.rm,
  dotsize, stackratio, binwidth, layout,
  overlaps, overflow,
  subguide,
  verbose,
  ...
) {
  define_orientation_variables(orientation)

  # slab thickness is fixed to 1 for dotplots
  s_data$thickness = 1
  subguide_params = NULL
  c(s_data, subguide_params) %<-% rescale_slab_thickness(
    s_data, orientation, normalize, na.rm, name = "geom_dotsinterval"
  )
  s_data = self$override_slab_aesthetics(s_data)
  if (nrow(s_data) == 0) return(list())

  # in order for the dots grob to respect the `justification` aesthetic, we
  # need to adjust the y position based on where ymin and ymax are, as ymin/ymax
  # are determined by the justification but dots_grob uses y (not ymin/ymax)
  # to determine where to draw its dots
  s_data[[y]] = case_when_side(s_data$side, orientation,
    topright = s_data[[ymin]],
    bottomleft = s_data[[ymax]],
    both = (s_data[[ymax]] + s_data[[ymin]])/2
  )

  if (!coord$is_linear()) {
    stop0("geom_dotsinterval does not work properly with non-linear coordinates.")
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

  xscale = max(panel_params[[x.range]]) - min(panel_params[[x.range]])
  if (isTRUE(is.na(binwidth)) && inherits(s_data[[x]], "mapped_discrete")) {
    # no user-supplied binwidth and x is discrete:
    # use a max binwidth of 1
    binwidth = c(0, 1)
  }
  if (!isTRUE(is.na(binwidth)) && !is.unit(binwidth)) {
    #binwidth is expressed in terms of data coordinates, need to translate into standardized space
    binwidth = binwidth / xscale
  }

  s_data = s_data[order(s_data[["order"]] %||% s_data[[x]]), ]

  # draw the dots grob (which will draw dotplots for all the slabs)
  slab_grobs = list(dots_grob(
    s_data,
    x, y,
    xscale = xscale,
    dotsize = dotsize,
    stackratio = stackratio,
    binwidth = binwidth,
    layout = layout,
    overlaps = overlaps,
    overflow = overflow,
    subguide = subguide,
    verbose = verbose,
    orientation = orientation
  ))
}


# geom_dotsinterval ---------------------------------------------------------------

#' Automatic dotplot + point + interval meta-geom
#'
#' This meta-geom supports drawing combinations of dotplots, points, and intervals.
#' Geoms and stats based on [geom_dotsinterval()] create dotplots that automatically determine a bin width that
#' ensures the plot fits within the available space. They also ensure dots do not overlap, and allow
#' the generation of quantile dotplots using the `quantiles` argument to [stat_dotsinterval()]/[stat_dots()].
#' Generally follows the naming scheme and
#' arguments of the [geom_slabinterval()] and [stat_slabinterval()] family of
#' geoms and stats.
#'
#' @template details-dotsinterval-family
#' @template references-quantile-dotplots
#' @template details-x-y-xdist-ydist
#' @eval rd_layer_params("dotsinterval")
#' @eval rd_dotsinterval_aesthetics()
#' @inheritParams geom_slabinterval
#' @author Matthew Kay
#' @return A [ggplot2::Geom] or [ggplot2::Stat] representing a dotplot or combined dotplot+interval geometry which can
#' be added to a [ggplot()] object.
#' @seealso See the [stat_slabinterval()] family for other
#' stats built on top of [geom_slabinterval()].
#' See `vignette("dotsinterval")` for a variety of examples of use.
#' @family dotsinterval geoms
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
#'   ggplot(aes(x = u_tau, y = factor(i), fill = after_stat(x > 6))) +
#'   stat_dotsinterval(quantiles = 100)
#'
#' @importFrom rlang %||%
#' @importFrom stats ave
#' @import grid
#' @name geom_dotsinterval
NULL

#' @rdname ggdist-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomDotsinterval = ggproto("GeomDotsinterval", GeomSlabinterval,

  ## aesthetics --------------------------------------------------------------

  aes_docs = {
    aes_docs = GeomSlabinterval$aes_docs
    dots_aes_i = which(names(aes_docs) == "Slab-specific aesthetics")
    names(aes_docs)[[dots_aes_i]] = "Dots-specific (aka Slab-specific) aesthetics"
    aes_docs[[dots_aes_i]] = defaults(list(
      family =
        'The font family used to draw the dots.',
      order =
        'The order in which data points are stacked within bins. Can be used to create the effect of
      "stacked" dots by ordering dots according to a discrete variable. If omitted (`NULL`), the
      value of the data points themselves are used to determine stacking order. Only applies when
      `layout` is `"bin"` or `"hex"`, as the other layout methods fully determine both *x* and *y* positions.'
    ), aes_docs[[dots_aes_i]])
    aes_docs
  },

  hidden_aes = union(c(
    "thickness"
  ), GeomSlabinterval$hidden_aes),

  default_aes = defaults(aes(
    family = "",
    slab_shape = NULL,
    order = NULL
  ), GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    slab_shape = 21,
    slab_size = 0.75,
    slab_colour = "gray65"
  ), GeomSlabinterval$default_key_aes),

  optional_aes = union("order", GeomSlabinterval$optional_aes),

  override_slab_aesthetics = function(self, s_data) {
    s_data = ggproto_parent(GeomSlabinterval, self)$override_slab_aesthetics(s_data)
    s_data$shape = s_data[["slab_shape"]]
    s_data
  },


  ## parameters --------------------------------------------------------------

  param_docs = defaults(list(
    binwidth = glue_doc('The bin width to use for laying out the dots.
      One of:
        - `NA` (the default): Dynamically select the bin width based on the
          size of the plot when drawn. This will pick a `binwidth` such that the
          tallest stack of dots is at most `scale` in height (ideally exactly `scale`
          in height, though this is not guaranteed).
        - A length-1 (scalar) numeric or [unit] object giving the exact bin width.
        - A length-2 (vector) numeric or [unit] object giving the minimum and maximum
          desired bin width. The bin width will be dynamically selected within
          these bounds.

      If the value is numeric, it is assumed to be in units of data. The bin width
      (or its bounds) can also be specified using [unit()], which may be useful if
      it is desired that the dots be a certain point size or a certain percentage of
      the width/height of the viewport. For example, `unit(0.1, "npc")` would make
      dots that are *exactly* 10% of the viewport size along whichever dimension the
      dotplot is drawn; `unit(c(0, 0.1), "npc")` would make dots that are *at most*
      10% of the viewport size (while still ensuring the tallest stack is less than
      or equal to `scale`).
      '),
    dotsize = glue_doc('The width of the dots relative to the `binwidth`. The default,
      `1.07`, makes dots be just a bit wider than the bin width, which is a
      manually-tuned parameter that tends to work well with the default circular
      shape, preventing gaps between bins from appearing to be too large visually
      (as might arise from dots being *precisely* the `binwidth`). If it is desired
      to have dots be precisely the `binwidth`, set `dotsize = 1`.
      '),
    stackratio = glue_doc('The distance between the center of the dots in the same
      stack relative to the dot height. The default, `1`, makes dots in the same
      stack just touch each other.
      '),
    smooth = glue_doc('Smoother to apply to dot positions.
      One of:
        - A function that takes a numeric vector of dot positions and returns a
          smoothed version of that vector, such as `smooth_bounded()`,
          `smooth_unbounded()`, smooth_discrete()`, or `smooth_bar()`.
        - A string indicating what smoother to use, as the suffix to a function
          name starting with `smooth_`; e.g. `"none"` (the default) applies
          `smooth_none()`, which simply returns the given vector without
          applying smoothing.

      Smoothing is most effective when the smoother is matched to the support of
      the distribution; e.g. using `smooth_bounded(bounds = ...)`.
      '),
    overflow = glue_doc('How to handle overflow of dots beyond the extent of the geom
      when a minimum `binwidth` (or an exact `binwidth`) is supplied.
      One of:
        - `"keep"`: Keep the overflow, drawing dots outside the geom bounds.
        - `"compress"`: Compress the layout. Reduces the `binwidth` to the size necessary
          to keep the dots within bounds, then adjusts `stackratio` and `dotsize` so that
          the apparent dot size is the user-specified minimum `binwidth` times the
          user-specified `dotsize`.

      If you find the default layout has dots that are too small, and you are okay
      with dots overlapping, consider setting `overflow = "compress"` and supplying
      an exact or minimum dot size using `binwidth`.
      '),
    layout = glue_doc('The layout method used
      for the dots: \\itemize{
        \\item `"bin"` (default): places dots on the off-axis at the midpoint of their bins as in the classic Wilkinson dotplot.
          This maintains the alignment of rows and columns in the dotplot. This layout is slightly different from the
          classic Wilkinson algorithm in that: (1) it nudges bins slightly to avoid overlapping bins and (2) if
          the input data are symmetrical it will return a symmetrical layout.
        \\item `"weave"`: uses the same basic binning approach of `"bin"`, but places dots in the off-axis at their actual
          positions (unless `overlaps = "nudge"`, in which case overlaps may be nudged out of the way). This maintains
          the alignment of rows but does not align dots within columns.
        \\item `"hex"`: uses the same basic binning approach of `"bin"`, but alternates placing dots `+ binwidth/4` or
          `- binwidth/4` in the off-axis from the bin center. This allows hexagonal packing by setting a `stackratio`
          less than 1 (something like `0.9` tends to work).
        \\item `"swarm"`: uses the `"compactswarm"` layout from [beeswarm::beeswarm()]. Does not maintain alignment of rows or
          columns, but can be more compact and neat looking, especially for sample data (as opposed to quantile
          dotplots of theoretical distributions, which may look better with `"bin"`, `"weave"`, or `"hex"`).
        \\item `"bar"`: for discrete distributions, lays out duplicate values in rectangular bars.
      }'),
    overlaps = glue_doc('How to handle overlapping dots or bins in the `"bin"`,
      `"weave"`, and `"hex"` layouts (dots never overlap in the `"swarm"` or `"bar"` layouts).
      For the purposes of this argument, dots are only considered to be overlapping
      if they would be overlapping when `dotsize = 1` and `stackratio = 1`; i.e.
      if you set those arguments to other values, overlaps may still occur.
      One of: \\itemize{
        \\item `"keep"`: leave overlapping dots as they are. Dots may overlap
          (usually only slightly) in the `"bin"`, `"weave"`, and `"hex"` layouts.
        \\item `"nudge"`: nudge overlapping dots out of the way. Overlaps are avoided
          using a constrained optimization which minimizes the squared distance of
          dots to their desired positions, subject to the constraint that adjacent
          dots do not overlap.
      }'),
    verbose = glue_doc('If `TRUE`, print out the bin width of the dotplot. Can be useful
      if you want to start from an automatically-selected bin width and then adjust it
      manually. Bin width is printed both as data units and as normalized parent
      coordinates or `"npc"`s (see [unit()]). Note that if you just want to scale the
      selected bin width to fit within a desired area, it is probably easier to use
      `scale` than to copy and scale `binwidth` manually, and if you just want to
      provide constraints on the bin width, you can pass a length-2 vector to `binwidth`.

      ')
  ), GeomSlabinterval$param_docs),

  default_params = defaults(list(
    normalize = "none",
    binwidth = NA,
    dotsize = 1.07,
    stackratio = 1,
    layout = "bin",
    overlaps = "nudge",
    smooth = "none",
    overflow = "keep",
    verbose = FALSE
  ), GeomSlabinterval$default_params),

  hidden_params = union(c(
    "normalize", "fill_type"
  ), GeomSlabinterval$hidden_params),


  ## other methods -----------------------------------------------------------

  setup_data = function(self, data, params) {
    data = ggproto_parent(GeomSlabinterval, self)$setup_data(data, params)
    define_orientation_variables(params$orientation)

    # override any thickness values --- all thicknesses must be == 1 since we
    # don't actually show a function (for this geom it is just used to determine positioning)
    data$thickness = 1

    # apply smooths --- must do this here in case resulting data exceeds boundaries of
    # original data, meaning scales must be adjusted
    smooth = match_function(params$smooth %||% "none", prefix = "smooth_")
    s_data = data[data$datatype == "slab", c("group", x, y)]
    data[data$datatype == "slab", x] = ave(s_data[[x]], s_data[, c("group", y)], FUN = smooth)

    data
  },

  # workaround (#84)
  draw_slabs = function(self, ...) draw_slabs_dots(self, ...),

  draw_key_slab = function(self, data, key_data, params, size) {
    # slab key is different from usual - it's actually a point!
    # size is not in this list because if size it set but colour is not then there's nothing to draw,
    # so size can only occur in cases where colour is also set (so we can just check colour)
    if (
      params$show_slab &&
      any(!is.na(data[,c(
        "fill","alpha","slab_fill","slab_colour","slab_linewidth","slab_size",
        "slab_linetype","slab_alpha","slab_shape"
      )]))
    ) {
      s_key_data = self$override_slab_aesthetics(key_data)

      # what point calls "stroke" is what we call "linewidth", since "linewidth" is determined automatically
      s_key_data$stroke = s_key_data$linewidth
      # TODO: allow size of points in the key to be modified (not clear how to do this
      # without breaking the override for slab_size / slab_linewidth, so will just leave
      # this for now and expect people to use geom_dots if they want good legends anyway...)
      s_key_data$size = 2
      draw_key_point(s_key_data, params, size)
    }
  }
)

#' @rdname geom_dotsinterval
#' @export
geom_dotsinterval = make_geom(GeomDotsinterval)


# shortcut geoms ----------------------------------------------------------
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
    linewidth = 0.75,
    size = 2,
    colour = "gray65"
  ), GeomSlabinterval$default_key_aes),

  rename_size = TRUE,

  override_slab_aesthetics = function(self, s_data) {
    # we define these differently from geom_dotsinterval to make this easier to use on its own
    s_data$colour = s_data[["slab_colour"]] %||% s_data[["colour"]]
    s_data$colour = apply_colour_ramp(s_data[["colour"]], s_data[["colour_ramp"]])
    s_data$fill = s_data[["slab_fill"]] %||% s_data[["fill"]]
    s_data$fill = apply_colour_ramp(s_data[["fill"]], s_data[["fill_ramp"]])
    s_data$alpha = s_data[["slab_alpha"]] %||% s_data[["alpha"]]
    s_data$linewidth = s_data[["slab_linewidth"]] %||% s_data[["slab_size"]] %||% s_data[["linewidth"]] %||% s_data[["size"]]
    s_data$shape = s_data[["slab_shape"]] %||% s_data[["shape"]]
    s_data
  },

  default_params = defaults(list(
    show_point = FALSE,
    show_interval = FALSE
  ), GeomDotsinterval$default_params),

  hidden_params = union(c(
    "show_slab", "show_point", "show_interval",
    "interval_size_domain", "interval_size_range", "fatten_point", "arrow"
  ), GeomDotsinterval$hidden_params),

  draw_key_slab = function(self, data, key_data, params, size) {
    # can drop all the complicated checks from this key since it's just one geom
    s_key_data = self$override_slab_aesthetics(key_data)

    # what point calls "stroke" is what we call "linewidth", since "linewidth" is determined automatically
    s_key_data$stroke = s_key_data$linewidth
    s_key_data$size = s_key_data$size %||% 2
    draw_key_point(s_key_data, params, size)
  }
)
# have to unset these here because defaults() does not treat NULLs as unsetting values
GeomDots$default_key_aes$slab_colour = NULL
GeomDots$default_key_aes$slab_size = NULL

#' @eval rd_dotsinterval_shortcut_geom("dots", "dot")
#' @export
geom_dots = make_geom(GeomDots)
