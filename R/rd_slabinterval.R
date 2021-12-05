# Documentation methods for the slabinterval family
#
# Author: mjskay
###############################################################################



# documentation methods ---------------------------------------------------

rd_shortcut_stat = function(stat_name, geom_name = stat_name) {
  stat = get(paste0("Stat", title_case(stat_name)))
  geom = get(paste0("Geom", title_case(geom_name)))

  # find the changed aesthetics and params for this stat
  changed_values = function(list, exclude = character()) {
    # find values changes in the child stat
    values = stat[[list]][
      map_lgl_(names(stat[[list]]), function(name)
        !identical(stat[[list]][[name]], StatSlabinterval[[list]][[name]])
      )
    ]
    # find deleted values
    deleted_names = setdiff(names(StatSlabinterval[[list]]), names(stat[[list]]))
    deleted_values = rep(list(NULL), length(deleted_names))
    names(deleted_values) = deleted_names
    values = c(values, deleted_values)

    # turn values into strings like x = "foo", y = "bar"
    value_text = lapply(values, function(x) deparse0(get_expr(x)))
    value_text = value_text[!names(value_text) %in% exclude]
    if (length(value_text)) paste(names(value_text), "=", value_text, collapse = ", ")
  }
  changed_aes = changed_values(
    "default_aes",
    exclude = c("datatype")
  )
  changed_params = changed_values(
    "default_params",
    exclude = c("show_point", "show_interval")
  )
  changed_args = changed_values(
    "layer_args"
  )

paste0('@description
```
stat_slabinterval(', paste0(collapse = ',', '\n  ', c(
  if (length(changed_aes)) paste0('aes(', changed_aes, ')'),
  if (geom_name != "slabinterval") paste0('geom = "', geom_name, '"'),
  if (length(changed_params)) changed_params,
  if (length(changed_args)) changed_args
)), '
)
```
')
}

rd_slabinterval_shortcut_stat = function(
  stat_name, chart_type,
  geom_name = stat_name,
  example_layers = NULL,
  describe = TRUE
) {
  stat = get(paste0("Stat", title_case(stat_name)))
  geom = get(paste0("Geom", title_case(geom_name)))

  if (length(example_layers) > 0) {
    example_layers = paste0(" +\n  ", paste0(example_layers, collapse = " +\n  "))
  }

  c(
    paste0('@title ', title_case(chart_type), ' plot (shortcut stat)'),
    if (describe) c(
paste0('@description
Shortcut version of [stat_slabinterval()] with [geom_', geom_name, '()] for creating ', chart_type, ' plots.

Roughly equivalent to:\n'),
      rd_shortcut_stat(stat_name, geom_name)
    ),
    '@inheritParams stat_slabinterval',
    '@inheritParams geom_slabinterval',
    rd_slabinterval_params(geom_name, stat, as_dots = TRUE),
    paste0('@param geom Use to override the default connection between [stat_',
      stat_name, '()] and [geom_', geom_name, '()]'
    ),
    '@template details-x-y-xdist-ydist',
    '@return A [ggplot2::Stat] representing a ', chart_type, ' geometry which can
     be added to a [ggplot()] object.',
    rd_slabinterval_computed_variables(stat),
    rd_slabinterval_aesthetics(geom_name, stat),
    '@seealso',
    paste0('See [geom_', geom_name, '()] for the geom underlying this stat.\n'),
    'See [stat_slabinterval()] for the stat this shortcut is based on.\n',
    '@family stat_slabinterval shortcut stats',
    paste0('@examples
library(dplyr)
library(ggplot2)
library(distributional)

theme_set(theme_ggdist())

# ON SAMPLE DATA
set.seed(1234)
df = data.frame(
  group = c("a", "b", "c"),
  value = rnorm(1500, mean = c(5, 7, 9), sd = c(1, 1.5, 1))
)
df %>%
  ggplot(aes(x = value, y = group)) +
  stat_', stat_name, '()', example_layers, '

# ON ANALYTICAL DISTRIBUTIONS
dist_df = data.frame(
  group = c("a", "b", "c"),
  mean =  c(  5,   7,   8),
  sd =    c(  1, 1.5,   1)
)
# Vectorized distribution types, like distributional::dist_normal()
# and posterior::rvar(), can be used with the `xdist` / `ydist` aesthetics
dist_df %>%
  ggplot(aes(y = group, xdist = dist_normal(mean, sd))) +
  stat_', stat_name, '()', example_layers)
  )
}

rd_slabinterval_computed_variables = function(stat = StatSlabinterval) {
  out = paste0('@section Computed Variables:
The following variables are computed by this stat and made available for
use in aesthetic specifications ([aes()]) using the [stat()] or [after_stat()]
functions:
- `x` or `y`: For slabs, the input values to the slab function.
  For intervals, the point summary from the interval function. Whether it is `x` or `y` depends on `orientation`
- `xmin` or `ymin`: For intervals, the lower end of the interval from the interval function.
- `xmax` or `ymax`: For intervals, the upper end of the interval from the interval function.
- `.width`: For intervals, the interval width as a numeric value in `[0, 1]`.
- `level`: For intervals, the interval width as an ordered factor.
',
    if (isTRUE(stat$default_params$show_slab)) {
'-  `f`: For slabs, the output values from the slab function (such as the PDF, CDF, or CCDF),
  determined by `slab_type`.
- `pdf`: For slabs, the probability density function.
- `cdf`: For slabs, the cumulative distribution function.
- `n`: For slabs, the number of data points summarized into that slab. If the slab was created from
  an analytical distribution via the `xdist`, `ydist`, or `dist` aesthetic, `n` will be `Inf`.
'})

  out
}

#' Provides documentation of params for slabinterval geoms
#' @noRd
rd_slabinterval_params = function(geom_name = "slabinterval", stat = NULL, as_dots = FALSE) {
  geom = get(paste0("Geom", title_case(geom_name)))

  params = list(
    orientation =
'Whether this geom is drawn horizontally (`"horizontal"`) or
vertically (`"vertical"`). The default, `NA`, automatically detects the orientation based on how the
aesthetics are assigned, and should generally do an okay job at this. When horizontal (resp. vertical),
the geom uses the `y` (resp. `x`) aesthetic to identify different groups, then for each group uses
the `x` (resp. `y`) aesthetic and the `thickness` aesthetic to draw a function as an slab, and draws
points and intervals horizontally (resp. vertically) using the `xmin`, `x`, and `xmax` (resp.
`ymin`, `y`, and `ymax`) aesthetics. For compatibility with the base
ggplot naming scheme for `orientation`, `"x"` can be used as an alias for `"vertical"` and `"y"` as an alias for
`"horizontal"` (tidybayes had an `orientation` parameter before ggplot did, and I think the tidybayes naming
scheme is more intuitive: `"x"` and `"y"` are not orientations and their mapping to orientations is, in my
opinion, backwards; but the base ggplot naming scheme is allowed for compatibility).
',
    normalize =
'How to normalize heights of functions input to the `thickness` aesthetic. If `"all"`
(the default), normalize so that the maximum height across all data is `1`; if `"panels"`, normalize within
panels so that the maximum height in each panel is `1`; if `"xy"`, normalize within
the x/y axis opposite the `orientation` of this geom so that the maximum height at each value of the
opposite axis is `1`; if `"groups"`, normalize within values of the opposite axis and within
groups so that the maximum height in each group is `1`; if `"none"`, values are taken as is with no
normalization (this should probably only be used with functions whose values are in \\[0,1\\], such as CDFs).
',
    fill_type = 'What type of fill to use when the fill color or alpha varies within a slab. The default,
`"segments"`, breaks up the slab geometry into segments for each unique combination of fill color and
alpha value. This approach is supported by all graphics devices and works well for sharp cutoff values,
but can result in ugly results if a large number of unique fill colors are being used (as in gradients,
like in [stat_gradientinterval()]). When `fill_type == "gradient"`, a `grid::linearGradient()` is used to
create a smooth gradient fill. This works well for large numbers of unique fill colors, but requires
R > 4.1 and is not yet supported on all graphics devices.
',
    interval_size_domain =
'The minimum and maximum of the values of the size aesthetic that will be translated into actual
sizes for intervals drawn according to `interval_size_range` (see the documentation for that argument.)
',
    interval_size_range =
'(Deprecated). This geom scales the raw size aesthetic values when drawing interval and point sizes, as
they tend to be too thick when using the default settings of [scale_size_continuous()], which give sizes
with a range of `c(1, 6)`. The `interval_size_domain` value indicates the input domain of raw size values
(typically this should be equal to the value of the `range` argument of the [scale_size_continuous()]
function), and `interval_size_range` indicates the desired output range of the size values (the min and max of
the actual sizes used to draw intervals). Most of the time it is not recommended to change the value of this argument,
as it may result in strange scaling of legends; this argument is a holdover from earlier versions
that did not have size aesthetics targeting the point and interval separately. If you want to adjust the
size of the interval or points separately, you can instead use the `interval_size` or `point_size`
aesthetics; see [scales].
',
    fatten_point =
'A multiplicative factor used to adjust the size of the point relative to the size of the
thickest interval line. If you wish to specify point sizes directly, you can also use the `point_size`
aesthetic and [scale_point_size_continuous()] or [scale_point_size_discrete()]; sizes
specified with that aesthetic will not be adjusted using `fatten_point`.
',
    step = 'Should the line/ribbon be drawn as a step function? One of: `FALSE` (do not draw as a step
function, the default), `TRUE` (draw a step function using the `"mid"` approach), `"mid"`
(draw steps midway between adjacent x values), `"hv"` (draw horizontal-then-vertical steps), `"vh"`
(draw as vertical-then-horizontal steps). `TRUE` is an alias for `"mid"` because for a step function with
ribbons, `"mid"` is probably what you want (for the other two step approaches the ribbons at either the
very first or very last x value will not be visible).',
    show_slab = 'Should the slab portion of the geom be drawn?',
    show_point = 'Should the point portion of the geom be drawn?',
    show_interval = 'Should the interval portion of the geom be drawn?',
    na.rm = 'If `FALSE`, the default, missing values are removed with a warning. If `TRUE`, missing
values are silently removed.'
  )

  # filter out hidden params or ones defined in the stat
  param_names = setdiff(
    names(geom$default_params),
    c(names(stat$default_params), geom$hidden_params, stat$hidden_params)
  )
  params = params[param_names]

  missing_docs = sapply(params, is.null)
  if (any(missing_docs)) {
    stop("Missing docs for params: ", paste0(param_names[missing_docs], collapse = ", "))
  }

  if (length(params)) {
    if (as_dots) {
      paste0('@param ...  Other arguments passed to [layer()]. These are often aesthetics, used to set an aesthetic
        to a fixed value, like `colour = "red"` or `size = 3` (see **Aesthetics**, below). They may also be
        parameters to the paired geom/stat. When paired with the default geom, [geom_', geom_name, '()],
        these include:',
      paste0(
        '    \\describe{',
        paste0('\\item{\\code{', names(params), '}}{', params, '}'),
        '}\n',
        collapse = ''
      ))
    } else {
      paste0('@param ', names(params), ' ', params)
    }
  }
}


#' Given a names list of aesthetic / aesthetic doc pairs, output a list of them
#' for use in docs. Used by rd_slabinterval_aesthetics
#' @noRd
rd_aesthetics = function(aes_docs, include_only) {
  aes_docs = aes_docs[intersect(names(aes_docs), include_only)]

  c(
    "\\itemize{",
    paste0("  \\item \\code{", names(aes_docs), "}: ", aes_docs),
    "}"
  )
}

#' Provides documentation of aesthetics for slabintervals
#' @noRd
rd_slabinterval_aesthetics = function(geom_name = "slabinterval", stat = NULL) {
  geom = get(paste0("Geom", title_case(geom_name)))

  # build docs
  out = c(
    "@section Aesthetics:",
    "The slab+interval `stat`s and `geom`s have a wide variety of aesthetics that control
    the appearance of their three sub-geometries: the **slab**, the **point**, and
    the **interval**.\n"
  )

  # stat aesthetics
  pos_aes = list(
    x = 'x position of the geometry',
    y = 'y position of the geometry'
  )
  if (!is.null(stat)) {
    stat_aes = list(
      x = 'x position of the geometry (when orientation = `"vertical"`); or sample data to be summarized
       (when `orientation = "horizontal"` with sample data).',
      y = 'y position of the geometry (when orientation = `"horizontal"`); or sample data to be summarized
       (when `orientation = "vertical"` with sample data).',
      xdist =
        'When using analytical distributions, distribution to map on the x axis: a \\pkg{distributional} object (e.g. [dist_normal()]) or
        a [posterior::rvar()] object.',
      ydist =
        'When using analytical distributions, distribution to map on the y axis: a \\pkg{distributional} object (e.g. [dist_normal()]) or
        a [posterior::rvar()] object.',
      dist =
        'When using analytical distributions, a name of a distribution (e.g. `"norm"`), a \\pkg{distributional} object (e.g. [dist_normal()]), or
        a [posterior::rvar()] object. See **Details**.',
      args = 'Distribution arguments (`args` or `arg1`, ... `arg9`). See **Details**.'
    )
    out = c(out,
      "These `stat`s support the following aesthetics:",
      rd_aesthetics(stat_aes, stat$aesthetics()),
      paste0("In addition, in their default configuration (paired with [geom_", geom_name, "()]) ",
        "the following aesthetics are supported by the underlying geom:\n")
    )
  } else {
    # do not include positional aesthetics with stats (since those are included).

    # positional aesthetics
    out = c(out, "**Positional aesthetics**", rd_aesthetics(pos_aes, geom$aesthetics()))
  }


  # slab aesthetics
  slab_aes = list(
    thickness =
      'The thickness of the slab at each `x` value (if `orientation = "horizontal"`) or
       `y` value (if `orientation = "vertical"`) of the slab.',
    side =
      'Which side to place the slab on. `"topright"`, `"top"`, and `"right"` are synonyms
       which cause the slab to be drawn on the top or the right depending on if `orientation` is `"horizontal"`
       or `"vertical"`. `"bottomleft"`, `"bottom"`, and `"left"` are synonyms which cause the slab
       to be drawn on the bottom or the left depending on if `orientation` is `"horizontal"` or
       `"vertical"`. `"topleft"` causes the slab to be drawn on the top or the left, and `"bottomright"`
       causes the slab to be drawn on the bottom or the right. `"both"` draws the slab mirrored on both
       sides (as in a violin plot).',
    scale =
      'What proportion of the region allocated to this geom to use to draw the slab. If `scale = 1`,
       slabs that use the maximum range will just touch each other. Default is `0.9` to leave some space.',
    justification =
      'Justification of the interval relative to the slab, where `0` indicates bottom/left
       justification and `1` indicates top/right justification (depending on `orientation`). If `justification`
       is `NULL` (the default), then it is set automatically based on the value of `side`: when `side` is
       `"top"`/`"right"` `justification` is set to `0`, when `side` is `"bottom"`/`"left"`
       `justification` is set to `1`, and when `side` is `"both"` `justification` is set to 0.5.',
    datatype =
      'When using composite geoms directly without a `stat` (e.g. [geom_slabinterval()]), `datatype` is used to
       indicate which part of the geom a row in the data targets: rows with `datatype = "slab"` target the
       slab portion of the geometry and rows with `datatype = "interval"` target the interval portion of
       the geometry. This is set automatically when using ggdist `stat`s.'
  )
  if (isTRUE(geom$default_params$show_slab)) {
    out = c(out, "**Slab-specific aesthetics**", rd_aesthetics(slab_aes, geom$aesthetics()))
  }

  # interval-specific aesthetics
  int_aes = list(
    xmin = 'Left end of the interval sub-geometry (if `orientation = "horizontal"`).',
    xmax = 'Right end of the interval sub-geometry (if `orientation = "horizontal"`).',
    ymin = 'Lower end of the interval sub-geometry (if `orientation = "vertical"`).',
    ymax = 'Upper end of the interval sub-geometry (if `orientation = "vertical"`).'
  )
  if (isTRUE(geom$default_params$show_interval)) {
    out = c(out, "**Interval-specific aesthetics**", rd_aesthetics(int_aes, geom$aesthetics()))
  }

  # interval-specific aesthetics
  point_aes = list(
    shape = 'Shape type used to draw the **point** sub-geometry.'
  )
  if (isTRUE(geom$default_params$show_point)) {
    out = c(out, "**Point-specific aesthetics**", rd_aesthetics(point_aes, geom$aesthetics()))
  }

  # color aesthetics
  color_aes = list(
    colour = '(or `color`) The color of the **interval** and **point** sub-geometries.
     Use the `slab_color`, `interval_color`, or `point_color` aesthetics (below) to
     set sub-geometry colors separately.',
    fill = 'The fill color of the **slab** and **point** sub-geometries. Use the `slab_fill`
     or `point_fill` aesthetics (below) to set sub-geometry colors separately.',
    alpha = 'The opacity of the **slab**, **interval**, and **point** sub-geometries. Use the `slab_alpha`,
     `interval_alpha`, or `point_alpha` aesthetics (below) to set sub-geometry colors separately.',
    colour_ramp = '(or `color_ramp`) A secondary scale that modifies the `color`
     scale to "ramp" to another color. See [scale_colour_ramp()] for examples.',
    fill_ramp = '(or `fill_ramp`) A secondary scale that modifies the `fill`
     scale to "ramp" to another color. See [scale_fill_ramp()] for examples.'
  )
  out = c(out, "**Color aesthetics**", rd_aesthetics(color_aes, geom$aesthetics()))

  # line aesthetics
  line_aes = list(
    size = 'Width of the outline around the **slab** (if visible). Also determines the width of
     the line used to draw the **interval** and the size of the **point**, but raw
     `size` values are transformed according to the `interval_size_domain`, `interval_size_range`,
     and `fatten_point` parameters of the `geom` (see above). Use the `slab_size`,
     `interval_size`, or `point_size` aesthetics (below) to set sub-geometry line widths separately
     (note that when size is set directly using the override aesthetics, interval and point
     sizes are not affected by `interval_size_domain`, `interval_size_range`, and `fatten_point`).',
    stroke = 'Width of the outline around the **point** sub-geometry.',
    linetype = 'Type of line (e.g., `"solid"`, `"dashed"`, etc) used to draw the **interval**
     and the outline of the **slab** (if it is visible). Use the `slab_linetype` or
     `interval_linetype` aesthetics (below) to set sub-geometry line types separately.'
  )
  out = c(out, "**Line aesthetics**", rd_aesthetics(line_aes, geom$aesthetics()))

  # slab override aesthetics
  slab_override_aes = list(
    slab_fill = 'Override for `fill`: the fill color of the slab.',
    slab_colour = '(or `slab_color`) Override for `colour`/`color`: the outline color of the slab.',
    slab_alpha = 'Override for `alpha`: the opacity of the slab.',
    slab_size = 'Override for `size`: the width of the outline of the slab.',
    slab_linetype = 'Override for `linetype`: the line type of the outline of the slab.',
    slab_shape = 'Override for `shape`: the shape of the dots used to draw the dotplot slab.'
  )
  if (isTRUE(geom$default_params$show_slab)) {
    out = c(out, "**Slab-specific color/line override aesthetics**", rd_aesthetics(slab_override_aes, geom$aesthetics()))
  }

  # interval override aesthetics
  int_override_aes = list(
    interval_colour = '(or `interval_color`) Override for `colour`/`color`: the color of the interval.',
    interval_alpha = 'Override for `alpha`: the opacity of the interval.',
    interval_size = 'Override for `size`: the line width of the interval.',
    interval_linetype = 'Override for `linetype`: the line type of the interval.'
  )
  if (isTRUE(geom$default_params$show_interval)) {
    out = c(out, "**Interval-specific color/line override aesthetics**", rd_aesthetics(int_override_aes, geom$aesthetics()))
  }

  # point override aesthetics
  point_override_aes = list(
    point_fill = 'Override for `fill`: the fill color of the point.',
    point_colour = '(or `point_color`) Override for `colour`/`color`: the outline color of the point.',
    point_alpha = 'Override for `alpha`: the opacity of the point.',
    point_size = 'Override for `size`: the size of the point.'
  )
  if (isTRUE(geom$default_params$show_point)) {
    out = c(out, "**Point-specific color/line override aesthetics**", rd_aesthetics(point_override_aes, geom$aesthetics()))
  }

  # undocumented aesthetics
  documented_aes = c(
    pos_aes, slab_aes, int_aes, point_aes, color_aes, line_aes, slab_override_aes, int_override_aes, point_override_aes
  )
  undocumented_aes = setdiff(geom$aesthetics(), names(documented_aes))
  if (length(undocumented_aes) > 0) {
    out = c(out,
      "**Other aesthetics** (these work as in standard `geom`s)",
      "\\itemize{",
      paste0("  \\item \\code{", undocumented_aes, "}"),
      "}"
    )
  }


  c(out,
    "See examples of some of these aesthetics in action in \\code{vignette(\"slabinterval\")}. ",
    "Learn more about the sub-geom override aesthetics (like \\code{interval_color}) in the \\link[ggdist]{scales} documentation. ",
    "Learn more about basic ggplot aesthetics in \\code{vignette(\"ggplot2-specs\")}. "
  )
}


# helpers -----------------------------------------------------------------

title_case = function(x) {
  substring(x, 1, 1) = toupper(substring(x, 1, 1))
  x
}
