# Deprecated functions
#
# Author: mjskay
###############################################################################



# tidybayes-deprecated ----------------------------------------------------

#' Deprecated functions, arguments, and column names in tidybayes
#'
#' Deprecated functions, arguments, and column names and their alternatives are listed below.
#' Many of the deprecations are due to a naming scheme overhaul in tidybayes version 1.0
#' (see *Deprecated Functions* and *Deprecated Arguments and Column Names* below) or due to
#' the deprecation of horizontal shortcut geoms and stats in tidybayes 2.1 (see
#' *Deprecated Horizontal Shortcut Geoms and Stats*).
#'
#' @section Deprecated Functions:
#'
#' Several deprecated versions of functions use slightly different output
#' formats (e.g., they use names like `term` and `estimate` where new
#' functions use `.variable` and `.value`; or they set `.iteration` even
#' when iteration information is not available --- new functions always set `.draw`
#' but may not set `.iteration`), so be careful when upgrading to new function names.
#' See *Deprecated Arguments and Column Names*, below, for more information.
#'
#' Deprecated functions are:
#'
#' \itemize{
#'
#'   \item `spread_samples`, `extract_samples`, and `tidy_samples` are
#'   deprecated names for [spread_draws()]. The spread/gather terminology
#'   better distinguishes the resulting data frame format, and *draws* is more
#'   correct terminology than *samples* for describing multiple realizations from
#'   a posterior distribution.
#'
#'   \item `gather_samples` is a deprecated name for [gather_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `unspread_samples` is a deprecated name for [unspread_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `ungather_samples` is a deprecated name for [ungather_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `fitted_samples` / `add_fitted_samples` are deprecated names for
#'   [fitted_draws()] / [add_fitted_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `predicted_samples` / `add_predicted_samples` are deprecated names for
#'   [predicted_draws()] / [add_predicted_draws()],
#'   reflecting a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `gather_lsmeans_samples` and `gather_emmeans_samples` are deprecated aliases
#'   for [gather_emmeans_draws()]. The new name (estimated marginal means) is more
#'   appropriate for Bayesian models than the old name (least-squares means), and reflects the
#'   naming of the newer `emmeans` package. It also reflects
#'   a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `as_sample_tibble` and `as_sample_data_frame` are deprecated aliases
#'   for [tidy_draws()]. The original intent of `as_sample_tibble` was to be
#'   used primarily internally (hence its less user-friendly name); however, increasingly
#'   I have come across use cases of `tidy_draws` that warrant a more user-friendly name.
#'   It also reflects a package-wide move to using *draws* instead of *samples* for
#'   describing multiple realizations from a distribution.
#'
#'   \item `ggeye` is deprecated: for a package whose goal is flexible and customizable
#'   visualization, monolithic functions are inflexible and do not sufficiently capitalize on users'
#'   existing knowledge of ggplot; instead, I think it is more flexible to design geoms and stats
#'   that can used within a complete ggplot workflow. [stat_eye()] offers a horizontal
#'   eye plot geom that can be used instead of `ggeye`.
#'
#'   \item See [tidybayes-deprecated-geoms] for a list of deprecated geoms (mainly
#'   horizontal shortcut geoms that are no longer necessary due to automatic orientation detection.)
#'
#' }
#'
#' @section Deprecated Eye Geom Spellings:
#'
#' `geom_eye`, `geom_eyeh`, and `geom_halfeyeh` are deprecated spellings of [stat_eye()] and
#' [stat_halfeye()] from before name standardization of stats and geoms. Use those functions instead.
#'
#' @section Deprecated Horizontal Shortcut Geoms and Stats:
#'
#' Due to the introduction of automatic orientation detection in tidybayes 2.1,
#' shortcut geoms and stats (which end in `h`) are no longer necessary, and are
#' deprecated. In most cases, these can simply be replaced with the same
#' geom without the `h` suffix and they will remain horizontal; e.g.
#' `stat_halfeyeh(...)` can simply be replaced with `stat_halfeye(...)`.
#' If automatic orientation detection fails, override it with the `orientation`
#' parameter; e.g. `stat_halfeye(orientation = "horizontal")`.
#'
#' These deprecated stats and geoms include:
#'
#' - `stat_eyeh` / `stat_dist_eyeh`
#' - `stat_halfeyeh` / `stat_dist_halfeyeh`
#' - `geom_slabh` / `stat_slabh` / `stat_dist_slabh`
#' - `geom_intervalh` / `stat_intervalh` / `stat_dist_intervalh`
#' - `geom_pointintervalh` / `stat_pointintervalh` / `stat_dist_pointintervalh`
#' - `stat_gradientintervalh` / `stat_dist_gradientintervalh`
#' - `stat_cdfintervalh` / `stat_dist_cdfintervalh`
#' - `stat_ccdfintervalh` / `stat_dist_ccdfintervalh`
#' - `geom_dotsh` / `stat_dotsh` / `stat_dist_dotsh`
#' - `geom_dotsintervalh` / `stat_intervalh` / `stat_dist_intervalh`
#' - `stat_histintervalh`
#'
#' @section Deprecated Arguments and Column Names:
#'
#' Versions of tidybayes before version 1.0 used a different naming scheme for several
#' arguments and output columns.
#'
#' Deprecated arguments and column names are:
#'
#' \itemize{
#'   \item `term` is now `.variable`
#'   \item `estimate` is now `.value`
#'   \item `pred` is now `.prediction`
#'   \item `conf.low` is now `.lower`
#'   \item `conf.high` is now `.upper`
#'   \item `.prob` is now `.width`
#'   \item The `.draw` column was added, and should be used instead of `.chain`
#'     and `.iteration` to uniquely identify draws when you do not care about chains. (`.chain` and
#'     `.iteration` are still provided for identifying draws *within* chains, if desired).
#' }
#'
#' To translate to/from the old naming scheme in output, use [to_broom_names()]
#' and [from_broom_names()].
#'
#' Many of these names were updated in version 1.0 in order to
#' make terminology more consistent and in order to satisfy these criteria:
#'
#' \itemize{
#'   \item Ignore compatibility with broom names on the assumption an adapter function can be created.
#'   \item Use names that could be compatible with frequentist approaches (hence `.width` instead of `.prob`).
#'   \item Always precede with "." to avoid collisions with variable names in models.
#'   \item No abbreviations (remembering if something is abbreviated or not can be a pain).
#'   \item No two-word names (multi-word names can always be standardized on and used in documentation, but I think data frame output should be succinct).
#'   \item Names should be nouns (I made an exception for lower/upper because they are common).
#' }
#'
#' @format NULL
#' @usage NULL
#' @author Matthew Kay
#' @name tidybayes-deprecated
#' @import ggplot2
#' @export
ggeye = function(data = NULL, mapping = NULL, ...) {
  .Deprecated("geom_eyeh", package = "tidybayes")
  ggplot(data = data, mapping = mapping) + geom_eye(...) + coord_flip()
}


# deprecated eye geom spellings -------------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_eye = function(
  ...,
  scale = 0.9,
  .width = c(.66, .95),

  #deprecated arguments
  relative_scale,
  .prob
) {
  .Deprecated("stat_eye", package = "tidybayes")
  .width = .Deprecated_argument_alias(.width, .prob)
  scale = .Deprecated_argument_alias(scale, relative_scale)

  stat_eye(..., .width = .width, scale = scale)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_eyeh = function(
  ...,
  scale = 0.9,
  .width = c(.66, .95),

  #deprecated arguments
  relative_scale,
  .prob
) {
  .Deprecated("stat_eye", package = "tidybayes")
  .width = .Deprecated_argument_alias(.width, .prob)
  scale = .Deprecated_argument_alias(scale, relative_scale)

  stat_eye(..., .width = .width, scale = scale, orientation = "horizontal")
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_halfeyeh = function(
  ...,
  scale = 0.9,
  .width = c(.66, .95),

  #deprecated arguments
  relative_scale,
  .prob
) {
  .Deprecated("stat_halfeye", package = "tidybayes")
  .width = .Deprecated_argument_alias(.width, .prob)
  scale = .Deprecated_argument_alias(scale, relative_scale)

  stat_halfeye(..., .width = .width, scale = scale, orientation = "horizontal")
}


# deprecated horizontal geoms -----------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_slabh = function(..., orientation = "horizontal") {
  .Deprecated("geom_slab", package = "tidybayes")
  geom_slab(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_intervalh = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,

  side = "both",
  orientation = "horizontal",
  interval_size_range = c(1, 6),
  show_slab = FALSE,
  show_point = FALSE
) {
  .Deprecated("geom_interval", package = "tidybayes")

  layer_geom_slabinterval(
    data = data,
    mapping = mapping,
    default_mapping = aes(xmin = .lower, xmax = .upper, color = forcats::fct_rev(ordered(.width))),
    stat = stat,
    geom = GeomIntervalh,
    position = position,
    ...,

    side = side,
    orientation = orientation,
    interval_size_range = interval_size_range,
    show_slab = show_slab,
    show_point = show_point,

    datatype = "interval"
  )
}
#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
GeomIntervalh = ggproto("GeomIntervalh", GeomSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval"
  ), GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    size = 4,
    fill = NA
  ), GeomSlabinterval$default_key_aes),

  default_params = defaults(list(
    side = "both",
    orientation = "horizontal",
    interval_size_range = c(1, 6),
    show_slab = FALSE,
    show_point = FALSE
  ), GeomSlabinterval$default_params),

  default_datatype = "interval"
)

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_pointintervalh = function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,

  side = "both",
  orientation = "horizontal",
  show_slab = FALSE,

  show.legend = c(size = FALSE)
) {
  .Deprecated("geom_pointinterval", package = "tidybayes")

  layer_geom_slabinterval(
    data = data,
    mapping = mapping,
    default_mapping = aes(xmin = .lower, xmax = .upper, size = -.width),
    stat = stat,
    geom = GeomPointintervalh,
    position = position,
    ...,

    side = side,
    orientation = orientation,
    show_slab = show_slab,

    datatype = "interval",

    show.legend = show.legend
  )
}

#' @rdname tidybayes-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomPointintervalh = ggproto("GeomPointintervalh", GeomSlabinterval,
  default_aes = defaults(aes(
    datatype = "interval"
  ), GeomSlabinterval$default_aes),

  default_key_aes = defaults(aes(
    fill = NA
  ), GeomSlabinterval$default_key_aes),

  default_params = defaults(list(
    side = "both",
    orientation = "horizontal",
    show_slab = FALSE
  ), GeomSlabinterval$default_params),

  default_datatype = "interval"
)

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_dotsh = function(..., orientation = "horizontal") {
  .Deprecated("geom_dots", package = "tidybayes")
  geom_dots(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
geom_dotsintervalh = function(..., orientation = "horizontal") {
  .Deprecated("geom_dotsinterval", package = "tidybayes")
  geom_dotsinterval(..., orientation = orientation)
}

# deprecated horizontal stat_dists ----------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_eyeh = function(..., side = "both", orientation = "horizontal") {
  .Deprecated("stat_dist_eye", package = "tidybayes")
  stat_dist_slabinterval(..., side = side, orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_halfeyeh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_halfeye", package = "tidybayes")
  stat_dist_slabinterval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_slabh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_slab", package = "tidybayes")
  stat_dist_slab(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_intervalh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_interval", package = "tidybayes")
  stat_dist_interval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_pointintervalh = function(..., show_slab = FALSE, orientation = "horizontal") {
  .Deprecated("stat_dist_pointinterval", package = "tidybayes")
  stat_dist_slabinterval(..., show_slab = show_slab, orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_gradientintervalh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_gradientinterval", package = "tidybayes")
  stat_dist_gradientinterval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_cdfintervalh = function(...,
  slab_type = "cdf", justification = 0.5, side = "topleft", orientation = "horizontal", normalize = "none"
) {
  .Deprecated("stat_dist_cdfinterval", package = "tidybayes")
  stat_dist_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_ccdfintervalh = function(...,
  slab_type = "ccdf", justification = 0.5, side = "topleft", orientation = "horizontal", normalize = "none"
) {
  .Deprecated("stat_dist_ccdfinterval", package = "tidybayes")
  stat_dist_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_dotsh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_dots", package = "tidybayes")
  stat_dist_dots(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dist_dotsintervalh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dist_dotsinterval", package = "tidybayes")
  stat_dist_dotsinterval(..., orientation = orientation)
}


# deprecated horizontal stats ---------------------------------------------

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_eyeh = function(..., side = "both", orientation = "horizontal") {
  .Deprecated("stat_eye", package = "tidybayes")
  stat_sample_slabinterval(..., side = side, orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_halfeyeh = function(..., orientation = "horizontal") {
  .Deprecated("stat_halfeye", package = "tidybayes")
  stat_sample_slabinterval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_slabh = function(..., orientation = "horizontal") {
  .Deprecated("stat_slab", package = "tidybayes")
  stat_slab(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_intervalh = function(
  mapping = NULL,
  data = NULL,
  geom = "interval",
  position = "identity",
  ...,

  orientation = "horizontal",
  interval_function = NULL,
  interval_args = list(),
  point_interval = median_qi,
  .width = c(.50, .80, .95),
  show_point = FALSE,
  show_slab = FALSE,
  na.rm = FALSE,

  show.legend = NA,
  inherit.aes = TRUE,

  #deprecated arguments
  .prob,
  fun.data,
  fun.args
) {
  .Deprecated("stat_interval", package = "tidybayes")
  interval_function = .Deprecated_argument_alias(interval_function, fun.data)
  interval_args = .Deprecated_argument_alias(interval_args, fun.args)
  .width = .Deprecated_argument_alias(.width, .prob)

  layer(
    data = data,
    mapping = mapping,
    stat = StatIntervalh,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      orientation = orientation,
      interval_function = interval_function,
      interval_args = interval_args,
      point_interval = point_interval,
      .width = .width,
      show_point = show_point,
      show_slab = show_slab,
      na.rm = na.rm,
      ...
    )
  )
}
StatIntervalh = ggproto("StatIntervalh", StatInterval,
  default_params = defaults(list(
    orientation = "horizontal"
  ), StatInterval$default_params)
)

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_pointintervalh = function(
  mapping = NULL,
  data = NULL,
  geom = "pointintervalh",
  position = "identity",
  ...,

  orientation = "horizontal",
  interval_function = NULL,
  interval_args = list(),
  point_interval = median_qi,
  .width = c(.66, .95),
  show_slab = FALSE,
  na.rm = FALSE,

  show.legend = c(size = FALSE),
  inherit.aes = TRUE,

  #deprecated arguments
  .prob,
  fun.data,
  fun.args
) {
  .Deprecated("stat_pointinterval", package = "tidybayes")
  interval_function = .Deprecated_argument_alias(interval_function, fun.data)
  interval_args = .Deprecated_argument_alias(interval_args, fun.args)
  .width = .Deprecated_argument_alias(.width, .prob)

  layer(
    data = data,
    mapping = mapping,
    stat = StatPointintervalh,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      orientation = orientation,
      interval_function = interval_function,
      interval_args = interval_args,
      point_interval = point_interval,
      .width = .width,
      show_slab = show_slab,
      na.rm = na.rm,
      ...
    )
  )
}
StatPointintervalh = ggproto("StatPointintervalh", StatPointinterval,
  default_params = defaults(list(
    orientation = "horizontal"
  ), StatPointinterval$default_params)
)

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_gradientintervalh = function(..., orientation = "horizontal") {
  .Deprecated("stat_gradientinterval", package = "tidybayes")
  stat_gradientinterval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_cdfintervalh = function(...,
  slab_type = "cdf", justification = 0.5, side = "topleft", orientation = "horizontal", normalize = "none"
) {
  .Deprecated("stat_cdfinterval", package = "tidybayes")
  stat_sample_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_ccdfintervalh = function(...,
  slab_type = "ccdf", justification = 0.5, side = "topleft", orientation = "horizontal", normalize = "none"
) {
  .Deprecated("stat_ccdfinterval", package = "tidybayes")
  stat_sample_slabinterval(...,
    slab_type = slab_type, justification = justification, side = side, orientation = orientation, normalize = normalize
  )
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dotsh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dots", package = "tidybayes")
  stat_dots(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_dotsintervalh = function(..., orientation = "horizontal") {
  .Deprecated("stat_dotsinterval", package = "tidybayes")
  stat_dotsinterval(..., orientation = orientation)
}

#' @rdname tidybayes-deprecated
#' @format NULL
#' @usage NULL
#' @export
stat_histintervalh = function(..., slab_type = "histogram" , orientation = "horizontal") {
  .Deprecated("stat_dotsinterval", package = "tidybayes")
  stat_sample_slabinterval(..., slab_type = slab_type, orientation = orientation)
}
