# A geom base class designed to reduce boilerplate
#
# Author: mjskay
###############################################################################



# AbstractGeom ------------------------------------------------------------

#' Geom base class designed to reduce boilerplate
#'
#' A base class for orientation-aware geoms that handles boilerplate generation
#' (e.g. for default parameter values). Should never be created on its own,
#' only used as a base class.
#'
#' Differences from ggplot2::Geom:
#'
#' - It is assumed that child classes will use `draw_panel()` and not
#' `draw_group()`, so parameter names are always auto-detected from `draw_panel()`.
#' - A `default_params` property is used to hold a list mapping from parameters
#' onto their default values (used in boilerplate generation). This list is also
#' used instead of `extra_params` in `parameters()` to auto-detect parameters.
#' These default values are also automatically applied in `setup_params()`.
#' - An `orientation_options` property provides parameters passed to `get_flipped_aes()`.
#' Necessary orientation detection boilerplate is included in `setup_params()` and
#' `setup_data()`.
#' - `hidden_params` provides a list of parameters not to be exposed in the
#' `geom_XXX()` constructor
#' - `deprecated_params` provides a list of deprecated parameters that will cause
#' a warning to be generated if used.
#' - `layer_args` provides a list of layer arguments (such as `show.legend` and
#' `inherit.aes`) and their default values.
#' - `default_computed_aes` provides a set of aesthetic mappings to be applied
#' to the layer in the `geom_XXX()` constructor using `add_default_computed_aesthetics()`
#'
#' @keywords internal
#' @noRd
AbstractGeom = ggproto("AbstractGeom", Geom,

  ## aesthetics --------------------------------------------------------------

  default_computed_aes = aes(),

  # named list of named lists: section title => aesthetic => doc string
  aes_docs = list(),

  get_aes_docs = function(self, ...) {
    self$aes_docs
  },

  # aesthetics to hide from documentation
  hidden_aes = character(),


  ## layer arguments ---------------------------------------------------------

  # arguments passed to the geom_XXX() constructor and the underlying layer() call
  layer_args = list(
    show.legend = NA,
    inherit.aes = TRUE
  ),


  ## parameters --------------------------------------------------------------

  param_docs = list(
    orientation = glue_doc('
      Whether this geom is drawn horizontally or vertically. One of:
      \\itemize{
        \\item `NA` (default): automatically detect the orientation based on how the aesthetics
          are assigned. Automatic detection works most of the time.
        \\item `"horizontal"` (or `"y"`): draw horizontally, using the `y` aesthetic to identify different
          groups. For each group, uses the `x`, `xmin`, `xmax`, and `thickness` aesthetics to
          draw points, intervals, and slabs.
        \\item `"vertical"` (or `"x"`): draw vertically, using the `x` aesthetic to identify different
          groups. For each group, uses the `y`, `ymin`, `ymax`, and `thickness` aesthetics to
          draw points, intervals, and slabs.
      }
      For compatibility with the base ggplot naming scheme for `orientation`, `"x"` can be used as an alias
      for `"vertical"` and `"y"` as an alias for `"horizontal"` (\\pkg{ggdist} had an `orientation` parameter
      before base ggplot did, hence the discrepancy).
      '),

    na.rm = glue_doc('
      If `FALSE`, the default, missing values are removed with a warning. If `TRUE`, missing
      values are silently removed.
      ')
  ),

  get_param_docs = function(self, ...) {
    self$param_docs
  },

  default_params = list(
    orientation = NA,
    na.rm = FALSE
  ),

  # parameters to hide from user input in the geom_XXX() constructor
  hidden_params = character(),

  # parameters that have been deprecated and which should throw a warning if used
  deprecated_params = character(),

  # arguments to get_flipped_aes that determine orientation detection
  orientation_options = list(),

  setup_params = function(self, data, params) {
    params = ggproto_parent(Geom, self)$setup_params(data, params)
    params = defaults(params, self$default_params)

    # detect orientation
    orientation_args = c(list(quote(data), quote(params)), self$orientation_options)
    params$flipped_aes = do.call(get_flipped_aes, orientation_args)
    params$orientation = get_orientation(params$flipped_aes)

    params
  },

  setup_data = function(self, data, params) {
    data = ggproto_parent(Geom, self)$setup_data(data, params)

    #set up orientation
    data$flipped_aes = params$flipped_aes

    data
  },

  # Based on ggplot2::Geom$parameters, except we always take parameters
  # from draw_panel(), because draw_group() is not used by this geom,
  # and we also take default_params instead of extra_params
  parameters = function(self, extra = TRUE) {
    panel_args = names(ggproto_formals(self$draw_panel))

    # Remove arguments of draw_group(), which are not parameters
    params = setdiff(panel_args, c(names(ggproto_formals(Geom$draw_group)), "..."))

    # we ignore the `extra` argument and just always include the "extra"
    # parameters (based on default_params instead of extra_params as in ggplot2::Geom)
    union(params, names(self$default_params))
  },


  ## other methods -----------------------------------------------------------

  use_defaults = function(self, data, params = list(), modifiers = aes(), ...) {
    # we must provide our own check for Geom$rename_size because our fallbacks
    # from default_aes to default_key_aes require us to be able to have a non-missing
    # (but NULL) linewidth aesthetic in default_aes, which ggplot2::Geom$use_defaults
    # with rename_size = TRUE treats as a *missing* aesthetic and generates a warning for
    if (self$rename_size && is.null(data$linewidth) && is.null(params$linewidth)) {
      data$linewidth = data$size
      params$linewidth = params$size
    }
    rename_size = self$rename_size
    self$rename_size = FALSE
    out = ggproto_parent(Geom, self)$use_defaults(data, params, modifiers, ...)
    self$rename_size = rename_size
    out
  }
)


# make_geom ---------------------------------------------------------------

#' @importFrom rlang syms new_function pairlist2 expr
make_geom = function(geom,
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...
) {
  geom_name = substitute(geom)

  # geom parameters
  params = geom$default_params[!names(geom$default_params) %in% geom$hidden_params]
  params_to_defaults = lapply(params, to_expression)
  params_to_syms = syms(names(params_to_defaults))
  names(params_to_syms) = names(params_to_defaults)

  # layer arguments
  args_to_defaults = lapply(geom$layer_args, to_expression)
  args_to_syms = syms(names(args_to_defaults))
  names(args_to_syms) = names(args_to_defaults)

  new_f = new_function(
    pairlist2(
      mapping = mapping,
      data = data,
      stat = stat,
      position = position,
      ... = ,
      !!!params_to_defaults,
      !!!args_to_defaults
    ),
    expr({                                                     # nocov start
      .Deprecated_arguments(!!geom$deprecated_params, ...)

      l = layer(
        data = data,
        mapping = mapping,
        geom = !!geom_name,
        stat = stat,
        position = position,

        !!!args_to_syms,

        params = list(
          !!!params_to_syms,
          ...
        )
      )

      !!(
        if (length(geom$default_computed_aes) > 0L) {
          expr(add_default_computed_aesthetics(l, !!geom$default_computed_aes))
        } else {
          quote(l)
        }
      )
    }),                                                        # nocov end
    env = parent.frame()
  )
  new_f = removeSource(new_f)

  new_f
}


# helpers -----------------------------------------------------------------

#' Convert simple objects to expressions representing those objects
#' Needed for code generation so that the formals of a function's documentation
#' (which will be expressions) match the formals of the generated code.
#' @noRd
to_expression = function(x) {
  if (inherits(x, "waiver")) {
    quote(waiver())
  } else {
    parse(text = deparse(x), keep.source = FALSE)[[1L]]
  }
}
