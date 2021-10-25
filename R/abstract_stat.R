# A stat base class designed to reduce boilerplate
#
# Author: mjskay
###############################################################################



#' Stat base class designed to reduce boilerplate
#'
#' A base class for orientation-aware stats that handles boilerplate generation
#' (e.g. for default parameter values). Should never be created on its own,
#' only used as a base class.
#'
#' Differences from ggplot2::Stat:
#'
#' - It is assumed that child classes will use `compute_panel()` and not
#' `compute_group()`, so parameter names are always auto-detected from `compute_panel()`.
#' - A `default_params` property is used to hold a list mapping from parameters
#' onto their default values (used in boilerplate generation). This list is also
#' used instead of `extra_params` in `parameters()` to auto-detect parameters.
#' These default values are also automatically applied in `setup_params()`.
#' - An `orientation_options` property provides parameters passed to `get_flipped_aes()`.
#' Necessary orientation detection boilerplate is included in `setup_params()` and
#' `setup_data()`.
#' - `deprecated_params` provides a list of deprecated parameters that will cause
#' a warning to be generated if used.
#' - `layer_args` provides a list of layer arguments (such as `show.legend` and
#' `inherit.aes`) and their default values.
#'
#' @keywords internal
#' @noRd
AbstractStat = ggproto("AbstractStat", Stat,
  default_params = list(
    orientation = NA,
    na.rm = FALSE
  ),

  # arguments passed to the stat_XXX() constructor and the underlying layer() call
  layer_args = list(
    show.legend = NA,
    inherit.aes = TRUE
  ),

  # parameters that have been deprecated and which should throw a warning if used
  deprecated_params = character(),

  # arguments to get_flipped_aes that determine orientation detection
  orientation_options = list(),

  setup_params = function(self, data, params) {
    params = defaults(params, self$default_params)

    # detect orientation
    orientation_args = c(list(quote(data), quote(params)), self$orientation_options)
    params$flipped_aes = do.call(get_flipped_aes, orientation_args)
    params$orientation = get_orientation(params$flipped_aes)

    params
  },

  setup_data = function(self, data, params) {
    #set up orientation
    data$flipped_aes = params$flipped_aes

    data
  },

  # Based on ggplot2::Stat$parameters, except we always take parameters
  # from compute_panel(), because compute_group() is not used by this stat,
  # and we also take default_params instead of extra_params
  parameters = function(self, extra = TRUE) {
    panel_args = names(ggproto_formals(self$compute_panel))

    # Remove arguments of compute_group(), which are not parameters
    params = setdiff(panel_args, c(names(ggproto_formals(Stat$compute_group)), "..."))

    # we ignore the `extra` argument and just always include the "extra"
    # parameters (based on default_params instead of extra_params as in ggplot2::Stat)
    union(params, names(self$default_params))
  }
)

#' @importFrom rlang enexpr syms
make_stat = function(stat, geom,
  mapping = NULL,
  data = NULL,
  position = "identity",
  ...
) {
  stat_name = enexpr(stat)

  # stat parameters
  params_to_defaults = lapply(stat$default_params, to_expression)
  params_to_syms = syms(names(params_to_defaults))
  names(params_to_syms) = names(params_to_defaults)

  # layer arguments
  args_to_defaults = lapply(stat$layer_args, to_expression)
  args_to_syms = syms(names(args_to_defaults))
  names(args_to_syms) = names(args_to_defaults)

  f = eval(bquote(
      function(
        mapping = .(mapping),
        data = .(data),
        geom = .(geom),
        position = .(position),
        ...
      ) {
        .Deprecated_arguments(.(stat$deprecated_params), ...)

        layer(
          data = data,
          mapping = mapping,
          stat = .(stat_name),
          geom = geom,
          position = position,

          ..(args_to_syms),

          params = list(
            ..(params_to_syms),
            ...
          )
        )
      },
      splice = TRUE
    ),
    envir = parent.frame()
  )

  formals(f) = c(
    formals(f),
    params_to_defaults,
    args_to_defaults
  )

  f
}


# helpers -----------------------------------------------------------------

#' Convert simple objects to expressions representing those objects
#' Needed for code generation so that the formals of a function's documentation
#' (which will be expressions) match the formals of the generated code.
#' @noRd
to_expression = function(x) {
  parse(text = deparse(x), keep.source = FALSE)[[1]]
}
