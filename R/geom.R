# Helper methods for creating geoms
#
# Author: mjskay
###############################################################################

# from ggstance:::ggname
ggname = function(prefix, grob) {
  grob$name = grobName(grob, prefix)
  grob
}


# default computed aesthetics ---------------------------------------------

# add default computed aesthetics to a layer --- useful for creating default aesthetics
# that are computed from the input data rather than default non-data-mapped aesthetics
#' @importFrom rlang get_expr as_quosure
add_default_computed_aesthetics = function(l, default_mapping) {
  ggproto(NULL, l,
    setup_layer = function(self, data, plot) {
      data = ggproto_parent(l, self)$setup_layer(data, plot)

      mapping = computed_mapping(self)

      for (aesthetic in names(default_mapping)) {
        # we don't use exact matching here because if someone is using ggnewscale
        # then aesthetic "x" will be replaced with "x_new" and we don't want to
        # re-create the default "x" aesthetic mapping in that case.
        default_aes_mapping = get_expr(default_mapping[[aesthetic]])
        vars_in_mapping = all_names(default_aes_mapping)
        if (
          # only add the aesthetic if it isn't already set and if the variables it uses
          # are in the provided data and none of them are NA
          is.null(mapping[[aesthetic, exact = FALSE]]) &&
          (!isTRUE(self$inherit.aes) || is.null(computed_mapping(plot)[[aesthetic, exact = FALSE]])) &&
          all(vars_in_mapping %in% names(data)) &&
          !anyNA(data[,vars_in_mapping])
        ) {
          # We reconstruct the quosure here instead of using default_mapping[[aesthetic]]
          # as a hack because for some reason when this is run inside {covr} it
          # gets mangled. So we need to recreate it from the underlying expression
          # and the environment (which in this case should be the package
          # environment, which is the same as environment(add_default_computed_aesthetics))
          mapping[[aesthetic]] = as_quosure(
            default_aes_mapping,
            env = environment(add_default_computed_aesthetics)
          )
        }
      }

      computed_mapping(self) = mapping

      data
    }
  )
}

#' the mapping property of layers changed to computed_mapping in ggplot 3.3.4
#' to avoid statefulness; this function encapsulates that change
#' see https://github.com/tidyverse/ggplot2/pull/4475
#' @importFrom utils packageVersion
#' @noRd
computed_mapping = function(x) {
  mapping = if (packageVersion("ggplot2") >= "3.3.3.9000") {
    x$computed_mapping
  } else {
    x$mapping     # nocov
  }
  mapping %||% list()
}
`computed_mapping<-` = function(x, value) {
  if (packageVersion("ggplot2") >= "3.3.3.9000") {
    x$computed_mapping = value
  } else {
    x$mapping = value     # nocov
  }
  x
}

# orientation detection ---------------------------------------------------

# detects the orientation of the geometry
#' @importFrom ggplot2 has_flipped_aes
get_flipped_aes = function(data, params, ..., secondary_is_dist = NA, main_is_orthogonal = NA) {
  params$orientation =
    if (params$orientation %in% c("horizontal", "y")) "y"
    else if (params$orientation %in% c("vertical", "x")) "x"
    else if (is.na(params$orientation)) NA
    else stop0("Unknown orientation: ", deparse0(params$orientation))

  # checks based on xdist or ydist
  if (is.na(params$orientation) && !is.na(secondary_is_dist)) {
    if (!is.null(data$xdist)) {
      return(secondary_is_dist)
    } else if (!is.null(data$ydist)) {
      return(!secondary_is_dist)
    } else if (!is.null(data$dist)) {
      # when dist is provided, we can't determine orientation at this point but
      # main_is_orthogonal must be determined by secondary_is_dist
      main_is_orthogonal = !secondary_is_dist
    }
  }

  has_flipped_aes(data, params, ..., main_is_orthogonal = main_is_orthogonal)
}

# detects the orientation of the geometry
get_orientation = function(flipped_aes) {
  if (flipped_aes) "y"
  else "x"
}


# defines "orientation" variables in the environment of the calling
# function (for convenience): these are variables (typically aesthetics)
# that differ depending on whether the geom's orientation is horizontal
# or vertical. They are named assuming a horizontal orientation.
globalVariables(c("height", "y", "ymin", "ymax", "yend", "x", "xmin", "xmax", "xend","x.range","y.range"))
define_orientation_variables = function(orientation) {
  f = parent.frame()

  if (orientation == "horizontal" || orientation == "y") {
    f$height = "height"
    f$width. = "width"

    f$y = "y"
    f$ymin = "ymin"
    f$ymax = "ymax"
    f$yend = "yend"
    f$y.range = "y.range"
    f$ydist = "ydist"

    f$x = "x"
    f$xmin = "xmin"
    f$xmax = "xmax"
    f$xend = "xend"
    f$x.range = "x.range"
    f$xdist = "xdist"
  } else if (orientation == "vertical" || orientation == "x") {
    f$height = "width"
    f$width. = "height"

    f$y = "x"
    f$ymin = "xmin"
    f$ymax = "xmax"
    f$yend = "xend"
    f$y.range = "x.range"
    f$ydist = "xdist"

    f$x = "y"
    f$xmin = "ymin"
    f$xmax = "ymax"
    f$xend = "yend"
    f$x.range = "y.range"
    f$xdist = "ydist"
  } else {
    stop0("Unknown orientation: ", deparse0(orientation))
  }
}


# ggproto -----------------------------------------------------------------

#' Base ggproto classes for ggdist
#'
#' @seealso [ggproto]
#' @keywords internal
#' @name ggdist-ggproto
NULL

# from ggplot2:::ggproto_formals
ggproto_formals = function(x) formals(environment(x)$f)
