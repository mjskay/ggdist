#' @description
#' Dotplot layouts can be passed to the `layout` argument of [bin_dots()],
#' [find_dotplot_binwidth()], and dots stats and geoms like [geom_dots()]. All dotplot layout
#' algorithms are \pkg{S7} classes that are also functions that support
#' [automatic partial function application][auto_partial]. They can be constructed without passing
#' their first argument, `dots`, which is filled in later by dotplot layout functions like
#' [bin_dots()] and [find_dotplot_binwidth()].
#' @family dotplot layouts
