# Documentation methods for the density functions
#
# Author: mjskay
###############################################################################



# shared parameter docs ---------------------------------------------------

#' construct the first part of the `@param` docstring for a parameter of one of the
#' functions in the density family (or, if `passed_to` is provided, construct
#' the first part of the `@param` docstring for that parameter as documented in
#' a `stat` that will pass that parameter to the `density` parameter, expecting
#' a density-family function like the one named `passed_to`.
#' @param description short description of the param
#' @param default description of the default value
#' @param passed_to `NULL`, or the name of an example function in the `density`
#' family that would have this parameter.
#' @noRd
rd_param_density = function(types, description, default, passed_to = NULL) {
  paste0(
    "<", types, if (is.null(passed_to)) "> ",
    if (!is.null(passed_to)) glue_doc(" | [waiver]> Passed to `density` (e.g. [<<passed_to>>()]): "),
    description,
    " Default ",
    if (!is.null(passed_to)) "[waiver()] defers to the default of the density estimator, which is usually ",
    default
  )
}

rd_param_density_adjust = function(passed_to = NULL) {
  short_description = rd_param_density(
    types = 'scalar [numeric]',
    description = 'Value to multiply the bandwidth of the density estimator by.',
    default = '`1`.',
    passed_to = passed_to
  )
  glue_doc('@param adjust <<short_description>>')
}

rd_param_density_trim = function(passed_to = NULL) {
  short_description = rd_param_density(
    types = 'scalar [logical]',
    description = 'Should the density estimate be trimmed to the range of the data?',
    default = '`TRUE`.',
    passed_to = passed_to
  )
  glue_doc('@param trim <<short_description>>')
}

rd_param_density_align = function(passed_to = NULL) {
  short_description = rd_param_density(
    types = 'scalar [numeric] | [function] | [string][character]',
    description = 'Determines how to align the breakpoints defining bins.',
    default = '`"none"` (performs no alignment).',
    passed_to = passed_to
  )
  glue_doc('
    @param align <<short_description>> One of:
    \\itemize{
      \\item A scalar (length-1) numeric giving an offset that is subtracted
        from the breaks. The offset must be between `0` and the bin width.
      \\item A function taking a sorted vector of `breaks` (bin edges) and
        returning an offset to subtract from the breaks.
      \\item A string giving the suffix of a function that starts with
        `"align_"` used to determine the alignment, such as [align_none()],
        [align_boundary()], or [align_center()].
    }
    For example, `align = "none"` will provide no alignment,
    `align = align_center(at = 0)` will center a bin on `0`, and
    `align = align_boundary(at = 0)` will align a bin edge on `0`.
    ')
}

rd_param_density_breaks = function(passed_to = NULL) {
  short_description = rd_param_density(
    types = '[numeric] | [function] | [string][character]',
    description = 'Determines the breakpoints defining bins.',
    default = '`"Scott"`.',
    passed_to = passed_to
  )
  glue_doc('
    @param breaks
    <<short_description>> Similar to (but not exactly the same as) the `breaks`
    argument to [graphics::hist()]. One of:
    \\itemize{
      \\item A scalar (length-1) numeric giving the number of bins
      \\item A vector numeric giving the breakpoints between histogram bins
      \\item A function taking `x` and `weights` and returning either the
        number of bins or a vector of breakpoints
      \\item A string giving the suffix of a function that starts with
        `"breaks_"`. \\pkg{ggdist} provides weighted implementations of the
        `"Sturges"`, `"Scott"`, and `"FD"` break-finding algorithms from
        [graphics::hist()], as well as [breaks_fixed()] for manually setting
        the bin width. See [breaks].
    }
    For example, `breaks = "Sturges"` will use the [breaks_Sturges()] algorithm,
    `breaks = 9` will create 9 bins, and `breaks = breaks_fixed(width = 1)` will
    set the bin width to `1`.
    ')
}
