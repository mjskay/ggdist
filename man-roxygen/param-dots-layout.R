#' @param layout The layout method used for the dots:
#'  - `"bin"` (default): places dots on the off-axis at the midpoint of their bins as in the classic Wilkinson dotplot.
#'    This maintains the alignment of rows and columns in the dotplot. This layout is slightly different from the
#'    classic Wilkinson algorithm in that: (1) it nudges bins slightly to avoid overlapping bins and (2) if
#'    the input data are symmetricaly it will return a symmetrical layout.
#'  - `"weave"`: uses the same basic binning approach of `"bin"`, but places dots in the off-axis at their actual
#'    positions (modulo overlaps, which are nudged out of the way). This maintains the alignment of rows but does not
#'    align dots within columns. Does not work well when `side = "both"`.
#'  - `"swarm"`: uses the `"compactswarm"` layout from `beeswarm::beeswarm()`. Does not maintain alignment of rows or
#'    columns, but can be more compact and neat looking, especially for sample data (as opposed to quantile
#'    dotplots of theoretical distributions, which may look better with `"bin"` or `"weave"`).
