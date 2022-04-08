#' @param keep_order If `layout == "bin"`, should the original order of the data
#' be kept when stacking the dots within each bin?
#'  - If `FALSE`, dots within each bin will be stacked from bottom to top in
#'    the order of the values of each data point.
#'  - If `TRUE`, dots within each bin will be placed in the original order they
#'    appear in the data, from bottom to top. This can be used to create the
#'    appearance of "stacked" groups of dots by sorting your data by the desired
#'    stacking groups and then setting `keep_order` to `TRUE`.
#'
#' Applies only to the `"dots"` layout, as the other layouts fully determine
#' both the x and y positions of dots.
