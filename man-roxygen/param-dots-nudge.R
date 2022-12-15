#' @param nudge_overlaps logical: Should overlapping dots be nudged out of the way? When `FALSE`, dots may overlap
#' (usually only slightly) in the `"bin"`, `"weave"`, and `"hex"` layouts. When `TRUE`, overlaps are avoided by
#' using a constrained optimization which minimizes the squared distance of dots to their desired positions, subject
#' to the constraint that adjacent dots do not overlap.
