#' @param overlaps How to handle overlapping dots or bins in the `"bin"`,
#'   `"weave"`, and `"hex"` layouts (dots never overlap in the `"swarm"` layout).
#'   For the purposes of this argument, dots are only considered to be overlapping
#'   if they would be overlapping when `dotsize = 1` and `stackratio = 1`; i.e.
#'   if you set those arguments to other values, overlaps may still occur.
#'   One of:
#'   - `"keep"`: leave overlapping dots as they are. Dots may overlap
#'     (usually only slightly) in the `"bin"`, `"weave"`, and `"hex"` layouts.
#'   - `"nudge"`: nudge overlapping dots out of the way. Overlaps are avoided
#'     using a constrained optimization which minimizes the squared distance of
#'     dots to their desired positions, subject to the constraint that adjacent
#'     dots do not overlap.
