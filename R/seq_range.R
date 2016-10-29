# seq_range
# 
# Author: mjskay
###############################################################################

# TODO: drop in favor of modelr::seq_range
#' @export
seq_range = function(x, ...) {
    seq(from = min(x), to = max(x), ...)
}
