# Documentation methods for the dotsinterval family
#
# Author: mjskay
###############################################################################



# shared parameter docs ---------------------------------------------------

rd_param_dots_layout = function() {
  paste0("@param layout ", GeomDotsinterval$get_param_docs()$layout)
}

rd_param_dots_overlaps = function() {
  paste0("@param overlaps ", GeomDotsinterval$get_param_docs()$overlaps)
}

rd_param_slab_side = function() {
  paste0("@param side ", GeomSlabinterval$get_aes_docs()[["Slab-specific aesthetics"]]$side)
}
