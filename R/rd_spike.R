# Documentation methods for the spike family
#
# Author: mjskay
###############################################################################



# aesthetics --------------------------------------------------------------

#' Provides documentation of aesthetics for spikes
#' @noRd
rd_spike_aesthetics = function(
  geom_name = "spike",
  stat = NULL,
  vignette = "slabinterval"
) {

  out = glue_doc('
    @section Aesthetics:
    The spike `geom` has a wide variety of aesthetics that control
    the appearance of its two sub-geometries: the **spike** and the **point**.

    ')

  out = c(out, rd_aesthetics_sections(geom_name, stat, vignette = vignette))

  glue_collapse(out, "\n")
}
