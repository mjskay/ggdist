# ggdist 2.1.0

* Initial split from tidybayes: ggdist now contains all stats/geoms from tidybayes (except deprecated ones),
  all support functions for stats/geoms (such as `point_interval()`), `vignette("slabinterval")`, and
  `vignette("freq-uncertainty-vis")`. Tidybayes will retain all other functions, and will re-export all
  `ggdist` functions for now.
* All stats and geoms now support automatic orientation determination. Thus, all `h`-suffix geoms are now
  deprecated. Those geoms have been left in `tidybayes` and give a deprecation warning when used; they
  cannot be used from `ggdist` directly.
* `geom_interval()`, `geom_pointinterval()`, and `geom_lineribbon()` no longer automatically set the 
  `ymin` and `ymax` aesthetics if `.lower` or `.upper` are present in the data. This allows them to work
  better with automatic orientation detection (and was a bad feature to have existed in the first place
  anyway). The deprecated `tidybayes::geom_intervalh()` and `tidybayes::geom_pointintervalh()` still
  automatically set those aesthetics, since they are deprecated anyway (so supporting the old behavior
  is fine in these functions).
* `geom_lineribbon()`/`stat_lineribbon()` now supports a `step` argument for creating stepped lineribbons.
  H/T to Solomon Kurz for the suggestion.
* `ggdist` now has its own implementation of the scaled and shifted Student's t distribution (`dstudent_t()`,
  `qstudent_t()`, etc), since it is very useful for visualizing confidence distributions.
