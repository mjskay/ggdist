# ggdist 2.0.3.9000

* Initial split from tidybayes
* automatic orientation determination in geoms
* `geom_interval()` and `geom_pointinterval()` no longer automatically set the `ymin` and `ymax` aesthetics in order
  to work better with automatic orientation detection. `geom_intervalh()` and `geom_pointintervalh()` still do,
  since they are deprecated anyway (so supporting the old behavior is fine in these functions).
