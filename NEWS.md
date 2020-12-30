# ggdist 2.4.0

New features:

* add `pdf` and `cdf` computed variables for the `stat_sample_slabinterval()` subfamily. See
  new examples of usage in the last section of `vignette("slabinterval")`. (#11)
* add `cut_cdf_qi()` for creating (amongst other things) interval-filled halfeyes, in the 
  style of `bayesplot::mcmc_areas()` (#11)
* add `fill_ramp` and `color_ramp` scales to `geom_slabinterval()` and `geom_lineribbon()` families,
  making it easier to separate group colors from interval/density/CDF colors. See new examples in
  `vignette("slabinterval")`, `vignette("lineribbon")`, and `vignette("freq-uncertainty-vis")`. (#16)
* auto-detect finite limits on analytical distributions, so (e.g.) distributions like
  the beta will not cut off tails close to 0 or 1. (#18)
* add `brms::brmsprior` implementation for `parse_dist()` (#34)

New documentation:

* `vignette("freq-uncertainty-vis")` now uses `distributional::dist_student_t()` (#14)
* add references to *fuzzygrams* in `vignette("slabinterval")` (#23)
* add examples with separate positioning of slab and interval (#27)
* add discussion of limitations of curvewise intervals to `vignette("lineribbon")` (#22)
* soft-deprecate usage of `interval_size_range` argument in docs (#35)
* initial versions of some cheat sheets

Bug fixes:

* add limited `na.rm` support to `curve_interval()` (#22)
* use analytical instead of numerical derivatives on scale transformations where possible, improving reliability.

# ggdist 2.3.0

New features and documentation:

* Add `curve_interval()` for generating curvewise (joint) intervals for curve boxplots (#22)
* Add `vignette("lineribbon")` describing `geom_lineribbon()`, `stat_lineribbon()`,
  `stat_dist_lineribbon()`, and `curve_interval()`.

Bug fixes:

* Support `dist` aesthetics that are factors (#25)
* Fix slab drawing order for overlapping (ggridges-style) slabs (#30)
* Workaround for changes to {distributional} distribution functions until #31 is fixed.

# ggdist 2.2.0

* Support for [distributional](https://pkg.mitchelloharawild.com/distributional/), including new
  examples in `vignette("slabinterval")` (#14).
* `stat_dist_...` geoms now calculate `pdf` and `cdf` columns to allow mashup geoms that involve both 
  functions, such as Correll-style gradient plots combined with violins, as in Helske *et al.* (#11).
* `stat_dist_...` geoms should now work with `gganimate` (#15).
* Examples updated to fix errors introduced by `broom::augment()` defaulting to `se_fit = FALSE`.

# ggdist 2.1.1

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
