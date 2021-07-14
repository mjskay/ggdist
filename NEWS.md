# ggdist 3.0.0

Breaking changes:

* The positioning of `geom_slabinterval()` family geoms when using `position_dodge()`
  is now slightly different in order to match up with how other geoms are positioned (#85).
  This may slightly change existing charts that use `position = "dodge"`, and in
  some cases may cause slabs to be drawn slightly outside plot boundaries, but makes
  it much easier to combine `geom_slabinterval()` with other geoms in the expected
  way. If dodging more similar to the old approach is needed, use the new
  "justification-preserving dodge", `position_dodgejust()`, in place of `position_dodge()`.

New features:

* For `geom_slabinterval()`, `side`, `justification`, and `scale` can now be
  used as aesthetics instead of parameters, allowing them to vary across slabs
  within the same geom.
* Varying `fill`s within a slab  in `geom_slabinterval()` can now be drawn as
  true gradients rather than segmented polygons in R >= 4.1 by setting
  `fill_type = "gradient"`. This substantially improves the appearance of 
  gradient fills in graphics engines that support it (#44).
* Improved support for discrete distributions:
  * `stat_dist_slabinterval()` and company now detect discrete distributions and
    display them as histograms (#19).
  * `geom_dotsinterval()` now adjusts bin widths on discrete distributions when
    they would result in bins that are taller than the allocated space to ensure
    that they fit within the required space (#42).
* Allow user-specified lower and/or upper bounds on dynamic `geom_dotsinterval()`
  bin width by passing a vector of two values to the `binwidth` parameter.
* The automatic bin selection algorithm used by `geom_dotsinterval()` has been
  factored out and exported as `find_dotplot_binwidth()` and `bin_dots()` for
  others to use (#77).
* Previously, `curve_interval()` used a common (but naive) approach to finding
  a cutoff on data depth to identify the X% "deepest" curves, simply taking the
  envelope around the X% quantile of curves ranked by depth. This is quite
  conservative and tends to create intervals that are too wide; `curve_interval()`
  now searches for a cutoff in data depth such that X% of curves are contained
  within its envelope (#67).
* `point_interval()` and company now accept `distributional` objects and 
  `posterior::rvar()`s (full support for `distributional` objects requires
  `distributional` > 0.2.2).
* Reduce dependencies substantially, making the geoms more suitable for use by
  other packages (thanks to Brenton Wiernik for the help).

New documentation:

* Substantial improvements to the documentation of aesthetics and computed
  variables in `geom_slabinterval()`, `stat_slabinterval()`, and company, listing
  all custom aesthetics, computed variables, and their usage.

* Several new examples in `vignette("slabinterval")`, including "rain cloud" 
  plots and an example of histograms for discrete analytical distributions.

Bug fixes:

* Ensure `stat_dist_slabinterval()` preserves group order (#88).
* Improve test coverage up to ~96%.
* Restore computed variable `n` for `stat_sample_slabinterval()`.
* Various improvements in correct `NA` handling across the geoms (#74, #51).


# ggdist 2.4.1

New features:

* Added `"weave"` and `"swarm"` layouts for dots geoms (#64). These provide 
  alternative layouts that keep datapoints in their actual positions on the
  data axis. The `"weave"` layout maintains rows but not columns and works well 
  for quantile dotplots; the `"swarm"` layout uses the `"compactswarm"` method from 
  `beeswarm::beeswarm()` (courtesy James Trimble) and works well on sample data.
  See the dotplot section of `vignette("slabinterval")` for comparisons.
* Allow the use of `unit()` to specify bin widths manually for dots geoms and stats,
  which can be helpful when you need dotplots across facets to have the same bin width
  (#53).

New documentation:

* Add example of lineribbon gradients using `fill_ramp` in `vignette("lineribbon")`.
* Add example of Tukey-like pencils in `vignette("slabinterval")`.
* Add example of two slab used together (densities and dotplots to make "rain clouds")
  in `vignette("slabinterval")`.

Bug fixes:

* Fix issues with *ggplot2* 3.3.4 (#72) and *vdiffr* 1.0.
* Handle interactions between alpha and fill/color properly when not set by user (#62).
* Use step function for all ECDFs, which should also fix constant CDFs (#55).
* Move *fda* to suggests as it brings in a large number of dependencies and is rarely used.
* Use trimmed density for mode estimation (#57).

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
