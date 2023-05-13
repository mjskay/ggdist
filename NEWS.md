# ggdist 3.3.0

Breaking changes: The following changes, mostly due to new default density 
estimators, may cause some plots on sample data to change. Changes should usually
be small, and generally should result in more accurate density estimation. Revert
to the old behavior by setting `density = density_unbounded(bandwidth = "nrd0")`.

* `stat_slabinterval()` now uses `density_bounded()` as its default density
  estimator, which uses a bounded density estimator that also estimates the 
  bounds of the data. The default bandwidth estimator is also now `bandwidth_dpi()`,
  which is the Sheather-Jones direct plug-in estimator (the same as
  `stats::bw.SJ(..., method = "dpi")`). These changes may cause existing charts
  using densities to change; usually only slightly. These changes should be worth
  it, as they should drastically improve the accuracy of density estimates, 
  especially on bounded data, and should have little noticeable impact on densities 
  on unbounded data.
* `density_bounded()` now estimates bounds from the data when not provided
  (i.e. when one of `bounds` is `NA`). See the `bounder_` functions (e.g.
  `bounder_cdf()`, `bounder_cooke()`) for more on bounds estimation.
* Improved `Mode()` and `hdi()` estimators based on bounded density estimator.

New features and enhancements:

* Improved `hdci()` estimator using quantile estimation.
* Histograms are now implemented using `density_histogram()`, a histogram
  density estimator. Finer-grained control of bin positions is now possible
  using the `breaks` argument (including the new `breaks_fixed()` for manually-specified
  bin widths) and the `align` argument (including the new `align_boundary()` and
  `align_center()` for choosing how to align bin positions to reference points). (#118)
* New `geom_spike()` and `stat_spike()` for adding spike annotations to slabs
  created with `geom_slabinterval()` or `stat_slabinterval()`. See example
  in `vignette("slabinterval")`. (#58, #124)
* `parse_dist()` now outputs *distributional* objects in a `.dist_obj` column in
  addition to the name-plus-arguments (`.dist`+`.args`) format, and these objects respect truncation
  parameters from prior specifications. This makes it easier to  visualize standard
  deviation priors, for example, giving a better solution to #20.
* `marginalize_lkjcorr()` adjusts the `.dist_obj` column output by `parse_dist()`
  in addition to the `.dist` and `.args` columns.
* `geom_lineribbon()` now obeys the `order` aesthetic, allowing you to arbitrarily
  set the draw order of ribbons (#171). Enabled by this change, `stat_lineribbon()` 
  now sets `order = after_stat(level)` by default, making its draw order more correct
  by ensuring all ribbons of the same level are drawn together.
* Some improved error messages using `cli`.
* *Very* experimental adaptive KDE is available through the `adapt` parameter; 
  note that it is unsupported and both the implementation and interface are 
  highly likely to change.

Deprecations:

* The `slab_type` parameter for `stat_slabinterval()` is now deprecated in favor
  of mapping the corresponding computed variable (`pdf` or `cdf`) onto the desired
  aesthetic. For `slab_type = "histogram"`, use the `pdf` computed variable 
  combined with the new `density_histogram()` density estimator (e.g. set
  `density = "histogram"`). (#165)

Bug fixes:

* Ensure scale transformations work even when no slab is present; e.g. in
  `stat_interval()`. (#168)
* Ensure `curve_interval()` works with `posterior::rvar`s. (#158)
* `geom_lineribbon()` draw order is now correct even when some portions of a 
  ribbon has `NA` widths. (#171)
* Improve the appearance of logical fill conditions at bin edges on histograms. (#175)


# ggdist 3.2.1

New features and enhancements:

* Support for non-numeric distributions in `stat_slabinterval()` and 
  `stat_dotsinterval()`, including `dist_categorical()`, `dist_bernoulli()`,
  and the upcoming `posterior::rvar_factor()` type. (#108)
* Various improvements to dotplot layout in `geom_dotsinterval()`:
  * new `layout = "hex"` allows a hexagonal circle-packing style layout (#161).
  * new mechanism for smoothing dotplots using the `smooth` parameter, including 
    `smooth = "bounded"` / `smooth = "unbounded"` (for "density dotplots") and 
    `smooth = "discrete"` / `smooth = "bar"` (for improved layout of large-n 
    discrete distributions). (#161)
  * a better bin/dot-nudging algorithm using constrained optimization (#163)
  * new `overlaps = "keep"` option disables bin/dot nudging in `"bin"`, `"hex"`,
    and `"weave"` layouts. This means `layout = "weave"` with `overlaps = "keep"`
    will yield exact dot positions. (#161)
  * The `"weave"` layout now works properly with `side = "both"`
  * fixed binning artifacts when there is high density on the edges, particularly
    right edges (#144)
  * use a max `binwidth` of 1 for discrete distributions (#159)
  * new `overflow = "compress"` allows layouts to be compressed to fit into the
    geom bounds if a user-specified `binwidth` would otherwise cause the dots
    to exceed the geom bounds. (#162)
* Two new shortcut geoms for `geom_dotsinterval()`: `geom_swarm()` and `geom_weave()`.
  Both can be used to quickly create "beeswarm"-like plots.
* A new "mirrored" scale for the `side` aesthetic, `scale_side_mirrored()`, makes it
  easier to create mirrored slabs and dotplots. (#142)
* Custom density estimators can now be used with `stat_slabinterval()` via the
  `density` argument, including a new bounded density estimator (`density_bounded()`).
  (#113)
* Following the split between `size` and `linewidth` aesthetics in *ggplot2* 3.4,
  the following aesthetics have been updated (#138):
  * `interval_size` is now `linewidth`
  * `slab_size` is now `slab_linewidth`
  * in `geom_slab()`, `geom_dots()`, and `geom_lineribbon()`, `size` is now `linewidth`
* A new **experimental** mini domain-specific language for probability expressions
  in *ggdist* `stat`s: the `Pr_()` and `p_()` functions can be used to generate
  `after_stat()` expressions in terms of *ggdist* computed variables; e.g.
  `aes(thickness = !!Pr_(X <= x))` maps the CDF of the distribution onto the 
  `thickness` aesthetic; `aes(thickness = !!p_(x))` maps the PDF onto the
  `thickness` aesthetic. (#160)
* Several function families in *ggdist* now use "currying" (automatic partial
  function application). These function families partially apply themselves until all
  non-optional arguments have been supplied: `point_interval()`, `smooth_...`,
  and `density_...`. See `help("automatic-partial-functions")`.
* Performance improvements for `point_interval()` on grouped data frames. (#154)

Documentation:

* Uses of `stat()` have been replaced with `after_stat()` to be consistent with
  the deprecation of `stat()` in *ggplot2* 3.4.


# ggdist 3.2.0

New features and enhancements:

* Several computed variables in `stat_slabinterval()` can now be shared across
  sub-geometries:
  * The `.width` and `level` computed variables can now be used in slab / dots
    sub-geometries. These values correspond to the smallest interval computed
    in the interval sub-geometry containing that portion of the slab. This
    gives a more flexible alternative to using `cut_cdf_qi()` to create densities
    filled according to a set of intervals (this approach which also works on
    highest-density intervals, which `cut_cdf_qi()` does not). Examples in
    `vignette("slabinterval")` have been updated to use the new approach, and
    an example has been added to `vignette("dotsinterval")` showing how to 
    color dots by intervals.
  * As an experimental feature (currently a bit fragile) enabled via
    `options(ggdist.experimental.slab_data_in_intervals = TRUE)`,
    the `pdf` and `cdf` computed variables can now be used in interval 
    sub-geometries to get the PDF and CDF at the point summary. `pdf_min`,
    `pdf_max`, `cdf_min`, and `cdf_max` also give the PDF and CDF at the lower
    and upper ends of the interval. An example in `vignette("lineribbon")`
    shows how to use this to make lineribbon gradients whose color approximates
    density (as opposed to the classic gradient fan chart examples already
    in that vignette, where color approximates the CDF).
* `scale_thickness_shared()` is now provided to allow the thickness scale to be
  shared across geometries, making certain plot types easier to create
  (e.g. plots of prior and posterior densities together). See
  `vignette("slabinterval")` for an example.
* If `thickness` is less than 0 it is normalized to have a minimum of zero when
  normalization is turned on; this makes it easier to use slab functions that
  go below zero. A new example in `vignette("slabinterval")` shows how to use
  this to create [raindrop plots](https://doi.org/10.1198/0003130032369). 
* The stacking order of dots within bins for `geom_dotsinterval(layout = "bin")`
  can now be set using the `order` aesthetic. This makes it possible to create 
  "stacked" dotplots by mapping a discrete variable onto the `order` aesthetic
  (#132). As part of this change, `bin_dots()` now maintains the original data
  order within bins when `layout = "bin"`. See an example in 
  `vignette("dotsinterval")`.
* A new `verbose = TRUE` flag in `geom_dotsinterval()` outputs the selected
  `binwidth` in both data units and normalized parent coordinates. This may be
  useful if you want to start with an automatically-selected bin width and then 
  adjust it manually. Though note: if you just want to scale the selected
  bin width to fit within a desired area, it is probably better to use `scale`, 
  and if you want to provide constraints on the bin width, you can pass a 
  2-vector to `binwidth`.
* The `expand` argument in `stat_slabinterval()` can now take a length-two logical 
  vector to control expansion to the lower and upper limits respectively (#129).
  Thanks to @teunbrand.
* `geom_dotsinterval()` now supports the `family` aesthetic for setting the font
  used to display its dots (based on a conversation with @gdbassett).
* Experimental `guide_rampbar()` for creating gradient-like legends for
  continuous color/fill ramp scales, based on `ggplot2::guide_colorbar()`.
  See an example in `vignette("lineribbon")`.

Bug fixes:

* If there are `NA`s in the `thickness` aesthetic of a slab, these are now
  rendered as gaps in the slab (#129).
* Fixed the check for empty x/y scales to avoid extending the scale to cover 0/1
  when plotting distributional objects whose bulk lies outside that region
  (when there is nothing else on the plot).


# ggdist 3.1.1

Bug fixes:

* If a string is supplied to the `point_interval` argument of `stat_slabinterval()`,
  a function with that name will be searched for in the calling environment and
  the `ggdist` package environment. The latter ensures that `stat`s work when
  ggdist is loaded but not attached to the search path (#128).


# ggdist 3.1.0

New features and enhancements:

* The `stat_sample_...` and `stat_dist_...` families of stats have been merged (#83).
  * All `stat_dist_...` stats are deprecated in favor of their `stat_...` counterparts,
    which now understand the `dist`, `args`, and `arg1`...`arg9` aesthetics.
  * `xdist` and `ydist` can now be used in place of the `dist` aesthetic to
    specify the axis one is mapping a distribution onto (`dist` may be
    deprecated in the future).
  * Passing dist-like objects to the `x` or `y` aesthetics now raise a helpful
    error message suggesting you probably want to use `xdist` or `ydist`.
  * Restructured internals for stats and geoms makes it much easier to maintain
    shortcut geoms and stats, eliminating a large amount of code duplication (#106).
  * New `expand` parameter to `stat_slabinterval()` allows explicitly setting 
    whether or not the slab is expanded to the limits of the scale (rather than
    implicitly setting this based on `slab_type`).
* The `point_interval()` family of functions can now be passed `distributional`
  and `posterior::rvar()` objects, meaning that means and modes (in addition
  to medians) and highest-density intervals (in addition to quantile intervals)
  can now be visualized for analytical distributions.
  * As part of this, multivariate distribution objects and `rvar`s will generate
    a `.index` column when passed to `point_interval()` functions (#111).
    Based on a suggestion from @mitchelloharawild.
* New `stat_ribbon()` provided as a shortcut stat for `stat_lineribbon()` with
  no line (#48). Also, if you supply only an `x` or `y` aesthetic to
  `geom_lineribbon()`, you will get ribbons without a line (#127).
* One-sided intervals (i.e. quantiles) can now be calculated using `ul()` (upper
  limit) or `ll()` (lower limit), e.g. with `point_interval()` explicitly or
  via `mean_ll()`, `median_ll()`, `mode_ll()`, `mean_ul()`, `median_ul()`, 
  or `mode_ul()` (#49).
* Constant distributions are now reliably detected in a variety of situations
  and rendered as point masses in both density plots and histograms (#103, #32).
* Minor improvements and changes to dotplot layouts that may result in minor
  changes to the appearance of existing dotplots:
  * Minor improvements to automatic bin width selection; the maximum
    dot stack height should be closer to or equal to `scale` more often.
  * A formerly-internal fudge factor of `1.07` for dot sizes is now exposed as
    the default value of the `dotsize` parameter instead of being applied
    internally. This fudge factor tends (in my opinion) to make dotplots look a
    bit better due to the visual distance between circles, but is (I think) 
    better used as an explicit value than an implicit one, hence the change.
    This may create subtle changes to plots that use the `dotsize` or `stackratio` 
    parameters, but allows those parameters to have a more precise 
    geometric interpretation.

Documentation:

* New vignette for the `stat_dotsinterval()` sub-family: `vignette("dotsinterval")` (#120).
* Vastly improved and expanded documentation for the `stat_slabinterval()` and
  `geom_slabinterval()` family: each shortcut stat/geom now has its own documentation
  page that comprehensively lists all parameters, aesthetics, and computed variables,
  including those pulled in via `...` from typically-paired geoms. These docs are
  auto-generated and should be easy to maintain going forward. (#36)
* The `stat_lineribbon()` and `geom_lineribbon()` family now also has separate
  documentation pages with a comprehensive listing of aesthetics and parameters (#107).
* Ridge plot-like example in `vignette("slabinterval")` using the new `expand`
  parameter (#115).
  
Deprecations and removals:

* The `.prob` argument, a long-deprecated alias for `.width`, was removed.
* The `limits_function`, `limits_args`, `slab_function`, `slab_args`, `interval_function`,
  and `interval_args` arguments to `stat_slabinterval()` were removed: these were
  largely internal-use parameters only needed by subclasses of the base class for
  creating shortcut stats, yet added a lot of noise to the documentation, so these
  were replaced with the `$compute_limits()`, `$compute_slabs()`, and 
  `$compute_intervals()` methods on the new `AbstractStatSlabinterval` 
  internal base class.
  
Bug fixes:

* Improved handling of `NA`s for analytical distributions.
* Fixed bug where within-bin order of dots in dotplots for `"bin"` and `"weave"`
  layouts could be incorrect with aesthetics mapped at a sub-bin level.
* `stackratio`s that are not equal to `1` are now accounted for in 
  `find_dotplot_binwidth()` (i.e. automatic dotplot bin width selection).
* Ensure distinct fill colors in lineribbons are still treated as distinct for
  grouping even if the `fill_ramp` aesthetic ramps them to the same color.


# ggdist 3.0.1

Bug fixes:

* Forward-compatibility fixes for `distributional` >= 0.2.2.9000 (#91).
* Allow densities for samples of size 1 in `stat_sample_slabinterval()` (#98).
* Avoid NOTE about missing `linearGradient()` function on R < 4.1.
* Do not draw legend components for inactive sub-geoms in `geom_slabinterval()`.


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
