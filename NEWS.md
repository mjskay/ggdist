# tidybayes 2.0.1

* Various geoms and stats have been merged together under the `geom_slabinterval()` and `stat_slabinterval()`
  "meta-geom" (#84). This has enabled a bunch of new geoms to be created (see `vignette("slabinterval")` 
  and fixed a number of outstanding issues:
  * Histogram geoms and histogram+interval geoms (#162)
  * CCDF bar charts and gradient plots
  * The alpha aesthetic can now be mapped on eye plots (and all related geoms) (#163)
  * Vertical version of eye plot (and vertical/horizontal variants of all slabinterval variants) (#56)
  * Intervals and densities are now correctly grouped in eye plots (e.g. when dodging) (#83)
  * Fill and color aesthetics can now be mapped within the slab part of eyes (and all slabintervals), allowing 
    gradients to be made easily (#136) and regions of practical equivalence (ROPEs) to be annotated easily.
    Examples of ROPEs have been added to the main vignettes (#129).
  * Intervals and eyes support `position = "dodge"` correctly (#180)
  * The new geoms (and replacements for old ones) have custom scales allowing fine-grained targeting of fill, 
    color, and size aesthetics of all the component parts of the composite geoms.
  * There is a new sub-family of auto-sizing Wilkinson dotplot stats and geoms, `geom_dots()` and `geom_dotsinterval()` 
    (#210). These include a `quantiles` parameter on the stats to make it easy to create quantile dotplots.
* Analytical distributions can be visualized using the new `stat_dist_...` family of geoms for both
  the `geom_slabinterval()` family and `geom_lineribbon()` (see `stat_dist_slabinterval()` and `stat_dist_lineribbon()`).
* The new `parse_dist()`, which parses distribution specifications (like `normal(0,1)`) into tidy columns, can
  be combined with the `stat_dist_...` family of geoms to easily to visualize priors (e.g. from `brms`).
* New distribution functions for the marginal LKJ distribution (`dlkjcorr_marginal()` and company), combined
  with `parse_dist()` and the `stat_dist_...` family make it easy to visualize the marginal LKJ prior on 
  a cell in a correlation matrix. (#191 #192)
* There is a new vignette on frequentist uncertainty visualization, `vignette("freq-uncertainty-vis")`,
  also made possible by the new `stat_dist_...` family of geoms (#188)
* `tidy_draws()` can now be applied to already-tidied data frames, allowing dependent functions (like `spread_draws()`
  and `gather_draws()`) to also be applied to data frames directly (#82). This can be a useful optimization in workflows 
  where the initial tidying is slow but spreading/gathering is fast (see discussion in #144)
* Kruschke-style distribution-of-distribution plots are now easier to construct with `stat_dist_slabh()`.
  An example of this usage is in `vignette("tidy-brms")`.
* `hdi()` now uses trimmed densities by default to avoid odd behavior with bounded distributions (#165).
* `compare_levels(comparison = )` now uses a modern tidy approach to dealing with unevaluated expressions,
  so `rlang::exprs()` can be used in place of `plyr::.()` (#174, #175)
* `geom_lineribbon()` now works with `ggnewscale` (#178)
* `fitted_draws()`/`predicted_draws()` give more helpful error messages on unsupported models (#177)


# tidybayes 1.1.0

New features and documentation:

* Support matrices, n-d arrays, and lists of vectors in compose_data (#159)
* Support nested vectors, matrices, n-d arrays, and ragged arrays through x[.,.] syntax in gather/spread_draws (#154)
* Add detached-line-ribbon HOPs example for ordinal models in brms vignette

Bug fixes:

* Fixed errors on CRAN from changes in brms
* Properly handle Dirichlet responses in predicted_draws (#164)


# tidybayes 1.0.4

New features and documentation:

* Initial support for add_residual_draws, towards #133
* Add tidybayes-residuals vignette
* Add add_draws to support models that add_[fitted|predicted]_draws does not (closes #149) 
* Add sample_draws to make it easier to take fewer draws anywhere in the pipeline (towards #144)
* Add hypothetical outcome plots (HOPs) to examples

Minor changes:

* Fixed errors on CRAN from changes in dplyr
* Fix bug to support multivariate models in `predicted_draws()`, closes #134
* Add support for `emm_list` in `gather_emmeans_draws()`, closes #126
* Default for show.legend no longer omits all guides
* Make default `geom_lineribbon()` color black, closes #153


# tidybayes 1.0.3

* Added `gather_pairs` method for creating custom scatterplot matrices (and more!) 
* Ordinal models in brms now use original category labels (#122)
* `NA` values are now better supported in `point_interval`, and it has an `na.rm` argument (#123)
* Added sampler diagnostics to tidy_draws() Stan output (#109)
* Added MCMCglmm+emmeans example to vignettes
* Add guards to prevent usage of packages listed in `Suggests`


# tidybayes 1.0.0

Major changes:

* First CRAN release.
* Various function, argument, and column name changes towards unification
with the Stan ecosystem. See help("tidybayes-deprecated") for more information.
