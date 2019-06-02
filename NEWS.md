# tidybayes 1.1.0

New features and documentation:

* Support matrices, n-d arrays, and lists of vectors in compose_data (#159)
* Support nested vectors, matrices, n-d arrays, and ragged arrays through x[.,.] syntax in gather/spread_draws (#154)
* Add detached-line-ribbon HOPs example for ordinal models in brms vignette

Bug fixes:

* Fixed errors on CRAN from changes in brms
* Properly handle dirichlet responses in predicted_draws (#164)


# tidybayes 1.0.4

New features and documentation:

* Initial support for add_residual_draws, towards #133
* Add tidybayes-residuals vignette
* Add add_draws to support models that add_[fitted|predicted]_draws does not (closes #149) 
* Add sample_draws to make it easier to take fewer draws anywhere in the pipeline (towards #144)
* Add hypothetical outcome plots (HOPs) to examples

Minor changes:

* Fixed errors on CRAN from changes in dplyr
* Fix bug to support multivariate models in predicted_draws, closes #134
* Add support for emm_list in gather_emmeans_draws, closes #126
* Default for show.legend no longer omits all guides
* Make default lineribbon color black, closes #153


# tidybayes 1.0.3

* Added `gather_pairs` method for creating custom scatterplot matrices (and more!) 
* Ordinal models in brms now use original category labels (#122)
* `NA` values are now better supported in `point_interval`, and it has an na.rm argument (#123)
* Added sampler diagnostics to tidy_draws() Stan output (#109)
* Added MCMCglmm+emmeans example to vignettes
* Add guards to prevent usage of packages listed in `Suggests`


# tidybayes 1.0.0

Major changes:

* First CRAN release.
* Various function, argument, and column name changes towards unification
with the Stan ecosystem. See help("tidybayes-deprecated") for more information.
