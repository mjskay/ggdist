# tidybayes 1.0.3

* Added `gather_pairs` method for creating custom scatterplot matrices (and more!) 
* Ordinal models in brms now use original category labels (#122)
* `NA` values are now better supported in `point_interval`, and it has an na.rm argument (#123)
* Added sampler diagnostics to tidy_draws() stan output (#109)
* Added MCMCglmm+emmeans example to vignettes
* Add guards to prevent usage of packages listed in `Suggests`


# tidybayes 1.0.0

Major changes:

* First CRAN release.
* Various function, argument, and column name changes towards unification
with the Stan ecosystem. See help("tidybayes-deprecated") for more information.
