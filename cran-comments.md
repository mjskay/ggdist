## Submission comments
This is a new submission as part of splitting tidybayes (already on CRAN) into two parts.
The tidybayes package has grown into two related, large pieces of functionality: (1) functions
for visualizing distributions and uncertainty, and (2) functions for manipulating posteriors
from Bayesian models. Because the visualization functions (category 1) can also be applied to
non-Bayesian models (and have become a large-ish API unto themselves), I created this new
package (ggdist) to contain all of those functions. 

If/when ggdist hits CRAN, I will submit a new version of tidybayes (already prepared) that
depends on ggdist for the visualization functions so there is only one implementation of those
functions across the two packages. Tidybayes will re-export the visualization functions from
ggdist so that existing code that depends on tidybayes is not affected.

## Test environments
* Windows 10 (local), R-release 4.0.0
* MacOS (Github), R-release 4.0.0
* MacOS (Github), R-devel 2020-06-04 r78644
* Linux (Github), R-release 4.0.0
* Solaris (rhub), R-release 4.0.0
* Windows (win-builder), R-release 4.0.0

## R CMD check results
0 errors | 0 warnings | 1 note

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Matthew Kay <mjskay@umich.edu>'
  
  New submission

## Downstream dependencies
As it is a new package, there are no downstream dependencies for this package.
