## Resubmission
This is a resubmission. In this version I have:

* Added `rmarkdown` as a dependency in 'Suggests' 

## Submission comments
This is the first submission to CRAN of the package `tidybayes`.
I have been developing and maintaining `tidybayes` for a couple
of years; it has been available on Github for awhile 
(https://github.com/mjskay/tidybayes) and gained some users there. 

With the goal of making the API more stable long-term (and thus
the package more suitable for CRAN) I have recently solidified 
the API into a more consistent, well-tested package. This included 
renaming of functions and arguments aimed at better-aligning the
API with terminology from the Stan ecosystem; see this thread:
http://discourse.mc-stan.org/t/unifying-names-of-output-columns-in-bayesplot-tidybayes-etc/4577

As a result of this reorganization (combined with the fact that the package
already has existing users), even though this is its first submission
to CRAN, the package contains a number of deprecated function
aliases along with a package startup message notifying users of 
the changes. I hope this is a reasonable approach.

## Test environments
* Windows 10 (local), R-release 3.5.1
* Windows 10 (local), R-devel 3.6.0 2018-07-30 r75016
* Ubuntu 16.04.4 LTS (local), R-release 3.5.1
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.5.0

## R CMD check results
There were no ERRORS or WARNINGs. There was one NOTE:

> NOTE
> Maintainer: 'Matthew Kay <mjskay@umich.edu>'
> 
> New submission

## Downstream dependencies
There are no downstream dependencies for this package.
