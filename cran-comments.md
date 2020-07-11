## Submission comments
This submission fixes a WARNING on CRAN tests due to recent 
changes in broom. It also introduces a few minor new features
and bugfixes (see NEWS.md).

## Test environments
* Windows 10 (local), R-release 4.0.2

* MacOS (Github), R-release 4.0.0
* MacOS (Github), R-devel 2020-06-04 r78644
* Linux (Github), R-release 4.0.0
* Solaris (rhub), R-release 4.0.0
* Windows (win-builder), R-release 4.0.0

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
There are two downstream dependencies for this package. Both 
have as many or fewer errors with the new version of ggdist.

* tidybayes:
  CRAN:        0 errors | 0 warnings | 1 note
  New version: 0 errors | 0 warnings | 1 note

* multinma:
  CRAN:        0 errors | 0 warnings | 1 note
  New version: 0 errors | 0 warnings | 1 note
