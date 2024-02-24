## Submission comments

This release addresses some minor issues introduced by the recent ggplot2 3.5.0 release. It also has some other unrelated (mostly minor) changes and bugfixes, see NEWS.md.

## Test environments

* MacOS (Github), R-release
* Windows (local), R-release
* Windows (Github), R-release
* Windows (win-builder), R-devel
* Windows (win-builder), R-release
* Windows (win-builder), R-oldrel
* Linux (Github), R-devel
* Linux (Github), R-release
* Linux (Github), R-oldrel
* Linux (Github), R 4.1
* Linux (Github), R 4.0
* Linux (Github), R 3.6

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 28 reverse dependencies (26 from CRAN + 2 from Bioconductor), comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 1 new problems
 * We failed to check 0 packages

Issues with CRAN packages are summarised below.

### New problems

* fabletools currently fails in one example and two tests due to a change in some internal ggdist data structures (related to colour ramps) that fabletools made use of. The details of those data structures have been changed in this release (causing the error), but this change was done in order to solidify and improve ggdist's public-facing API for colour ramps. As part of that change, the new `ggdist::ramp_colours()` function is now exported that fabletools can use. A PR has been submitted to fabletools to make this change: https://github.com/tidyverts/fabletools/pull/397
