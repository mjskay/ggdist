## Submission comments
This is a resubmission to bring the total package size under 5 megabytes.

The submission also addresses an error in tests on CRAN due to changes in brms.

In addition, a number of new features have been added, including a new "meta-geom", geom_slabinterval(),
that underlies a variety of geometries designed for visualizing distributions. See NEWS.md.

## Test environments
* Linux (travis), R-release 3.6.2
* Windows 10 (local), R-release 3.6.2
* Windows 10 (local), R-devel 2020-01-24 r77710
* Windows (win-builder), R-release 3.6.2

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
There are two downstream dependencies for this package: trialr and mcp.
Tests for both pass with the latest version of tidybayes.
