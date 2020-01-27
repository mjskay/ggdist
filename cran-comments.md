## Submission comments
This submission addresses an error in tests on CRAN due to changes in brms.

In addition, a number of new features have been added, including a new "meta-geom", geom_slabinterval(),
that underlies a variety of geometries designed for visualizing distributions. See NEWS.md.

## Test environments
* Linux (travis), R-release 3.6.2
* Windows 10 (local), R-release 3.6.2
* Windows 10 (local), R-devel 2020-01-24 r77710
* Windows (win-builder), R-release 3.6.2

## R CMD check results
0 errors | 0 warnings | 2 notes:

> checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Matthew Kay <mjskay@umich.edu>'
  
  Size of tarball: 6211495 bytes

> checking installed package size ... NOTE
    installed size is  5.8Mb
    sub-directories of 1Mb or more:
      doc   4.8Mb
      
Per the instructions in Writing CRAN Extensions, the doc folder is less than 5MB.
The documentation size is a result of the fact that tidybayes vignettes 
comprehensively cover the geoms in the package to aid users in understanding
the possible combinations of options available. This is difficult to demonstrate
without providing visual examples.

## Downstream dependencies
There are two downstream dependencies for this package: trialr and mcp.
Tests for both pass with the latest version of tidybayes.
