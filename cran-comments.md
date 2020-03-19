## Submission comments
This is a minor submission to fix forward compatibility issues with 
R 4 (stringsAsFactors and [[<- changes) and with dplyr 1.0.0.

## Test environments
* Linux (travis), R-release 3.6.2
* Windows 10 (local), R-release 3.6.2
* Windows 10 (local), R-devel 2020-03-16 r77936
* Windows (win-builder), R-release 3.6.3
* Windows (win-builder), R-devel

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
There are two downstream dependencies for this package: trialr and mcp.
Tests for both pass with the latest version of tidybayes.
