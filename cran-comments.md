## Submission comments
This is a minor submission to fix issues with tibble 3.0.0.

## Test environments
* Windows 10 (local), R-release 3.6.3
* Linux (travis), R-release 3.6.2
* Windows 10 (local), R-devel 2020-03-16 r77936
* Windows (win-builder), R-release 3.6.3
* Windows (win-builder), R-devel

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
There are two downstream dependencies for this package. Both 
have fewer errors with the new version of tidybayes.

- mcp:
  - on CRAN:     1 error , 0 warnings, 1 note  (28 failed tests)
  - new version: 1 error , 0 warnings, 0 notes ( 4 failed tests)

- trialr:
  - on CRAN:     1 error , 1 warning , 3 notes ( 3 failed tests)
  - new version: 0 errors, 0 warnings, 3 notes ( 0 failed tests)
