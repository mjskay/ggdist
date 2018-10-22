## Resubmission
This is a resubmission addressing adding `svglite` as a dependency in `Suggests`.

## Submission comments
This submisison is primarily to address this message from Brian Ripley:

> According to §1.1.3.1 of 'Writing R Extensions', packages in Suggests 
> should be used conditionally.  After a StanHeaders update, rstanarm can 
> only be installed on platforms using C++11 or later: this is not the 
> default in R 3.5.x and only the default on some platforms in R-devel.
>
> Please check for yourselves and correct ASAP and before Oct 22 to safely 
> retain the package on CRAN.

I have adjusted the usage of rstanarm (and a few other packages with the
same problem) to only be used conditionally.

A few other minor changes have also been made since the previous release.

## Test environments
* Windows 10 (local), R-release 3.5.1
* Windows 10 (local), R-devel 3.6.0 2018-10-19 r75470
* Windows (win-builder), R-release 3.5.1
* Windows (win-builder), R-devel
* Ubuntu 16.04.4 LTS (local), R-release 3.5.1

## R CMD check results
0 errors | 0 warnings | 0 notes

## Downstream dependencies
There are no downstream dependencies for this package.
