# On package load
#
# Author: mjskay
###############################################################################

.onAttach = function(libname, pkgname) {
  packageStartupMessage (
    "NOTE: As of tidybayes version 1.0, several functions, arguments, and output column names\n",
    "      have undergone significant name changes in order to adopt a unified naming scheme.\n",
    "      See help('tidybayes-deprecated') for more information.\n"
  )
}
