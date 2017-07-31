# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put here; Names used in specific files should
# be put at the top of the corresponding file.
#
# Author: mjskay
###############################################################################

# names used in dlpyr functions
globalVariables(c(".", ".data"))
