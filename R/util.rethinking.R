# Utility functions for models from the "rethinking" package (e.g. map and map2stan models)
#
# Author: mjskay
###############################################################################

#return the parameter name for the first link in the given model (or
#the only one, if there is only one)
first_link_name = function(fit) UseMethod("first_link_name", fit)
first_link_name.map = function(fit) fit@links[[1]][[1]]
first_link_name.map2stan = function(fit) fit@formula_parsed$lm[[1]]$parameter

#return the expression for the first y (response) variable/expression as a quoted AST
first_y_expr = function(fit) fit@formula[[1]][[2]]
