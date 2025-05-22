
is_rhs_only <- function(formula) {
  return(length(as.character(formula)) == 2)
}

make_rhs_only <- function(formula) {
  return(as.formula(paste0("~", as.character(formula)[3]), env = globalenv()))
}
