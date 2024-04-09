#' Extract effect decomposition table for cmest() models
#'
#' This function takes a cmest() object as an input and returns the
#' extracted values of OR, standard error, 95% confidence
#' intervals (95% CIs), and P value for the effect decomposition.
#'
#' @param cmest_obj An object given by cmest() function
#' @return A table of extracted model values
#'
#' # Extract effect decomposition table from cmest() models
#' tab_2 = extract_decompo(example_obj)
#' @export
extract_decompo = function(cmest_obj) {
  
  sum_obj = summary(cmest_obj)
  # Extract values from the summary
  paramter = names(sum_obj$effect.pe)
  estimate = sum_obj$effect.pe
  std.err = sum_obj$effect.se
  ci.low = sum_obj$effect.ci.low
  ci.high = sum_obj$effect.ci.high
  p.val = sum_obj$effect.pval
  # Make a dataframe
  effect_tbl = as.data.frame(cbind(paramter, estimate, std.err, ci.low, ci.high, p.val))
  rownames(effect_tbl) = NULL
  colnames(effect_tbl) = c('Parameter', 'OR.Estimate', 'Std.error', '95%_CIL', '95%_CIU', 'P.val')
  
  return(effect_tbl)
}
