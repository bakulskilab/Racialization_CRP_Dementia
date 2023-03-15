#' Extract effect decomposition evalue table for cmsens models
#'
#' This function takes a cmsens() object as an input and returns the
#' extracted values of estRR, lowerRR, upperRR, E.value.estRR, Evalue.lowerRR, Evalue.upperRR.
#'
#' @param cmsens_obj An object given by cmsens() function
#' @return A table of extracted model values
#'
#' # Extract effect decomposition table from cmsens() models
#' tab_2 = extract_decompo(example_obj)
#' @export
extract_evalue = function(cmsens_obj) {
  
  sum_obj = print(cmsens_obj)
  # Make a dataframe
  effect_tbl = as.data.frame(sum_obj)
  rownames(effect_tbl) = NULL
  colnames(effect_tbl) = c('estRR', 'lowerRR', 'upperRR', 'Evalue.estRR', 'Evalue.lowerRR', 'Evalue.upperRR')
  return(effect_tbl)
}

