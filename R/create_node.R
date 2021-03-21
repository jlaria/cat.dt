#' Node creator
#'
#' Generates a list that represents a specific node of the CAT decision tree
#'
#' @param ID integer that represents the specified node identification in the
#' form of \code{10000*level+position}.
#' @param dens_vec vector of the density function values in the specified node
#' of the evaluated ability levels
#' @param item integer that represents the item of the specified node
#' @param item_prev vector of items of the previous nodes
#' @param est estimated ability level in the specified node
#' @param SE standard error of the estimated ability level
#' @param ID_sons data frame containing the information of the sons of the
#' specified node. Rows represent sons and columns represent the ID of the son,
#' the response given to the item of the specified node that leaded to the son
#' and the probability of reaching the son given that response (not equal to
#' one if the son had previously splitted)
#' @param D confluency of the specified node
#' @param as_val associated value of the specified node. It can be the MSE if
#' the selection criterium is "MEPV" and the FI if the selection criterium is
#' "MFI" 
#' @return A list that represents a node of the decision tree
#' @author Javier Rodr?guez-Cuadrado
#'
#' @export
create_node = function(ID, dens_vec, item, item_prev, est, SE, ID_sons, D, as_val) {

  #Create and return the list
  return(list(ID = ID, dens_vec = dens_vec, item = item, item_prev = item_prev,
              est = est, SE = SE, ID_sons = ID_sons, D = D, as_val = as_val))

}
