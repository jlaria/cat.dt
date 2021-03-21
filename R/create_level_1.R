#' Level 1 CAT decision tree generator
#'
#' Generates a list of nodes lists for the first level of the CAT decision tree
#'
#' @param bank matrix of the item bank. Rows represent items, and columns
#' represent parameters. If the model is \code{"GRM"}, the first column
#' represents the \code{alpha} parameters and the next columns represent the
#' \code{beta} parameters. If the model is \code{"NRM"}, odd columns represent
#' the \code{alpha} parameters and even columns represent \code{beta}
#' parameters
#' @param crit item selection criterion. Options: "MEPV" for Minimum
#' Expected Posterior Variance and "MFI" for Maximum Fisher Information
#' @param dens_vec vector of the a priori density function values of the
#' evaluated ability levels
#' @param C vector of item capacities
#' @param nres vector of number of possible responses for every item
#' @param prob_array 3-D array of probability responses. Dim 1 represent items,
#' dim 2 represent evaluated ability levels and dim 3 represent possible
#' responses
#' @return A list of lists. Each of these lists represent a node of the first
#' level of the decision tree
#' @author Javier Rodr?guez-Cuadrado
#'
#' @export
create_level_1 = function(bank, crit, dens_vec, C, nres, prob_array) {

  #Create the matrix of "associated values" for the linear programming solver
  switch(crit,
         MEPV = {

           E = create_E_MEPV(bank, dens_vec, nres, prob_array, C)
           minmax = F

         },
         MFI = {

           E = create_E_MFI(bank, estimate(dens_vec)[[1]], nres, C)
           minmax = T

         }
  )

  #Calculate the item exposure in the nodes
  X = item_selector(E, 1, C, minmax)

  #Select those with non-zero exposure (the items selected for the first level
  #nodes)
  item_sel = which(X != 0)

  #Initialise the list of node lists
  nodes = list()

  #Fill the node lists
  for (i in 1:length(item_sel)) {

    est = estimate(dens_vec) #Calculate the estimation and the SE
    
    nodes[[i]] = create_node(10000+i, dens_vec, item_sel[i], c(),
                             est[[1]],
                             est[[2]],
                             data.frame(matrix(nrow = 0, ncol = 3)),
                             X[item_sel[i]], E[item_sel[i]])

    colnames(nodes[[i]]$ID_sons) = c("ID_son", "Response", "Probability")

  }

  return(nodes) #Return the list of node lists

}
