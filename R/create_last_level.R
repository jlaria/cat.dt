#' CAT decision tree last level generator
#'
#' Generates a list of node lists for the last level of the CAT decision tree
#'
#' @param nodes_prev list of node lists of the nodes from the previous level
#' @param nres vector of number of possible responses for every item
#' @param level last-level number (equals the length of the test plus one)
#' @param prob_array 3-D array of probability responses. Dim 1 represent items,
#' dim 2 represent evaluated ability levels and dim 3 represent possible
#' responses
#' @return A list of lists. Each of these lists represent a node of the
#' last level of the decision tree
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
create_last_level = function(nodes_prev, nres, level, prob_array) {

  #Initialise level nodes
  nodes = list()

  indx = 0 #Auxiliary variable

  #Add known information to the level nodes
  for (i in 1:length(nodes_prev)) {

    for (j in 1:nres[nodes_prev[[i]]$item]) {

      indx = indx+1 #Update auxiliary variable

      it = nodes_prev[[i]]$item #Item of the father node

      #A posteriori density function values calculus
      apos = a_posteriori(nodes_prev[[i]]$dens_vec, prob_array[it, , j])

      #Add information
      nodes[[indx]] = create_node(level*10000+indx, apos, NA,
                                  c(nodes_prev[[i]]$item,
                                    nodes_prev[[i]]$item_prev),
                                  estimate(apos), NA, NA, NA)

      #Add the ID of the father node and the response to that node that leaded
      #to the current node
      nodes[[indx]][[9]] = nodes_prev[[i]]$ID
      nodes[[indx]][[10]] = j
      nodes[[indx]][[11]] = 1

    }

  }

  return(nodes) #Return the list of node lists

}
