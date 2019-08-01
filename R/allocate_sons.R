#' Allocate sons in the CAT decision tree
#'
#' Fills the information of the sons of the previous level nodes
#'
#' @param nodes_prev list of node lists of the nodes from the previous level
#' @param nodes list of node lists of the nodes from the current level
#' @param level level of the CAT decision tree
#' @return A list of node lists updated with the information of the sons
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
allocate_sons = function(nodes_prev, nodes, level) {

  for (i in 1:length(nodes)) { #For every son

    for (j in 1:length(nodes[[i]][[9]])) { #For every father

      indx = nodes[[i]][[9]][j]-(level-1)*10000 #Father index

      nodes_prev[[indx]]$ID_sons[nrow(nodes_prev[[indx]]$ID_sons)+1, ] =
        c(nodes[[i]]$ID, nodes[[i]][[10]][j],
          nodes[[i]][[11]][j]) #Add the information of the son

      nodes_prev[[indx]]$dens_vec = NULL #Eliminate non-necessary density
      #information in the previous nodes

    }

  }

  return(nodes_prev) #Return the list of updated node lists

}
