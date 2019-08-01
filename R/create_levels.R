#' CAT decision tree level generator
#'
#' Generates a list of node lists for a specific level of the CAT decision tree
#'
#' @param nodes_prev list of node lists of the nodes from the previous level
#' @param bank matrix of the item bank. Rows represent items, and columns
#' represent parameters. If the model is \code{"GRM"}, the first column
#' represents the \code{alpha} parameters and the next columns represent the
#' \code{beta} parameters. If the model is \code{"NRM"}, odd columns represent
#' the \code{alpha} parameters and even columns represent \code{beta}
#' parameters
#' @param crit item selection criterion. Options: "MEPV" for Minimum
#' Expected Posterior Variance and "MFI" for Maximum Fisher Information
#' @param C vector of item capacities
#' @param nres vector of number of possible responses for every item
#' @param level level number
#' @param prob_array 3-D array of probability responses. Dim 1 represent items,
#' dim 2 represent evaluated ability levels and dim 3 represent possible
#' responses
#' @param limit maximum number of level nodes
#' @param tol maximum distance between estimated ability levels in the nodes
#' of the evaluated pair in order to consider whether to join them
#' @param inters minimum common area between density functions in the nodes of
#' the evaluated pair in order to join them
#' @return A list of lists. Each of these lists represent a node of the
#' specified level of the decision tree
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
create_levels = function(nodes_prev, bank, crit, C, nres, level, prob_array,
                         limit, tol, inters) {

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
                                  estimate(apos),
                                  data.frame(matrix(nrow = 0, ncol = 3)),
                                  st*sum(nodes_prev[[i]]$dens_vec*
                                           prob_array[it, , j])*
                                    nodes_prev[[i]]$D, NA)

      colnames(nodes[[indx]]$ID_sons) = c("ID_son", "Response", "Probability")

      #Add the ID of the father node and the response to that node that leaded
      #to the current node
      nodes[[indx]][[9]] = nodes_prev[[i]]$ID
      nodes[[indx]][[10]] = j

    }

  }

  #Check if there are possible joints
  nodes = join_node(nodes, level, limit, tol, inters)

  #Create the matrix of "associated values" for the linear programming solver
  E_mat = matrix(rep(0,nrow(bank)*length(nodes)), nrow(bank))
  D = c() #Initialisation of node confluencies

  switch(crit,
         MEPV = {

           for (i in 1:length(nodes)) {

             #Create the matrix column by column
             E_mat[, i] = create_E_MEPV(bank, nodes[[i]]$dens_vec, nres,
                                  prob_array, C)

             E_mat[nodes[[i]]$item_prev, i] = 1e6 #High value for those items of
             #the previous nodes that are consequently not available

             D[i] = nodes[[i]]$D #Node confluency

           }

           minmax = F #Optimisation direction

         },
         MFI = {

           for (i in 1:length(nodes)) {

             #Create the matrix column by column
             E_mat[, i] = create_E_MFI(bank, nodes[[i]]$est, nres, C)

             E_mat[nodes[[i]]$item_prev, i] = 0 #Zero value for those items of
             #the previous nodes that are consequently not available

             D[i] = nodes[[i]]$D #Node confluency

           }

           minmax = T #Optimisation direction

         }
  )

  #Calculate the item exposure in the nodes
  X = item_selector(E_mat, D, C, minmax)



  #Add the information left to the level nodes
  indx = ncol(X) #Auxiliary variable

  for (i in 1:ncol(X)) { #For every level node

    #Selected items for the node
    item_sel = which(X[, i] > 1e-14)

    #Complete node information
    nodes[[i]]$item = item_sel[1]
    nodes[[i]]$as_val = E_mat[item_sel[1], i]
    nodes[[i]]$D = X[item_sel[1], i]
    nodes[[i]][[11]] = rep(X[item_sel[1], i]/sum(X[item_sel, i]),
                           length(nodes[[i]][[10]]))

    #If the node splits (more than one item with positive exposure)
    if (length(item_sel) > 1) {

      for (j in 2:length(item_sel)) { #For the "extra" nodes due to spliting

        #Add node information
        indx = indx+1
        nodes[[indx]] = nodes[[i]]
        nodes[[indx]]$ID = 10000*level+indx
        nodes[[indx]]$item = item_sel[j]
        nodes[[indx]]$as_val = E_mat[item_sel[j], i]
        nodes[[indx]]$D = X[item_sel[j], i]
        nodes[[indx]][[11]] = rep(X[item_sel[j], i]/sum(X[item_sel, i]),
                                  length(nodes[[indx]][[10]]))

      }

    }

  }

  return(nodes) #Return the list of node lists

}
