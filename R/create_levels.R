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
#' @param SE minimum standard error of the ability level
#' @return A list of lists and a scalar. Each of the lists represent a node of
#' the specified level of the decision tree, and the scalar represents if the
#' created level is the last (1) or not (0) due to the SE stopping criterion
#' @author Javier Rodr?guez-Cuadrado
#'
#' @export
create_levels = function(nodes_prev, bank, crit, C, nres, level, prob_array,
                         limit, tol, inters, SE) {

  #Initialise level nodes
  nodes = list()

  indx = 0 #Auxiliary variable (counter)
  not_join = c(); #Auxiliary variable (final nodes due to SE to not join)

  #Add known information to the level nodes
  for (i in 1:length(nodes_prev)) {
    
    #If the SE stopping criterion is not satisfied
    if (nodes_prev[[i]]$SE > SE) {
      
      for (j in 1:nres[nodes_prev[[i]]$item]) {
        
        indx = indx+1 #Update auxiliary variable
        
        it = nodes_prev[[i]]$item #Item of the father node
        
        #A posteriori density function values calculus
        apos = a_posteriori(nodes_prev[[i]]$dens_vec, prob_array[it, , j])
        
        #Add information
        est = estimate(apos) #Calculate the estimation and the SE
        
        nodes[[indx]] = create_node(level*10000+indx, apos, NA,
                                    c(nodes_prev[[i]]$item,
                                      nodes_prev[[i]]$item_prev),
                                    est[[1]],
                                    est[[2]],
                                    data.frame(matrix(nrow = 0, ncol = 3)),
                                    st*sum(nodes_prev[[i]]$dens_vec*
                                             prob_array[it, , j])*
                                      nodes_prev[[i]]$D, NA)
        
        colnames(nodes[[indx]]$ID_sons) = c("ID_son", "Response", "Probability")
        
        #Add the ID of the father node and the response to that node that leaded
        #to the current node
        nodes[[indx]][[10]] = nodes_prev[[i]]$ID
        nodes[[indx]][[11]] = j
        nodes[[indx]][[12]] = 1
        
        #If the SE stopping criterion is accomplished, these nodes must not be
        #joined
        if (est[[2]] < SE) {
          
          not_join = c(not_join, indx)
          
        }
        
      }
      
    }
    
  }

  #If there are nodes that are final nodes due to SE stopping criterion
  if (length(not_join) > 0)  {
    
    nodes_join = nodes[-not_join]
    nodes_not_join = nodes[not_join]
    
  } 
  else
  {
    
    nodes_join = nodes
    nodes_not_join = c()
    
  }
    
  
  #Check if there are possible joints
  if (length(nodes_join) > 1) {
    
    nodes_join = join_node(nodes_join, level, limit, tol, inters)
    
  }
  
  #Re-allocate IDs for final nodes
  if (length(not_join) > 0)  {
    
    for (i in 1:length(nodes_not_join)) {
      
      nodes_not_join[[i]]$ID = level*10000+length(nodes_join)+i
      
    }
    
  }
  
  #If all the nodes achieve the SE stopping criterion
  if (length(nodes_join) == 0) {
    return(list(nodes_not_join, 1)) 
  }

  #Create the matrix of "associated values" for the linear programming solver
  E_mat = matrix(rep(0,nrow(bank)*length(nodes_join)), nrow(bank))
  D = c() #Initialisation of node confluencies

  switch(crit,
         MEPV = {

           for (i in 1:length(nodes_join)) {

             #Create the matrix column by column
             E_mat[, i] = create_E_MEPV(bank, nodes_join[[i]]$dens_vec, nres,
                                  prob_array, C)

             E_mat[nodes_join[[i]]$item_prev, i] = 1e6 #High value for those items of
             #the previous nodes that are consequently not available

             D[i] = nodes_join[[i]]$D #Node confluency

           }

           minmax = F #Optimisation direction

         },
         MFI = {

           for (i in 1:length(nodes_join)) {

             #Create the matrix column by column
             E_mat[, i] = create_E_MFI(bank, nodes_join[[i]]$est, nres, C)

             E_mat[nodes_join[[i]]$item_prev, i] = 0 #Zero value for those items of
             #the previous nodes that are consequently not available

             D[i] = nodes_join[[i]]$D #Node confluency

           }

           minmax = T #Optimisation direction

         }
  )

  #Calculate the item exposure in the nodes
  X = item_selector(E_mat, D, C, minmax)



  #Add the information left to the level nodes
  indx = 0
  nodes_join_split = c()

  for (i in 1:ncol(X)) { #For every level node

    #Selected items for the node
    item_sel = which(X[, i] > 1e-14)

    #Complete node information
    nodes_join[[i]]$item = item_sel[1]
    nodes_join[[i]]$as_val = E_mat[item_sel[1], i]
    nodes_join[[i]]$D = X[item_sel[1], i]
    nodes_join[[i]][[12]] = rep(X[item_sel[1], i]/sum(X[item_sel, i]),
                           length(nodes_join[[i]][[11]]))

    #If the node splits (more than one item with positive exposure)
    if (length(item_sel) > 1) {

      for (j in 2:length(item_sel)) { #For the "extra" nodes due to spliting

        #Add node information
        indx = indx+1
        nodes_join_split[[indx]] = nodes_join[[i]]
        nodes_join_split[[indx]]$ID = 10000*level+length(nodes_not_join)+
                                                    length(nodes_join)+indx
        nodes_join_split[[indx]]$item = item_sel[j]
        nodes_join_split[[indx]]$as_val = E_mat[item_sel[j], i]
        nodes_join_split[[indx]]$D = X[item_sel[j], i]
        nodes_join_split[[indx]][[12]] = rep(X[item_sel[j], i]/sum(X[item_sel, i]),
                                  length(nodes_join_split[[indx]][[11]]))

      }

    }

  }
  
  #If every nod achieves the SE stopping criterion, this is the last level
  last_level = 0 #Initialize
  if (length(nodes_join) == 0) last_level = 1

  #Return the list of node lists and the last level information
  return(list(c(nodes_join, nodes_not_join, nodes_join_split), last_level)) 
  

}
