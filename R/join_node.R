#' Node joiner
#'
#' Given all the nodes from one level, \code{join_node} evaluates all possible
#' pairs one by one and decides whether or not to join them based on the
#' similarity between the estimated ability levels and the density functions.
#' If a pair of nodes is joined, the density function of the resulting node is
#' the mean of the density functions of the joined nodes and the confluencies
#' are summed.
#'
#' @param nodes list of node lists. Every node list must contain the ID of the
#' node, the vector of density function values of the evaluated abiity levels,
#' the vector of previous items, the estimated ability level and the node
#' confluency
#' @param level level of the CAT decision tree
#' @param limit maximum number of level nodes
#' @param inters minimum common area between density functions in the nodes of
#' the evaluated pair in order to join them
#' @param tol minimum distance between estimated ability levels to join two nodes
#' @return A list of node lists. This list is the input list updated with the
#' joined nodes
#' @author Javier Rodr?guez-Cuadrado
#'
#' @export
join_node = function(nodes, level, limit, tol, inters) {

  elim = c() #Vector storing the indexes of the nodes to eliminate due to the
  #joint

  #Evaluate all possible pairs
  for (i in 1:(length(nodes)-1)) {

    for (j in (i+1):length(nodes)) {

      #If the distance between estimated ability levels is below the maximum
      if (abs(nodes[[i]]$est-nodes[[j]]$est) < tol) {

        #If there are more nodes than allowed
        if (length(nodes) > limit) {

          #Join the nodes
          nodes[[j]]$dens_vec = nodes[[j]]$D/(nodes[[j]]$D+nodes[[i]]$D)*
                                   nodes[[j]]$dens_vec+
                                nodes[[i]]$D/(nodes[[j]]$D+nodes[[i]]$D)*
                                      nodes[[i]]$dens_vec
          nodes[[j]]$item_prev = c(nodes[[j]]$item_prev,
                                     nodes[[i]]$item_prev)
          
          est = estimate(nodes[[j]]$dens_vec)
          
          nodes[[j]]$est = est[[1]]
          nodes[[j]]$SE = est[[2]]
          nodes[[j]]$D = nodes[[j]]$D+nodes[[i]]$D
          nodes[[j]][[10]] = c(nodes[[j]][[10]], nodes[[i]][[10]])
          nodes[[j]][[11]] = c(nodes[[j]][[11]], nodes[[i]][[11]])

          #Store the nodes to eliminate
          elim = c(elim, i)

          #Stop evaluating the i node
          break()

        }

        #If the number of nodes is not over the limit
        else {

          #Common area of density functions
          area = st*sum(apply(cbind(nodes[[j]]$dens_vec,
                                          nodes[[i]]$dens_vec), 1, min))

          #If this area is over the minimum
          if (area > inters) {

            #Join the nodes
            nodes[[j]]$dens_vec = nodes[[j]]$D/(nodes[[j]]$D+nodes[[i]]$D)*
                                     nodes[[j]]$dens_vec+
                                  nodes[[i]]$D/(nodes[[j]]$D+nodes[[i]]$D)*
                                     nodes[[i]]$dens_vec
            nodes[[j]]$item_prev = c(nodes[[j]]$item_prev,
                                       nodes[[i]]$item_prev)
            
            est = estimate(nodes[[j]]$dens_vec)
            
            nodes[[j]]$est = est[[1]]
            nodes[[j]]$SE = est[[2]]
            nodes[[j]]$D = nodes[[j]]$D+nodes[[i]]$D
            nodes[[j]][[10]] = c(nodes[[j]][[10]], nodes[[i]][[10]])
            nodes[[j]][[11]] = c(nodes[[j]][[11]], nodes[[i]][[11]])

            #Store the nodes to eliminate
            elim = c(elim, i)

            #Stop evaluating the i node
            break()

          }

        }

      }

    }

  }

  #Eliminate one of the nodes from every joined pair
  nodes[elim] = NULL

  #Re-allocate IDs
  for (i in 1:length(nodes)) nodes[[i]]$ID = 10000*level+i

  return(nodes) #Return the updated list

}
