#' Ability level estimation using a CAT decision tree
#'
#' Computes the test taker's estimated ability level based on the CAT decision
#' tree previously built and the test taker's responses to every item at every
#' tree level
#'
#' @param cat.dt A \code{cat.dt} object returned by \code{\link{CAT_DT}}.
#' @param res vector containing the test taker's responses to every item
#' @return A list containing the following elements:
#'
#' \code{$estimation} Estimated ability level after each level of the tree.
#'
#' \code{$linf} Lower limit of the final estimation at 95%
#'
#' \code{$lsup} Upper limit of the final estimation at 95%
#'
#' \code{$items} Administered item in each level.
#'
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
CAT_ability_est = function(cat.dt, res) {

  tree = cat.dt$nodes

  #For the first level
  num_lev_1 = length(tree[[1]]) #Number of nodes in the first level

  estimation = c() #Initialise the estimated ability level vector

  pr = c() #Initialise probabilities of picking every node in the first level

  for (i in 1:num_lev_1) { #Compute the probabilities based on confluencies
    pr[i] = tree[[1]][[i]]$D
  }

  node_sel = sample(1:num_lev_1, 1, prob = pr) #Finally selected node

  #For every level
  levels = length(tree) #Number of levels

  items = rep(0, levels-1) #Items of the selected node at every level

  for (i in 1:(levels-1)) {

    items[i] = tree[[i]][[node_sel]]$item

    r = res[items[i]] #Test taker's repsonse to the item of the selected node

    son = which(tree[[i]][[node_sel]]$ID_sons[, "Response"] == r) #The son(s)
    #of the selected node given the test taker's response

    if (length(son) > 1) { #If there is more than one son
      son = sample(son, 1, prob =
                     tree[[i]][[node_sel]]$ID_sons[son, "Probability"]) #Pick just
      #one son based on their probabilities
    }

    node_sel = tree[[i]][[node_sel]]$ID_sons[son, "ID_son"]-10000*(i+1) #The
    #node that corresponds to the son

    estimation[i] = tree[[i+1]][[node_sel]]$est #Estimation at every level

  }

  Dist = cumsum(tree[[levels]][[node_sel]]$dens_vec)*st #Distribution function
  #values for the final selected node

  linf = theta[which.min(abs(Dist-0.025))] #Lower limit of a 95% confidence
  #interval

  lsup = theta[which.min(abs(Dist-0.975))] #Upper limit of a 95% confidence
  #interval

  #Return the estimation, the 95% confidence interval and the items
  return(list(estimation = estimation, linf = linf,
              lsup = lsup, items = items))

}
