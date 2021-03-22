#' Ability level estimation for an individual using a CAT decision tree
#'
#' Computes the test taker's estimated ability level based on the CAT decision
#' tree previously built and the test taker's responses to every item at every
#' tree level
#'
#' @param cat.dt A \code{cat.dt} object returned by \code{\link{CAT_DT}}.
#' @param res Vector containing the test taker's responses to every item
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
#' \code{$graphics} Plot object of the evolution of the ability level
#' estimation. It shows the ability level estimation after the individual
#' has answered to every administered item.
#'
#' @author Javier Rodr?guez-Cuadrado
#'
#' @examples
#' data("itemBank")
#' # Build the cat.dt
#' nodes = CAT_DT(bank = itemBank, model = "GRM", crit = "MEPV",
#'                C = 0.3, stop = c(6,0), limit = 200, inters = 0.98,
#'                p = 0.9, dens = dnorm, 0, 1)
#'
#' # Estimate the ability level of a subject with responses res
#' estimation = CAT_ability_est(nodes, res = itemRes[1, ])
#'
#' #plot the estimations
#' plot(estimation$graphics)
#'
#' @export
#' 
#' @import ggplot2
#' 
CAT_ability_est = function(cat.dt, res) {

  if (is.matrix(res)) {
    print("The input must be a vector. For group evaluation use the 'CAT_ability_est_group' function")
    return()
  }

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

  items = c() #Items of the selected node at every level
  i = 1; #Initialize counter
  
  while (!is.na(tree[[i]][[node_sel]]$item)) {
    
    items = c(items, tree[[i]][[node_sel]]$item)
    
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
    
    #If the next item has already been administered, exit the loop
    if(!is.na(tree[[i+1]][[node_sel]]$item)) {
      if(sum(as.numeric(items == tree[[i+1]][[node_sel]]$item))>0) break
    }
    
    i = i+1;
    
  }
  
  levels = i; #update levels

  Dist = cumsum(tree[[levels]][[node_sel]]$dens_vec)*st #Distribution function
  #values for the final selected node

  llow = theta[which.min(abs(Dist-0.025))] #Lower limit of a 95% credible
  #interval

  lupp = theta[which.min(abs(Dist-0.975))] #Upper limit of a 95% credible
  #interval

  #Generate a graphic of estimation vs. number of administered items

  graphics = ggplot()+
    ggtitle("Evolution of the ability level estimation")+
    aes(x = 1:(levels-1), y = estimation)+
    geom_point(size = 2, aes(colour = as.factor(res[items])))+
    geom_line(linetype = "dashed")+
     xlab("Number of administered items")+
     ylab("Ability level estimation")+
     geom_text(aes(label = paste("item ", items)),
                       vjust = -1)+
     theme(plot.title =  element_text(hjust = 0.5),
      panel.grid.minor =  element_line(size = , linetype = 'solid',
                                               colour = "skyblue1"),
      panel.grid.major =  element_line(size = , linetype = 'solid',
                                               colour = "skyblue1"))+
     scale_x_continuous(minor_breaks = seq(1, 10, 1),
                                expand = c(.1, .1),
                                breaks = 1:(levels-1))+
     scale_y_continuous(expand = c(.05, .05))+

     labs(colour = "Response")

  #Return the estimation, the 95% confidence interval and the items
  return(list(estimation = estimation, llow = llow,
              lupp = lupp, items = items, graphics = graphics))

}



#' Predict S3 method for \code{cat.dt}
#'
#' @param object A \code{cat.dt} object returned by \code{\link{CAT_DT}}.
#' @param res Vector containing the test taker's responses to every item
#' @param ... Not used
#' 
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
#' \code{$graphics} Plot object of the evolution of the ability level
#' estimation. It shows the ability level estimation after the individual
#' has answered to every administered item.
#'
#' @author Javier Rodr?guez-Cuadrado
#' @export
#' 
predict.cat.dt <- function(object, res, ...){
  if(is.vector(res)){
    return(CAT_ability_est(object, res))
  }else{
    return(CAT_ability_est_group(object, res))
  }
}