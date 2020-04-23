#' CAT decision tree
#'
#' Generates a \code{cat.dt} object containing the CAT decision tree.
#' This object has all the necessary information to build the tree.
#'
#' @param bank \code{data.frame} or \code{matrix} of the item bank.
#' Rows represent items, and columns
#' represent parameters. If the model is \code{"GRM"}, the first column
#' represents the \code{alpha} parameters and the next columns represent the
#' \code{beta} parameters. If the model is \code{"NRM"}, odd columns represent
#' the \code{alpha} parameters and even columns represent \code{beta}
#' parameters
#' @param model polytomous IRT model. Options: \code{"GRM"} for Graded Response
#' Model and \code{"NRM"} for Nominal Response Model
#' @param crit item selection criterion. Options: "MEPV" for Minimum
#' Expected Posterior Variance and "MFI" for Maximum Fisher Information
#' @param C vector of maximum item exposures. If it is an integer, this value
#' is replicated for every item
#' @param stop vector of two components that represent the decision tree
#' stopping criterion. The firs component represents the maximum level of the
#' decision tree, and the second represents the minimum estimated ability level
#' precision
#' @param limit maximum number of level nodes
#' @param inters minimum common area between density functions in the nodes of
#' the evaluated pair in order to join them
#' @param p a-priori probability that controls the tolerance to join similar nodes
#' @param dens density function (e.g. dnorm, dunif, etc.)
#' @param ... parameters of the density function
#' @return An object of class \code{cat.dt}
#' @author Javier Rodríguez-Cuadrado
#'
#' @examples
#' \dontrun{
#' data("itemBank")
#' # Build the cat.dt
#' nodes = CAT_DT(bank = itemBank, model = "GRM", crit = "MEPV",
#'                C = 0.3, stop = 6, limit = 200, inters = 0.98,
#'                p = 0.9, dens = dnorm, 0, 1)
#'
#' # Estimate the ability level of a subject with responses res
#' CAT_ability_est(nodes, res = itemRes[1, ])
#' # or
#' nodes$predict(res = itemRes[1, ])
#' # or
#' predict(nodes, itemRes[1, ])
#' }
#' @export
CAT_DT = function(bank, model = "GRM", crit = "MEPV", C = 0.3,
                  stop = 6, limit = 200,
                  inters = 0.98, p = 0.9,
                  dens, ...) {

  #Check limit
  if (limit > 10000) {
    message("Too large value for limit. limit = 10000 is set.\n")
    limit = 10000
  }
  #Turn the data frame into a matrix
  bank = as.matrix(bank)

  #Calculate the number of item responses for every item depending on the
  #IRT model and allocate the corresponding Fisher Information function
  switch(model,
         GRM = {nres = apply(!apply(bank, 2, is.na), 1, sum)
         Fisher_Inf = Fisher_GRM},
         NRM = {nres = apply(!apply(bank, 2, is.na), 1, sum)/2+1
         Fisher_Inf = Fisher_NRM})

  #Calculate density function values
  dens_vec = ability_density(dens, ...)

  #Create multidimensional array of probability responses. Dim 1 represent
  #items, dim 2 represent evaluated ability levels and dim 3 represent possible
  #responses
  prob_array = create_prob_array(model, bank, nres)


  #Calculate the minimum distance between estimated ability levels to join two
  #nodes
  tol = (theta[which.min(abs(cumsum(dens_vec)*st-(1+p)/2))]-
           theta[which.min(abs(cumsum(dens_vec)*st-(1-p)/2))])/limit

  #Turn C into a vector if it is an integer
  if (length(C) == 1) C = rep(C, nrow(bank))

  #Store C in another variable to keep its original value
  C_org = C

  #Create the first level
  nodes = create_level_1(bank, crit, dens_vec, C, nres, prob_array)

  #Update item capacities
  num_lev = length(nodes) #Number of current level nodes

  for (i in 1:num_lev) { #Update
    C[nodes[[i]]$item] = C[nodes[[i]]$item]-nodes[[i]]$D
  }

  C[C<1e-10] = 0 #Compromise solution

  #Create the following levels
  level = 2 #Level number to create
  nodes = list(nodes,list()) #Add the (empty) next level list

  while (level <= stop[1]) {
    # Check if there are items available
    if(sum(C)==0) stop("Available capacity of items reached zero before completing the tree. Please, decrease the number of levels or increase the number of items in bank. Current level: ", level-1)

    nodes[[level]] = list() #Create the (empty) next level list
    nodes[[level]] = create_levels(nodes[[level-1]], bank, crit, C, nres,
                                   level, prob_array, limit, tol, inters)
                                   #Fill the list

    #Update item capacities
    num_lev = length(nodes[[level]]) #Number of current level nodes

    #Allocate sons
    nodes[[level-1]] = allocate_sons(nodes[[level-1]], nodes[[level]], level)

    #Remove unnecessary information and update capacities
    for (i in 1:num_lev) {

      nodes[[level]][[i]][c(9, 10, 11)] = NULL

      C[nodes[[level]][[i]]$item] = C[nodes[[level]][[i]]$item]-
        nodes[[level]][[i]]$D

    }

    C[C<1e-10] = 0 #Compromise solution

    level = level+1 #Update level number to create

  }

  #Create last level
  nodes[[stop[1]+1]] = list()
  nodes[[stop[1]+1]] = create_last_level(nodes[[stop[1]]], nres, level,
                                         prob_array)

  #Allocate sons
  nodes[[stop[1]]] = allocate_sons(nodes[[stop[1]]], nodes[[stop[1]+1]],
                                   stop[1]+1)

  #Remove unnecessary information
  for (i in 1:length(nodes[[stop[1]]])) {
    nodes[[level]][[i]][c(9, 10, 11)] = NULL
  }

  #rm(Fisher_Inf, envir = .GlobalEnv)

  cat.dt = list(
    nodes = nodes,
    model = model,
    crit = crit,
    bank = bank,
    C = C_org,
    C_left = C,
    stop = stop,
    limit = limit,
    inters = inters,
    dens = dens,
    predict = NA)
  cat.dt$predict = function(res){
    CAT_ability_est(cat.dt, res)
  }
  cat.dt$predict_group = function(res) {
    CAT_ability_est_group(cat.dt, res)
  }

  class(cat.dt) = "cat.dt"

  return(cat.dt) #Return the list of level lists

}

