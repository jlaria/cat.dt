#' Ability level estimation for a group using a CAT decision tree
#'
#' Computes the test takers' estimated ability level based on the CAT decision
#' tree previously built and the test takers' responses to every item at every
#' tree level
#'
#' @param cat.dt A \code{cat.dt} object returned by \code{\link{CAT_DT}}.
#' @param res Matrix containing the test takers' responses to every item. Rows
#' represent each individual and columns represent the responses given to each
#' item
#' @return A list of lists containing the following elements for each
#' individual:
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
#'                C = 0.3, stop = 6, limit = 200, inters = 0.98,
#'                p = 0.9, dens = dnorm, 0, 1)
#'
#' # Estimate the ability level of a subject with responses res
#' CAT_ability_est_group(nodes, res = itemRes)
#'
#' @export
CAT_ability_est_group = function(cat.dt, res) {

  set.seed(0) #Set the seed when a node has to be randomly chosen
  
  group_est = list() #Initialization of the vector of lists

  for (i in 1:nrow(res)) {

    #Call to CAT_ability_est for every individual of the group
    group_est[[i]] = CAT_ability_est(cat.dt, res[i, ])

  }

  return(group_est)

}
