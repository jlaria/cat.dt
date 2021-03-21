#' Vector of a posteriori density values of ability level
#'
#' Computes the a posteriori density function values of the evaluated ability
#' levels given the item response
#'
#' @param apriori a vector of a priori density function values of the evaluated
#' ability levels
#' @param prob a vector of probability response for every evaluated ability
#' level given the item response
#' @return A vector of a posteriori density values
#' @author Javier Rodriguez-Cuadrado
#' @export
a_posteriori = function(apriori, prob) {

  prod = apriori*prob #product of a priori and probability values for every
  #evaluated ability level

  return(prod/(st*sum(prod))) #Return the a posteriori using Bayes' theorem

}
