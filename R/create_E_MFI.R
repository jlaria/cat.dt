#' Fisher Information of every item for an specified node
#'
#' Computes a vector of the Fisher Information of every item allocated to the
#' specified level node in the CAT decision tree. Every FI is computed using
#' the estimated ability level in the specified node
#'
#' @param bank matrix of the item bank. Rows represent items, and columns
#' represent parameters. If the model is \code{"GRM"}, the first column
#' represents the \code{alpha} parameters and the next columns represent the
#' \code{beta} parameters. If the model is \code{"NRM"}, odd columns represent
#' the \code{alpha} parameters and even columns represent \code{beta}
#' parameters
#' @param theta_est estimated ability level
#' @param nres vector of number of possible responses for every item
#' @param C vector of item capacities
#' @return A vector of all item Fisher Information for the specified node
#' @author Javier Rodr?guez-Cuadrado
#'
#' @export
create_E_MFI = function(bank, theta_est, nres, C) {

  nit = nrow(bank) #Number of items

  E = rep(0, nit) #Initialise the vector

  it_av = which(C != 0) #Available items (those with not null capacity)

  for (i in 1:length(it_av)) {
    E[it_av[i]] = pkg.env$Fisher_Inf(theta_est, bank[it_av[i], ], nres[it_av[i]])
  }#Note that the non-available items are not computed and remain 0

  return(E) #Return the vector of MSE for every item

}
