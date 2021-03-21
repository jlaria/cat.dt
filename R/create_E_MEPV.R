#' MSE of every item for an specified node
#'
#' Computes a vector of the mean squared error of every item allocated to the
#' specified level node in the CAT decision tree. Every MSE is computed using
#' the ability level density function in the specified node and the ability
#' level estimations given the item responses
#'
#' @param bank matrix of the item bank. Rows represent items, and columns
#' represent parameters. If the model is \code{"GRM"}, the first column
#' represents the \code{alpha} parameters and the next columns represent the
#' \code{beta} parameters. If the model is \code{"NRM"}, odd columns represent
#' the \code{alpha} parameters and even columns represent \code{beta}
#' parameters
#' @param dens_vec vector of the density function values in the specified node
#' of the evaluated ability levels
#' @param nres vector of number of possible responses for every item
#' @param prob_array 3-D array of probability responses. Dim 1 represent items,
#' dim 2 represent evaluated ability levels and dim 3 represent possible
#' responses
#' @param C vector of item capacities
#' @return A vector of all item MSE for the specified node
#' @author Javier Rodr?guez-Cuadrado
#'
#' @export
create_E_MEPV = function(bank, dens_vec, nres, prob_array, C) {

  nit = nrow(bank) #Number of items

  E = rep(1e6, nit) #Initialise the vector

  it_av = which(C != 0) #Available items (those with not null capacity)

  for (i in 1:length(it_av)) { #Note that the non-available items are not
    #computed and remain 1e6

    mse = 0 #Initialise the MSE

    for (j in 1:nres[it_av[i]]) {

      pr = prob_array[it_av[i], , j] #Probability response of response j for
      #item it_av[i]
      apos = a_posteriori(dens_vec, pr) #A posteriori density given the response
      est = 
      mse = mse+(theta-estimate(apos)[[1]])^2*pr #MSE calculus for every evaluated
      #ability level given the estimation for the response j

    }

    E[it_av[i]] = st*sum(dens_vec*mse) #Integration over all evaluated ability
    #levels
  }

  return(E) #Return the vector of MSE for every item

}
