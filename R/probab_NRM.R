#' Item response NRM probabilities
#'
#' Computes the probabilities of picking every possible response of an
#' specified item from the item bank for all evaluated ability levels
#' using the Nominal Response Model
#'
#' @param item_par vector containing the item parameters. Odd components are
#' the \code{alpha} parameters and even are the \code{beta} parameters
#' @param nres number of possible item responses
#' @return A matrix of response probabilities. Rows represent evaluated ability
#' levels and columns represent responses
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
probab_NRM = function(item_par, nres) {

  #Get parameters
  alpha = item_par[seq(1,2*nres-3,2)]
  beta = item_par[seq(2,2*nres-2,2)]

  #Calculate the numerator of the NRM probability expression in matrix form.
  #Rows represent evaluated ability levels and columns represent responses
  #(the first column for response one is missing)
  M =  mapply(function(a,c){
         exp(a*theta+c)},
       alpha,beta)

  #Calculate and return probabilities
  return(cbind(1,M)/(apply(M,1,sum)+1))

}
