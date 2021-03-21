#' Item response GRM probabilities
#'
#' Computes the probabilities of picking every possible response of an
#' specified item from the item bank for all evaluated ability levels
#' using the Graded Response Model
#'
#' @param item_par vector containing the item parameters. First component
#' is the \code{alpha} parameter and the next are the \code{beta} parameters
#' @param nres number of possible item responses
#' @return A matrix of response probabilities. Rows represent evaluated ability
#' levels and columns represent responses
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
probab_GRM = function(item_par, nres) {

  #Calculate in matrix form the probabilities of picking a response equal or
  #greater than itself. Rows represent evaluated ability levels and columns
  #represent responses (the first column for response one is missing)
  M = sapply(item_par[2:nres], function(x){
        temp = exp(item_par[1]*(theta-x))
        temp/(1+temp)}
      )

  #Calculate and return probabilities
  if (nres > 2) {return(cbind(1-M[, 1],M[, 1:(nres-2)]-M[,2 :(nres-1)],
                              M[, nres-1]))}
  else {return(cbind(1-M[, 1], M[, 1]))}

}
