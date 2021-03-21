#' Fisher Information under GRM
#'
#' Computes the item Fisher Information given an ability level based on the GRM
#' model
#'
#' @param theta_est ability level
#' @param item_par vector containing the item parameters. First component
#' is the \code{alpha} parameter and the next are the \code{beta} parameters
#' @param nres number of possible item responses
#' @return An integer that represents the Fisher Information value of the
#' specified item given the ability level
#' @author Javier Rodriguez-Cuadrado
#'
#' @export
Fisher_GRM = function(theta_est, item_par, nres) {

  #Calculus of the exponentials from the expression
  M = sapply(item_par[2:nres], function(x){exp(item_par[1]*(theta_est-x))})

  #Calculus of every element that sums the Fisher Information
  G = rep(0, nres)#Initialisation
  G[1] = item_par[1]^2*M[1]/(1+M[1])^3 #First element
  G[nres] = item_par[1]^2*M[nres-1]^2/(1+M[nres-1])^3 #Last element
  if (nres > 2) { #Intemediate elements (if there are any)

    G[2:(nres-1)] = item_par[1]^2*((M[1:(nres-2)]/(1+M[1:(nres-2)])^2-
                                   M[2:(nres-1)]/(1+M[2:(nres-1)])^2)^2/
    (M[1:(nres-2)]/(1+M[1:(nres-2)])-M[2:(nres-1)]/(1+M[2:(nres-1)]))+
      (M[1:(nres-2)]-1)*M[1:(nres-2)]/(1+M[1:(nres-2)])^3-
      (M[2:(nres-1)]-1)*M[2:(nres-1)]/(1+M[2:(nres-1)])^3)

  }

  return(sum(G)) #Calculate and return the Fisher Information

}
