#' Fisher Information under NRM
#'
#' Computes the item Fisher Information given an ability level based on the NRM
#' model
#'
#' @param theta_est ability level
#' @param item_par vector containing the item parameters. Odd components are
#' the \code{alpha} parameters and even are the \code{beta} parameters
#' @param nres number of possible item responses
#' @return An integer that represents the Fisher Information value of the
#' specified item given the ability level
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
Fisher_NRM = function(theta_est, item_par, nres) {

  #Calculus of the exponentials from the expressions
  expon = rep(0, nres-1) #Initialisation
  expon1 = expon
  expon2 = expon

  for (i in 1:(nres-1)) {

    expon[i] = exp(item_par[2*i-1]*theta_est+item_par[2*i])
    expon1[i] = item_par[2*i-1]*expon[i]
    expon2[i] = item_par[2*i-1]*expon1[i]

  }

  #Calculus of the sums from the expressions
  summ = sum(expon)
  summ1 = sum(expon1)
  summ2 = sum(expon2)

  #Probabilities calculus
  pr = expon/(1+summ)
  pr1 = (expon1+expon1*summ-expon*summ1)/(1+summ)^2
  pr2 = ((expon2+expon2*summ-expon*summ2)*(1+summ)-2*summ1*
           (expon1+expon1*summ-expon*summ1))/(1+summ)^3

  return(sum(pr1^2/pr-pr2)) #Calculate and return the Fisher Information

}
