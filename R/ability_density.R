#Common values among functions in the package
st = 0.005 #Distance between every two consecutive evaluated ability levels
lim = 10 #Maximum evaluated ability level
theta = seq(-lim, lim, st) #Evaluated ability level values
num_theta = 2*lim/st+1 #Number of evaluated ability levels

#' Vector of density values of ability level
#'
#' Computes the density function values of the evaluated ability levels
#'
#' @param dens density function (e.g. dnorm, dunif, etc.)
#' @param ... parameters of the density function
#' @return A vector of density values
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
ability_density = function(dens,...) {

  return(dens(theta,...)) #return the vector

}
