#' Ability level estimation
#'
#' Computes the estimated ability level given the ability level density
#' function values
#'
#' @param dens_vec vector of density function values of the evaluated ability
#' levels
#' @return A number, the expected value of the ability level density function
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
estimate = function(dens_vec) {

  return(st*sum(theta*dens_vec)) #Return the expected value

}
