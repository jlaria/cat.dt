#' Ability level estimation
#'
#' Computes the estimated ability level given the ability level density
#' function values and its standard error
#'
#' @param dens_vec vector of density function values of the evaluated ability
#' levels
#' @return A list containing the expected value of the ability level density 
#' function and the standard error of that expectated value
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
estimate = function(dens_vec) {

  est = st*sum(theta*dens_vec) #Expected value (Ability level estimation)
  SE = sqrt(st*sum((theta-est)^2*dens_vec)) #Standard deviation (SE)
  
  return(list(est, SE)) #Return both elements

}
