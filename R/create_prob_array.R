#' Multidimensional array of response probabilities
#'
#' For every item (dim 1) in an item bank and every evaluated ability level
#' (dim 2), computes the probability of picking every possible response (dim 3)
#' given the ability level
#'
#' @param model polytomous IRT model. Options: \code{"GRM"} for Graded Response
#' Model and \code{"NRM"} for Nominal Response Model
#' @param bank matrix of the item bank. Rows represent items, and columns
#' represent parameters. If the model is \code{"GRM"}, the first column
#' represents the \code{alpha} parameters and the next columns represent the
#' \code{beta} parameters. If the model is \code{"NRM"}, odd columns represent
#' the \code{alpha} parameters and even columns represent \code{beta}
#' parameters
#' @param nres vector of number of possible responses for every item
#' @return A 3-dimensional array of probability responses
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
create_prob_array = function(model, bank, nres) {

  nit = nrow(bank) #Number of items

  #Initialize array
  prob_array = array(0, dim = c(nit, num_theta, max(nres)))

  #Calculate depending on the model
  switch(model,
         GRM = {
           for (i in 1:nit) {
             prob_array[i, , 1:nres[i]] = probab_GRM(bank[i, ], nres[i])
           }
         },
         NRM = {
           for (i in 1:nit) {
             prob_array[i, , 1:nres[i]] = probab_NRM(bank[i, ], nres[i])
           }
         }
  )

  return(prob_array)
}
