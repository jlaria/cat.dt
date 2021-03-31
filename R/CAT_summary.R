#' CAT summary
#'
#' Summary of the \code{cat.dt} object generated
#'
#' @param object A \code{cat.dt} object
#' @param ... not used
#'
#' @return A summary of the \code{cat.dt} object
#' @author Javier Rodr?guez-Cuadrado
#'
#' @export
summary.cat.dt = function(object, ...) {
  tree <- object
  cat("----------------------------------------------------------------------\n")

  cat("Number of tree levels:", length(tree$nodes)-1, "\n\n")

  for (i in 1:(length(tree$nodes)-1)) {
    cat("Number of nodes in level", i,":", length(tree$nodes[[i]]),"\n")
  }

  cat("----------------------------------------------------------------------\n")

  cat("Psychometric probabilistic model:", tree$model, "\n")

  cat("Item selection criterion:", tree$crit, "\n")

  cat("----------------------------------------------------------------------\n")

  cat("Item exposure:\n")

  #Falta pulirlo. Tiene un for y no hay que imprimir C sino C_org-C
  for (i in 1:length(tree$C)) {
    cat("item ",i,":",format(round(tree$C[i] - tree$C_left[i], digits = 4), nsmall = 3),"  ")
    if (floor(i/4) == i/4) {cat("\n")}
  }

  cat("\n")
  cat("Percentage of items used:", sum((tree$C-tree$C_left) > 0)*100/length(tree$C),"%\n")

  cat("----------------------------------------------------------------------\n")

}

