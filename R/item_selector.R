#' Linear programming solver
#'
#' Computes the exposure rate of every item allocated to every level node. If
#' more than one item is allocated to the same level node, the node splits.
#'
#' @param E_mat matrix of the associated value of every item allocated to
#' every level node. Rows represent items and columns represent level nodes.
#' The "associated value" can be the MSE if the selection criterium is "MEPV",
#' the FI if the selection criterium is "MFI" and ****
#' @param D vector of confluencies of every level node
#' @param C vector of item capacities
#' @param minmax optimisation direction. Options: \code{TRUE} to maximise and
#' \code{FALSE} to minimise
#' @return A matrix of exposure rates. Rows represent items and columns
#' represent level nodes. Every item with a positive exposure rate for a level
#' node is allocated to that node
#' @author Javier Rodríguez-Cuadrado
#'
#' @export
item_selector = function(E_mat, D, C, minmax) {

  nit = length(C) #Number of items

  nnod = length(D) #Number of level nodes

  obj = as.vector(E_mat) #Objective function coefficients

  #Build constraint matrix
  i = c(rep(1:nit, nnod), rep(nit+1:nnod, each=nit),
        (nit+nnod+1):(nit+nnod+nit*nnod)) #Row indexes for non-zero entries

  j = rep(1:(nnod*nit), 3) #Column indexes for non-zero entries

  A = Matrix::sparseMatrix(i = i, j = j, x = 1)

  rhs = c(C, D, rep(0, nit*nnod))*1e5 #Right-hand sides of the constraints
  #(1e5 multiplication is needed for computational reasons)

  direc = c(rep("<=", nit), rep("==", nnod), rep(">=", nit*nnod)) #Direction
  #of the constraints

  #Solve the linear programming problem
  sol = Rglpk::Rglpk_solve_LP(obj = obj, mat = A, dir = direc, rhs = rhs,
                              max = minmax)

  return(matrix(sol$solution, nit)*1e-5) #Return solution undoing the
  #multiplication

}
