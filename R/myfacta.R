#' myfacta
#'
#' This function generates a heat map of the matrix R, proportion of total variance due to the jth factor, the matrix Psi,
#'     the residual matrix, and the communalities.
#'
#' @param S input sample var-cov matrix
#' @param R input sample correlation matrix
#' @param m input number of factors
#'
#' @return a heat map of the matrix R, proportion of total variance due to the jth factor, the matrix Psi,
#'         the residual matrix, and the communalities
#' @export
#'
#' @importFrom stats cov2cor
#'
#' @examples
#' S = matrix(c(1.0, 0.63, 0.45, 0.63, 1.0, 0.35, 0.45, 0.35, 1.0), nrow=3, ncol=3, byrow=TRUE)
#' m = 1
#' myfacta(S=S, m=1)
myfacta <- function(S=NULL, R=NULL, m=NULL) {
  if (is.null(S) && is.null(R)) {
    rlang::abort("Neither a sample covariance or correlation matrix was provided.")
  }

  if (is.null(m)) {
    rlang::abort("A factor number, m, was not provided.")
  }

  if (!is.null(S) && !is.null(R)) {
    rlang::abort("Please provided either the cov or cor matrix, not both")
  }

  nRows = max(nrow(S), nrow(R))

  if (m <= 0 || !is.numeric(m) || m != round(m) || m > nRows) {
    rlang::abort("The number of factors is not a positive integer less than the number of observed variables.")
  }

  if (!is.null(S)) {
    S = as.matrix(S)
  }

  if (!is.null(R)) {
    R = as.matrix(R)
  }

  if (!is.null(S) && (!is.matrix(S) || !isSymmetric(S, check.attributes = FALSE))) {
    rlang::abort("S must be a symmetric covariance matrix.")
  }

  if (!is.null(R) && (!is.matrix(R) || !isSymmetric(R, check.attributes = FALSE) || any(diag(R) != 1.00))) {
    rlang::abort("R must be a symmetric correlation matrix with diagonals equal to 1.")
  }

  if (!is.null(S)) {
    R = cov2cor(S)
  }

  # Heat map of correlation matrix, R
  heatmap <- pheatmap::pheatmap(R, display_numbers = TRUE, number_color = 'black',
                                cluster_rows = FALSE, cluster_cols = FALSE)

  # Proportion of total variance cause by the jth variable
  p = nrow(R)

  eigens_R <- eigen(R)
  eigenvalues_R <- eigens_R$values
  eigenvectors_R <- eigens_R$vectors

  tot_var_jth <- round((eigenvalues_R / p), 2)

  # Loadings
  sqrt_eigenvalues_R <- sqrt(eigenvalues_R)
  eigenvectors_R[,2] <- eigenvectors_R[,2] * (-1) # Flip sign to match book results
  load_mat <- sqrt_eigenvalues_R[1:m] * t(eigenvectors_R[, 1:m])
  load_mat <- round(t(load_mat), 2)

  # Communalities
  communalities <- round(apply(abs(load_mat)^2, 1, sum), 2)

  # Psi Matrix
  psi_mat <- round(diag(1-communalities),2)

  # Residual Matrix
  res_mat <- round((load_mat %*% t(load_mat) + psi_mat), 2)

  #Results

  results_list <- list(
    "Heatmap" = heatmap,
    "TotalVar" = tot_var_jth,
    "Loadings" = load_mat,
    "Communalities" = communalities,
    "PsiMat" = psi_mat,
    "ResMat" = res_mat,
    "CorMat" = R
    )
  return (results_list)
}
