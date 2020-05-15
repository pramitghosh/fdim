#' Calculates the box-counting dimension of a polygon
#'
#' @param l Numeric or matrix; represents sequence of cell side lengths that will be used to generate the grid. Numeric value represents the length of a side of a single square box of the grid to calculate self-similarity; matrix columns represents the length and width of rectangular cells for calculating self-affinity.
#' @param plot Logical; if true, a log-log plot of the number of cells used to cover the polygon vs the cell size will be plotted
#' @param k Spatial Vector data as an `sf` object
#' 
#' @return A numeric vector representing the box-counting dimension of the spatial feature, `k`.
#' @name bcd
#' @export
bcd = function(k, l = seq(10000, 100000, 10000), plot = FALSE)
{
  if("numeric" %in% class(l))
  class(k) = c("self_similarity", class(k))
  if("matrix" %in% class(l))
    class(k) = c("self_affinity", class(k))
  UseMethod("bcd", k)
}

# bcd.selfSA = function(k, l = seq(10000, 100000, 10000), plot = FALSE)
# {
#   if("numeric" %in% class(l))
#     class(k) = c("self_similarity", class(k))
#   if("matrix" %in% class(l))
#     class(k) = c("self_affinity", class(k))
#   UseMethod("bcd")
# }
  
#' @name bcd
#' @export
bcd.self_similarity = function(k, l = seq(10000, 100000, 10000), plot = FALSE)
{
  bcd_matrix = generate_matrix(l, k)
  bcd_lm = linreg_bcd(bcd_matrix)
  if(plot) plot_slope(bcd_matrix, bcd_lm)
  return(calc_slope(bcd_lm))
}


#' @name bcd
#' @export
bcd.self_affinity = function(k, l = matrix(rep(seq(10000, 100000, 10000), 2), ncol = 2), plot = FALSE)
{
  cs_x = matrix(c(l[,1], rep(min(l[,2]), times = nrow(l))), ncol = 2)
  cs_y = matrix(c(rep(min(l[,1]), times = nrow(l)), l[,2]), ncol = 2)
  
  # bcd_matrix_x = generate_matrix_affinity(l = cs_x, k, dimension = 1)
  # bcd_matrix_y = generate_matrix_affinity(l = cs_y, k, dimension = 2)
  
  bcd_matrix_x = generate_matrix(l = cs_x, k, dimension = 1)
  bcd_matrix_y = generate_matrix(l = cs_y, k, dimension = 2)
  bcd_matrix = list(x = bcd_matrix_x, y = bcd_matrix_y)
  
  bcd_lm = lapply(bcd_matrix, linreg_bcd)
  if(plot)
  {
    par(mfrow = c(1,2))
    plot_slope(bcd_matrix[[1]], bcd_lm[[1]])
    plot_slope(bcd_matrix[[2]], bcd_lm[[2]])
    par(mfrow = c(1,1))
  }
  
  return(sapply(bcd_lm, calc_slope))
}
