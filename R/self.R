#' Calculates the box-counting dimension of a polygon
#'
#' @param l Numeric or matrix; represents sequence of cell side lengths that will be used to generate the grid. Numeric value represents the length of a side of a single square box of the grid to calculate self-similarity; matrix columns represents the length and width of rectangular cells for calculating self-affinity.
#' @param plot Logical; if true, a log-log plot of the number of cells used to cover the polygon vs the cell size will be plotted
#' @param k Spatial Vector data as an `sf` object
#' 
#' @return A numeric vector representing the box-counting dimension of the spatial feature, `k`.
#' @name bcd
#' @export
bcd = function(k, l = seq(10000, 100000, 10000), ...)
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
bcd.self_affinity = function(k, l = seq(10000, 100000, 10000), plot = FALSE)
{
  # Write code for self-similarity using BCD
}
