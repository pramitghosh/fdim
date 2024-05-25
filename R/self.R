#' Calculates the box-counting dimension of a polygon
#'
#' @param l Numeric; represents the length of a side of a single square box of the grid to calculate box-counting dimension
#' @param plot Logical; if true, a log-log plot of the number of cells used to cover the polygon vs the cell size will be plotted
#' @param k Spatial Vector data as an `sf` object
#' @param type One of {self_similarity, self_affinity} that is to be calculated
#' 
#' @return A numeric representing the box-counting dimension of the spatial feature, `k`.
#' @name bcd
#' @export
bcd = function(k, type, l = seq(10000, 100000, 10000), plot = FALSE)
{
  if(type == "self_similarity")
    class(k) = c("self_similarity", class(k))
  if(type == "self_affinity")
    class(k) = c("self_affinity", class(k))
  UseMethod("bcd", k)
}
  
#' @name bcd
#' @export
bcd.self_similarity = function(k, type = "self_similarity", l = seq(10000, 100000, 10000), plot = FALSE)
{
  bcd_matrix = generate_matrix(l, k)
  bcd_lm = linreg_bcd(bcd_matrix)
  if(plot) plot_slope(bcd_matrix, bcd_lm)
  return(calc_slope(bcd_lm))
}


#' @name bcd
#' @importFrom graphics par
#' @export
bcd.self_affinity = function(k, type = "self_affinity", l = seq(10000, 100000, 10000), plot = FALSE)
{
  # Find correct rotation angle to have the longest length on the x-axis
  # and rotate the feature accordingly
  
  # Scale the x-dimension to have the same extent as the y-dimension
  
  return(bcd.self_similarity(k, l, plot))
}
