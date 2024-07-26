#' Calculates the box-counting dimension of a polygon
#'
#' @param l Numeric; represents the length of a side of a single square box of the grid to calculate box-counting dimension
#' @param plot Logical; if true, a log-log plot of the number of cells used to cover the polygon vs the cell size will be plotted
#' @param k Spatial Vector data as an `sf` object
#' @param type One of {'s', 'a'} (representing self-similarity and self-affinity respectively) that is to be calculated
#' 
#' @return A numeric representing the box-counting dimension of the spatial feature, `k`.
#' @name bcd
#' @export
bcd = function(k, type, l = seq(10000, 100000, 10000), plot = FALSE)
{
  if(type == "s")
    class(k) = c("self_similarity", class(k))
  if(type == "a")
    class(k) = c("self_affinity", class(k))
  UseMethod("bcd", k)
}

#' Calculates self-similar box-counting dimensions  
#' @name bcd
#' @export
bcd.self_similarity = function(k, type = "s", l = seq(10000, 100000, 10000), plot = FALSE)
{
  bcd_matrix = generate_matrix(l, k)
  bcd_lm = linreg_bcd(bcd_matrix)
  if(plot) plot_slope(bcd_matrix, bcd_lm)
  return(calc_slope(bcd_lm))
}

#' Calculates self-affine box-counting dimensions
#'
#' @name bcd
#' @importFrom sf st_geometry
#' @export
bcd.self_affinity = function(k, type = "a", l = seq(10000, 100000, 10000), plot = FALSE)
{
  k_rotated_geom = find_max_extent(st_geometry(k))
  k_scaled_geom = scale_x(k_rotated_geom)
  
  return(bcd(k_scaled_geom, type = 's', l, plot))
}

