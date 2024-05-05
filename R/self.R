#' Calculates the box-counting dimension of a polygon
#'
#' Calculates the self-similar or self-affine fractal dimension. If argument `l` is a numeric vector, self-similarity is calculated. If it is a matrix, then self-affinity is calculated and returns the global self-affine fractal dimension. If `affine_local` is set to `TRUE`, then global and local self-affine fractal dimensions are returned.
#'
#' @param l Numeric or matrix; represents sequence of cell side lengths that will be used to generate the grid. Numeric value represents the length of a side of a single square box of the grid to calculate self-similarity; matrix columns represents the length and width of rectangular cells for calculating self-affinity.
#' @param plot Logical; if true, a log-log plot of the number of cells used to cover the polygon vs the cell size will be plotted
#' @param k Spatial Vector data as an `sf` object
#' @param ... Arguments `anisotropy` and `affine_local` passed on to `bcd.self_affinity()`
#' 
#' @return A numeric vector representing the box-counting dimension of the spatial feature, `k`.
#' @name bcd
#' @export
bcd = function(k, l, plot, ...)
{
  if("numeric" %in% class(l))
    class(k) = c("self_similarity", class(k))
  if("matrix" %in% class(l))
    class(k) = c("self_affinity", class(k))
  UseMethod("bcd", k)
}

#' Calculates self-similar box-counting dimensions  
#' @name bcd
#' @export
#' @examples
#' import_SVD(rnaturalearth::ne_countries(scale = "medium", country = "Nepal", returnclass = "sf")) |>
#' bcd(l = seq(10000, 100000, 15000), plot = TRUE)
#' 
bcd.self_similarity = function(k, l = seq(10000, 100000, 10000), plot = FALSE, ...)
{
  bcd_matrix = generate_matrix(l, k)
  bcd_lm = linreg_bcd(bcd_matrix)
  if(plot) plot_slope(bcd_matrix, bcd_lm)
  return(calc_slope(bcd_lm))
}

#' Calculates self-affine global and local box-counting dimensions
#' @param anisotropy Ratio of directional scaling exponents `v_x/v_y` (`v_x > v_y`) beyond which the feature will be considered to exhibit self-affinity
#' @param affine_local Whether local self-affine fractal dimension is to be calculated
#'
#' @name bcd
#' @importFrom graphics par
#' @export
#' @examples
#' rnaturalearth::ne_countries(scale = "medium", country = "Ukraine", returnclass = "sf") |>
#' import_SVD() |>
#' bcd(l = matrix(rep(seq(10000, 100000, 10000), 2), ncol = 2), affine_local = TRUE, plot = TRUE)
#' 
bcd.self_affinity = function(k, l = matrix(rep(seq(10000, 100000, 10000), 2), ncol = 2), plot = FALSE, anisotropy = 1.25, affine_local = FALSE, ...)
{
  cs_x = matrix(c(l[,1], rep(min(l[,2]), times = nrow(l))), ncol = 2)
  cs_y = matrix(c(rep(min(l[,1]), times = nrow(l)), l[,2]), ncol = 2)
  
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
  
  scaling_exponents = sapply(bcd_lm, calc_slope)
  calculated_anisotropy = calc_anisotropy(scaling_exponents)
  
  if(calculated_anisotropy < anisotropy)
  {
    cat(paste("Important Note!\nAnisotropy calculated to be ", calculated_anisotropy, ". The feature doesn't seem to exhibit self-affinity. Local and global self-affine fractal dimensions may not be reliable!\n"))
  }
  
  v_x = max(scaling_exponents)
  v_y = min(scaling_exponents)
  
  global = (v_y - v_x + 1)/v_y
  local = (v_x - v_y + 1)/v_x
  
  if(affine_local == TRUE)
    return(c(global, local)) else
      return(global)
}

