#' Wrapper for `sf::st_read()``
#'
#' @param dsn Data source name such as the directory where the file is located
#' @param layer Layer name such as the filename of the file to be converted
#' @param ... Other arguments passed on to `st_read()`
#'
#' @return An object of class sf
#' @importFrom sf st_read
#' @importFrom sf st_is_longlat
#' @importFrom sf st_transform
import_sf = function(dsn, layer, ...)
{
  sfo = st_read(dsn, layer, ...)
  cat("\n")
  if(st_is_longlat(sfo))
  {
    cat("Note: Coordinates in Lat/Long; reprojecting to EPSG:3857...\n")
    trans_sfo = st_transform(sfo, 3857)
  }
  trans_sfo
}

#' Create grids for polygons supplied as arguments
#' @param f The sf object based on which the grid will be created
#' @param cs The cell size of the grid that is to be created. This will be set as the cell width in both x- and y-directions
#' @importFrom sf st_make_grid
#' @return An object of class `sfc` with square polygons
overlay_grid = function(cs, f)
{
  st_make_grid(f, cellsize = cs)
}


#' Counts the number of grid cells required to cover the shape polygon
#' @param grid The grid as an object of class sfc
#' @param f sf object that the grid is based on
#' @importFrom sf st_intersection
#' @return The number of grid cells of grid required to cover the polygon f
count_cells = function(grid, f)
{
  num_intersects = length(st_intersection(grid, f))
}

generate_matrix = function(l, k, dimension = 1)
{
  cat("Generating grids...\n")
  mode = if(inherits(l, "numeric")) "ss" else
            if(inherits(l, "matrix")) "sa"
  
  if(mode == "ss")
    grids = lapply(as.list(l), overlay_grid, k)
  else if(mode == "sa")
  {
    cs_list = lapply(1:nrow(l), function(x) l[x,])
    grids = lapply(cs_list, overlay_grid, k)
  }
  
  cat("Counting intersecting cells...\n")
  int_grids = lapply(grids, count_cells, k)
  
  if(mode == "ss")
    bcd_matrix = log(cbind(1/l, as.numeric(int_grids)))
  else if(mode == "sa")
    bcd_matrix = log(cbind(1/l[, dimension], as.numeric(int_grids)))
  
  class(bcd_matrix) = c(class(bcd_matrix), "bcd_matrix")
  return(bcd_matrix)
}

#' @importFrom stats lm
linreg_bcd = function(bcd_matrix)
{
  cat("Performing simple linear regression to determine Box-Counting dimension...\n")
  bcd_lm = lm(bcd_matrix[,2] ~ bcd_matrix[,1])
}

#' Import Spatial Vector Data
#'
#' @param x Object of class `sf`
#' @param dsn Data source name such as the directory where the file is located
#' @param layer Layer name such as the filename of the file to be converted
#' @param ... Other arguments passed on to `sf::st_read()` together with `dsn` and `layer`
#'
#' @importFrom sf st_transform
#' @return Object of class `sf`. If CRS of the original data is in Long/Lat, it is transformed to EPSG:3857
#' @export
#' 
import_SVD = function(x = NULL, dsn, layer, ...)
{
  k = if(is.null(x))
    import_sf(dsn, layer, ...) else
      if(st_is_longlat(x))
      {
        cat("Note: Coordinates in Lat/Long; reprojecting to EPSG:3857...\n")
        st_transform(x, 3857)
      } else x
  # if("sf" %in% class(k))
  # {
  #   class(k) = c("selfSA", class(k))
  # }
  return(k)
}

#' @importFrom graphics abline
#' @importFrom graphics plot
plot_slope = function(bcd_matrix, bcd_lm)
{
  cat("Plotting requested...\n")
  plot(bcd_matrix, ylab = "log(Number of boxes needed to cover)", xlab = "-log(Length of a side of the square box)", main = "Box-Counting Dimension")
  
  cat("Plotting best-fit line...\n")
  abline(reg = bcd_lm, col = "blue", lty = 2)
}

#' @importFrom stats coef
calc_slope = function(bcd_lm)
{
  as.numeric(coef(bcd_lm))[2]
}

# generate_matrix_affinity = function(l, k, dimension = 1)
# {
#   cat("Generating grids...\n")
#   cs_list = lapply(1:10, function(x) l[x,])
#   
#   grids = lapply(cs_list, overlay_grid, k)
#   
#   cat("Counting intersecting cells...\n")
#   int_grids = lapply(grids, count_cells, k)
#   
#   bcd_matrix = log(cbind(1/l[, dimension], as.numeric(int_grids)))
#   class(bcd_matrix) = c(class(bcd_matrix), "bcd_matrix")
#   return(bcd_matrix)
# }

