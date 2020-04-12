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


#' Calculates the box-counting dimension of a polygon
#'
#' @param x Object of class `sf`
#' @param dsn Data source name such as the directory where the file is located
#' @param layer Layer name such as the filename of the file to be converted
#' @param l Numeric sequence of different cell sizes that will be used to generate the grid. The cell size represents the length of a side of a single square box of the grid
#' @param plot Logical; if true, a log-log plot of the number of cells used to cover the polygon vs the cell size will be plotted
#' @param ... Other arguments passed on to `st_read()` together with `dsn` and `layer`
#' @importFrom graphics abline
#' @importFrom stats coef lm
#' @importFrom sf st_transform
#' @return A numeric vector of length 1 representing the box-counting dimension of the polygon
#' @export
bcd = function(x = NULL, dsn, layer, l = seq(10000, 100000, 10000), plot = FALSE, ...)
{
  k = if(is.null(x))
    import_sf(dsn, layer, ...) else
  if(st_is_longlat(x))
  {
    cat("Note: Coordinates in Lat/Long; reprojecting to EPSG:3857...\n")
    st_transform(x, 3857)
  } else x
  
  cat("Generating grids...\n")
  grids = lapply(as.list(l), overlay_grid, k)
  
  cat("Counting intersecting cells...\n")
  int_grids = lapply(grids, count_cells, k)
  
  bcd_matrix = log(cbind(1/l, as.numeric(int_grids)))
  if(plot)
  {
    cat("Plotting requested...\n")
    plot(bcd_matrix, ylab = "log(Number of boxes needed to cover)", xlab = "-log(Length of a side of the square box)", main = "Box-Counting Dimension")
  }
  
  cat("Performing simple linear regression to determine Box-Counting dimension...\n")
  bcd_lm = lm(bcd_matrix[,2] ~ bcd_matrix[,1])
  
  if(plot)
    abline(reg = bcd_lm, col = "blue", lty = 2)
  
  as.numeric(coef(bcd_lm))[2]
}
