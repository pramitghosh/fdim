#' Wrapper for `sf::st_read()``
#' @param dsn Data source name such as the directory where the file is located
#' @param layer Layer name such as the filename of the file to be converted
#' @return An object of class sf
#' @import sf
import_sf = function(dsn, layer, ...)
{
  requireNamespace("sf", quietly = TRUE)
  st_read(dsn, layer, ...)
}

#' Create grids for polygons supplied as arguments
#' @param f The sf object based on which the grid will be created
#' @param cs The cell size of the grid that is to be created. This will be set as the cell width in both x- and y-directions
#' @import sf
#' @return An object of class `sfc` with square polygons
overlay_grid = function(f, cs)
{
  # requireNamespace("sp", quietly = TRUE)
  # bb = st_bbox(f)
  # cell_size = c(cs, cs)
  # cell_offset = bb[,1] + cell_size/2
  # cell_nos = c((bb['xmax'] - bb['xmin'])/cs, (bb['ymax'] - bb['ymin'])/cs)
  # grd = sp::SpatialGrid(sp::GridTopology(cell_offset, cell_size, ceiling(cell_nos)), proj4string = sp::CRS(sp::proj4string(shp)))
  # grd_poly = as(grd, "SpatialPolygons")
  st_make_grid(f, cellsize = cs)
}

#' Counts the number of grid cells required to cover the shape polygon
#' @param grid The grid as an object of class sfc
#' @param f sf object that the grid is based on
#' @import sf
#' @return The number of grid cells of grid required to cover the polygon f
count_cells = function(grid, f)
{
  # requireNamespace("rgeos", quietly = TRUE)
  # rows_true = na.omit(over(grid,f))
  num_intersects = length(st_intersection(grid, f))
}

#' Iterates over a loop to generate grids with different cell-sizes, acquire the number of such cells required to cover the polygon and stores them in a matrix
#' @param directory The directory name with path where the shapefile is located (without the trailing slash)
#' @param shapefile The shapefile in question (without the extension) within the directory
#' @return An object of class fdim which contains a matrix containing cell sizes and corresponding number of those cells required to cover the polygon
#' @export
#' @examples
#' \dontrun{
#' calc_tab = mb_dim()
#' }
#' \dontshow{
#' calc_tab = mb_dim(system.file(package = "fdim"), "madhya_pradesh")
#' }
mb_dim = function(directory = readline(prompt = "Please enter the directory without the trailing slash: "), shapefile = readline(prompt = "Please enter the filename without the extension: "))
{
  init_cell_size = 5
  calc_tab = matrix(nrow = 5, ncol = 2)
  f = import_shp(directory, shapefile)
  for(i in 1:dim(calc_tab)[1])
  {
    grid = overlay_grid(f, init_cell_size)
    calc_tab[i,1] = init_cell_size
    init_cell_size = init_cell_size/2
    calc_tab[i,2] = count_cells(grid, f)
  }
  structure(calc_tab, class = c("fdim"))
}

#' Plots a fdim object with the y-axis representing the number of cells required to cover the polygon and the x-axis representing the cell-sizes. It also plots a best-fit line through the points
#' @param x The matrix with the two columns - the cell size and the number of cells of that size required to cover the polygon
#' @param ... Other arguments
#' @return NULL
#' @importFrom graphics abline
#' @export
#' @method plot fdim
#' @examples
#' \dontshow{
#' calc_tab = mb_dim(system.file(package = "fdim"), "madhya_pradesh")
#' }
#' plot(calc_tab)
plot.fdim = function(x, ...)
{
  log_N = log(x[,2])
  log_1be = log(1/x[,1])
  plot(log_N ~ log_1be, main = "Number of cells (N) vs. reciprocal of cell-size (1/e)\n and the best-fit line", xlab = "log(1/e)", ylab = "log(N)")
  abline(linreg.fdim(x))
}

#' Generates a linear model relating the the number of cells and cell sizes on a logarithmic scale
#' @param calc_tab An object of class fdim
#' @return A linear model of class lm
#' @importFrom stats lm
linreg.fdim = function(calc_tab)
{
  log_N = log(calc_tab[,2])
  log_1be = log(1/calc_tab[,1])
  reg_mod = lm(log_N ~ log_1be)
}


#' Prints the fractal dimension of the polygon
#' @param object An object of class fdim
#' @param ... Other arguments
#' @return NULL
#' @export
#' @method summary fdim
#' @examples
#' \dontshow{
#' calc_tab = mb_dim(system.file(package = "fdim"), "madhya_pradesh")
#' }
#' summary(calc_tab)
summary.fdim = function(object, ...)
{
  lm_model = linreg.fdim(object)
  dimension = summary(lm_model)$coefficients[2,1]
  cat("The fractal dimension of the polygon is: ", dimension, ".")
}
