# `fdim`

Author: Pramit Ghosh

The `fdim` package is used for calculating the fractal dimension of a single polygonal feature stored in an ESRI Shapefile using the box-counting technique. It is done by performing the following important steps:

- Importing the shapefile and calculating the extents of the polygon
- Overlaying a grid of square cells on top of this polygon
- Fill out a 5-by-2 matrix row-by-row with the size of the cell and the number of such cells required to completely cover the polygon
- Decrease the size of the cell and repeat the previous step until the matrix has been populated
- Plot a log-log scatterplot of the number of cells vs. the reciprocal of cell size
- Calculate the best-fit line through these points and find its slope
- Report the slope of this line as the fractal dimension of the polygon
