
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

#' Rotate sf geometry about its centroid
#'
#' @param sf_geom sf geometry object
#' @param angle Angle (in radians) by which the geometry is to be rotated
#' 
#' @importFrom sf st_centroid
#'
#' @return sf geometry object rotated about its centroid by the desired angle
#'
rotate_about_centroid = function(sf_geom, angle)
{
  ctrd = st_centroid(sf_geom)
  return((sf_geom - ctrd) * rot(angle) + ctrd)
}

#' Calculate extent along x-axis
#'
#' @param sf_geom sf geometry object whose extent along x-axis is to be calculated
#' 
#' @importFrom sf st_bbox
#'
#' @return Length of the extent along x-axis
#'
x_extent = function(sf_geom)
{
  return(st_bbox(sf_geom)[3] - st_bbox(sf_geom)[1])
}

#' Calculate extent along y-axis
#'
#' @param sf_geom sf geometry object whose extent along y-axis is to be calculated
#' 
#' @importFrom sf st_bbox
#'
#' @return Length of the extent along y-axis
#'
y_extent = function(sf_geom)
{
  return(st_bbox(sf_geom)[4] - st_bbox(sf_geom)[2])
}

#' Rotates geometry so that maximum possible extent lies along x-axis
#'
#' @param sf_geom sf geometry object whose extent is to be maximized along x-axis
#'
#' @return Rotated sf geometry object such that its maximum possible extent is along x-axis
#'
find_max_extent = function(sf_geom)
{
  max_length = 0
  axis = NULL
  angle = 0
  
  for(test_angle in seq(0, pi/2, 0.008))
  {
    #print(test_angle)
    test_geom = rotate_about_centroid(sf_geom, test_angle)
    
    xl = x_extent(test_geom)
    yl = y_extent(test_geom)
    
    if(xl >= yl)
    {
      if(xl > max_length)
      {
        max_length = xl
        axis = 'x'
        angle = test_angle
      }
    } else
    {
      if(yl > max_length)
      {
        max_length = yl
        axis = 'y'
        angle = test_angle
      }
    }
  }
  
  if(axis == 'y')
  {
    angle = angle + pi/2
  }
  
  cat('Max extent: ')
  cat(max_length)
  cat('\nAngle: ')
  cat(angle)
  
  rotated_geom = rotate_about_centroid(sf_geom, angle)
  return(rotated_geom)
}

gen_scaling_operator = function(scaling_ratio)
{
  scaling_mat = matrix(c(1/scaling_ratio, 0, 0, 1), 2, 2)
  print(scaling_mat)
  return(scaling_mat)
}

scale_x = function(rotated_geom)
{
  ratio = x_extent(rotated_geom)/y_extent(rotated_geom)
  scaled_geom = rotated_geom * gen_scaling_operator(ratio)
  
  return(scaled_geom)
}
