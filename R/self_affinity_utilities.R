
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

rotate_about_centroid = function(sf_geom, angle)
{
  ctrd = st_centroid(sf_geom)
  return((sf_geom - ctrd) * rot(angle) + ctrd)
}

x_extent = function(sf_geom)
{
  return(st_bbox(sf_geom)[3] - st_bbox(sf_geom)[1])
}

y_extent = function(sf_geom)
{
  return(st_bbox(sf_geom)[4] - st_bbox(sf_geom)[2])
}

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
