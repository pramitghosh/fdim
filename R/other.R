
similarity = function(p1, p2, plot = TRUE)
{
  p1_coords = st_transform(p1, crs = 32642) |> st_coordinates()
  p2_coords = st_transform(p2, crs = 32642) |> st_coordinates()
  
  dist_TA(p1_coords, p2_coords)
  
}
