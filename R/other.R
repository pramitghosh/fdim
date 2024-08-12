
similarity = function(p1, p2, plot = TRUE)
{
  if(is.na(st_crs(p1)))
  {
    p1_coords = st_coordinates(p1)
  } else
  {
    p1_coords = st_transform(p1, crs = 32642) |> st_coordinates()
  }
  
  if(is.na(st_crs(p2)))
  {
    p2_coords = st_coordinates(p2)
  } else
  {
    p2_coords = st_transform(p2, crs = 32642) |> st_coordinates()
  }
  # p1_coords = st_transform(p1, crs = 32642) |> st_coordinates() 
  # p2_coords = st_transform(p2, crs = 32642) |> st_coordinates()
  
  p1_TA = as.data.frame(turning_angle(p1_coords))
  p2_TA = as.data.frame(turning_angle(p2_coords))
  
  alpha = diff_auc(p2_TA$px, p1_TA$px, p2_TA$py, p1_TA$py, unequal = TRUE)
  
  min_config = list(dist = -1)
  
  all_points = list()
  for(i in 1:nrow(p2_coords))   #g
  {
    t_outer = p2_TA$px[i]
    p_outer = shifter(p2_coords, i - 1)
    
    for(j in 1:nrow(p1_coords)) #f
    {
      if(i == nrow(p2_coords) && j == nrow(p1_coords))
        break
      
      t_inner = p1_TA$px[j]
      t_net = t_outer - t_inner
      if(t_net < 0)
      {
        t_net = 1 + t_net
      }
      p_inner = shifter(p1_coords, j - 1)
      
      theta_optimal = alpha - (2 * pi * t_net)
      # print(paste("i = ", i, ", j = ", j, ", theta = ", theta_optimal, sep = ""))
      
      dist_obj = dist_TA(p1_coords, p2_coords, theta_optimal)
      # print(dist_obj$dist)
      
      all_points = append(all_points, list(c(dist = dist_obj$dist, i = i, j = j, theta = theta_optimal, t = t_net)))
      
      if(dist_obj$dist <= min_config$dist || min_config$dist < 0)
      {
        dist_obj = append(dist_obj, list(params = c(i = i, j = j, theta = theta_optimal, t = t_net)))
        min_config = dist_obj
      }
    }
  }
  
  if(plot)
  {
    p1_TA = as.data.frame(min_config$p1_TA)
    p2_TA = as.data.frame(min_config$p2_TA)
    
    y_range = c(min(p1_TA$py, p2_TA$py), max(p1_TA$py, p2_TA$py))
    # par(mfrow = c(2, 1))
    plot_TA(p1_TA, col = "red", ylim = y_range)
    plot_TA(p2_TA, col = "blue", add = TRUE)
    # par(mfrow = c(1, 1))
  }
  
  all_points_df = t(as.data.frame(all_points))
  rownames(all_points_df) = 1:nrow(all_points_df)
  min_config = append(min_config, list(df = all_points_df))
  
  return(min_config)
}
