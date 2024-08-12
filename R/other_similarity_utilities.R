poly_angles = function(p_coords)
{
  coords_df = as.data.frame(p_coords)[c('X', 'Y')]
  
  coords_df_false = rbind(coords_df, coords_df[2, ])
  TrajAngles(TrajFromCoords(coords_df_false))
  
  # tan_angle = mat.or.vec(dim(coords_df)[1], 6)
  # 
  # for(i in 1:dim(coords_df)[1])
  # {
  #   tan_angle[i, 1] = (coords_df$Y[i+1] - coords_df$Y[i])/(coords_df$X[i+1] - coords_df$X[i])
  #   tan_angle[i, 2] = sign(coords_df$Y[i+1] - coords_df$Y[i])
  #   tan_angle[i, 3] = sign(coords_df$X[i+1] - coords_df$X[i])
  # }
  # 
  # # tan_angle[dim(coords_df)[1], 1] = (coords_df$Y[1] - coords_df$Y[dim(coords_df)[1]])/(coords_df$X[1] - coords_df$X[dim(coords_df)[1]])
  # # tan_angle[dim(coords_df)[1], 2] = sign(coords_df$Y[1] - coords_df$Y[dim(coords_df)[1]])
  # # tan_angle[dim(coords_df)[1], 3] = sign(coords_df$X[1] - coords_df$X[dim(coords_df)[1]])
  # 
  # tan_angle[,4] = atan(tan_angle[,1]) #* 180 / pi
  # tan_angle = as.data.frame(tan_angle)
  # colnames(tan_angle) = c("tan_theta", "sign_dY", "sign_dX", "atan_degree", "quadrant_corrected", "turning_angle")
  # tan_angle = na.omit(tan_angle)
  # 
  # for(i in 1:dim(tan_angle)[1])
  # {
  #   if(tan_angle$sign_dY[i] < 0)
  #   {
  #     if(tan_angle$sign_dX[i] < 0)
  #       tan_angle$quadrant_corrected[i] = tan_angle$atan_degree[i] + pi else
  #         if(tan_angle$sign_dX[i] > 0)
  #           tan_angle$quadrant_corrected[i] = tan_angle$atan_degree[i] + 2*pi
  #   } else
  #     if(tan_angle$sign_dY[i] > 0 & tan_angle$sign_dX[i] < 0)
  #       tan_angle$quadrant_corrected[i] = tan_angle$atan_degree[i] + pi else
  #         tan_angle$quadrant_corrected[i] = tan_angle$atan_degree[i]
  # }
  # 
  # tan_angle$turning_angle[1] = tan_angle$quadrant_corrected[dim(tan_angle)[1]] - tan_angle$quadrant_corrected[1]
  # for(i in 2:dim(tan_angle)[1])
  # {
  #   tan_angle$turning_angle[i] = tan_angle$quadrant_corrected[i-1] - tan_angle$quadrant_corrected[i]
  # }
  # # tan_angle$turning_angle[dim(tan_angle)[1]] = tan_angle$quadrant_corrected[1] - tan_angle$quadrant_corrected[dim(tan_angle)[1]]
  # 
  # tan_angle$turning_angle = ifelse(tan_angle$turning_angle < -pi, tan_angle$turning_angle + 2*pi, tan_angle$turning_angle)
  # tan_angle$turning_angle = ifelse(tan_angle$turning_angle > pi, tan_angle$turning_angle - 2*pi, tan_angle$turning_angle)
  # 
  # # plot(cumsum(tan_angle$naive_turning), type = 'b')
  # 
  # tan_angle
}


poly_distances = function(p_coords)
{
  coords_df = as.data.frame(p_coords)[c('X', 'Y')]
  distances = mat.or.vec(dim(coords_df)[1], 3)
  
  for(i in 1:dim(coords_df)[1])
  {
    distances[i, 1] = sqrt((coords_df$X[i+1] - coords_df$X[i])^2 + (coords_df$Y[i+1] - coords_df$Y[i])^2)
  }
  distances = as.data.frame(distances)
  distances = na.omit(distances)
  colnames(distances) = c("UTM_dist", "normalized_dist", "cumnor_dist")
  perimeter = sum(distances$UTM_dist)
  distances$normalized_dist = distances$UTM_dist / perimeter
  distances$cumnor_dist = cumsum(distances$normalized_dist)
  c(0, distances$normalized_dist)
}


shifter = function(x, n = 1)
{
  x = x[1:dim(x)[1] - 1,]
  if (n == 0) x = x else x = rbind(tail(x, -n), head(x, n))
  rbind(x, x[1,])
}


turning_angle = function(p_coords)
{
  x = cumsum(poly_distances(p_coords))
  
  # x = x[2:length(x)]
  
  y = poly_angles(p_coords)
  y = c(0, y) |> cumsum()
  
  # if(plot)
  # {
  #   plot(x = c(x, 1), y = c(y[1], y), type = 'S', yaxt = "none", xlab = "Normalised perimeter", ylab = "Turning Angle", ...)
  #   breaks_pi = pretty(range(c(y[1], y)/pi))
  #   axis(2, at = breaks_pi * pi, labels = paste(breaks_pi, "\u03c0"))
  # }
  
  return(list(px = x, py = y))
}

plot_TA = function(p, ylim = NULL, col = "black", add = FALSE)
{
  x = p$px
  y = p$py
  
  if(add == FALSE)
  {
    plot(x = x, y = y, type = 's', yaxt = "none", xlab = "Normalised perimeter", ylab = "Turning Angle", col = col, ylim = ylim)
    breaks_pi = pretty(range(y)/pi)
    axis(2, at = breaks_pi * pi, labels = paste(breaks_pi, "\u03c0"))
  } else
  {
    lines(x = x, y = y, type = 's', col = col)
  }
}


diff_auc = function(x1_vals, x2_vals = x1_vals, y1_vals, y2_vals, unequal = FALSE, false_offset = min(c(y1_vals, y2_vals)))
{
  if(false_offset < 0)
  {
    false_offset = -false_offset + 1
  } else
  {
    false_offset = 0
  }
  
  if(unequal)
  {
    # print(false_offset)
    a1 = AUC(x1_vals, y1_vals + false_offset, method = "step")
    a2 = AUC(x2_vals, y2_vals + false_offset, method = "step")
    # print(paste("a1 = ", a1, ", a2 = ", a2))
    return(abs(a1 - a2))
  }
  
  AUC(x1_vals, abs(y2_vals - y1_vals), method = "step")
}

merge_x_fill_y = function(p1_TA, p2_TA)
{
  df1 = as.data.frame(p1_TA)
  df2 = as.data.frame(p2_TA)
  
  merged_df = merge(df1, df2, by = "px", sort = TRUE, all = TRUE)
  colnames(merged_df) = c('x_vals', 'y1_vals', 'y2_vals')
  
  duplicated_xs = which(diff(merged_df$x_vals) < 1e-10)
  if(length(duplicated_xs) > 0)
  {
    for(i in 1:length(duplicated_xs))
    {
      flag = FALSE
      row_num = duplicated_xs[i]
      df_subset = merged_df[row_num:(row_num+1), c('y1_vals', 'y2_vals')]

      if(is.na(df_subset[1,1]) && is.na(df_subset[2,2]) && abs(df_subset[1,2] - df_subset[2,1]) < 1e-10)
      {
        merged_df$x_vals[row_num] = -1
        merged_df$y2_vals[row_num + 1] = df_subset[1,2]
      } else
        if(is.na(df_subset[1,2]) && is.na(df_subset[2,1]) && abs(df_subset[1,1] - df_subset[2,2]) < 1e-10)
        {
          merged_df$x_vals[row_num + 1] = -1
          merged_df$y2_vals[row_num] = df_subset[2,2]
        }
    }
    merged_df = merged_df[merged_df$x_vals > -1, ]
  }
  
  harmonised_df = tidyr::fill(merged_df, c(y1_vals, y2_vals), .direction = "downup")
  
  return(harmonised_df)
}

dist_TA = function(p1, p2, p1_theta)
{
  p1_TA = turning_angle(p1)
  p2_TA = turning_angle(p2)
  
  p1_TA$py = p1_TA$py + p1_theta
  
  harmonised_df = merge_x_fill_y(p1_TA, p2_TA)
  
  dist = diff_auc(x1_vals = harmonised_df$x_vals, y1_vals = harmonised_df$y1_vals, y2_vals = harmonised_df$y2_vals)
  
  return(list(dist = dist, p1_TA = p1_TA, p2_TA = p2_TA, h_df = harmonised_df))
}
