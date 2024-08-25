a = pol_rot
b = b
c = st_sf((pol - 5)*2)

mp1 = st_union(st_union(a, b), c)
plot(mp1, axes = T)

split_to_polygons = function(mp)
{
  ps = st_cast(mp, "POLYGON")
  ind_polygons = lapply(ps[[1]], function(x) {st_sf(st_sfc(x))})
  ind_coordinates = lapply(ind_polygons, st_centroid) |>
    sapply(st_coordinates) |>
    t()
  ind_coordinates = rbind(ind_coordinates, ind_coordinates[1, ])
  
  
  p_centroid = list(ind_coordinates) |>
    st_polygon() |>
    st_sfc() |>
    st_sf()
  
  # plot(mp, axes = T)
  # plot(p_centroid, border = "yellow", add = T, xlim = c(st_bbox(mp)[1]:st_bbox(mp)[3]))
  return(list(loip = ind_polygons, cp = p_centroid))
}

dist_mp = function(mp1, mp2)
{
  pset1 = split_to_polygons(mp1)
  pset2 = split_to_polygons(mp2)
  
  loip1 = pset1$loip
  loip2 = pset2$loip
  
  cp1 = pset1$cp
  cp2 = pset2$cp
  
  similarity_config = similarity_polygon(cp1, cp2, plot = FALSE)$dist
  
  if(length(loip1) == length(loip2))
  {
    similarities = NULL
    for(i in 1:length(loip1))
    {
      similarities = c(similarities, similarity_polygon(loip1[[i]], loip2[[i]], plot = FALSE)$dist)
    }
  } else
  {
    print("Number of individual polygons in the multipolygons are unequal")
    return(FALSE)
  }
  
  return(c(configuration = similarity_config, agg_ind_sims = median(similarities)))
}

