# make end points into a linestring 
make_line <- function(lon, lat, llon, llat) {
  st_linestring(matrix(c(lon, llon, lat, llat), 2, 2))
}

# edits to visgraph of pathroutr
prt_visgraph_bh <- function (barrier, buffer = 0, centroids = FALSE, 
                             tolerance = NULL,
                             centroid_limit = 1e+07, 
                             aug_points = NULL) 
{
  stopifnot(
    `barrier must be a simple feature collection with geometry type'POLYGON' or 'MULTIPOLYGON'` = 
      inherits(barrier %>% 
                 st_geometry(), "sfc_POLYGON") | 
      inherits(barrier %>% 
                 st_geometry(), 
               "sfc_MULTIPOLYGON"))
  # cast barrier to POLOYGON and sfc object ""sfc_POLYGON", "
  # sfc_MULTIPOLYGON" "sfc"  
  barrier <- barrier %>% 
    st_cast("POLYGON") %>% 
    st_union()
  
  # if buffer is greater than 0 buffer the barrire by that value in m 
  if (buffer > 0) {
    buf_poly <- barrier %>% 
      st_buffer(buffer)
  } else {
    buf_poly <- barrier # otherwise just leave barrier as is 
  }
  
  # if aug_oints isn't null than augment = TRUE otherwise it is FALSE 
  if (!is.null(aug_points)) {
    stopifnot(`aug_points must be a simple feature collection with geometry type 'POINT'` 
              = inherits(aug_points %>% 
                           st_geometry(), 
                         "sfc_POINT"))
    augment <- TRUE
  } else {
    augment <- FALSE
  }
  
  # added in tolerance may remove 
  if (is.null(tolerance)) {
    tolerance <- 0
    
  }
  # ---- if centroids are suppplied and augment isn't then do this ----
  if (centroids & !augment) {
    centroid_limit <- units::set_units(centroid_limit, "m^2")
    init_dt <- buf_poly %>% 
      st_cast('MULTIPOINT') %>% 
      st_triangulate(dTolerance = tolerance) %>% 
      st_collection_extract("POLYGON")
    
    ctr_pts <- st_centroid(init_dt[st_area(init_dt) > 
                                     centroid_limit])
    edges <- c(buf_poly %>% 
                 st_cast("MULTIPOINT") %>% 
                 st_cast("POINT"), ctr_pts) %>% 
      st_union() %>% 
      st_triangulate(bOnlyEdges = TRUE, 
                     # dTolerance = 0
      ) %>% st_cast("LINESTRING") %>% 
      st_sf()
    crosses <- do.call(c, st_intersects(st_buffer(barrier, 
                                                  -1), edges))
    edges <- edges[-crosses, ]
  }
  
  
  if (centroids & augment) {
    # set units of centroid -----
    centroid_limit <- units::set_units(centroid_limit, "m^2")
    
    # cast buffered polygon to multipoints 
    init_dt <- buf_poly %>% 
      st_cast("MULTIPOINT",) %>% 
      st_triangulate() %>% # then trianglualte to created dilanted trinagles 
      # between barrier points 
      st_collection_extract("POLYGON") # then make those triangles into polygons
    
    
    ctr_pts <- st_centroid(init_dt[st_area(init_dt) > centroid_limit])
    edges <- c(
      buf_poly %>% 
        st_cast("MULTIPOINT") %>% 
        st_cast("POINT"), ctr_pts, aug_points
    ) %>% 
      st_union() %>% 
      st_triangulate(bOnlyEdges = TRUE, 
                     # dTolerance = tolerance
      ) %>% 
      st_cast("LINESTRING") %>% 
      st_sf()
    
    crosses <- do.call(
      c, barrier %>% 
        st_buffer(-1) %>% 
        st_intersects(edges)
    )
    edges <- edges[-crosses, ]
  }
  
  
  
  if (!centroids & augment) {
    centroid_limit <- units::set_units(centroid_limit, "m^2")
    
    init_dt <- buf_poly %>% 
      st_cast("MULTIPOINT") %>% 
      st_triangulate(dTolerance = tolerance) %>% 
      st_collection_extract("POLYGON")
    
    edges <- c(buf_poly %>% 
                 st_cast("MULTIPOINT") %>% 
                 st_cast("POINT"), aug_points) %>% 
      st_union() %>% 
      st_triangulate(bOnlyEdges = TRUE, 
                     # dTolerance = tolerance
      ) %>% 
      st_cast("LINESTRING") %>% 
      st_sf()
    crosses <- do.call(c, barrier %>% 
                         st_buffer(-1) %>% 
                         st_intersects(edges)
    )
    edges <- edges[-crosses, ]
  }
  if (!centroids & !augment) {
    edges <- buf_poly %>% 
      st_cast("MULTIPOINT") %>% 
      st_union() %>% 
      st_triangulate(bOnlyEdges = TRUE, 
                     dTolerance = tolerance) %>% 
      st_cast("LINESTRING") %>%
      st_sf()
    crosses <- do.call(c, barrier %>% 
                         st_buffer(-1) %>% 
                         st_intersects(edges))
    edges <- edges[-crosses, ]
  }
  sln <- suppressWarnings(sfnetworks::as_sfnetwork(edges, 
                                                   directed = FALSE, 
                                                   length_as_weight = TRUE))
  return(sln)
}

