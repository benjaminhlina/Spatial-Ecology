# make end points into a linestring 
make_line <- function(lon, lat, llon, llat) {
  st_linestring(matrix(c(lon, llon, lat, llat), 2, 2))
}

# edits to visgraph of pathroutr
prt_visgraph_bh <- function (barrier, buffer = 0, centroids = FALSE,
                             # tolerance = NULL,
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
    aug_pts <- aug_points %>%
      st_geometry()
  } else {
    augment <- FALSE
  }

  if (centroids & !augment) {
    centroid_limit <- units::set_units(centroid_limit, "m^2")
    init_dt <- buf_poly %>%
      st_cast("MULTIPOINT") %>%
      st_triangulate() %>%
      st_collection_extract("POLYGON")

    ctr_pts <- st_centroid(init_dt[st_area(init_dt) > centroid_limit])
    edges <- c(buf_poly %>%
                 st_cast("MULTIPOINT") %>%
                 st_cast("POINT"), ctr_pts) %>%
      st_union() %>%
      st_triangulate(bOnlyEdges = TRUE) %>%
      st_cast("LINESTRING") %>%
      st_sf()
    crosses <- do.call(
      c, barrier %>%
        st_buffer(-1) %>%
        st_intersects(edges)
    )
    edges <- edges[-crosses, ]
  }
  if (centroids & augment) {
    centroid_limit <- units::set_units(centroid_limit, "m^2")

    init_dt <- buf_poly %>%
      st_cast("MULTIPOINT") %>%
      st_triangulate() %>%
      st_collection_extract("POLYGON")

    ctr_pts <- st_centroid(init_dt[st_area(init_dt) > centroid_limit])
    edges <- c(buf_poly %>%
                 st_cast("MULTIPOINT") %>%
                 st_cast("POINT"), ctr_pts, aug_pts) %>%
      st_union() %>%
      st_triangulate(bOnlyEdges = TRUE) %>%
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
      st_triangulate() %>%
      st_collection_extract("POLYGON")
    edges <- c(buf_poly %>%
                 st_cast("MULTIPOINT") %>%
                 st_cast("POINT"), aug_pts) %>%
      st_union() %>%
      st_triangulate(bOnlyEdges = TRUE) %>%
      st_cast("LINESTRING") %>%
      st_sf()
    crosses <- do.call(
      c, barrier %>%
        st_buffer(-1) %>%
        st_intersects(edges)
    )
    edges <- edges[-crosses, ]
  }
  if (!centroids & !augment) {
    edges <- buf_poly %>%
      st_cast("MULTIPOINT") %>%
      st_union() %>%
      st_triangulate(bOnlyEdges = TRUE) %>%
      st_cast("LINESTRING") %>%
      st_sf()
    crosses <- do.call(
      c, barrier %>%
        st_buffer(-1) %>%
        st_intersects(edges)
    )
    edges <- edges[-crosses, ]
  }
  sln <- suppressWarnings(sfnetworks::as_sfnetwork(edges,
                                                   directed = FALSE,
                                                   length_as_weight = TRUE))
  return(sln)
}

