##' Extract slope of transect along wind direction
##'
##'
##' @title
##' @param
##' @param
##' @return
##' @export
##'
slopeExtract <-  function(p, dm_proxy, dir_proxy, transect_distance = 5000, sample = 10, sf = TRUE) {
  pProj <- start %>% st_transform(st_crs(dir_proxy))
  st_extract(dir_proxy, pProj)[[1]]
  
  transect <- st_linestring(
    matrix(c(destPoint(p,  st_extract(dir_proxy, pProj)[[1]],               trans_dist/2),
             destPoint(p, (st_extract(dir_proxy, pProj)[[1]] + 180) %% 360, trans_dist/2)), ncol = 2, byrow = TRUE)) %>%
    st_sfc(crs = 4326) %>% st_transform(st_crs(dm_proxy)) %>%
    st_sample(size = sample, type = "regular") %>% st_cast("POINT") %>% st_sf()
  
  if(sf) {
    transect %>% mutate(dm    = st_extract(dm_proxy, transect)[[1]],
                        dir   = st_extract(dir_proxy, transect)[[1]],
                        slope = summary(lm(dm ~ c(1:nrow(transect))))$coefficients[2,1]) %>%
      dplyr::select(dm, dir, slope)
  } else {
    summary(lm(st_extract(dm_proxy, transect)[[1]] ~ c(1:nrow(transect))))$coefficients[2,1]
  }
}