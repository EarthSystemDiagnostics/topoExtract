##' Extract slope of transect along wind direction
##'
##' @title
##' @param
##' @param
##' @return
##' @export

slopeExtract <-  function(p, dm_proxy, dir_proxy, transect_distance = 5000, sample = 10, sf = TRUE) {
  
  require(geosphere)
  
  start <- st_point(p) %>% st_sfc() %>% st_set_crs(4326) 
  pProj <- start %>% st_transform(st_crs(dir_proxy))
  
  transect <- st_linestring(
    matrix(c(destPoint(p,  st_extract(dir_proxy, pProj)[[1]],               transect_distance/2),
             destPoint(p, (st_extract(dir_proxy, pProj)[[1]] + 180) %% 360, transect_distance/2)), ncol = 2, byrow = TRUE)) %>%
    st_sfc(crs = 4326) %>% st_transform(st_crs(dm_proxy)) %>%
    st_sample(size = sample, type = "regular") %>% st_cast("POINT") %>% st_sf()
  
  if(sf) {
    transect %>% mutate(dm    = st_extract(dm_proxy, transect)[[1]],
                        dir   = st_extract(dir_proxy, transect)[[1]],
                        slope = summary(lm(dm ~ seq(0, transect_distance, length = sample)))$coefficients[2,1]) %>%
                 dplyr::select(dm, dir, slope)
  } else {
    summary(lm(st_extract(dm_proxy, transect)[[1]] ~ c(1:nrow(transect))))$coefficients[2,1]
  }
}