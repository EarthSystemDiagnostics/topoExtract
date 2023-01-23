##' Extract interpolated values from raster along ice-flow trajectories
##'
##' Description ... .
##'
##' @title Extract interpolated values along ice-flow trajectories
##' @param p a two column matrix of (lon,lat) location.
##' @param ...
##' @return a data.frame with location, distance, elevation, speed and direction.
##' @export
topoExtract <- function(p, dm_proxy,  vel_proxy, traj_length_m, sampling_distance_m, interpolate = TRUE) {

  invisible(lapply(c("sf", "stars", "dplyr", "geosphere"), require, 
                 character.only = TRUE))
  invisible(sf_use_s2(FALSE))

  start <- st_point(p) %>% st_sfc() %>% st_set_crs(4326) 

  ## start point
  res <-   c(start %>% st_coordinates(),
             (st_extract(dm_proxy, start %>% st_transform(st_crs(dm_proxy)), bilinear = TRUE) %>% unlist())[1],
             (st_extract(vel_proxy, start %>% st_transform(st_crs(vel_proxy)), bilinear = TRUE) %>% unlist())) %>% 
                matrix(ncol = 5, nrow = 1) %>% as_tibble() %>% setNames(c("lon", "lat", "elev", "vx", "vy")) %>%
                mutate(dist = 0, dir = ((atan(vx / vy) * (180/pi)+ 360)) %% 360, speed = sqrt(vx^2 + vy^2))
  
  repeat{
    nextP <- destPoint(res[nrow(res), c("lon", "lat")] %>% as.matrix(), res$dir[nrow(res)], sampling_distance_m) %>% 
      st_point() %>% st_sfc() %>% st_set_crs(4326) 
    
    res <- res %>% rbind(
      c(nextP %>% st_coordinates(),
        (st_extract(dm_proxy, nextP %>% st_transform(st_crs(dm_proxy)), bilinear = TRUE) %>% unlist())[1],
        (st_extract(vel_proxy, nextP %>% st_transform(st_crs(vel_proxy)), bilinear = TRUE) %>% unlist())) %>% 
        matrix(ncol = 5, nrow = 1) %>% as_tibble() %>% setNames(c("lon", "lat", "elev", "vx", "vy")) %>%
        mutate(dist = res$dist[nrow(res)] + sampling_distance_m, dir = ((atan(vx / vy) * (180/pi)+ 360)) %% 360, speed = sqrt(vx^2 + vy^2))
    )
    
    if(res$dist[nrow(res)]>=traj_length_m) break
  }
  
  res

}
