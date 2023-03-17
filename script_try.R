library(raster)
library(dplyr)
library(stars)
library(geosphere)
library(tidyverse)
library(parallel)

dm_proxy  <- read_stars("~/Documents/data/REMA_200m_dem_filled.tiff", proxy = T) %>%
  st_set_crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
plot(dm_proxy)

vel_proxy <- stars::read_stars("~/Documents/data/antarctic_ice_vel_phase_map_v01.tif", proxy = T)          
plot(vel_proxy)

# wnd_proxy <- stack(raster("~/Documents/data/wind.avg.antarctica.nc", varname = "u10"),
#                    raster("~/Documents/data/wind.avg.antarctica.nc", varname = "v10")) %>% st_as_stars() %>%
#               split() %>% setNames(c("u", "v")) %>% merge() %>%
#               st_warp(st_warp(dm_proxy, cellsize = 65000, use_gdal = TRUE))
# 
# dir_proxy <- wnd_proxy %>% st_apply(., 1:2, function(x) (atan2(x[1], x[2]) * (180/pi) + 360) %% 360) %>%
#                 st_warp(.,dm_proxy %>% st_as_stars(), crs = st_crs(wnd_proxy), use_gdal = TRUE, method = "bilinear")
# write_stars(dir_proxy %>% st_as_stars(), "~/Documents/data/dir_200m.tif")

dir_proxy <- read_stars("~/Documents/data/dir_200m.tif", proxy = T)
# plot(dir_proxy)


## Start point
p <- matrix(c(0, -75), ncol = 2, nrow = 1)
start <- st_point(p) %>% st_sfc() %>% st_set_crs(4326)

# ## wind direction transect
# windTrans <- function(p, wnd_proxy, trans_dist = 50000, trans_smp  = 1000) {
#   
#   wnd_extr <- st_extract(wnd_proxy, st_point(p) %>% st_sfc() %>% st_set_crs(4326) %>% st_transform(st_crs(wnd_proxy))) %>% unlist()
#   wnd_dir  <- (atan2(wnd_extr[1], wnd_extr[2]) * (180/pi) + 360) %% 360
#   
#   matrix(c(destPoint(p, wnd_dir, trans_dist/2),
#            destPoint(p, (wnd_dir + 180) %% 360, trans_dist/2)), ncol = 2, byrow = TRUE) %>%
#     st_linestring() %>% st_sfc() %>% st_set_crs(4326) %>% st_transform(st_crs(vel_proxy)) %>% 
#     st_sample(trans_dist/trans_smp, type = "regular") %>% st_cast("POINT")
#   
# }
# 
# trans <- windTrans(p, wnd_proxy)
# plot(trans)
# 
# ## topo slope along wind transect
# topoSlope <- function(trans, dm_proxy) {
#   slp_extract <- st_extract(dm_proxy, trans %>% st_transform(st_crs(dm_proxy))) %>% st_drop_geometry() %>% unlist()
#   summary(lm(slp_extract~c(1:length(trans))))$coefficients[2,1]
# }
# 
# topoSlp <- topoSlope(trans, dm_proxy) 
# 
# dm_aggr <- dm_proxy %>% st_warp(cellsize = 10000, crs = st_crs(dm_proxy), use_gdal = TRUE)
# plot(dm_aggr)
# 
# library(parallel)
# 
# crdsTab <- dm_aggr %>% st_transform(4326) %>% as_tibble() %>%
#   setNames(c(c("x", "y", "vals"))) %>% filter(!is.na(vals)) %>% dplyr::select(-vals) %>%
#   split(1:nrow(.)) %>% mclapply(., function(x) {
#     trans   <- windTrans(as.numeric(x), wnd_proxy)
#     tibble(x, slope = topoSlope(trans, dm_proxy))
#   }, mc.cores = 5) %>% do.call("rbind",.) %>% 
#   st_as_sf(coords = c("x", "y")) %>% st_set_crs(4326)
# 
# plot(crdsTab %>% st_transform(st_crs(dm_aggr)) %>% st_rasterize(dm_aggr), axes = TRUE, col = rainbow(100))

bbox <- start %>% st_transform(st_crs(dm_proxy)) %>% st_buffer(250000)
grid <- st_make_grid(bbox, cellsize = 25000)
plot(grid)

# dm_aggr_all <- dm_proxy[bbox] %>% st_as_stars()
# plot(dm_aggr_all, axes = T)



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


pt  <- start %>% st_transform(st_crs(dm_proxy)) %>% st_buffer(40000)
pts <- dm_proxy[pt] %>% st_as_stars()
plot(pts)

crds       <- st_coordinates(pts %>% st_transform(4326))
crds$slope <- parallel::mclapply(1:nrow(crds), function(x) slopeExtract(crds[x,], dm_proxy, dir_proxy, sf = FALSE), 
                                mc.cores = 10) %>% unlist()

slope_stars <- crds %>% st_as_sf(coords = c("x", "y"), crs = 4326) %>% st_transform(st_crs(pts)) %>%
  st_rasterize(template = pts)

ggplot() +
  geom_stars(data = slope_stars) +
  scale_fill_continuous(type = "viridis", name = "slope") +
  theme_minimal()