# topoExtract

The aim of topoExtract is to extract interpolated elevation values from a digital elevation model along a trajectory defined by ice-flow vectors.

## Datasets
### REMA Mosaic v1.1 200m (filled)

The Reference Elevation Model of Antarctica (REMA) is a high resolution, time-stamped Digital Surface Model (DSM) of Antarctica.
Downloaded from [here](https://data.pgc.umn.edu/elev/dem/setsm/REMA/mosaic/v1.1/200m/)

### MEaSUREs Phase-Based Antarctica Ice Velocity Map, Version 1 (450 m)
This data set, as part of the NASA Making Earth System Data Records for Use in Research Environments (MEaSUREs) Program, combines interferometric phases from multiple satellite interferometric synthetic-aperture radar systems to derive the first comprehensive phase-based map of Antarctic ice velocity. 
Downloaded from [here](https://n5eil01u.ecs.nsidc.org/MEASURES/NSIDC-0754.001/)

The downloaded dataset containts 10 variables in a netcdf format. I extracted vx and vz values and transferred them into a geotif for better and faster data handling with the R package `stars`.

``` r
library(stars)

nc_file <- read_ncdf("antarctic_ice_vel_phase_map_v01.nc", var = c("VX", "VY"), 
                     proxy = FALSE)
write_stars(nc_file, "antarctic_ice_vel_phase_map_v01.tif")
```

## Loading datasets as stars_proxy object

``` r
library(dplyr)

dm_proxy  <- read_stars("REMA_200m_dem_filled.tiff", proxy = T) %>%
                st_set_crs("+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
vel_proxy <- stars::read_stars("antarctic_ice_vel_phase_map_v01.tif", proxy = T)                 

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(name == "Antarctica")
  
  mp <- ggplot(data = world %>% st_transform(st_crs(dm_proxy))) +
    geom_sf(fill = NA, colour = NA) +
    geom_sf(fill = NA, colour = "cornflowerblue") +
    theme_light() +
    scale_x_continuous(name="") +
    scale_y_continuous(name="") +
    theme(axis.text = element_text(size = 10))
  
  pl1 <- mp +
          geom_stars(data = dm_proxy %>% setNames("Elevation"), 
             downsample = 35, na.action = na.omit)
  pl2 <- mp + 
          geom_stars(data = vel_proxy[,,,1] %>% setNames("VX"), 
            downsample = 35, na.action = na.omit) +
          scale_fill_continuous(type = "viridis")
  pl3 <-  mp + 
          geom_stars(data = vel_proxy[,,,2] %>% setNames("VY"), 
                     downsample = 35, na.action = na.omit) +
          scale_fill_continuous(type = "viridis")
  
  ggpubr::ggarrange(pl1, pl2, pl3, labels = c("A", "B", "C"),
                    ncol = 3, nrow = 1)
```

<center>

<img src="img/img1.png"></img>

<figcaption>

Figure 1: The three datasets: A) Elevation, B) Ice velocity in x, C) Ice velocity in y.

</figcaption>

</center>