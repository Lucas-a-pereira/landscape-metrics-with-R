#'---
#' Using maps of forest cover to calculate five landscape metrics: % of forest cover, number of patches,
#' edge density, splitting index, and mean patch area

# memory
rm(list = ls())

# packages
library(raster)
library(sf)
library(rgdal) 
library(rnaturalearth)
library(landscapemetrics)
library(landscapetools)
library(tidyverse)
library(tmap)
library(snow)

# raster options
raster::rasterOptions(maxmemory = 1e+200, chunksize = 1e+200)
raster::beginCluster(n = parallel::detectCores() - 6)

# directory
setwd("C:\\Users\\lucas\\OneDrive\\Documentos\\01_MASTER\\02_data\\09_africa_land_metrics_1km_5km")

# import data -------------------------------------------------------------
# limits
li <- rnaturalearth::countries110 %>%
  sf::st_as_sf()
li

# directory
setwd("05_africa_treecover_loss_extracted_buffers")

# list files
# threshold of 80% for neotropics and 70% for central africa
fi <- dir(pattern = "deforestation_threshold70_binary_cgs_wgs84.tif")
fi

# landscapes
la <- purrr::map(fi, raster::brick)
la

# map
plot(la[[1]])

# import points
setwd("C:\\Users\\lucas\\OneDrive\\Documentos\\01_MASTER\\01_raw")
po <- sf::st_read("01_africa\\comm_africa_data_2020_10_d06.shp", quiet = TRUE)
po

# plotting points in a map

tm_shape(li, bbox = po) +
  tm_polygons(po, col = "red", alpha = .5) +
  tm_shape(po) +
  tm_bubbles(size = .1, col = "red") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"))

utm_epsg <- sf::st_read("02_utm\\utm_zones_epsg.shp")
utm_epsg

tm_shape(li, bbox = po) +
  tm_polygons() +
  tm_shape(utm_epsg, bbox = po) +
  tm_borders() +
  tm_text("zone", size = .7, col = "blue") +
  tm_shape(po) +
  tm_bubbles(size = .1, col = "red") +
  tm_graticules(lines = FALSE) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"))

# metrics -----------------------------------------------------------------
# directory
setwd("C:\\Users\\lucas\\OneDrive\\Documentos\\01_MASTER\\02_data\\09_africa_land_metrics_1km_5km"); #dir.create("06_neotropics_landscape_metrics"); setwd("06_neotropics_landscape_metrics")

# metrics
metrics <- NULL

# for
for(i in 1:nrow(po)){
  
  # info
  print(i)
  
  # filter
  po_i <- po %>% 
    dplyr::slice(i) %>% 
    sf::st_join(utm_epsg)
  
  # project landscape
  la_i <- la[[i]] %>%
    raster::projectRaster(crs = po_i$prj4, res = 30, method = "ngb")
  
  # buffers
  for(j in seq(1000, 5000, 200)){
    
    # info
    print(paste0("Buffer ", j, " m"))
    
    # transform and buffer
    po_i_j <- po_i %>% 
      sf::st_transform(crs = po_i$epsg_code) %>% 
      sf::st_buffer(dist = j)
    
    # crop and mask
    la_i_j <- la_i %>%
      raster::crop(po_i_j) %>% 
      raster::mask(po_i_j)
    
    # values
    la_i_j_val <- la_i_j %>% 
      raster::freq() %>%
      tibble::as_tibble() %>% 
      dplyr::filter(value == 1)
    
    # 1. percentage of landscape (landscape composition - proportional abundance of the class "Forest Cover")
    if(nrow(la_i_j_val) == 1){
      pl <- landscapemetrics::lsm_c_pland(la_i_j) %>% 
        dplyr::filter(class == 1)
    } else{
      pl <- tibble::tibble(layer = i, 
                           level = "class",
                           class = 1,
                           id = NA,
                           metric = "pl",
                           value = 0)
    }
    
    # 2. number of patches (landscape configuration - aggregation metric (subdivision))
    if(nrow(la_i_j_val) == 1){
      np <-  landscapemetrics::lsm_c_np(la_i_j, directions = 8) %>% 
        dplyr::filter(class == 1)
    } else{
      np <- tibble::tibble(layer = i, 
                           level = "class",
                           class = 1,
                           id = NA,
                           metric = "np",
                           value = 0)
    }
    
    
    # 3. edge density (landscape configuration - aggregation metric (subdivision))
    if(nrow(la_i_j_val) == 1 & pl$value < 100){
      ed <- landscapemetrics::lsm_c_ed(la_i_j, directions = 8) %>% 
        dplyr::filter(class == 1)
    } else{
      ed <- tibble::tibble(layer = i, 
                           level = "class",
                           class = 1,
                           id = NA,
                           metric = "ed",
                           value = 0)
    }
    
    # 4. splitting index (landscape configuration - aggregation metric (subdivision)) 
    if(nrow(la_i_j_val) == 1){
      si <- landscapemetrics::lsm_c_split(la_i_j, directions = 8) %>% 
        dplyr::filter(class == 1)
    } else{
      si <- tibble::tibble(layer = i, 
                           level = "class",
                           class = 1,
                           id = NA,
                           metric = "si",
                           value = 0)
    }
    
    # 5. mean of patch area (landscape composition - area and edge metric)
    if(nrow(la_i_j_val) == 1){
      patch <- landscapemetrics::lsm_c_area_mn(la_i_j, directions = 8) %>%
        dplyr::filter(class == 1)
    } else{
      patch <- tibble::tibble(layer = i,
                           level = "class",
                           class = 1,
                           id = NA,
                           metric = "patch",
                           value = 0)
    }
    
    # map
    if(la_i_j[] %>% unique %>% na.omit %>% as.numeric %>% length == 2){
      pal <- c("palegreen", "forestgreen")
    }else if(la_i_j[] %>% unique %>% na.omit %>% as.numeric == 1){
      pal <- c("forestgreen")
    } else{
      pal <- c("palegreen")
    }
    
    pal
    
     map <- tm_shape(po_i_j) +
       tm_borders() +
       tm_shape(la_i_j) +
       tm_raster(style = "cat", pal = pal) +
       tm_shape(po_i_j) +
       tm_borders() +
       tm_layout(legend.show = FALSE) +
       tm_credits(paste0("co=", i,"; bf=", j, "; pl=", round(pl$value, 2), 
                         "; np=", round(np$value, 2), "; ed=", round(ed$value, 2), 
                         "; si=", round(si$value, 2), "; patch=", round(patch$value, 2)), size = 1,
                  position = c(.2, -.01))
     map
     tmap_save(map, paste0("map_com_", i, "_buf_", j, "m.png"))
    
    # combine
    metrics <- dplyr::bind_rows(pl, np, ed, si, patch) %>%
      dplyr::mutate(id = i, buffer = j) %>% 
      dplyr::select(id, buffer, class, metric, value) %>% 
      dplyr::bind_rows(metrics, .)
    
  }
  
}

readr::write_csv(metrics, "00_africa_land_metrics_2021_06_d19.csv")
