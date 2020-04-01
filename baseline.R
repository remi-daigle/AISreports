library(AIScanR)
library(spocc)
library(robis)
library(rgbif)
library(sf)
library(rnaturalearth)
library(mregions)
library(rcanvec)
library(parallel)
library(tidyverse)


# Setting up basic geography ----------------------------------------------

# This project uses both a latitude/longitude coordinate reference system and [Statistics Canada Lambert](https://spatialreference.org/ref/epsg/nad83-statistics-canada-lambert/) projection. Let's set this up below for ease of use.

latlong <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#statscan
proj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "


# EEZ ---------------------------------------------------------------------


# The EEZ is from [Marineregions.org](http://www.marineregions.org/) acquired via the `mregions` package. The code below only needs to be run if you do not yet have a locally saved version.
# 
# EEZ <- mr_shp(key="MarineRegions:eez",maxFeatures = 12) %>%
# st_as_sf() %>%
# dplyr::filter(geoname=="Canadian Exclusive Economic Zone")
# st_write(EEZ,"data/EEZ.shp")
# 
# Instead of the above, we load the local copy.

EEZ <- st_read("data/EEZ.shp") %>% 
  st_combine() %>% 
  st_transform(proj)



# Landmass ----------------------------------------------------------------


# The Canadian landmass is from [Natural Earth](https://www.naturalearthdata.com/) acquired via the `rnaturalearth` package

Canada <- ne_states(country = c("Canada"),
                    returnclass = "sf") %>% 
  st_combine() %>% 
  st_transform(proj)


# Sampling grid -----------------------------------------------------------


# Since sampling for Canada as a whole in one shot would actually break either your computer or the internet, we will divide up the process into a spatial grid and do it cell by cell.
# 
# grid <- st_make_grid(st_union(st_as_sfc(st_bbox(EEZ)),
#                               st_as_sfc(st_bbox(Canada))),n = 150) %>%
#   st_as_sf() %>%
#   mutate(canada=lengths(st_intersects(.,Canada))>0,
#          eez=lengths(st_intersects(.,EEZ))>0) %>%
#   filter(eez|canada)
# st_write(grid,"data/grid.shp")
# 
# # fill in missing grid cells from very big lakes
# missinggrid <- ((st_make_grid(st_union(st_as_sfc(st_bbox(EEZ)),
#                               st_as_sfc(st_bbox(Canada))),n = 150) %>%
#   st_as_sf())[c(92,93,243,244,394,395,547,698,841,991,1148,1291,1299:1301,1441,1452,1590:1592,1603,1740,1741,1744,1889,1890,1893,2043,2335,2634,2781:2784,2927,2930:2932,3078:3081,3229:3231,5461,5462,5761,10393,10543,10544,13092,13391,13392),])%>%
#   mutate(canada=1,
#          eez=1)%>%
#   mutate(geometry=x) %>%
#   st_set_geometry("geometry") %>%
#   select(-x)
# 
# erie <- rbind(missinggrid[1,],grid[2,]) %>%
#   mutate(geometry=geometry - c(0,698046.2-664912.8)) %>%
#   st_set_crs(st_crs(missinggrid))
# 
# newgrid <- rbind(grid,missinggrid,erie)
# 
# st_write(newgrid,"data/grid.shp")

# But again to save time, instead of running the above, let's just load the file it generates

grid <- st_read("data/grid.shp")


# Geography Map -----------------------------------------------------------



# Let's see what we've done so far and identify a 'test' grid cell (red) to demonstrate the effectiveness of the functions below.
# 
# g <- 339
# ggplot(grid)+
#   geom_sf(data=Canada,fill='lightgreen')+
#   geom_sf(data=EEZ,fill='lightblue')+
#   geom_sf(fill='transparent')+
#   geom_sf(data=grid[g,],fill='red')



# Querying and cleaning ---------------------------------------------------



# This will be a giant process in a loop, so to make it readable, tractable, and fixable, lets test some functions from the AIScanR package to break up the process. 

## iswet

# The `iswet` will create a polygon delineating which parts of a grid cell are 'wet' (i.e. in the ocean, lake, wetland, etc), and provides a 1m buffer around all such features

# wet <- iswet(grid,g,EEZ,latlong,proj)
# 
# 
# ggplot(grid[g,])+
#   geom_sf(fill='transparent',colour='yellow')+
#   geom_sf(data=wet,fill='lightblue',colour='lightblue')




## getdata

# The `getdata` function will query the Ocean Biogeographic Information System ([OBIS](https://obis.org/)), the Global Biodiversity Information Facility ([GBIF](https://www.gbif.org/)), and [iNaturalist](https://inaturalist.ca)


# occ <- getdata(grid, g,latlong)
# 
# head(occ)


## isaquatic

# test <- occ %>% 
#   left_join(isaquatic(occ,wet),by="scientificName")
# 
# 
# ggplot(grid[g,])+
#   geom_sf(fill='transparent',colour='yellow')+
#   geom_sf(data=wet,fill='lightblue',colour='transparent')+
#   geom_sf(data=test, aes(fill=aquatic,colour=aquatic))




# Loop: query, clean, repeat ----------------------------------------------



# # temporarily restrict to NS
NS <- ne_states(country = c("Canada"),
                returnclass = "sf") %>%
  filter(name_en=="Nova Scotia") %>%
  st_transform(proj) %>%
  st_buffer(1000000)

ns <- (lengths(st_intersects(grid,NS))>0) %>%
  which()



wetfiles <- list.files(path="data/wet",pattern=".shp") %>%
  gsub(".shp","",.) %>%
  gsub("wet_","",.) %>%
  as.numeric()

grid <- grid %>%
  mutate(done=row.names(.) %in% wetfiles,
         ns=row.names(.) %in% ns)

ggplot(grid)+
  geom_sf(data=Canada,fill='lightgreen')+
  geom_sf(data=EEZ,fill='lightblue')+
  geom_sf(aes(fill=done))

no_cores <- detectCores()
clust <- makeCluster(no_cores/2)
clusterExport(clust,"grid")
clusterExport(clust,"EEZ")
clusterExport(clust,"latlong")
clusterExport(clust,"proj")
clusterExport(clust,"Canada")

x=clusterApply(clust,as.numeric(row.names(grid)), function(g) {
  if(sum(match(paste0("occ_",sprintf("%05d",g),"_",format(seq(Sys.time()-30*24*3600,Sys.time(),24*3600),"%Y_%m_%d"),".rds"),list.files("data/occurences/")),na.rm=TRUE)==0){
    try({
      wet <- AIScanR::iswet(grid,g,EEZ,latlong,proj)
      # occ <- getdata(grid, g,latlong)
      # isaquatic(occ,wet)
      # saveRDS(occ,paste0("data/occurences/occ_",sprintf("%05d",g),"_",format(Sys.time(), "%Y_%m_%d"),".rds"))
    
    })
    
    return("done")
  } else{
    return("already done")
  }}
) %>% unlist
parallel::stopCluster(clust)

files <- file.info(paste0(path = "data/wet/",list.files(path = "data/wet/",pattern = "shp"))) %>% 
  mutate(fn=gsub("data/wet/","",row.names(.))) %>% 
  filter(size<=100) 

removeme <- lapply(files$fn,
               function(x) list.files(path = "data/wet/",
                                      pattern=gsub("shp","",x))) %>% 
  unlist()
