# Get all the data!
library(spocc)
library(robis)
library(rgbif)
library(sf)
library(rnaturalearth)
library(mregions)
library(rcanvec)
library(tidyverse)

# Get basic spatial info
latlong <- 4326
#statscan
proj <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=63.390675 +lon_0=-91.86666666666666 +x_0=6200000 +y_0=3000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

# This is how I got the EEZ in the first place
# EEZ <- mr_shp(key="MarineRegions:eez",maxFeatures = 12) %>%
#   st_as_sf() %>%
#   dplyr::filter(geoname=="Canadian Exclusive Economic Zone")
# st_write(EEZ,"EEZ.shp")

EEZ <- st_read("EEZ.shp") %>% 
  st_combine() %>% 
  st_transform(proj)


Canada <- ne_states(country = c("Canada"),
                    returnclass = "sf") %>% 
  st_combine() %>% 
  st_transform(proj)


# make a sampling grid
grid <- st_make_grid(st_union(st_as_sfc(st_bbox(EEZ)),
                      st_as_sfc(st_bbox(Canada))),n = 100) %>% 
  st_as_sf() %>% 
  mutate(canada=lengths(st_intersects(.,Canada))>0,
         eez=lengths(st_intersects(.,EEZ))>0) %>% 
  filter(eez|canada)


ggplot(grid)+
  geom_sf(data=Canada,fill='lightgreen')+
  # geom_sf(data=EEZ,fill='lightblue')+
  geom_sf(fill='transparent')+
  geom_sf(data=grid[193,],fill="red")

g=193

cell <- st_transform(grid[g,],latlong)

NTSs <- nts(bbox=sp::bbox(as(cell,"Spatial")))

canvec.download(NTSs)

freshwater <- c(canvec.load(NTSs,"waterbody"),
                 canvec.load(NTSs,"river"),
                 canvec.load(NTSs,"string_bog"),
                 canvec.load(NTSs,"wetland"),
                 canvec.load(NTSs,"palsa_bog"),
                 canvec.load(NTSs,"tundra_pond")) %>% 
  lapply(st_as_sf) %>%
  lapply(function(x) as.data.frame(x) %>% select(geometry)) %>% 
  bind_rows() %>% 
  st_as_sf(crs="+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>% 
  st_combine() %>%
  st_union()%>%
  st_transform(proj) %>% 
  st_buffer(1) %>% 
  st_crop(grid[g,]) %>% 
  st_transform(latlong)


wet <- EEZ %>% 
  st_crop(grid[g,]) %>% 
  st_transform(latlong) %>%
  st_union(.,freshwater) 

simplewet <- wet%>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 0.000001)

ggplot(cell)+
  geom_sf(fill='transparent',colour='yellow')+
  geom_sf(data=wet,fill='black',colour='black')+
  geom_sf(data=simplewet,fill='red',colour='red')+
  coord_sf(xlim = c(-65.43,-65.42), ylim = c(43.625,43.63))



# query the databases -----------------------------------------------------


obis <- robis::occurrence(geometry = st_as_text(cell$x)) 


gbif <- data.frame()
while(nrow(gbif)%%500==0){
  gbif <- bind_rows(gbif,
                    rgbif::occ_search(geometry = st_as_text(cell$x),
                            basisOfRecord="OBSERVATION",
                            return='data',
                            start=nrow(gbif),
                            limit=500))
  print(nrow(gbif))
}

inat <- data.frame()
while(nrow(inat)%%30==0){
  inat <- bind_rows(inat,
                    spocc::occ(from=c("inat"),
                               geometry= st_as_text(cell$x),
                               page=nrow(inat)/30+1)$inat$data[[1]])
  print(nrow(inat))
}


# clean the data ----------------------------------------------------------


occ <- rbind(obis %>% 
               st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=latlong) %>%
               mutate(link=paste0('https://obis.org/dataset/',dataset_id)) %>% 
               select(scientificName,link),
             gbif %>% 
               st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=latlong) %>%
               mutate(link=paste0('https://www.gbif.org/occurrence/',gbifID)) %>% 
               select(scientificName,link),
             inat %>%
               filter(!is.na(longitude),
                      !is.na(latitude),
                      !is.na(name),
                      quality_grade=='research') %>% 
               st_as_sf(coords=c("longitude","latitude"),crs=latlong) %>%
               mutate(link=paste0('https://www.inaturalist.org/observations/',id),
                      scientificName=name) %>% 
               select(scientificName,link)) 

aquaticsp <- occ %>%
  mutate(aquatic=lengths(st_covered_by(.,wet))>0) %>% 
  select() %>% 
  unique() %>% 
  group_by(scientificName) %>% 
  summarize(geometry=st_combine(geometry)) %>% 
  mutate(aquatic=lengths(st_intersects(.,wet))>0) 

start <- Sys.time()
test <- (gbif %>% 
  st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=latlong) %>%
  mutate(link=paste0('https://www.gbif.org/occurrence/',gbifID)) %>% 
  select()) %>% 
  unique() %>%
  mutate(aquatic=lengths(st_covered_by(.,wet))>0)
Sys.time()-start

test <- obis %>% 
  st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=latlong) %>%
  mutate(link=paste0('https://obis.org/dataset/',dataset_id),
         aquatic=TRUE) %>% 
  select(scientificName,link,aquatic)



start <- Sys.time()
test <- obis %>% 
  st_as_sf(coords=c("decimalLongitude","decimalLatitude"),crs=latlong) %>%
  mutate(link=paste0('https://obis.org/dataset/',dataset_id)) %>% 
  select(scientificName) %>% 
  unique() %>% 
  group_by(scientificName) %>% 
  summarize(geometry=st_combine(geometry))
test <- test %>% 
           mutate(aquatic=lengths(st_intersects(.,wet))>0)
Sys.time()-start



ggplot(cell)+
  geom_sf(fill='transparent',colour='yellow')+
  geom_sf(data=simplewet,fill='lightblue',colour='transparent')+
  geom_sf(data=test,aes(fill=aquatic,colour=aquatic))

require(leaflet)
leaflet::leaflet(wet %>% st_transform(latlong)) %>% 
  addTiles() %>% 
  addPolygons()
