setwd("Q:/Backscatter_coefs/backscatter_EGC")

filenames<- list.files()[-14]
filelist<- lapply(filenames,function(x)read.csv(x))
library(plyr)
filelist_dat<- ldply(filelist)

library(sf)
library(dplyr)


names(hydrometa_data)[c(6,7)]<- c("GPS_LONG","GPS_LAT")

backscatter_data<- filelist_dat[,c("bbp470","bbp532","bbp650","GPS_LONG","GPS_LAT")]
# Convert both data frames into spatial sf objects (WGS84 projection EPSG:4326)
# Remove NAs in coordinates
backscatter_dt <- na.omit(backscatter_data)

# 2. Extract numeric matrix of coordinates (X, Y)
coords_hydro <- as.matrix(hydrometa_data[,c("GPS_LONG","GPS_LAT")])
coords_back  <- as.matrix(backscatter_dt[,c("GPS_LONG","GPS_LAT")])

# 3. Find the nearest neighbor in backscatter for every hydrometa point
# k = 1 searches for the 1 nearest neighbor
# searchtype = "radius" restricts search to a max distance (in degrees/units of coordinates)
max_dist_deg <- 10# Approx ~500m in degrees depending on latitude
library(RANN)
nn_res <- nn2(
  data = coords_back ,       # Reference points
  query =  coords_hydro,      # Points to search for
  k = 1,
  searchtype = "radius",
  radius = max_dist_deg
)

# 4. Filter original hydrometa table
# nn_res$nn.idx > 0 indicates a match was found within radius
filtered_hydrometa <- hydrometa_data[nn_res$nn.idx[, 1] > 0, ]
filtered_backscatter <- backscatter_data[nn_res$nn.idx[, 1] > 0, ]

#merge
library(dplyr)

# Standard left join on both coordinates
merged_df <- merge(
  filtered_backscatter,
  filtered_hydrometa,
  by = c("GPS_LONG", "GPS_LAT"),
  all.x = TRUE
)
