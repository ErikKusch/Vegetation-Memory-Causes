rm(list = ls()) # clearing environment
####--------------- PACKAGES ----------------------------------------------------
if("KrigR" %in% rownames(installed.packages()) == FALSE){ # KrigR check
  devtools::install_github("https://github.com/ErikKusch/KrigR")
}else{ 
  library(KrigR) 
}

####--------------- DIRECTORIES -------------------------------------------------
mainDir <- getwd() # extract the project folder location
# WORKING DIRECTORY FOR DATA
Dir.Data <- paste(mainDir, "/X - Data", sep="")
# WORKING DIRECTORY FOR RAW GIMMS DATA
Dir.EVI <- paste(Dir.Data, "/1 - EVI", sep="")
# WORKING DIRECTORY FOR RAW ERA5 DATA
Dir.ERA <- paste(Dir.Data, "/2 - Climate", sep="")
if(!dir.exists(Dir.ERA)){dir.create(Dir.ERA)}
# WORKING DIRECTORY FOR KRIGING COVARIATES
Dir.COV <- paste(Dir.Data, "/3 - Covariates", sep="")
if(!dir.exists(Dir.COV)){dir.create(Dir.COV)}
# WORKING DIRECTORY FOR MEMORY EFFECT DATA
Dir.Memory <- paste(Dir.Data, "/4 - Memory_Effects", sep="")
if(!dir.exists(Dir.Memory)){dir.create(Dir.Memory)}
# WORKING DIRECTORY FOR COMPADRE DATA
Dir.Compadre <- paste(Dir.Data, "/5 - COMPADRE", sep="")
# WORKING DIRECTORY FOR TRY PFT DATA
Dir.TRY <- paste(Dir.Data, "/5 - PFTs", sep="")
# WORKING DIRECTORY FOR SHAPEFILES (contains masking file for water bodies)
Dir.Mask <- paste(Dir.Data, "/6 - ShapeFiles", sep="")
if(!dir.exists(Dir.Mask)){dir.create(Dir.Mask)}

####--------------- CHECKS & PREPARATIONS ------------------------------------------

source("PersonalSettings.R") # I do this here to specify number of cores and API credentials and am thus not sharing this file

#### CDS API (needed for ERA5-Land downloads)
if(!exists("API_Key") | !exists("API_User")){ # CS API check: if CDS API credentials have not been specified elsewhere
  API_User <- readline(prompt = "Please enter your Climate Data Store API user number and hit ENTER.")
  API_Key <- readline(prompt = "Please enter your Climate Data Store API key number and hit ENTER.")
} # end of CDS API check

#### NUMBER OF CORES
if(!exists("numberOfCores")){ # Core check: if number of cores for parallel processing has not been set yet
  numberOfCores <- readline(prompt = paste("How many cores do you want to allocate to these processes? Your machine has", parallel::detectCores()))
} # end of Core check

#### LAND MASK (for masking species in the sea which are terrestrial and marine)
if(!file.exists(file.path(Dir.Mask, "LandMask.zip"))){ # if land mask has not been downloaded yet
  download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_land.zip", destfile = paste(Dir.Mask, "LandMask.zip", sep="/")) # download cultural vector
  unzip(paste(Dir.Mask, "LandMask.zip", sep="/"), exdir = Dir.Mask) # unzip the data
}
Land_shp <- raster::shapefile(file.path(Dir.Mask , "ne_10m_land.shp"))

#### DRYLAND MASK
if(!file.exists(file.path(Dir.Mask, "systems_dryland"))){
  stop("You don't have the Millenium Ecosystem Assessment Dryland shapefile yet. Please download it at https://sedac.ciesin.columbia.edu/data/set/ma-ecosystems/data-download and extract the folder 'systems_dryland' into ./X - Data/6 - ShapeFiles.")
}
Drylands_shp <- raster::shapefile(file.path(Dir.Mask, "systems_dryland", "dryland_2"))

####--------------- VEGETATION DATA ------------------------------------------------
FUN_EVI <- function(){
  setwd(Dir.EVI) # set working directory to MOD13A2 folder, the .tiff files therein have been downloaded via the Google Earth Engine (GEE), there should be 874
  GEE_fs <- list.files(pattern = ".tif") # identify all files with a .tif ending
  Dates_vec <- gsub("-.*.", "", GEE_fs) # retain only dates of file names (YYYY_MM_DD)
  Fails_Doubles <- names(table(Dates_vec)[which(table(Dates_vec) != 2)]) # identify all dates for which we don't have exactly 2 .tif files (which is what we get from GEE)
  if(length(Fails_Doubles) > 0){ # fail check: if we don't have exactly 2 .tif files for each date
    stop(paste("You are missing one of the files associated with the following time steps:", paste(Fails_Doubles, collapse = ",")))
  } # end of fail check
  Dates_vec <- unique(Dates_vec) # reduce Dates_vec to singular mentions of each date
  Dir.Fixed <- file.path(Dir.EVI, "FIXEDEVI") # create a working directory for the fixed MOD13A2 files, this is needed in case FUN_EVI gets interrupted and needs to be started again
  dir.create(Dir.Fixed) # create Dir.Fixed on hard drive
  cl <- makeCluster(numberOfCores) # Assuming X node cluster
  registerDoParallel(cl) # registering cores
  foreach(MOD_Iter = 1:length(Dates_vec)) %dopar% { # loop: over all Dates in the MOD13A2 data
    Iter_fs <- GEE_fs[startsWith(x = GEE_fs, Dates_vec[MOD_Iter])] # identify the files for this date
    West <- raster::raster(Iter_fs[1]) # load the bigger, western .tif
    East <- raster::raster(Iter_fs[2]) # load the smaller, eastern .tif
    Mosaic_ras <- raster::mosaic(West, East, fun = mean) # fuse the two rasters
    MODIS_ras <- raster::projectRaster(from = Mosaic_ras, crs = "+proj=longlat +datum=WGS84 +no_defs") # change projection from sinusodial to unprojected (like ERA data)
    raster::values(MODIS_ras)[which(raster::values(MODIS_ras) < 0)] <- NA # Cloud cover is identified with negative values in the MOD13A2 data set. We set these to NA
    raster::writeRaster(MODIS_ras, filename = file.path(Dir.Fixed, Dates_vec[MOD_Iter]), format = "GTiff", overwrite = TRUE) # write the new, fixed raster to the directory for fixed .tifs
    unlink(x = Iter_fs, recursive = TRUE) # delete GEE .tifs from the hard drive to save space
  } # end of loop
  stopCluster(cl)
  ## file.copy() from Dir.Fixed
  ## unlink Dir.Fixed
} # end of FUN_EVI

if(){ # check for data product being there, length(Dates) == length(unique(Dates))
  # execute FUN_EVI
} 


####--------------- CLIMATE DATA ---------------------------------------------------
AT_1 <- KrigR::download_ERA(Variable = "2m_temperature",
                    Type = "reanalysis",
                    DataSet = "era5-land",
                    DateStart = "2001-01-01",
                    DateStop = "2004-12-31",
                    TResolution = "day",
                    TStep = 16,
                    Extent = extent(-180, 180, -90, 90),
                    Dir = Dir.ERA,
                    FileName = "2m_temperature_raw.nc",
                    API_User = API_User,
                    API_Key = API_Key)

### repeat this a few times until full 01_01-2001 to 31_12_2019 coverage is established
extent(train) <- extent(-180,180,-90,90)

target <- raster(file.path(Dir.EVI, "EVI_Reference.nc"))
#extent(target) <- extent(-180,180,-90,90)

# Select an extent for a tile
Extents <- list()
res_tiles <- 2
Lat_Tiles <- (160-res_tiles)/res_tiles # this is 90 and -90 divided by bands of 10 degrees. This tells us how many bands we need
Lon_Tiles <- (360-res_tiles)/res_tiles 
z <- 1
for(i in 0:Lat_Tiles){
  for(j in 0:Lon_Tiles){
    Extents[[z]] <- extent(extent(-180+res_tiles*j,
                                  -180+res_tiles*(j+1),
                                  -60+res_tiles*i,
                                  -60+res_tiles*(i+1)))
    z <- z + 1
  }
}
Tiles <- (Lat_Tiles+1)*(Lon_Tiles+1) -1 

Regions = as.list(rep("GlobalKrig", length(Extents)))
RegionFiles = as.list(paste("BandName_", seq(1,Tiles+1,1), sep=""))

start <-  Sys.time()
for(Krig_Iter in 1:length(Extents)){
  # specification for KrigR() here indexing filenames as RegionFiles[[Krig_Iter]] and extents as Extents[[Krig_Iter]] for each run
  suppressWarnings(krigR)
  cropped_train <- crop(train, Extents[[Krig_Iter]])
  # cropped_target <- crop(target, Extents[[Krig_Iter]])
  cropped_shp <- crop(shp,Extents[[Krig_Iter]])
  
  extent(cropped_train) <- Extents[[Krig_Iter]]
  #extent(cropped_target) <- Extents[[Krig_Iter]]
  # res(cropped_target) <- c(0.0833333,0.0833333)  
  Covs_ls <- download_DEM(
    Train_ras = cropped_train,
    Target_res = 0.01, #cropped_target,
    Shape = cropped_shp, #NULL, # this is the default and does not need to be specified
    Dir = getwd(), # this is the default and does not need to be specified
    Keep_Temporary = TRUE 
  )
  
  Covs_train <- Covs_ls[[1]]
  Covs_target <- Covs_ls[[2]]
  # !all(is.na(c(NA, cropped_shp))) & 
  if ( length(which(!is.na(values(cropped_train)))) > 5  ){
    krigR(
      Data = cropped_train,
      Covariates_coarse = Covs_train, # What's my name? - TschickyTschicky
      Covariates_fine = Covs_target,   # Say my name! - Slim Shady
      KrigingEquation = "ERA ~ DEM",  # this is the default and does not need to be specified
      Cores = 1, # you set this to detectcores() - That only makes sense for kriging of rasters with multiple layers. Your test data has only one layer hence you gain nothing by parallel processing but lose the progress bar
      Dir = getwd(),  # this is the default and does not need to be specified
      FileName = RegionFiles[[Krig_Iter]], # you left this empty, but it is needed
      Keep_Temporary = FALSE  # this is the default and does not need to be specified
      # You had specified far more arguments here. These are only used for the full pipeline style analysis which we don't recommend. see ?krigR
    )
  } else {print(paste0("Skipping..",Krig_Iter))}
}

endtime <-  Sys.time()
duration  <- endtime - start
