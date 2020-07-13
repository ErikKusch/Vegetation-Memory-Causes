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

#### GEE MOD13A2 CHECK
setwd(Dir.EVI)
GEE_fs <- list.files(pattern = ".tif") # identify all files with a .tif ending
Dates_vec <- gsub("-.*.", "", GEE_fs) # retain only dates of file names (YYYY_MM_DD)
if(length(Dates_vec) == 0){ # GEE Check: if no .tifs are present in EVI directory
  stop("Please download the MOD13A2 data set using the Google Earth Engine (GEE) with the following code:
   var batch = require('users/fitoprincipe/geetools:batch');
   var dataset = ee.ImageCollection('MODIS/006/MOD13A2')
   .filterDate('2001-01-01', '2020-01-01');
   var EVI = dataset.select('EVI');
   batch.Download.ImageCollection.toDrive(EVI, 'MOD13A2_EVI');
and export the .tif files produced into ./X - Data/1 - EVI ")
} # end of GEE Check

####--------------- VEGETATION DATA ------------------------------------------------
FUN_EVI <- function(){
  setwd(Dir.EVI)
  GEE_fs <- list.files(pattern = ".tif") # identify all files with a .tif ending
  Dates_vec <- gsub("-.*.", "", GEE_fs) # retain only dates of file names (YYYY_MM_DD)
  Fails_Doubles <- names(table(Dates_vec)[which(table(Dates_vec) != 2)]) # identify all dates for which we don't have exactly 2 .tif files (which is what we get from GEE)
  if(length(Fails_Doubles) > 0){ # fail check: if we don't have exactly 2 .tif files for each date
    stop(paste("You are missing one of the files associated with the following time steps:", paste(Fails_Doubles, collapse = ",")))
  } # end of fail check
  Dates_vec <- unique(Dates_vec) # reduce Dates_vec to singular mentions of each date
  Dir.Fixed <- file.path(Dir.EVI, "FIXEDEVI") # create a working directory for the fixed MOD13A2 files, this is needed in case FUN_EVI gets interrupted and needs to be started again
  dir.create(Dir.Fixed) # create Dir.Fixed on hard drive
  cl <- parallel::makeCluster(numberOfCores) # Assuming X node cluster
  doParallel::registerDoParallel(cl) # registering cores
  foreach::foreach(MOD_Iter = 1:length(Dates_vec)) %dopar% { # loop: over all Dates in the MOD13A2 data
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
  file.copy(from = file.path(Dir.Fixed, list.files(Dir.Fixed)), to = file.path(Dir.EVI, list.files(Dir.Fixed))) # copy fixed output to evi directory
  unlink(Dir.Fixed, recursive = TRUE) # remove fixed directory
} # end of FUN_EVI
setwd(Dir.EVI)
Dates_vec <- gsub("-.*.", "", list.files(pattern = ".tif")) # retain only dates of file names (YYYY_MM_DD)
while(length(Dates_vec) != length(unique(Dates_vec))){ # MOD13A2 Product Check: check if all MOD13A2 data is present
  print("Merging MOD13A2 files from GEE now.")
  try(FUN_EVI()) # try because it stumbles on saving files here and there and needs to be restarted
} # end of MOD13A2 Product Check
setwd(mainDir)

####--------------- CLIMATE DATA DOWNLOAD ------------------------------------------
FUN_DownloadCLIM <- function(Var_long = "2m_temperature", Var_short = "AT"){
  setwd(Dir.EVI)
  Dates_vec <- c(gsub(pattern = "2001", replacement = "2000", x = Dates_vec[startsWith(x = Dates_vec, prefix = "2001")]), gsub("-.*.", "", list.files(pattern = ".tif")))
  Dates_vec2 <- gsub(pattern = "_", replacement = "-", x = Dates_vec) # change underscores to dashes for easier conversion to date format
  looptext <- "Clim_ras <- KrigR::download_ERA(Variable = Var_long,
                                    Type = 'reanalysis',
                                    DataSet = 'era5-land',
                                    DateStart = as.Date(Dates_vec2[Dates_Iter]),
                                    DateStop = as.Date(Dates_vec2[Dates_Iter])+15,
                                    TResolution = 'day',
                                    TStep = 16,
                                    Dir = Dir.ERA,
                                    FileName = Dates_vec[Dates_Iter],
                                    API_User = API_User,
                                    API_Key = API_Key)
    raster::writeRaster(Clim_ras, filename = file.path(Dir.ERA, paste0(Var_short, Dates_vec[Dates_Iter])), format = 'GTiff', overwrite = TRUE) # write the raster as a .tif
    unlink(file.path(Dir.ERA, paste0(Dates_vec[Dates_Iter], '.nc'))) # remove the netcdf that's exported by donwload_ERA
  "
  if(numberOfCores > 1){
    cl <- parallel::makeCluster(numberOfCores) # Assuming X node cluster
    doParallel::registerDoParallel(cl) # registering cores
    foreach::foreach(Dates_Iter = 1:length(Dates_vec), .packages = c("KrigR"), .export = c("Dir.ERA", "API_User", "API_Key", "Var_long", "Var_short", "Dates_vec", "Dates_vec2")) %:% when(!file.exists(file.path(Dir.ERA, paste0(Var_short, Dates_vec[Dates_Iter], ".tif")))) %dopar% { # Dates loop: loop over all 16-day time slots in the MOD13A2 data
      eval(parse(text=looptext)) # evaluate the kriging specification per layer
    } # end of Dates loop
    parallel::stopCluster(cl)
  }else{
    for(Dates_Iter in 1:length(Dates_vec)){
      if(file.exists(file.path(Dir.ERA, paste0(Var_short, Dates_vec[Dates_Iter], ".tif")))){ # file check: if file has already been downloaded
        print(paste(Var_long, Dates_vec[Dates_Iter], "already downloaded"))
        next() 
      } # end of file check
      print(paste(Var_long, Dates_vec[Dates_Iter], "now downloading"))
      eval(parse(text=looptext)) # evaluate the kriging specification per layer
    }
  }
} # end of FUN_DownloadCLIM
setwd(Dir.ERA)
ERA_fs <- list.files(pattern = ".tif")[!startsWith(prefix = "K_", x = list.files(pattern = ".tif"))] # list all unkriged files
if(length(ERA_fs) < (length(Dates_vec)+sum(startsWith(x = Dates_vec, prefix = "2001")))*2){ # ERA Product Check: if we do not have twice as many ERA files as MOD13A2 files
  print("Downloading ERA5-Land data now.")
  FUN_DownloadCLIM(Var_long = "2m_temperature", Var_short = "AT") # download airtemp data
  FUN_DownloadCLIM(Var_long = "volumetric_soil_water_layer_1", Var_short = "SM") # download qsoil1 data
} # end of ERA Product Check
setwd(mainDir)

####--------------- COVARIATE DATA DOWNLOAD ----------------------------------------
print("Loading covariate data.")
if(file.exists(file.path(Dir.COV, "GMTED2010_Target.nc"))){
  Covs_ls <- list(raster::raster(file.path(Dir.COV, "GMTED2010_Train.nc")),
                  raster::raster(file.path(Dir.COV, "GMTED2010_Target.nc")))
}else{
  Covs_ls <- KrigR::download_DEM(Train_ras = raster::raster(file.path(Dir.ERA, list.files(Dir.ERA)[1])),
                                 Target_res = raster::raster(file.path(Dir.EVI, list.files(Dir.EVI)[1])),
                                 Dir = Dir.COV,
                                 Keep_Temporary = TRUE
  )
}
raster::extent(Covs_ls[[1]]) <- raster::extent(-180,180,-90,90)
raster::extent(Covs_ls[[2]]) <- raster::extent(-180,180,-90,90)
####--------------- KRIGING OF CLIMATE DATA ----------------------------------------
#### ESTABLISH TILES
if(file.exists(file.path(Dir.COV, "Extents_ls.RData"))){
  load(file.path(Dir.COV, "Extents_ls.RData"))
}else{
  Extents <- list() # empty list for extent objects
  res_tiles <- 3 # resolution of tiles
  Lat_Tiles <- (150-res_tiles)/res_tiles # we only cover 150° of latitude
  Lon_Tiles <- (360-res_tiles)/res_tiles # we cover the full 360° of longitude
  z <- 1 # enumerator for list elements
  Clim_ras <- raster::raster(file.path(Dir.ERA, list.files(Dir.ERA)[1]))
  raster::extent(Clim_ras) <- raster::extent(-180,180,-90,90)
  print("Checking for which tiles to krig on.")
  Prog_Iter <- 0
  ProgBar <- txtProgressBar(min = 0, max = (Lat_Tiles+1) * (Lon_Tiles+1), style = 3)
  for(i in 0:Lat_Tiles){ # lat loop
    for(j in 0:Lon_Tiles){ # lon loop
      Extent_curr <- raster::extent(c(-180+res_tiles*j,
                                      -180+res_tiles*(j+1),
                                      -60+res_tiles*i,
                                      -60+res_tiles*(i+1)))
      Clim_check <- raster::crop(Clim_ras, Extent_curr)
      Land_check <- raster::crop(Covs_ls[[2]], Extent_curr)
      try(Land_check <- raster::mask(Land_check, cropped_shp), silent = TRUE)
      if(!all(is.na(c(NA, Land_check))) & length(which(!is.na(values(Clim_check)))) > 5){ # sanity check: if kriging can be performed with this extent
        Extents[[z]] <- Extent_curr # save extent to list
        z <- z + 1 # raise enumerator of list elements
      } # end of sanity check
      Prog_Iter <- Prog_Iter + 1
      setTxtProgressBar(ProgBar, Prog_Iter) # update progress bar
    } # end of lon loop
  } # end of lat loop
  save(Extents, file = file.path(Dir.COV, "Extents_ls.RData"))
}
Names_tiles = as.list(paste("TempFile_", 1:length(Extents), sep="")) # names of tiles for names of temporary files

#### KRIGING
FUN_Krig <- function(Var_short = "AT"){
  setwd(Dir.ERA)  
  Clim_fs <- list.files(pattern = ".tif")[startsWith(prefix = Var_short, x = list.files(pattern = ".tif"))] # list all unkriged files belonging to target variable
  for(Dates_Iter in 1:length(Clim_fs)){ # Dates loop: loop over all dates for which we've got ERA data
    Name <- gsub(pattern = ".tif", replacement ="", x = Clim_fs[Dates_Iter])
    Dir.Date <- file.path(Dir.ERA, Name) # register directory for tiles of this date
    dir.create(Dir.Date) # create directory for tiles of this date
    Clim_train <- raster::raster(file.path(Dir.ERA, Clim_fs[Dates_Iter])) # load training data for this date
    raster::extent(Clim_train) <- raster::extent(-180,180,-90,90) # set extent to prevent misalignment
    looptext <- "
      cropped_train <- raster::crop(Clim_train, Extents[[Krig_Iter]]) # crop training data
      raster::extent(cropped_train) <- Extents[[Krig_Iter]] # set extent of cropped training data (necessary because of rounding issues in late decimal points)
      cropped_shp <- raster::crop(Land_shp, Extents[[Krig_Iter]]) # crop land mask shapefile
      Covs_train <- raster::crop(Covs_ls[[1]], Extents[[Krig_Iter]]) # crop training covariates
      try(Covs_train <- raster::mask(Covs_train, cropped_shp), silent = TRUE) # attempt masking (fails if on sea pixel)
      Covs_target <- raster::crop(Covs_ls[[2]], Extents[[Krig_Iter]])  # crop target covariates
      try(Covs_target <- raster::mask(Covs_target, cropped_shp), silent = TRUE) # attempt masking (fails if on sea pixel)
      try( # try because of singular covariance matrices which can be an issue if there isn't enough data 
        Dummy_ls <- KrigR::krigR(
          Data = cropped_train,
          Covariates_coarse = Covs_train,
          Covariates_fine = Covs_target,
          Cores = 1,
          Dir = Dir.Date,
          FileName = Names_tiles[Krig_Iter],
          Keep_Temporary = FALSE
        ), 
        silent=TRUE)
        "
    if(numberOfCores > 1){
      cl <- parallel::makeCluster(numberOfCores) # Assuming X node cluster
      doParallel::registerDoParallel(cl) # registering cores
      foreach::foreach(Krig_Iter = 1:length(Extents), .packages = c("KrigR"), .export = c("Dir.ERA", "Covs_ls", "Clim_train", "Land_shp", "Var_short", "Extents", "ProgBar", "Dir.Date")) %:% when(!file.exists(file.path(Dir.Date, paste0(Names_tiles[Krig_Iter], ".nc")))) %dopar% { # tiles loop: loop over all tiles
        eval(parse(text=looptext)) # evaluate the kriging specification per layer
      } # end of tiles loop
      stopCluster(cl) # stop cluster
    }else{
      ProgBar <- txtProgressBar(min = 0, max = length(Extents), style = 3) # establish progress bar
      for(Krig_Iter in 1:length(Extents)){
        eval(parse(text=looptext)) # evaluate the kriging specification per layer
        setTxtProgressBar(ProgBar, Krig_Iter) # update progress bar
      }
    }
    setwd(Dir.Date)
    Krig_fs <- list.files(pattern = ".nc")[startsWith(prefix = "TempFile", x = list.files(pattern = ".nc"))] # list all data tiles of current date
    SE_fs <- list.files(pattern = ".nc")[startsWith(prefix = "SE", x = list.files(pattern = ".nc"))] # list all uncertainty tiles of current date
    print(paste("Merging", Var_short, "tiles for", Name))
    Krigs_ls <- as.list(rep(NA, length(Krig_fs)))
    SEs_ls <- as.list(rep(NA, length(SE_fs)))
    for(i in 1:length(Krigs_ls)) { # 
      Krigs_ls[[i]] <- raster::raster(Krig_fs[i])
      SEs_ls[[i]] <- raster::raster(SE_fs[i])
    }
    Krigs_ls$fun <- mean
    Krigs_ls$tolerance <- 1.5
    SEs_ls$fun <- mean
    SEs_ls$tolerance <- 1.5
    Krigs_glob <- do.call(raster::mosaic, Krigs_ls)
    values(Krigs_glob)[which(values(Krigs_glob) < 180 | values(Krigs_glob) > 320)] <- NA
    SEs_glob <- do.call(raster::mosaic, SEs_ls)
    values(SEs_glob)[which(values(Krigs_glob) < 180 | values(Krigs_glob) > 320)] <- NA
    setwd(Dir.ERA)
    Kriged_ras <- stack(Krigs_glob, SEs_glob)
    # Kriged_ras <- mask(Kriged_ras, Land_shp)
    raster::writeRaster(Kriged_ras, filename = paste0("K_", Name), format = "GTiff", overwrite = TRUE)
    unlink(Dir.Date, recursive = TRUE)
    unlink(file.path(Dir.ERA, paste0(Name, ".tif")), recursive = TRUE)
  } # end of Dates loop
} # end of FUN_Krig
setwd(Dir.ERA)
K_ERA_fs <- list.files(pattern = ".tif")[startsWith(prefix = "K_", x = list.files(pattern = ".tif"))] # list all kriged files
Dates_vec <- c(gsub(pattern = "2001", replacement = "2000", x = Dates_vec[startsWith(x = Dates_vec, prefix = "2001")]), gsub("-.*.", "", list.files(pattern = ".tif")))
if(length(K_ERA_fs) < length(Dates_vec)*2){ # ERA Product Check: if we do not have twice as many ERA files as MOD13A2 files
  print("Kriging ERA5-Land data now.")
  FUN_Krig(Var_short = "AT") # krig airtemp data
  FUN_Krig(Var_short = "SM") # krig qsoil data
} # end of ERA Product Check
