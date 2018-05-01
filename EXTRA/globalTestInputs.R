#global.R ==> TestInputs

library(SpaDES)
library(magrittr)
library(raster)

# set the directories
workDirectory <- getwd()

paths <- list(
  cachePath = file.path(workDirectory, "cache"),
  modulePath = file.path(workDirectory, "modules"),
  inputPath = file.path(workDirectory, "inputs"),
  outputPath = file.path(workDirectory, "outputs")
)

setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, outputPath = paths$outputPath, cachePath = paths$cachePath)

## list the modules to use
times <- list(start = 0, end = 2)
parameters <- list()
modules <- list("prepInTest") # TEST PREP INPUTS
objects <- list(
  url.vegMap = "ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip",
  url.studyArea = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/district/ecodistrict_shp.zip",
  tempPath.vegMap = file.path(tempdir(), "vegMap"),
  tempPath.studyArea = file.path(tempdir(), "studyArea")
)

mySim <- simInit(times = times, params = parameters, modules = modules, objects = objects, paths = paths)
mySimOut <- spades(mySim)