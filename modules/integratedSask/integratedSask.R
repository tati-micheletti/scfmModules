
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "integratedSask",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = c("scfmCrop", "scfmLandcoverInit", "scfmRegime", "scfmDisturbanceDriver", "scfmIgnition", "scfmEscape", "scfmSpread", "ageModule", "mapBurns", "loadYieldTables", "Hanzlik", "strataMapFromVegMap", "scfmHarvest", "stateVars", "birdsAlberta", "caribouAlberta"),
  version = list(SpaDES.core = "0.2.2.9006"),# integratedSask = "0.0.1", scfmCrop = "0.0.1", scfmLandcoverInit = "0.0.1", scfmRegime = "0.0.1", scfmDisturbanceDriver = "0.0.1", scfmIgnition = "0.0.1", scfmEscape = "0.0.1", scfmSpread = "0.0.1", ageModule = "0.0.1", mapBurns = "0.0.1", loadYieldTables = "0.0.1", Hanzlik = "0.0.1", strataMapFromVegMap = "0.0.1", scfmHarvest = "0.0.1", stateVars = "0.0.1", birdsAlberta = "0.0.1", caribouAlberta = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "integratedSask.Rmd")
))
