# LOAD_PREBAS_PUHTI ---------------------------------------------------------

library(devtools)

vPREBAS <- "master"
# vPREBAS <- "newVersion"


if(vPREBAS=="master") {
  print("Checking master...")
  RprebassoFolder = "/projappl/project_2000994/Rpackages/Rprebasso_master"
} else {
  print("Checking newVersion...")
  RprebassoFolder = "/projappl/project_2000994/Rpackages/Rprebasso_newV"
}

.libPaths(c(RprebassoFolder,
            "/projappl/project_2000994/Rpackages/project_rpackages",
            .libPaths()))

tryCatch({
  install_github("ForModLabUHel/Rprebasso", ref=vPREBAS)
}, error = function(e) {
  message("",e)
})

rm(vPREBAS, RprebassoFolder)




# LIBS --------------------------------------------------------------------

library(data.table)
library(Rprebasso)
library(testthat)
library(mockery)
library(yaml)



if(!exists("config_path")) {
  # Path to config file
  config_path <- paste0("config.yml")
}

config <- yaml.load_file(config_path)
























