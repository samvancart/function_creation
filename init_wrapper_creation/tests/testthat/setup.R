
config <- yaml.load_file("../tests_config.yml")

test_data <- readRDS(config$test_data_path)
siteInfo <- readRDS(config$test_siteInfo_path)
clim_list <- readRDS(config$test_clim_path)


multiInitVar <- test_data$multiInitVar
multiThin <- test_data$thinning

PAR <- clim_list$PAR
TAir <- clim_list$TAir
VPD <- clim_list$VPD
Precip <- clim_list$Precip
CO2 <- clim_list$CO2

nYearsMS <- test_data$nYears













