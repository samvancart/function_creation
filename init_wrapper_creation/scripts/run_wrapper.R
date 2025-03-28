source("scripts/settings.R")
source("r/init_wrapper.R")

# PATHS -------------------------------------------------------------------

files <- list.files(config$run_wrapper_base_path, recursive = T, full.names = T)


# GET_VARS ----------------------------------------------------------------


siteInfo <-  readRDS(grep("siteInfo", files, value = T))
clim_paths <- grep("detrended", files, value = T)
climate_data <- setNames(
  lapply(clim_paths, function(path) {
    # Read the file
    file <- readRDS(path)
    
    # Convert the matrix values to numeric
    if (is.matrix(file)) {
      file <- apply(file, 2, as.numeric)
    }
    file
  }),
  # Extract names for the list based on the filename
  sapply(clim_paths, function(path) tstrsplit(basename(path), split = "\\.", keep = 1)[[1]])
)




multiInitVar <- readRDS(grep("multiInitVar", files, value = T))

nYears <- floor(ncol(climate_data$parTran)/365)
nSites <- nrow(siteInfo)
nYearsMS <- rep(nYears, nSites)

dim(climate_data[["parTran"]])
dimnames(climate_data[["parTran"]][,-1])

# RUN ---------------------------------------------------------------------

init_args <- list(nYearsMS = nYearsMS,
             siteInfo = as.matrix(siteInfo),
             multiInitVar = multiInitVar,
             PAR = climate_data[["parTran"]][,-1], # Remove cell column (column was required for siteInfo)
             VPD = climate_data[["vpdTran"]][,-1],
             CO2 = climate_data[["co2Tran"]][,-1],
             Precip = climate_data[["precipTran"]][,-1],
             TAir = climate_data[["tairTran"]][,-1],
             ClCut = 1)



save_params_args = list(save_params_dir = paste0(getwd()), save_n_rows = 10, suffix_name = "test")
args <- c(list(save_params_args = save_params_args), 
          init_args)

names(init_args)

filtered_init_params <- do.call(init_wrapper, args)




init_params_check_function_exists("InitMultiSite")
exists("InitMultiSite", mode = "function")

initPrebas <- init_wrapper(InitMultiSite, args)

Rprebasso::InitMultiSite

dim(init_args$siteInfo)
dimnames(init_args$multiInitVar)
do.call(InitMultiSite, init_args)

# TRANSECT -----------------------------------------------------------------
?TransectRun

t_run <- TransectRun()


siteInfo <- t_run$siteInfo


names(t_run)

t_run$nYears

test_years <- init_params_filter_valid_numeric_vec(t_run$nYears, "nYearsMS", c(1:15))

# new weather inputs
clim <- t_run$weather
clim1 <- matrix(clim[,,,1], nrow = nrow(clim), ncol = ncol(clim) * dim(clim)[3])


clim <- t_run$weather
clim_list <- lapply(seq_len(dim(clim)[4]), function(i) {
  matrix(clim[,,,i], nrow = nrow(clim), ncol = ncol(clim) * dim(clim)[3])
})
names(clim_list) <-  c("PAR", "TAir", "VPD", "Precip", "CO2")




t_run$multiInitVar[c(1:5),,]
dim(t_run$multiInitVar)

res <- filter_one_dimension(clim1, "m", 1, indices = c(1:5))
dim(res)


save_params_args = list(save_params_dir = "hello", save_n_rows = 2)
init_wrapper(InitMultiSite, siteInfo = siteInfo, save_params_args = save_params_args)



siteInfo[c(1,2,1),]





