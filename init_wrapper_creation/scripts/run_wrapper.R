source("scripts/settings.R")
source("r/init_wrapper.R")


# PATHS -------------------------------------------------------------------

base_path <- "../forest-navigator23-r/data/acc/input/simulation_sites_200/clean/plgid_7302942"
files <- list.files(base_path, recursive = T, full.names = T)


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


# RUN ---------------------------------------------------------------------

args <- list(nYearsMS = nYearsMS,
             siteInfo = as.matrix(siteInfo),
             multiInitVar = multiInitVar,
             PAR = climate_data[["parTran"]][,-1], # Remove cell column (column was required for siteInfo)
             VPD = climate_data[["vpdTran"]][,-1],
             CO2 = climate_data[["co2Tran"]][,-1],
             Precip = climate_data[["precipTran"]][,-1],
             TAir = climate_data[["tairTran"]][,-1])



initPrebas <- init_wrapper(InitMultiSite, args)





library(Rprebasso)

t_run <- TransectRun()

siteInfo <- t_run$siteInfo

set.seed(123)
siteInfo_sample <- siteInfo[c(sample(1:7,3),3),]

oldclimIds <- unique(siteInfo_sample[,2])
newclimIds <- 1:length(oldclimIds)
new <- newclimIds[match(siteInfo_sample[,2], oldclimIds)]

newclimIDs_forSiteInfo <- newclimIds[match(siteInfo_sample[,2],oldclimIds)]


# new weather inputs
names(t_run)

dim(t_run$weather)
class(t_run$weather)

clim <- t_run$weather

clim1 <- matrix(clim[,,,1], nrow = nrow(clim), ncol = ncol(clim) * dim(clim)[3])

new_clim1 <- clim1[oldclimIds,]
new_clim1[, 1:20]

newPar <- Par[oldclimIds,]
siteInfo_sample[,2] <- newclimIDs_forSiteInfo



t_run$multiInitVar[c(1:5),,]
dim(t_run$multiInitVar)

res <- filter_one_dimension(clim1, "m", 1, indices = c(1:5))
dim(res)


save_params_args = list(save_params_dir = "hello", save_n_rows = 2)
init_wrapper(InitMultiSite, siteInfo = siteInfo, save_params_args = save_params_args)



siteInfo[c(1,2,1),]





















