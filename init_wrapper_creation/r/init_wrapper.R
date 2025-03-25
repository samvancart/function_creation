

# VALIDATE ----------------------------------------------------------------



init_params_check_missing <- function(init_params, required_init_param_names) {
  missing_init_params <- required_init_param_names[which(!required_init_param_names %in% names(init_params))]
  if(length(missing_init_params > 0)) {
    warning("Unable to save initialisation parameters because the following required parameters are missing: ", 
            paste(missing_init_params, collapse = ", "))
    return(invisible(FALSE))
  }
  return(invisible(TRUE))
}


init_params_check_valid_object <- function(object, name) {
  
  if(all(!class(object) %in% c("matrix", "array"))) {
    msg <- paste0("Invalid type for object ", name, ". Type is ", class(object))
    stop(msg)
  }
  
  if(nrow(object) < 1) {
    msg <- paste0("Object ", name, "must contain at least 1 row.")
    stop(msg)
  }
  
  return(object)
}




# GET_SITES ---------------------------------------------------------------



# Get the vector of sites (siteInfo rows) to save either by sampling (default) or by 
# selecting the vector 1:save_n_rows. Sampling is done with replace=TRUE.
init_params_get_sites <- function(siteInfo, save_n_rows = 1000, is_sample = TRUE, seed = 123, ...) {
  
  max_save_n_rows <- 2500
  
  if(length(save_n_rows) > 1) {
    stop("save_n_rows should be a numeric value of length 1.")
  }
  
  
  if(!class(save_n_rows) %in% c("numeric", "integer", "double")) {
    msg <- paste0("save_n_rows must be a numeric type but is of type ", class(save_n_rows))
    stop(msg)
  }
  
  
  if(save_n_rows < 1) {
    message("save_n_rows < 1. Setting save_n_rows to 1000.")
    save_n_rows <- 1000
  }
  
  if(save_n_rows > 2500) {
    msg <- paste0("save_n_rows is ", save_n_rows, " but max value for save_n_rows is " , 
                  max_save_n_rows, ". Setting save_n_rows to 1000.")
    message(msg)
    save_n_rows <- 1000
  }
  
  # Return head when no sampling
  if(!is_sample) {
    return(1:nrow(head(siteInfo, n = save_n_rows)))
  }
  
  if(nrow(siteInfo) < save_n_rows) {
    save_n_rows <- nrow(siteInfo)
    message("save_n_rows > nrow(siteInfo): Sampling with save_n_rows set to nrow(siteInfo).")
  }
  
  set.seed(seed)
  samples <- sample(nrow(siteInfo), save_n_rows, replace = T)

  return(samples)
}



# FILTER_SITEINFO ---------------------------------------------------------



init_params_get_new_siteInfo_and_old_clim_ids <- function(siteInfo, sites) {
  filtered_siteInfo <- siteInfo[sites, ]
  old_clim_ids <- unique(filtered_siteInfo[, 2])
  new_clim_ids <- 1:length(old_clim_ids)
  new_siteInfo_clim_ids <- new_clim_ids[match(filtered_siteInfo[, 2], old_clim_ids)]
  filtered_siteInfo[, 2] <- new_siteInfo_clim_ids
  
  return(list(new_siteInfo = filtered_siteInfo, old_clim_ids = old_clim_ids))
}



# SAVE --------------------------------------------------------------------



init_params_save_to_dir <- function(init_params, save_params_dir, suffix_name = NULL) {
  # Check if the directory exists
  if (!dir.exists(save_params_dir)) {
    warning("Directory does not exist: ", save_params_dir)
    return(NULL) # Exit the function without proceeding
  }
  
  # Generate a temporary file path
  filename <- tempfile(pattern = "initMultiSiteParams_", tmpdir = save_params_dir)
  
  # Append suffix_name if provided
  if (!is.null(suffix_name)) {
    filename <- paste0(filename, "_", suffix_name)
  }
  
  filepath <- paste0(filename, ".RData")
  
  # Save the init_params list into the file
  save(init_params, file = filepath)
  
  message("Parameters have been saved to: ", filepath)
}




# MAIN_FUN ----------------------------------------------------------------



init_wrapper <- function(init_FUN, ..., save_params_args = list(save_params_dir = NULL)) {
  if(!is.null(save_params_args$save_params_dir)) {
    init_params <- list(...)
    
    required_init_param_names <- c("siteInfo", "multiInitVar", "PAR", "TAir", "VPD", "Precip", "CO2")
    
    # init_params_check_missing(init_params = ..., required_init_param_names = required_init_param_names)
    
    # Check if user has provided additional params for "init_params_get_sites" function
    get_sites_default_args <- list(save_n_rows = 1000, is_sample = TRUE, seed = 123)
    
    # TODO wrap into helper function
    get_sites_user_args <- save_params_args[setdiff(names(save_params_args), "save_params_dir")]
    get_sites_args <-  modifyList(get_sites_default_args, get_sites_user_args)
    
    siteInfo <- init_params_check_valid_object(object = init_params$siteInfo, name = "siteInfo")
    sites <- do.call(init_params_get_sites, c(list(siteInfo = siteInfo), get_sites_args))
    
    filtered_siteInfo_list <- init_params_get_new_siteInfo_and_old_clim_ids(siteInfo = siteInfo, sites = sites)
    
    new_siteInfo <- filtered_siteInfo_list$new_siteInfo
    old_clim_ids <- filtered_siteInfo_list$old_clim_ids
    
    print(new_siteInfo)
    
    # new_multiInitVar <- init_params_check_valid_object(object = init_params$multiInitVar, name = "multiInitVar")[sites, ]
    # new_PAR <- init_params_check_valid_object(object = init_params$PAR, name = "PAR")[old_clim_ids, ]
    # new_TAir <- init_params_check_valid_object(object = init_params$TAir, name = "TAir")[old_clim_ids, ]
    # new_VPD <- init_params_check_valid_object(object = init_params$VPD, name = "VPD")[old_clim_ids, ]
    # new_Precip <- init_params_check_valid_object(object = init_params$Precip, name = "Precip")[old_clim_ids, ]
    # new_CO2 <- init_params_check_valid_object(object = init_params$CO2, name = "CO2")[old_clim_ids, ]
    # 
    # 
    # new_params <- list(siteInfo = new_siteInfo,
    #                    multiInitVar = new_multiInitVar,
    #                    PAR = new_PAR,
    #                    TAir = new_TAir,
    #                    VPD = new_VPD,
    #                    Precip = new_Precip,
    #                    CO2 = new_CO2)
    # 
    # filtered_init_params <- modifyList(init_params, new_params)
    
    
    
    
    
  }
  
  # initPrebas <- do.call(init_FUN, ...)
  # 
  # return(initPrebas)
}

















