

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
  
  print(object)
  print(class(object))
  
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

# Get the vector of sites (siteInfo rows) to save either by sampling (default) or by 
# selecting the vector 1:save_n_rows.
# Sampling is done with replace=TRUE. Additional args to function "sample" can be passed through (...).
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
  samples <- sample(nrow(siteInfo), save_n_rows, replace = T, ...)

  return(samples)
}


init_params_get_climIDs <- function(siteInfo) {
  
}

init_params_save_to_dir <- function(init_params, save_params_dir, save_n_rows = 1000) {
  
}


init_wrapper <- function(init_FUN, ..., save_params_args = list(save_params_dir = NULL)) {
  if(!is.null(save_params_args$save_params_dir)) {
    init_params <- list(...)
    
    print(init_params)
    
    required_init_param_names <- c("siteInfo", "multiInitVar", "PAR", "TAir", "VPD", "Precip", "CO2")
    
    # init_params_check_missing(init_params = ..., required_init_param_names = required_init_param_names)
    
    # siteInfo <- init_params_check_valid_object(object = init_params$siteInfo, name = "siteInfo")
    # multiInitVar <- init_params_check_valid_object(object = init_params$multiInitVar, name = "multiInitVar")
    PAR <- init_params_check_valid_object(object = init_params$PAR, name = "PAR")
    # TAir <- init_params_check_valid_object(object = init_params$TAir, name = "TAir")
    # VPD <- init_params_check_valid_object(object = init_params$VPD, name = "VPD")
    # Precip <- init_params_check_valid_object(object = init_params$Precip, name = "Precip")
    # CO2 <- init_params_check_valid_object(object = init_params$CO2, name = "CO2")
    
    
    
    
    
  }
  
  # initPrebas <- do.call(init_FUN, ...)
  # 
  # return(initPrebas)
}



# samples <- sample(1, nrow(siteInfo), replace = T)
# 
# 
# 
init_params <- list(PAR=clim1)
save_params_args = list(save_params_dir = "hello")
init_wrapper(InitMultiSite, init_params, save_params_args = save_params_args)

any(class(init_params$PAR) %in% c("matrix", "array"))










