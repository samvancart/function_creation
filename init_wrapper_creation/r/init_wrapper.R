

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




# HELPERS -----------------------------------------------------------------

# Combine user-provided arguments with defaults, excluding specified ones.
# You can then pass only the relevant args to a specific function.
get_user_args_list <- function(all_args_list, default_args_list, exclude_args_vec) {
  user_args <- all_args_list[setdiff(names(all_args_list), exclude_args_vec)]
  args <-  modifyList(default_args_list, user_args)
  
  return(args)
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
    print("save_n_rows < 1. Setting save_n_rows to 1000.")
    save_n_rows <- 1000
  }
  
  if(save_n_rows > 2500) {
    msg <- paste0("save_n_rows is ", save_n_rows, " but max value for save_n_rows is " , 
                  max_save_n_rows, ". Setting save_n_rows to 1000.")
    print(msg)
    save_n_rows <- 1000
  }
  
  # Return head when no sampling
  if(!is_sample) {
    return(1:nrow(head(siteInfo, n = save_n_rows)))
  }
  
  if(nrow(siteInfo) < save_n_rows) {
    save_n_rows <- nrow(siteInfo)
    print("save_n_rows > nrow(siteInfo): Sampling with save_n_rows set to nrow(siteInfo).")
  }
  
  set.seed(seed)
  samples <- sample(nrow(siteInfo), save_n_rows, replace = T)

  return(samples)
}



# FILTER_SITEINFO ---------------------------------------------------------


init_params_filter_siteInfo <- function(siteInfo, rows_vec) {
  return(siteInfo[rows_vec, , drop = FALSE])
}

init_params_generate_new_ids <- function(climate_ids) {
  old_clim_ids <- unique(climate_ids)
  new_clim_ids <- seq_along(old_clim_ids)
  return(list(old_clim_ids = old_clim_ids, new_clim_ids = new_clim_ids))
}

# Function to remap climate IDs
init_params_remap_climate_ids <- function(climate_ids, old_clim_ids, new_clim_ids) {
  return(new_clim_ids[match(climate_ids, old_clim_ids)])
}

# Refactored main function
init_params_get_new_siteInfo_and_old_clim_ids <- function(siteInfo, rows_vec) {
  # Step 1: Filter siteInfo
  filtered_siteInfo <- init_params_filter_siteInfo(siteInfo, rows_vec)
  
  # Step 2: Generate old and new climate IDs
  ids <- init_params_generate_new_ids(filtered_siteInfo[, 2])
  
  # Step 3: Remap climate IDs in filtered_siteInfo
  filtered_siteInfo[, 2] <- init_params_remap_climate_ids(filtered_siteInfo[, 2], ids$old_clim_ids, ids$new_clim_ids)
  
  # Return result as a list
  return(list(new_siteInfo = filtered_siteInfo, old_clim_ids = ids$old_clim_ids))
}


# FILTER_OBJECTS ----------------------------------------------------------

init_params_filter_one_dimension <- function(object, name, dim_to_filter, indices) {
  # Validate inputs
  if (is.null(dim(object))) stop(paste0(name, " must have dimensions."))
  if (dim_to_filter < 1 || dim_to_filter > length(dim(object))) {
    stop(paste0(name, " specified dimension is invalid."))
  }
  
  # Create a full indexing list for the array
  index_list <- lapply(seq_along(dim(object)), function(i) {
    if (i == dim_to_filter) {
      return(indices) # Filter the specified dimension
    } else {
      return(seq_len(dim(object)[i])) # Preserve other dimensions
    }
  })
  
  # Use do.call to subset dynamically
  return(do.call(`[`, c(list(object), index_list, list(drop = FALSE))))
}


init_params_filter_valid_object <- function(object, name, indices, dim_to_filter = 1) {
  # Validate object using init_params_check_valid_object
  valid_object <- init_params_check_valid_object(object = object, name = name)
  
  filtered_object <- init_params_filter_one_dimension(object = object, name = name, 
                                                      indices = indices, dim_to_filter = dim_to_filter)
  
  return(filtered_object)
}

init_params_filter_valid_numeric_vec <- function(vec, name, indices) {
  if(!is.numeric(vec)) {
    msg <- paste0(name, " must be numeric.")
    stop(msg)
  }
  
  if(!is.null(dim(vec))) {
    msg <- paste0(name, " must have NULL dimensions.")
    stop(msg)
  }
  
  filtered_vec <- vec[indices]
  
  if(any(is.na(filtered_vec))) {
    msg <- paste0(name, " contains NA values.")
    warning(msg)
  }
  
  return(filtered_vec)
  
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
    
    required_init_param_names <- c("nYearsMS", "siteInfo", "multiInitVar", "PAR", "TAir", "VPD", "Precip", "CO2")
    
    init_params_check_missing(init_params = ..., required_init_param_names = required_init_param_names)
    
    # Check if user has provided additional params for "init_params_get_sites" function
    get_sites_default_args <- list(save_n_rows = 1000, is_sample = TRUE, seed = 123)
    get_sites_args <- get_user_args_list(all_args_list = save_params_args, 
                                         default_args_list = get_sites_default_args,
                                         exclude_args_vec = c("save_params_dir"))
    
    
    siteInfo <- init_params_check_valid_object(object = init_params$siteInfo, name = "siteInfo")
    sites <- do.call(init_params_get_sites, c(list(siteInfo = siteInfo), get_sites_args))
    
    filtered_siteInfo_list <- init_params_get_new_siteInfo_and_old_clim_ids(siteInfo = siteInfo, rows_vec = sites)
    
    new_siteInfo <- filtered_siteInfo_list$new_siteInfo
    old_clim_ids <- filtered_siteInfo_list$old_clim_ids
    
    print(new_siteInfo)
    
    # Validate objects and filter dimension 1
    new_PAR <- init_params_filter_valid_object(object = init_params$PAR, name = "PAR", indices = old_clim_ids)
    new_TAir <- init_params_filter_valid_object(object = init_params$TAir, name = "TAir", indices = old_clim_ids)
    new_VPD <- init_params_filter_valid_object(object = init_params$VPD, name = "VPD", indices = old_clim_ids)
    new_Precip <- init_params_filter_valid_object(object = init_params$Precip, name = "Precip", indices = old_clim_ids)
    new_CO2 <- init_params_filter_valid_object(object = init_params$CO2, name = "CO2", indices = old_clim_ids)
    
    new_multiInitVar <- init_params_filter_valid_object(object = init_params$multiInitVar, name = "multiInitVar", 
                                                        indices = sites)
    
    new_nYearsMS <- init_params_filter_numeric_vec(object = init_params$nYearsMS, name = "nYearsMS", 
                                                   indices = c(1:length(sites)))
    
    new_params <- list(nYearsMS = new_nYearsMS,
                       siteInfo = new_siteInfo,
                       multiInitVar = new_multiInitVar,
                       PAR = new_PAR,
                       TAir = new_TAir,
                       VPD = new_VPD,
                       Precip = new_Precip,
                       CO2 = new_CO2)

    filtered_init_params <- modifyList(init_params, new_params)
    
    
    # Exclude all args except the ones for "init_params_save_to_dir"
    save_to_dir_exclude_args_vec <- names(save_params_args[!names(save_params_args) %in% c("save_params_dir", "suffix_name")])
    save_to_dir_default_args <- c(suffix_name = NULL)
    save_to_dir_filtered_args <- get_user_args_list(all_args_list = save_params_args, 
                                           default_args_list = save_to_dir_default_args,
                                           exclude_args_vec = save_to_dir_exclude_args_vec)
    
    # Add filtered init_params to save args
    save_to_dir_args <- c(list(init_params = filtered_init_params), save_to_dir_filtered_args)
    
    # Save
    do.call(init_params_save_to_dir, save_to_dir_args)
    
  }
  
  # initPrebas <- do.call(init_FUN, ...)
  # 
  # return(initPrebas)
}

















