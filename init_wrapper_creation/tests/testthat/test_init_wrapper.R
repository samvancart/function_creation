

# TEST HELPERS -------------------------------------------------


test_that("get_user_args_list uses default arguments when all_args_list is empty", {
  all_args_list <- list()
  default_args_list <- list(a = 1, b = 2)
  exclude_args_vec <- character()
  result <- get_user_args_list(all_args_list, default_args_list, exclude_args_vec)
  expect_equal(result, default_args_list)
})

test_that("get_user_args_list allows user arguments to overwrite default arguments", {
  all_args_list <- list(a = 10, b = 20)
  default_args_list <- list(a = 1, b = 2)
  exclude_args_vec <- character()
  result <- get_user_args_list(all_args_list, default_args_list, exclude_args_vec)
  expect_equal(result, list(a = 10, b = 20))
})

test_that("get_user_args_list excludes specified arguments", {
  all_args_list <- list(a = 10, b = 20, c = 30)
  default_args_list <- list(a = 1, b = 2, c = 3)
  exclude_args_vec <- c("a", "c")
  result <- get_user_args_list(all_args_list, default_args_list, exclude_args_vec)
  expect_equal(result, list(a = 1, b = 20, c = 3))
})

test_that("get_user_args_list keeps all arguments when exclude_args_vec is empty", {
  all_args_list <- list(a = 10, b = 20)
  default_args_list <- list(a = 1, b = 2)
  exclude_args_vec <- character()
  result <- get_user_args_list(all_args_list, default_args_list, exclude_args_vec)
  expect_equal(result, list(a = 10, b = 20))
})

test_that("get_user_args_list excludes all arguments if specified in exclude_args_vec", {
  all_args_list <- list(a = 10, b = 20)
  default_args_list <- list(a = 1, b = 2)
  exclude_args_vec <- c("a", "b")
  result <- get_user_args_list(all_args_list, default_args_list, exclude_args_vec)
  expect_equal(result, default_args_list)
})

test_that("get_user_args_list excludes correct arguments when all_args_list contains user args", {
  all_args_list <- list(a = 10, b = 20, c = 30, d = 40)
  default_args_list <- list(d = NULL)
  exclude_args_vec <- names(all_args_list[!names(all_args_list) %in% c("a", "d")])
  result <- get_user_args_list(all_args_list, default_args_list, exclude_args_vec)
  expect_equal(result, list(d = 40, a = 10))
})

test_that("get_user_args_list excludes correct arguments when all_args_list does not contain user args", {
  all_args_list <- list(a = 10, b = 20, c = 30)
  default_args_list <- list(d = NULL)
  exclude_args_vec <- names(all_args_list[!names(all_args_list) %in% c("a", "d")])
  result <- get_user_args_list(all_args_list, default_args_list, exclude_args_vec)
  expect_equal(result, list(d = NULL, a = 10))
})

test_that("get_user_args_list adds arguments from all_args_list that are not in default_args_list", {
  all_args_list <- list(a = 10, b = 20, c = 30)
  default_args_list <- list(a = 1, b = 2)
  exclude_args_vec <- character()
  result <- get_user_args_list(all_args_list, default_args_list, exclude_args_vec)
  expect_equal(result, list(a = 10, b = 20, c = 30))
})

test_that("get_user_args_list throws an error when all_args_list is not a list", {
  all_args_list <- "not_a_list"
  default_args_list <- list(a = 1, b = 2)
  exclude_args_vec <- character()
  
  expect_error(
    get_user_args_list(all_args_list, default_args_list, exclude_args_vec),
    "all_args_list must be a list"
  )
})

test_that("get_user_args_list throws an error when default_args_list is not a list", {
  all_args_list <- list(a = 1)
  default_args_list <- "not_a_list"
  exclude_args_vec <- character()
  
  expect_error(
    get_user_args_list(all_args_list, default_args_list, exclude_args_vec),
    "default_args_list must be a list"
  )
})

test_that("get_user_args_list throws an error when exclude_args_vec is not a character vector", {
  all_args_list <- list(a = 1, b = 2)
  default_args_list <- list(a = 1, b = 2)
  exclude_args_vec <- 123 # Not a character vector
  
  expect_error(
    get_user_args_list(all_args_list, default_args_list, exclude_args_vec),
    "exclude_args_vec must be a character vector"
  )
})







# TEST GET_SITES ----------------------------------------------------------

test_that("init_params_get_sites handles save_n_rows with invalid length", {
  siteInfo <- matrix(1:70, nrow = 7, ncol = 10)
  expect_error(init_params_get_sites(siteInfo, save_n_rows = 1:5), "save_n_rows should be a numeric value of length 1.")
})

test_that("init_params_get_sites handles valid numeric save_n_rows with matrix siteInfo", {
  siteInfo <- matrix(1:70, nrow = 7, ncol = 10)
  result <- init_params_get_sites(siteInfo, save_n_rows = 5)
  expect_equal(length(result), 5) # Should return a sample of 5 elements
})

test_that("init_params_get_sites handles invalid save_n_rows type", {
  siteInfo <- matrix(1:70, nrow = 7, ncol = 10)
  expect_error(init_params_get_sites(siteInfo, save_n_rows = "invalid"), "save_n_rows must be a numeric type but is of type character")
})

test_that("init_params_get_sites sets save_n_rows to default for negative values", {
  siteInfo <- matrix(1:70, nrow = 7, ncol = 10)
  result <- init_params_get_sites(siteInfo, save_n_rows = -5)
  expect_equal(length(result), nrow(siteInfo)) # Adjusted to nrow(siteInfo) = 7
})

test_that("init_params_get_sites sets save_n_rows to max_save_n_rows when exceeding limit", {
  siteInfo <- matrix(1:70, nrow = 7, ncol = 10)
  result <- init_params_get_sites(siteInfo, save_n_rows = 3000)
  expect_equal(length(result), nrow(siteInfo)) # Adjusted to nrow(siteInfo) = 7
})

test_that("init_params_get_sites returns correct sample when is_sample is FALSE", {
  siteInfo <- matrix(1:70, nrow = 7, ncol = 10)
  result <- init_params_get_sites(siteInfo, save_n_rows = 5, is_sample = FALSE)
  expect_equal(length(result), 5)
  expect_equal(result, c(1:5)) # Extracts column 2 for 5 rows
})

test_that("init_params_get_sites returns correct sample when is_sample is TRUE", {
  # Create siteInfo matrix with 20000 rows and 10 columns
  siteInfo <- matrix(1:(20000 * 10), nrow = 20000, ncol = 10)
  
  # Define test parameters
  seeds <- c(123, 456, 789, 987, 654, 321)  # A wider range of seed values
  save_n_rows_values <- c(500, 1000, 2000, 2500)  # Valid save_n_rows values
  
  # Loop through seeds and save_n_rows_values for testing
  for (seed in seeds) {
    for (save_n_rows in save_n_rows_values) {
      # Compute expected result using sample()
      set.seed(seed)
      expected_result <- sample(nrow(siteInfo), save_n_rows, replace = TRUE)
      
      # Run the function
      result <- init_params_get_sites(siteInfo, save_n_rows = save_n_rows, is_sample = TRUE, seed = seed)
      
      # Compare output with expected result
      expect_equal(result, expected_result)
      
      # Additional check: Ensure length matches save_n_rows
      expect_equal(length(result), save_n_rows)
    }
  }
})

test_that("init_params_get_sites adjusts save_n_rows to nrow(siteInfo) when save_n_rows > nrow(siteInfo)", {
  siteInfo <- matrix(1:70, nrow = 7, ncol = 10)
  result <- init_params_get_sites(siteInfo, save_n_rows = 10, is_sample = TRUE, seed = 123)
  
  expected_samples <- c(7, 7, 3, 6, 3, 2, 2) # Replace with actual output
  expect_equal(result, expected_samples)
})

test_that("init_params_get_sites handles edge case when nrow(siteInfo) == 0", {
  siteInfo <- matrix(numeric(), nrow = 0, ncol = 10)
  result <- init_params_get_sites(siteInfo, save_n_rows = 5)
  expect_equal(length(result), 0) # No rows to sample
})



# TEST FILTER_SITEINFO --------------------------------------

test_that("init_params_filter_siteInfo filters siteInfo based on rows_vec", {
  siteInfo <- matrix(
    c(1, 10, 2, 20, 3, 10, 4, 30, 5, 20),
    ncol = 2,
    dimnames = list(NULL, c("site_id", "climate_id"))
  )
  rows_vec <- c(1, 3, 5)
  result <- init_params_filter_siteInfo(siteInfo, rows_vec)
  expect_equal(result, siteInfo[rows_vec, , drop = FALSE])
})

test_that("init_params_filter_siteInfo handles empty rows_vec gracefully", {
  siteInfo <- matrix(
    c(1, 10, 2, 20, 3, 10, 4, 30, 5, 20),
    ncol = 2,
    dimnames = list(NULL, c("site_id", "climate_id"))
  )
  rows_vec <- integer(0)
  result <- init_params_filter_siteInfo(siteInfo, rows_vec)
  expect_equal(result, siteInfo[integer(0), , drop = FALSE])
})

test_that("init_params_generate_new_ids generates old and new climate IDs correctly", {
  climate_ids <- c(10, 20, 10, 30, 20)
  result <- init_params_generate_new_ids(climate_ids)
  expect_equal(result$old_clim_ids, c(10, 20, 30))
  expect_equal(result$new_clim_ids, c(1, 2, 3))
})

test_that("init_params_generate_new_ids handles empty climate IDs gracefully", {
  climate_ids <- character(0)
  result <- init_params_generate_new_ids(climate_ids)
  expect_equal(result$old_clim_ids, character(0))
  expect_equal(result$new_clim_ids, integer(0))
})

test_that("init_params_remap_climate_ids remaps old IDs to new IDs correctly", {
  climate_ids <- c(10, 20, 10, 30, 20)
  old_clim_ids <- c(10, 20, 30)
  new_clim_ids <- c(1, 2, 3)
  result <- init_params_remap_climate_ids(climate_ids, old_clim_ids, new_clim_ids)
  expect_equal(result, c(1, 2, 1, 3, 2))
})

test_that("init_params_remap_climate_ids handles unmatched IDs gracefully", {
  climate_ids <- c(10, 40) # 40 is not in old_clim_ids
  old_clim_ids <- c(10, 20, 30)
  new_clim_ids <- c(1, 2, 3)
  result <- init_params_remap_climate_ids(climate_ids, old_clim_ids, new_clim_ids)
  expect_equal(result, c(1, NA)) # Unmatched IDs should return NA
})


# TEST FILTER_OBJECTS -----------------------------------------------------

# init_params_filter_one_dimension 

test_that("init_params_filter_one_dimension filters along the specified dimension", {
  object <- array(1:24, dim = c(4, 3, 2))  # A 3D array
  name <- "test_array"
  dim_to_filter <- 1  # Filter along the first dimension
  indices <- c(1, 3)  # Select rows 1 and 3
  
  result <- init_params_filter_one_dimension(object, name, dim_to_filter, indices)
  expect_equal(dim(result), c(length(indices), 3, 2))  # Check filtered dimensions
  expect_equal(result[1, , ], object[1, , ])  # Check values of first row
  expect_equal(result[2, , ], object[3, , ])  # Check values of third row
})

test_that("init_params_filter_one_dimension filters along a higher dimension", {
  object <- array(1:24, dim = c(4, 3, 2))  # A 3D array
  name <- "test_array"
  dim_to_filter <- 3  # Filter along the third dimension
  indices <- c(1)  # Select the first "slice" along the third dimension
  
  result <- init_params_filter_one_dimension(object, name, dim_to_filter, indices)
  expect_equal(dim(result), c(4, 3, length(indices)))  # Check resulting dimensions
  expect_equal(result[, , 1], object[, , 1])  # Verify values in the filtered slice
})

test_that("init_params_filter_one_dimension handles empty indices gracefully", {
  object <- array(1:24, dim = c(4, 3, 2))  # A 3D array
  name <- "test_array"
  dim_to_filter <- 1  # Filter along the first dimension
  indices <- integer(0)  # Empty indices
  
  result <- init_params_filter_one_dimension(object, name, dim_to_filter, indices)
  expect_equal(dim(result), c(0, 3, 2))  # Result should have no rows
  expect_equal(result, array(numeric(0), dim = c(0, 3, 2)))  # Ensure correct values
})

test_that("init_params_filter_one_dimension preserves dimensions when filtering", {
  object <- array(1:48, dim = c(4, 3, 2, 2))  # A 4D array
  name <- "test_array"
  dim_to_filter <- 1  # Filter along the first dimension
  indices <- c(2, 4)  # Select rows 2 and 4
  
  result <- init_params_filter_one_dimension(object, name, dim_to_filter, indices)
  expect_equal(dim(result), c(length(indices), 3, 2, 2))  # Check all dimensions
})

test_that("init_params_filter_one_dimension throws error for invalid dimensions", {
  object <- array(1:24, dim = c(4, 3, 2))  # A 3D array
  name <- "test_array"
  dim_to_filter <- 5  # Invalid dimension
  indices <- c(1, 2)
  
  expect_error(
    init_params_filter_one_dimension(object, name, dim_to_filter, indices), 
    "test_array specified dimension is invalid."
  )
})

test_that("init_params_filter_one_dimension throws error when object has no dimensions", {
  object <- 1:10  # A vector, which has no dimensions
  name <- "test_vector"
  dim_to_filter <- 1
  indices <- c(1, 3)
  
  expect_error(
    init_params_filter_one_dimension(object, name, dim_to_filter, indices), 
    "test_vector must have dimensions."
  )
})

test_that("init_params_filter_one_dimension handles selecting all elements in a dimension", {
  object <- array(1:24, dim = c(4, 3, 2))  # A 3D array
  name <- "test_array"
  dim_to_filter <- 2  # Filter along the second dimension
  indices <- 1:3  # Select all columns
  
  result <- init_params_filter_one_dimension(object, name, dim_to_filter, indices)
  expect_equal(dim(result), c(4, length(indices), 2))  # Check dimensions
  expect_equal(result, object)  # Verify that the result matches the original
})

# init_params_filter_valid_numeric_vec 

test_that("init_params_filter_valid_numeric_vec filters numeric vector correctly", {
  vec <- c(10, 20, 30, 40, 50)
  name <- "test_vector"
  indices <- c(1, 3, 5)
  result <- init_params_filter_valid_numeric_vec(vec, name, indices)
  expect_equal(result, c(10, 30, 50))
})

test_that("init_params_filter_valid_numeric_vec handles empty indices correctly", {
  vec <- c(10, 20, 30, 40, 50)
  name <- "test_vector"
  indices <- integer(0)
  result <- init_params_filter_valid_numeric_vec(vec, name, indices)
  expect_equal(result, numeric(0)) # Should return an empty numeric vector
})

test_that("init_params_filter_valid_numeric_vec throws an error for non-numeric input", {
  vec <- c("a", "b", "c")
  name <- "test_vector"
  indices <- c(1, 2)
  expect_error(
    init_params_filter_valid_numeric_vec(vec, name, indices), 
    "test_vector must be numeric."
  )
})

test_that("init_params_filter_valid_numeric_vec throws an error for multi-dimensional input", {
  vec <- matrix(c(10, 20, 30, 40), nrow = 2)
  name <- "test_matrix"
  indices <- c(1, 2)
  expect_error(
    init_params_filter_valid_numeric_vec(vec, name, indices), 
    "test_matrix must have NULL dimensions."
  )
})

test_that("init_params_filter_valid_numeric_vec handles edge case of all indices", {
  vec <- c(10, 20, 30, 40, 50)
  name <- "test_vector"
  indices <- seq_along(vec) # All indices
  result <- init_params_filter_valid_numeric_vec(vec, name, indices)
  expect_equal(result, vec)
})

test_that("init_params_filter_valid_numeric_vec throws an error for out-of-bounds indices", {
  vec <- c(10, 20, 30, 40, 50)
  name <- "test_vector"
  indices <- c(1, 6) # 6 is out of bounds
  msg <- paste0(name, " contains NA values.")
  expect_warning(
    init_params_filter_valid_numeric_vec(vec, name, indices),
    msg
  )
})




# TEST SAVE ---------------------------------------------------------------

test_that("init_params_save_to_dir returns NULL and issues a warning for non-existent directory", {
  init_params <- list(a = 1, b = 2, c = 3)
  
  # Create a unique temporary directory path (non-existent)
  save_params_dir <- tempfile()
  
  # Ensure the directory does not exist
  unlink(save_params_dir, recursive = TRUE)  # Ensure the directory doesn't exist
  
  # Capture the warning using withCallingHandlers
  warning_message <- NULL
  result <- withCallingHandlers(
    {
      init_params_save_to_dir(init_params, save_params_dir)
    },
    warning = function(w) {
      warning_message <<- conditionMessage(w)  # Capture the warning message
      invokeRestart("muffleWarning")          # Suppress the warning
    }
  )
  
  # Validate the captured warning
  expect_match(warning_message, paste("Directory does not exist:", save_params_dir))
  expect_null(result)  # Ensure the function returns NULL
})

test_that("init_params_save_to_dir successfully saves file to an existing directory", {
  init_params <- list(a = 1, b = 2, c = 3)
  save_params_dir <- tempdir(check = TRUE)  # Use a temporary directory
  suffix_name <- "test_suffix"
  
  # Cleanup using on.exit
  saved_files <- NULL
  on.exit({
    if (!is.null(saved_files)) {
      unlink(saved_files)
    }
  })
  
  # Ensure messages don't disrupt testing
  withCallingHandlers(
    init_params_save_to_dir(init_params, save_params_dir, suffix_name),
    message = function(m) invokeRestart("muffleMessage")
  )
  
  # Check that the file exists
  saved_files <- list.files(save_params_dir, pattern = "initMultiSiteParams_.*test_suffix.RData", full.names = TRUE)
  expect_true(length(saved_files) == 1)  # Ensure one file was created
  
  # Load the saved file to verify its content
  loaded_data <- new.env()
  load(saved_files, envir = loaded_data)
  expect_equal(loaded_data$init_params, init_params)  # Verify saved content
})

test_that("init_params_save_to_dir saves file without suffix_name", {
  init_params <- list(a = 1, b = 2, c = 3)
  save_params_dir <- tempdir(check = TRUE)  # Use a temporary directory
  
  # Cleanup using on.exit
  saved_files <- NULL
  on.exit({
    if (!is.null(saved_files)) {
      unlink(saved_files)
    }
  })
  
  # Ensure messages don't disrupt testing
  withCallingHandlers(
    init_params_save_to_dir(init_params, save_params_dir),
    message = function(m) invokeRestart("muffleMessage")
  )
  
  # Check that the file exists
  saved_files <- list.files(save_params_dir, pattern = "initMultiSiteParams_.*\\.RData", full.names = TRUE)
  expect_true(length(saved_files) > 0)  # Ensure at least one file was created
  
  # Load the saved file to verify its content
  loaded_data <- new.env()
  load(saved_files[1], envir = loaded_data)
  expect_equal(loaded_data$init_params, init_params)  # Verify saved content
})







# TEST MAIN_FUN -----------------------------------------------------------

test_that("init_wrapper runs when save_params_dir is NULL", {

  mock_save <- mock()
  mock_InitMultiSite <- mock()
  mock_print <- mock()
  
  local_mock(
    init_params_save_to_dir = mock_save,
    InitMultiSite = mock_InitMultiSite,
    print = mock_print
  )
  
  save_params_args <- list(save_params_dir = NULL)
  
  result <- init_wrapper(
    save_params_args = save_params_args,
    nYearsMS = nYearsMS,
    siteInfo = siteInfo,
    multiInitVar = multiInitVar,
    PAR = PAR,
    TAir = TAir,
    VPD = VPD,
    Precip = Precip,
    CO2 = CO2
  )
  
  # Check if the mock functions were called
  expect_called(mock_save, 0) # This asserts that the mock was NOT triggered since saving is skipped
  expect_called(mock_InitMultiSite, 1)
})

test_that("init_wrapper runs when save_params_dir is not NULL", {
  
  mock_save <- mock()
  mock_InitMultiSite <- mock()
  mock_print <- mock()
  
  local_mock(
    init_params_save_to_dir = mock_save,
    InitMultiSite = mock_InitMultiSite,
    print = mock_print
  )
  
  save_params_args <- list(save_params_dir = "mock/dir", save_n_rows = 7)
  
  result <- init_wrapper(
    save_params_args = save_params_args,
    nYearsMS = nYearsMS,
    siteInfo = siteInfo,
    multiInitVar = multiInitVar,
    PAR = PAR,
    TAir = TAir,
    VPD = VPD,
    Precip = Precip,
    CO2 = CO2
  )
  
  # Check if the mock functions were called
  expect_called(mock_save, 1)
  expect_called(mock_InitMultiSite, 1)
  
})

test_that("init_wrapper passes correct args to save function", {
  
  mock_save <- mock()
  mock_InitMultiSite <- mock()
  mock_print <- mock()
  
  local_mock(
    init_params_save_to_dir = mock_save,
    InitMultiSite = mock_InitMultiSite,
    print = mock_print
  )
  
  save_params_args <- list(save_params_dir = "mock/dir", save_n_rows = 7, is_sample = FALSE)
  
  result <- init_wrapper(
    save_params_args = save_params_args,
    nYearsMS = nYearsMS,
    siteInfo = siteInfo,
    multiInitVar = multiInitVar,
    PAR = PAR,
    TAir = TAir,
    VPD = VPD,
    Precip = Precip,
    CO2 = CO2
  )
  
  # Check if the mock functions were called
  expect_called(mock_save, 1)
  expect_called(mock_InitMultiSite, 1)
  
  # Assert that arguments were passed correctly
  # mockery::expect_call(mock_save, 1, "mock/dir")
  init_params <- list(nYearsMS = nYearsMS,
                      siteInfo = siteInfo,
                      multiInitVar = multiInitVar,
                      PAR = PAR,
                      TAir = TAir,
                      VPD = VPD,
                      Precip = Precip,
                      CO2 = CO2)
  
  expect_args(mock_object = mock_save, 
              n= 1,
              init_params = init_params,
              suffix_name = NULL,
              save_params_dir = "mock/dir")
})




