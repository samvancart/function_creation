


# TEST GET SITES ----------------------------------------------------------

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





