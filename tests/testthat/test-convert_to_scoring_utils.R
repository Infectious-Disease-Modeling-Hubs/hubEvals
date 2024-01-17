
test_that("convert_to_scoring_utils works", {

  # using identical dataframes to represent model output
  # and processed target data should pass validations
  model_output_columns <- list(
    output_type = c("quantile", "quantile"),
    location = c("US", "20"),
    output_type_id = c(.1, .250),
    value = c(11, 22),
    model_id = c("model1", "model2"),
    origin_date = c("2023-01-17", "2023-01-17"),
    horizon = c(-4, 3)
  )

  # test correctly-formatted model_output and target dataframes
  model_output <- data.frame(model_output_columns)
  target_data <- dplyr::select(
    model_output, origin_date, location, horizon, value
  )

  expect_true(validate_inputs(model_output, target_data))

  # missing a required model_output field should fail validation
  missing_output_type <- dplyr::select(
    model_output, -output_type
  )
  # TODO: could be more specific about the error message
  expect_error(validate_inputs(missing_output_type, target_data))

})