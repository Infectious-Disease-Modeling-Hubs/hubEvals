#' Title
#'
#' @param model_output_data
#' @param target_data
#'
#' @return A data.frame in the scoringutils format
#' @export
#'
#' @examples

to_scoringutils_quantile <- function(
    model_output_data,
    target_data
) {

  ## validate inputs
  # general input checks
  # model_output_data needs to have a column `value`
  # model_output_data needs to have a column `model_id`
  # target_data needs to have a column `value`

  # model_output_data and target_data need to have the same column names apart
  # from the ones mentioned above (so the Task ID columns need be the same)

  # input check specific to quantile format
  # model_output_data needs to have a column `output_type_id` with value "quantile"

  ## merge model_out and target_data
  data <- left_join(example_quantile_model_output |> rename(predicted = value),
                    example_processed_target_data |> rename(observed = value)) |>
    rename(quantile = output_type_id,
           model = model_id)

  return(data)
}



validate_input <- function() {

}

 |>
  select(-output_type) |>
  scoringutils::score()
