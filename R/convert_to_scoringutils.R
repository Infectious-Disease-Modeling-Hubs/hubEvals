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


#' @import checkmate
validate_inputs <- function(model_output_data, target_data) {
  errors <- list()
  assert_data_frame(model_output_data)
  required_cols_output <- c(
    "value", "model_id", "output_type_id", "output_type"
  )

  if (!all(required_cols_output %in% names(model_output_data))) {
    errors[["model_output"]] <-
      paste0("model_output_data needs to have the following columns: ",
             paste(required_cols_output, collapse = ", "))
  }

  if (!("value" %in% names(target_data))) {
    errors[["target_date"]] <-
      paste0("target_data needs to have the following columns: `value`")
  }

  task_id_cols_output <- sort(
    setdiff(names(model_output_data), required_cols_output)
  )
  task_id_cols_target <- sort(setdiff(names(target_data), c("value")))

  if(!identical(task_id_cols_output, task_id_cols_target)) {
    errors[["task_id_cols"]] <-
      paste0(
        "model_output_data and target_data need to have matching column names."
      )
  }

  if (length(errors) > 0) {
    stop("Found the following issues: \n", paste(errors, collapse = "\n"))
  }
  return(invisible(NULL))
}

