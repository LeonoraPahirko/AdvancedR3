#' Title: Descriptive Statistics Table
#'
#' @param data
#'
#' @return A data.frame/tibble

descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(
      dplyr::across(
        value,
        list(
          mean = mean, # This is the function name
          sd = sd
        )
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric), ~ round(.x, digits = 1)
      )
    )
}
