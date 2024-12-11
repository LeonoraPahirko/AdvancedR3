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


#' Title
#'
#' @param data
#'
#' @return A plot object
#'
plot_distributions <- function(data) {
  data |>
    ggplot2::ggplot(aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(vars(metabolite), scales = "free")
}
