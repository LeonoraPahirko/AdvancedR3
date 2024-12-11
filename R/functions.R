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
          sd = sd,
          median = median,
          iqr = IQR
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
plot_distributions <- function(data) {
  data |>
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free")
}



#' Title: Converting column variables to snake case
#'
#' @param data The lipidomics data
#' @param columns The columns you want to convert
#'
#' @return A data.frame/tibble
column_values_to_snake_case <- function(data, columns) {
  data |>
    dplyr::mutate(dplyr::across({{ columns }}, snakecase::to_snake_case))
}




#' Title
#'
#' @param data
#'
#' @return A data.frame/tibble
metabolites_to_wider <- function(data) {
    data |>
        tidyr::pivot_wider(
            names_from = metabolite,
            values_from = value,
            values_fn = mean,
            names_prefix = "metabolite_"
        )
}
