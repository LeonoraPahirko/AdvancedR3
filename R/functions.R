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



#' Title
#'
#' @param data The lipidomics data set (wide)
#' @param metabolite_varible The column of the lipidomics metabolite
#'
#' @return A recipe object
create_recipe_spec <- function(data, metabolite_varible) {
  recipes::recipe(data) |>
    recipes::update_role(
      {{ metabolite_varible }},
      age,
      gender,
      new_role = "predictor"
    ) |>
    recipes::update_role(class, new_role = "outcome") |>
    recipes::step_normalize(tidyselect::starts_with("metabolite_"))
}


#' Create a workflow object
#'
#' @param model_specs The model specs
#' @param recipe_specs The recipe specs
#'
#' @return A workflow object
create_model_workflow <- function(model_specs, recipe_specs) {
  workflows::workflow() |>
    workflows::add_model(model_specs) |>
    workflows::add_recipe(recipe_specs)
}



#' Create a tidy output of the model results
#'
#' @param workflow_fitted_model The model workflow object that has been fitted
#'
#' @return A data frame
tidy_model_output <- function(workflow_fitted_model) {
  workflow_fitted_model |>
    workflows::extract_fit_parsnip() |>
    broom::tidy(exponentiate = TRUE)
}



#' Convert the long form data set into a list of wide form data frames based on metabolites
#'
#' @param data The lipidomics dataset
#'
#' @return A list of data frames
split_by_metabolites <- function(data) {
  data |>
    column_values_to_snake_case(metabolite) |>
    dplyr::group_split(metabolite) |>
    purrr::map(metabolites_to_wider)
}




#' Generate the result of a model
#'
#' @param data The lipidomics dataset
#'
#' @return A data frame
generate_model_results <- function(data) {
  create_model_workflow(
    parsnip::logistic_reg() |>
      parsnip::set_engine("glm"),
    data |>
      create_recipe_spec(tidyselect::starts_with("metabolite_"))
  ) |>
    parsnip::fit(data) |>
    tidy_model_output()
}
