#' Fit Change Point
#'
#' @importFrom rlang .data
#
#' @param count_data A dataset of visit counts, must contain variables 'days_since_dx' and
#' 'n'. Where 'days_since_dx' is the number of days since diagnosis (negative values
#' before diagnosis) and 'n' are the number of visits
#' @param low the low value in the training window (i.e., the point closest to
#' diagnosis to include in the training window)
#' @param high the high value in the training window (i.e., the point furthest from
#' diagnosis to include in the training window)
#' @param model the model to be used. Current options include "lm", "exp", "lm_period",
#' "exp_period"
#'
#'
#' @export
fit_cp_model <- function(count_data,low,high,model="lm",plot=TRUE,level=0.95){

  count_data <- count_data %>%
    dplyr::arrange(.data$days_since_dx) %>%
    dplyr::mutate(t=dplyr::row_number())

  }
