#' Plot SSD curve
#'
#' @importFrom rlang .data
#
#' @param count_data A dataset of visit counts, must contain variables 'days_since_dx' and
#' 'n'. Where 'days_since_dx' is the number of days since diagnosis (negative values
#' before diagnosis) and 'n' are the number of visits
#'
#' @examples
#'
#' plot_ssd_curve(count_data = visit_counts)
#'
#' @export
plot_ssd_curve <- function(count_data,max_window=NA){

  if (!is.na(max_window)){
    count_data <- count_data %>%
      dplyr::filter(-days_since_dx<max_window)
  }


  count_data %>%
    ggplot2::ggplot(ggplot2::aes(-days_since_dx,n)) +
    ggplot2::geom_point() +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_reverse() +
    ggplot2::xlab("Days before diagnosis") +
    ggplot2::ylab("Number of SSD visits")
}
