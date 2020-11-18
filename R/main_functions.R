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
#' @examples
#'
#' fit_cp_model(count_data = visit_counts,
#'              low = 40,high = 180,
#'              model = "lm",
#'              plot = FALSE)
#'
#' @export
fit_cp_model <- function(count_data,low,high,model="lm",plot=TRUE,level=0.95){

  # update data to include time component
  count_data <- count_data %>%
    dplyr::arrange(.data$days_since_dx) %>%
    dplyr::mutate(t=dplyr::row_number())

  # safety step so we don't try to fit a model to less than a week of data
  if (low>=high-7){
    return(list(plot=NA,
                cp=NA,
                fits=NA))
  }

  ### FIT MODEL AND GENERATE PREDICTIONS

  if (model=="lm"){

    # LINEAR TREND MODEL

    tmp_fit <- count_data %>%
      dplyr::filter(dplyr::between(-days_since_dx,low,high)) %>%
      lm(n~t,data=.)

    tmp_pred <- cbind(count_data,predict(tmp_fit,newdata = count_data,interval="predict", level=level)) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_at(dplyr::vars(fit,lwr,upr),dplyr::funs(ifelse(days_since_dx< -high,NA,.))) %>%
      dplyr::mutate(type=ifelse(dplyr::between(-days_since_dx,low,high),"Fitted","Predicted")) %>%
      dplyr::filter(days_since_dx<0)

  } else if (model=="exp"){

    # EXPONENTIAL TREND MODEL

    tmp_fit <- count_data %>%
      dplyr::filter(dplyr::between(-days_since_dx,low,high)) %>%
      lm(log(n)~t,data=.)

    tmp_pred <- cbind(count_data,predict(tmp_fit,newdata = count_data,interval="predict", level=level)) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_at(dplyr::vars(fit,lwr,upr),dplyr::funs(exp)) %>%
      dplyr::mutate_at(dplyr::vars(fit,lwr,upr),dplyr::funs(ifelse(days_since_dx< -high,NA,.))) %>%
      dplyr::mutate(type=ifelse(dplyr::between(-days_since_dx,low,high),"Fitted","Predicted")) %>%
      dplyr::filter(days_since_dx<0)

  } else if (model=="lm_period") {

    # LINEAR MODEL WITH PERIODICITY

    tmp_dat <- count_data %>%
      dplyr::mutate(period=as.factor(t%%7))

    tmp_fit <- tmp_dat %>%
      dplyr::filter(dplyr::between(-days_since_dx,low,high)) %>%
      lm(n~t+period,data=.)

    tmp_pred <- cbind(tmp_dat,predict(tmp_fit,newdata = tmp_dat,interval="predict", level=level)) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_at(dplyr::vars(fit,lwr,upr),dplyr::funs(ifelse(days_since_dx< -high,NA,.))) %>%
      dplyr::mutate(type=ifelse(dplyr::between(-days_since_dx,low,high),"Fitted","Predicted")) %>%
      dplyr::filter(days_since_dx<0)

  } else if (model=="exp_period") {

    # EXPONENTIAL MODEL WITH PERIODICITY

    tmp_dat <- count_data %>%
      dplyr::mutate(period=as.factor(t%%7))

    tmp_fit <- tmp_dat %>%
      dplyr::filter(dplyr::between(-days_since_dx,low,high)) %>%
      lm(log(n)~t+period,data=.)

    tmp_pred <- cbind(tmp_dat,predict(tmp_fit,newdata = tmp_dat,interval="predict", level=level)) %>%
      tibble::as_tibble() %>%
      dplyr::mutate_at(dplyr::vars(fit,lwr,upr),dplyr::funs(exp)) %>%
      dplyr::mutate_at(dplyr::vars(fit,lwr,upr),dplyr::funs(ifelse(days_since_dx< -high,NA,.))) %>%
      dplyr::mutate(type=ifelse(dplyr::between(-days_since_dx,low,high),"Fitted","Predicted")) %>%
      dplyr::filter(days_since_dx<0)
  }

  if (level > 0){
    tmp_cp <- tmp_pred  %>%
      dplyr::mutate(above=n>upr) %>%
      dplyr::arrange(desc(days_since_dx)) %>%
      dplyr::mutate(tmp=cumsum(1-above)) %>%
      dplyr::filter(tmp>=1) %>%
      dplyr::filter(-days_since_dx==min(-days_since_dx)) %>%
      .$days_since_dx
  } else {
    # if level is set to 0 find the point where observed consistently exceeds the fitted value
    tmp_cp <- tmp_pred  %>%
      dplyr::mutate(above=n>fit) %>%
      dplyr::arrange(desc(days_since_dx)) %>%
      dplyr::mutate(tmp=cumsum(1-above)) %>%
      dplyr::filter(tmp>=1) %>%
      dplyr::filter(-days_since_dx==min(-days_since_dx)) %>%
      .$days_since_dx
  }

  p <- NA
  if (plot==TRUE){

    p <- tmp_pred %>%
      dplyr::filter(-days_since_dx<=high) %>%
      ggplot2::ggplot(ggplot2::aes(days_since_dx,n)) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() +
      ggplot2::geom_line(ggplot2::aes(y=fit,linetype=type),color="red",size=1.5) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=lwr,ymax=upr),fill="red",alpha=0.3) +
      ggplot2::geom_vline(ggplot2::aes(xintercept=tmp_cp)) +
      ggplot2::ggtitle(paste0(model," model (cp = ",-tmp_cp,")")) +
      ggplot2::theme(legend.position = c(0.2, 0.8))

  }

  return(list(plot=p,
              cp=-tmp_cp,
              fits=tmp_pred))

  }
