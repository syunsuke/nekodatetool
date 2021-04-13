#' classify by year
#'
#' make label for classify by year.
#'
#' @param ts vector of Date
#'
#' @return vector of Date
#' @export
#'
#' @examples
#' classify_year(as.Date("2020-01-01"))
classify_year <- function(ts){

  # Date型のデータを先に確保する方法
  ans <- rep(as.Date("2000-01-01"), length(ts))

  for(i in seq_along(ts)){

    label_year <- lubridate::year(ts[[i]])
    label_month <- 1
    label_day <- 1
    .month <- lubridate::month(ts[[i]])

    label_date <-
      lubridate::ymd(
        sprintf("%d-%d-%d",
                label_year,
                label_month,
                label_day))

    ans[[i]] <- label_date
  }

  return(ans)
}

#' classify by quarter
#'
#' make label for classify by quarter.
#'
#' @param ts vector of Date
#'
#' @return vector of Date
#' @export
#'
#' @examples
#' classify_quarter(as.Date("2020-01-01"))
classify_quarter <- function(ts){

  # Date型のデータを先に確保する方法
  ans <- rep(as.Date("2000-01-01"), length(ts))

  for(i in seq_along(ts)){

    label_year <- lubridate::year(ts[[i]])
    label_month <- NULL
    label_day <- 1
    .month <- lubridate::month(ts[[i]])

    ifelse(.month < 4,
           label_month <- 1,
           ifelse(.month < 7,
                  label_month <- 4,
                  ifelse(.month < 10,
                         label_month <- 7,
                         label_month <- 10)))

    label_date <-
      lubridate::ymd(
        sprintf("%d-%d-%d",
                label_year,
                label_month,
                label_day))

    ans[[i]] <- label_date
  }

  return(ans)
}

#' classify by month
#'
#' make label for classify by month.
#'
#' @param ts vector of Date
#'
#' @return vector of Date
#' @export
#'
#' @examples
#' classify_month(as.Date("2020-01-01"))
classify_month <- function(ts){

  # Date型のデータを先に確保する方法
  ans <- rep(as.Date("2000-01-01"), length(ts))

  for(i in seq_along(ts)){

    label_year <- lubridate::year(ts[[i]])
    label_month <- lubridate::month(ts[[i]])
    label_day <- 1

    label_date <-
      lubridate::ymd(
        sprintf("%d-%d-%d",
                label_year,
                label_month,
                label_day))

    ans[[i]] <- label_date
  }

  return(ans)
}
