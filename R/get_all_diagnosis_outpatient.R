#' Title
#'
#' @param diagnosis
#' @param source
#' @param year
#' @param dates
#'
#' @return
#' @export
#'
#' @examples
get_all_diagnosis_outpatient <- function(diagnosis, source, year, dates) {

  tab_name <- paste0(c("outpatient", diagnosis, source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::select(enrolid, svcdate, dx) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::inner_join(dates, by = c("svcdate" = "svcdate", "enrolid" = "enrolid"))

  return(tab)


}
