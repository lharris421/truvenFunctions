#' Title
#'
#' @param diagnosis
#' @param source
#' @param year
#' @param dates
#' @param range
#'
#' @return
#' @export
#'
#' @examples
get_all_diagnosis_outpatient_range <- function(diagnosis, source, year, dates, range) {

  tab_name <- paste0(c("outpatient", diagnosis, source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::select(enrolid, svcdate, dx) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::left_join(dates, by = "enrolid") %>%
    dplyr::mutate(days_within = index_date - svcdate) %>%
    dplyr::filter(days_within >= range[1] & days_within <= range[2])

  return(tab)


}
