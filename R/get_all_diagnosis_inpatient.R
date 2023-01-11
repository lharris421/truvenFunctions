#' Title
#'
#' @param table
#' @param diagnosis
#' @param source
#' @param year
#' @param dates
#'
#' @return
#' @export
#'
#' @examples
get_all_diagnosis_inpatient <- function(table, diagnosis, source, year, dates) {

  tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")
  cross <- paste0(c(table, "core", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::select(caseid, dx) %>%
    dplyr::inner_join(dplyr::tbl(con, cross) %>% dplyr::select(caseid, admdate, enrolid), by = "caseid") %>%
    dplyr::select(-caseid) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::inner_join(dates, by = c("admdate" = "svcdate", "enrolid" = "enrolid"))

  return(tab)


}
