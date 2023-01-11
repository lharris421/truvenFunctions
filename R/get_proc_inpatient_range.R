#' Title
#'
#' @param source
#' @param year
#' @param dates
#' @param range
#'
#' @return
#' @export
#'
#' @examples
get_proc_inpatient_range <- function(source, year, dates, range) {

  tab_name <- paste0(c("inpatient_proc", source, year), collapse = "_")
  cross <- paste0(c("inpatient_core", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::select(caseid, proc) %>%
    dplyr::inner_join(dplyr::tbl(con, cross) %>% dplyr::select(caseid, admdate, enrolid), by = "caseid") %>%
    dplyr::select(-caseid) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::left_join(dates, by = "enrolid") %>%
    dplyr::mutate(days_within = index_date - admdate) %>%
    dplyr::filter(days_within >= range[1] & days_within <= range[2])

  return(tab)


}
