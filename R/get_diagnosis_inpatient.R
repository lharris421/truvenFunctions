#' Title
#'
#' @param table
#' @param diagnosis
#' @param source
#' @param year
#' @param codes
#' @param primary
#'
#' @return
#' @export
#'
#' @examples
get_diagnosis_inpatient <- function(table, diagnosis, source, year, codes, primary = FALSE) {

  tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")
  cross <- paste0(c(table, "core", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::mutate(dx = as.character(dx)) %>%
    dplyr::filter(dx %in% codes & (dx_num == 1 | primary == FALSE)) %>%
    dplyr::select(caseid, dx_num, dx) %>%
    dplyr::inner_join(dplyr::tbl(con, cross) %>% dplyr::select(caseid, admdate, enrolid), by = "caseid") %>%
    dplyr::select(-caseid) %>%
    dplyr::collect(n = Inf)

  return(tab)


}
