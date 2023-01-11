#' Title
#'
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
get_diagnosis_outpatient <- function(diagnosis, source, year, codes, primary = FALSE) {

  tab_name <- paste0(c("outpatient", diagnosis, source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::mutate(dx = as.character(dx)) %>%
    dplyr::filter(dx %in% codes & (dx_num == 1 | primary == FALSE)) %>%
    dplyr::select(enrolid, svcdate, dx_num, dx) %>%
    dplyr::collect(n = Inf)

  return(tab)


}
