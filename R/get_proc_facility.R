#' Title
#'
#' @param table
#' @param source
#' @param year
#' @param codes
#' @param primary
#'
#' @return
#' @export
#'
#' @examples
get_proc_facility <- function(table, source, year, codes, primary = TRUE) {

  tab_name <- paste0(c(table, "proc", source, year), collapse = "_")
  tab <- dplyr::tbl(con, tab_name) %>%
    dplyr::filter(proc %in% codes) %>%
    dplyr::select(svcdate, proc, enrolid) %>%
    dplyr::collect(n = Inf)

  return(tab)


}
