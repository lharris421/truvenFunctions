#' Title
#'
#' @param table_list
#' @param db_con
#'
#' @return
#' @export
#'
#' @examples
gether_enrollment_months <- function(table_list, db_con) {

  enrl_period_tabs <- table_list[stringr::str_detect(table_list, "enrollment_detail")]

  first <- TRUE
  for (tab in enrl_period_tabs) {

    yr <- stringr::str_extract(tab, ".{2}$")

    curr <- tbl(db_con, tab) %>%
      select(enrolid, month) %>%
      mutate(year = yr) %>%
      collect(n = Inf)

    if (first) {
      all_enrl <- curr
      first <- FALSE
    } else {
      all_enrl <- bind_rows(all_enrl, curr)
    }

  }

  return(all_enrl)

}
