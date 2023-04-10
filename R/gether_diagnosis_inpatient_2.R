#' Title
#'
#' @param table_list
#' @param codes
#'
#' @return
#' @export
#'
#' @examples
gether_diagnosis_inpatient_2 <- function(table_list, db_con, codes) {

  inpatient_tables <- table_list[str_detect(table_list, "inpatient")] %>%
    str_split("_")

  inpatient_tables <- do.call(rbind, inpatient_tables) %>%
    data.frame()
  colnames(inpatient_tables) <- c("table", "diagnosis", "source", "year")

  inpatient_dx_tables <- inpatient_tables %>%
    filter(str_detect(diagnosis, "dx"))

  inpatient_dxs <- list()
  for (i in 1:nrow(inpatient_dx_tables)) {

    table <- inpatient_dx_tables[i,1]
    diagnosis <- inpatient_dx_tables[i,2]
    source <- inpatient_dx_tables[i,3]
    year <- inpatient_dx_tables[i,4]
    tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")
    cross <- paste0(c(table, "core", source, year), collapse = "_")

    tmp <- dplyr::tbl(db_con, tab_name) %>%
      dplyr::mutate(dx = as.character(dx)) %>%
      dplyr::filter(dx %in% codes) %>%
      dplyr::inner_join(dplyr::tbl(db_con, cross) %>% dplyr::select(caseid, date = admdate, enrolid), by = "caseid") %>%
      dplyr::select(-caseid) %>%
      dplyr::select(enrolid, date, dx_num, dx) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::mutate(dx = as.character(dx))

    inpatient_dxs[[i]] <- tmp

    print(paste0(i, " of ", nrow(inpatient_dx_tables)))

  }

  inpatient_dxs <- bind_rows(inpatient_dxs)
  return(inpatient_dxs)

}
