#' Title
#'
#' @param collection_table
#' @param codes
#' @param primary
#'
#' @return
#' @export
#'
#' @examples
gether_diagnosis_outpatient_2 <- function(table_list, db_con, codes) {

  outpatient_tables <- table_list[str_detect(table_list, "outpatient")] %>%
    str_split("_")

  outpatient_tables <- do.call(rbind, outpatient_tables) %>%
    data.frame()
  colnames(outpatient_tables) <- c("table", "diagnosis", "source", "year")

  outpatient_dx_tables <- outpatient_tables %>%
    filter(str_detect(diagnosis, "dx"))

  outpatient_dxs <- list()
  for (i in 1:nrow(outpatient_dx_tables)) {

    table <- outpatient_dx_tables[i,1]
    diagnosis <- outpatient_dx_tables[i,2]
    source <- outpatient_dx_tables[i,3]
    year <- outpatient_dx_tables[i,4]
    tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")

    tmp <- dplyr::tbl(db_con, tab_name) %>%
      dplyr::mutate(dx = as.character(dx)) %>%
      dplyr::filter(dx %in% codes) %>%
      dplyr::select(enrolid, date = svcdate, dx_num, dx) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::mutate(dx = as.character(dx))

    outpatient_dxs[[i]] <- tmp

    print(paste0(i, " of ", nrow(outpatient_dx_tables)))

  }

  outpatient_dxs <- bind_rows(outpatient_dxs)
  return(outpatient_dxs)

}
