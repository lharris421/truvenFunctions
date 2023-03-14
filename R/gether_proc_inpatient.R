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
gether_proc_inpatient <- function(table_list, db_con, codes = NULL, dates = NULL, range = NULL, year_range = NULL, primary = FALSE) {

  inpatient_tables <- table_list[str_detect(table_list, "inpatient")] %>%
    str_split("_")

  inpatient_tables <- do.call(rbind, inpatient_tables) %>%
    data.frame()
  colnames(inpatient_tables) <- c("table", "diagnosis", "source", "year")

  inpatient_proc_tables <- inpatient_tables %>%
    filter(str_detect(diagnosis, "proc"))

  if (!is.null(year_range)) {
    inpatient_proc_tables %<>%
      filter(year >= year_range[1] & year <= year_range[2])
  }

  inpatient_procs <- list()
  for (i in 1:nrow(inpatient_proc_tables)) {

    table <- inpatient_proc_tables[i,1]
    diagnosis <- inpatient_proc_tables[i,2]
    source <- inpatient_proc_tables[i,3]
    year <- inpatient_proc_tables[i,4]
    tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")
    cross <- paste0(c(table, "core", source, year), collapse = "_")

    tmp <- dplyr::tbl(db_con, tab_name) %>%
      dplyr::filter(proc_num == 1 | primary == FALSE) %>%
      dplyr::select(caseid, proc) %>%
      dplyr::inner_join(dplyr::tbl(db_con, cross) %>% dplyr::select(caseid, date = admdate, enrolid), by = "caseid") %>%
      dplyr::select(-caseid) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::filter(is.null(codes) | proc %in% codes)

    if (!is.null(dates)) {

      tmp <- tmp %>%
        dplyr::left_join(dates, by = "enrolid") %>%
        dplyr::mutate(days_within = index_date - date) %>%
        dplyr::filter(days_within >= range[1] & days_within <= range[2])

    }

    inpatient_procs[[i]] <- tmp

    print(paste0(i, " of ", nrow(inpatient_proc_tables)))

  }

  inpatient_procs <- bind_rows(inpatient_procs)
  return(inpatient_procs)



}
