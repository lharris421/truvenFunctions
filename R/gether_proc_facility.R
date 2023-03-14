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
gether_diagnosis_facility <- function(table_list, db_con, codes = NULL, dates = NULL, range = NULL, primary = FALSE, year_range = NULL) {

  facility_tables <- table_list[str_detect(table_list, "facility")] %>%
    str_split("_")

  facility_tables <- do.call(rbind, facility_tables) %>%
    data.frame()
  colnames(facility_tables) <- c("table", "diagnosis", "source", "year")

  facility_proc_tables <- facility_tables %>%
    filter(str_detect(diagnosis, "proc"))

  if (!is.null(year_range)) {
    facility_proc_tables %<>%
      filter(year >= year_range[1] & year <= year_range[2])
  }

  facility_procs <- list()
  for (i in 1:nrow(facility_proc_tables)) {

    table <- facility_proc_tables[i,1]
    diagnosis <- facility_proc_tables[i,2]
    source <- facility_proc_tables[i,3]
    year <- facility_proc_tables[i,4]
    tab_name <- paste0(c(table, diagnosis, source, year), collapse = "_")

    tmp <- dplyr::tbl(db_con, tab_name) %>%
      dplyr::mutate(proc = as.character(proc)) %>%
      dplyr::filter(proc_num == 1 | primary == FALSE) %>%
      dplyr::select(enrolid, date = svcdate, proc_num, proc, fachdid) %>%
      dplyr::collect(n = Inf) %>%
      dplyr::mutate(proc = as.character(proc)) %>%
      dplyr::filter(is.null(codes) | proc %in% codes)

    if (!is.null(dates)) {

      tmp <- tmp %>%
        dplyr::left_join(dates, by = "enrolid") %>%
        dplyr::mutate(days_within = index_date - date) %>%
        dplyr::filter(days_within >= range[1] & days_within <= range[2])

    }

    print(paste0(i, " of ", nrow(facility_proc_tables)))
    facility_procs[[i]] <- tmp


  }

  facility_procs <- bind_rows(facility_procs)

  facility_core_tables <- all_tables[str_detect(table_list, "facility_core")]
  facility_facility_outpatient <- list()

  for (i in 1:length(facility_core_tables)) {

    facility_facility_outpatient[[i]] <- tbl(db_con, facility_core_tables[i]) %>%
      mutate(facility = ifelse(caseid == "", 0, 1)) %>%
      select(fachdid, facility) %>%
      collect(n = Inf)

  }

  facility_facility_outpatient <- bind_rows(facility_facility_outpatient)

  facility_procs %<>%
    left_join(facility_facility_outpatient, by = "fachdid") %>%
    mutate(facility = ifelse(is.na(facility) | facility == 0, 0, 1)) %>%
    select(-fachdid)

  return(facility_procs)

}

