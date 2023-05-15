#' Load all codes and children codes for a disease
#'
#' @param code
#'
#' @return
#' @export
#'
#' @examples
load_disease_codes_full <- function(code) {

  codes <- load_disease_codes(c(code))
  icd9 <- as.icd9cm(codes[[code]]$icd9_codes)
  icd10 <- as.icd10cm(codes[[code]]$icd10_codes)
  icd9 <- unique(c(icd9, icd::children(icd9)))
  icd10 <- unique(c(icd10, icd::children(icd10)))
  all_codes <- c(icd9, icd10)

  return(all_codes)

}
