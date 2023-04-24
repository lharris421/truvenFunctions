#' Auxillary function to get results
#'
#' @param summ
#' @param outcome
#' @param full_res
#'
#' @return
#' @export
#'
#' @examples
get_res_cov <- function(summ, outcome, full_res = NULL) {

  tmp <- coef(summ)

  p1 <- round(exp(tmp[rownames(tmp) == outcome, colnames(tmp) == "Estimate"]), 3)
  p2 <- round(exp(tmp[rownames(tmp) == outcome, colnames(tmp) == "Estimate"] + c(-1,1)*qnorm(.975)*tmp[rownames(tmp) == outcome, colnames(tmp) == "Std. Error"]), 3)
  p3 <- round(tmp[rownames(tmp) == outcome, colnames(tmp) %in% c("Pr(>|z|)", "Pr(>|t|)")], 3)
  p3 <- ifelse(p3 == 0, "<.0005", as.character(p3))

  add <- data.frame(Outcome = outcome, Estimate = p1, CI = paste0("(", paste0(p2, collapse = ", "), ")"), pval = p3, stringsAsFactors = FALSE)
  print(add)

  if(is.null(full_res)) {
    full_res <<- add
  } else if (outcome %in% full_res$Outcome) {
    print("Warning, overwriting previous results")
    full_res[full_res$Outcome == outcome,] <<- add
  } else {
    full_res <<- bind_rows(full_res, add)
  }

}
