df_to_list_of_list <- function(x, code_col = "code", concepts_col = "event_abbreviation", codying_system_col = T,
                               codying_system_recode = "auto", imputed_tags = NULL, type_col = "type", system = "system") {
  
  if(!require(data.table)){install.packages("data.table")}
  suppressPackageStartupMessages(library(data.table))

  x <- data.table::as.data.table(x)
  
  if (!codying_system_col) {
    x <- x[, coding_system := "ATC"]
  }
  
  if (!is.null(imputed_tags)) {
    if (tolower(imputed_tags) %in% c("narrow", "n")) {
      imputed_tags <- "narrow"
    } else if (tolower(imputed_tags) %in% c("possible", "p")) {
      imputed_tags <- "possible"
    } else if (!imputed_tags) {
      x <- x[!is.na(tags) & tags != "" & !(get(type_col) %in% c("COV", "PrA")), ]
    } else {
      stop("imputed_tags accepts only values narrow or possible")
    }
    
    if (x[tags == "", .N] > 0) {
      message(paste(x[tags == "", .N], "tags have been recoded to", imputed_tags))
      x <- x[tags == "", tags := imputed_tags]
    }
  }
  
  if ("tags" %in% colnames(x)) {
    x <- x[!is.na(tags) & tags != "" | get(type_col) %in% c("COV", "PrA"),
           (concepts_col) := paste(system, get(concepts_col), get(type_col), tags, sep = "_")]
  }
  
  x <- x[, .SD, .SDcols = c(code_col, "coding_system", concepts_col)]
  
  if (tolower(codying_system_recode) == "auto") {
    # x[, coding_system := data.table::fcase(
    #   coding_system %in% c("ICPC", "ICPC2P", "ICPC2EENG", "ICPC2"), "ICPC",
    #   default = coding_system
    # )]
    x[, coding_system := data.table::fifelse(coding_system %in% c("ICPC", "ICPC2P", "ICPC2EENG", "ICPC2"),
                                             "ICPC", coding_system)]
  } else if (!isFALSE(codying_system_recode)) {
    x[codying_system_recode, on = c("coding_system" = colnames(codying_system_recode)[[1]]),
      "coding_system" := c(colnames(codying_system_recode)[[2]])]
  }
  
  if (code_col == "ATC.codes") {
    x <- x[, (code_col) := sapply(get(code_col), trimws, simplify = TRUE)]
  }
  
  x <- lapply(split(x, by = concepts_col, keep.by = F),
              split, by = "coding_system", keep.by = F)
  
  x <- lapply(x, sapply, unlist, use.names = F, simplify = F)
  
  return(x)
}

