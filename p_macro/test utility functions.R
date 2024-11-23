extract_last_hash <- function(path) {
  git2r::blame(path = path)[["hunks"]][[1]]
}

check_filenames <- function(filenames, truth_folder, current_folder, custom_message) {
  
  files_GT_not_other <- setdiff(filenames[[truth_folder]], filenames[[current_folder]])
  files_other_not_GT <- setdiff(filenames[[current_folder]], filenames[[truth_folder]])
  
  flag <- F
  
  temp_df <- data.table()
  
  if (length(files_GT_not_other) > 0) {
    msg <- paste0("Some files are in the ground truth folder but not in the ", custom_message, " folder", "\n",
                  "Files: ", paste(files_GT_not_other, collapse = ", "))
    temp_df <- rbindlist(list(temp_df, data.table(folder = current_folder, type = "Check file names",
                                                  result = F, comment = msg)))
    flag <- T
  }
  
  if (length(files_other_not_GT) > 0) {
    msg <- paste0("Some files are in the ", custom_message, " folder but not in the ground truth folder", "\n",
                  "Files: ", paste(files_other_not_GT, collapse = ", "))
    temp_df <- rbindlist(list(temp_df, data.table(folder = current_folder, type = "Check file names",
                                                  result = F, comment = msg)))
    flag <- T
  }
  
  if (!flag) {
    temp_df <- rbindlist(list(temp_df, data.table(folder = current_folder, type = "Check file names",
                                                  result = T, comment = "")))
  }
  
  return(temp_df)
  
}

check_filenames_2 <- function(filenames, truth_folder, current_folder, custom_message) {
  
  files_GT_not_other <- setdiff(filenames[[truth_folder]], filenames[[current_folder]])
  files_other_not_GT <- setdiff(filenames[[current_folder]], filenames[[truth_folder]])
  
  flag <- F
  
  temp_df <- data.table()
  
  if (length(files_GT_not_other) > 0) {
    msg <- paste0("Some files are in the ", custom_message[["OP"]], " but not in the ", custom_message[["QC"]], " folder", "\n",
                  "Files: ", paste(files_GT_not_other, collapse = ", "))
    temp_df <- rbindlist(list(temp_df, data.table(folder = current_folder, type = "Check file names",
                                                  result = F, comment = msg)))
    flag <- T
  }
  
  if (length(files_other_not_GT) > 0) {
    msg <- paste0("Some files are in the ", custom_message[["QC"]], " folder but not in the ", custom_message[["OP"]], " folder", "\n",
                  "Files: ", paste(files_other_not_GT, collapse = ", "))
    temp_df <- rbindlist(list(temp_df, data.table(folder = current_folder, type = "Check file names",
                                                  result = F, comment = msg)))
    flag <- T
  }
  
  if (!flag) {
    temp_df <- rbindlist(list(temp_df, data.table(folder = current_folder, type = "Check file names",
                                                  result = T, comment = "")))
  }
  
  return(temp_df)
  
}

check_columns <- function(GT, other, current_file, custom_message) {
  
  cols_GT_not_other <- setdiff(colnames(GT), colnames(other))
  cols_other_not_GT <- setdiff(colnames(other), colnames(GT))
  
  flag <- F
  temp_df <- data.table()
  
  if (length(cols_GT_not_other) > 0) {
    msg <- paste0("Some variables are in the ground truth dataset but not in the ", custom_message, " dataset", "\n",
                  "Variables: ", paste(cols_GT_not_other, collapse = ", "))
    single_test <- data.table(folder = folder, filename = current_file, type = "Check variable existence",
                              result = F, comment = msg)
    temp_df <- rbindlist(list(temp_df, single_test))
    flag <- T
  }
  
  if (length(cols_other_not_GT) > 0) {
    msg <- paste0("Some variables are in the ", custom_message, " dataset but not in the ground truth dataset", "\n",
                  "Variables: ", paste(cols_other_not_GT, collapse = ", "))
    single_test <- data.table(folder = folder, filename = current_file, type = "Check variable existence",
                              result = F, comment = msg)
    temp_df <- rbindlist(list(temp_df, single_test))
    flag <- T
  }
  
  if (!flag) {
    single_test <- data.table(folder = folder, filename = current_file, type = "Check variable existence",
                              result = T, comment = "")
    temp_df <- rbindlist(list(temp_df, single_test))
  }
  
  classes_GT <- sapply(GT, function(x) normalize_class(class(x)))
  classes_other <- sapply(other, function(x) normalize_class(class(x)))
  
  common_cols <- intersect(colnames(GT), colnames(other))
  
  diff_cols <- common_cols[classes_GT[common_cols] != classes_other[common_cols]]
  diff_cols_GT <- classes_GT[diff_cols]
  diff_cols_other <- classes_other[diff_cols]
  
  if (length(diff_cols) == 0) {
    msg <- ""
    result = T
  } else {
    msg <- paste("Variable", diff_cols, "has format", paste(diff_cols_other, collapse = ", "), "however it has format",
                 paste(diff_cols_GT, collapse = ", "), "in the GT", collapse = "; \n")
    result = F
  }
  
  single_test <- data.table(folder = folder, filename = current_file, type = "Check variable type",
                            result = result, comment = msg)
  temp_df <- rbindlist(list(temp_df, single_test))
  
  return(temp_df)
  
}


check_columns_2 <- function(GT, other, current_file, custom_message) {
  
  cols_GT_not_other <- setdiff(colnames(GT), colnames(other))
  cols_other_not_GT <- setdiff(colnames(other), colnames(GT))
  
  flag <- F
  temp_df <- data.table()
  
  if (length(cols_GT_not_other) > 0) {
    msg <- paste0("Some variables are in the ", custom_message[["GT"]], " dataset but not in the ", custom_message[["QC"]], " dataset", "\n",
                  "Variables: ", paste(cols_GT_not_other, collapse = ", "))
    single_test <- data.table(folder = folder, filename = current_file, type = "Check variable existence",
                              result = F, comment = msg)
    temp_df <- rbindlist(list(temp_df, single_test))
    flag <- T
  }
  
  if (length(cols_other_not_GT) > 0) {
    msg <- paste0("Some variables are in the ", custom_message[["QC"]], " dataset but not in the ", custom_message[["GT"]], " dataset", "\n",
                  "Variables: ", paste(cols_other_not_GT, collapse = ", "))
    single_test <- data.table(folder = folder, filename = current_file, type = "Check variable existence",
                              result = F, comment = msg)
    temp_df <- rbindlist(list(temp_df, single_test))
    flag <- T
  }
  
  if (!flag) {
    single_test <- data.table(folder = folder, filename = current_file, type = "Check variable existence",
                              result = T, comment = "")
    temp_df <- rbindlist(list(temp_df, single_test))
  }
  
  classes_GT <- sapply(GT, function(x) normalize_class(class(x)))
  classes_other <- sapply(other, function(x) normalize_class(class(x)))
  
  common_cols <- intersect(colnames(GT), colnames(other))
  
  diff_cols <- common_cols[classes_GT[common_cols] != classes_other[common_cols]]
  diff_cols_GT <- classes_GT[diff_cols]
  diff_cols_other <- classes_other[diff_cols]
  
  if (length(diff_cols) == 0) {
    msg <- ""
    result = T
  } else {
    msg <- paste("Variable", diff_cols, "has format", paste(diff_cols_other, collapse = ", "), "however it has format",
                 paste(diff_cols_GT, collapse = ", "), "in the OP", collapse = "; \n")
    result = F
  }
  
  single_test <- data.table(folder = folder, filename = current_file, type = "Check variable type",
                            result = result, comment = msg)
  temp_df <- rbindlist(list(temp_df, single_test))
  
  return(temp_df)
  
}

normalize_class <- function(class_name) {
  fcase(
    any(class_name %in% c("Date", "IDate")), "Date",
    any(class_name %in% c("numeric", "integer", "double")), "numeric",
    default = class_name[1]
  )
}

