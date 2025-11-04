#install.packages("readxl")
#install.packages("dplyr")
#install.packages("purrr") # useful for iterating over sheets

install.packages(setdiff(c("glmnet", "readxl", "openxlsx", "dplyr", "purrr"), 
                         rownames(installed.packages())))

library(readxl)
library(purrr)
library(dplyr)

file1_path <- "RunElasticNetFunction_andSaveModelOutputs/ModelsOutputTrain_200iterations.xlsx"
file2_path <- "RunElasticNetFunction_andSaveModelOutputs/Change_lambda1se/ModelsOutputTrain_200iterations_Lambda1se.xlsx"

# Get sheet names from each file
sheets1 <- excel_sheets(file1_path)
sheets2 <- excel_sheets(file2_path)

# Read all sheets into lists of data frames
excel_data1 <- map(sheets1, ~read_excel(file1_path, sheet = .x))
excel_data2 <- map(sheets2, ~read_excel(file2_path, sheet = .x))

# Assign names to the list elements for easier access (optional but recommended)
names(excel_data1) <- sheets1
names(excel_data2) <- sheets2

if (!identical(sort(sheets1), sort(sheets2))) {
  print("Sheet names are different. Files are not identical.")
} else {
  print("Sheet names are the same.")
}

if (identical(sort(sheets1), sort(sheets2))) {
  all_sheets_match <- TRUE
  for (sheet_name in sheets1) {
    df1 <- excel_data1[[sheet_name]]
    df2 <- excel_data2[[sheet_name]]
    
    # Compare data frames, optionally ignoring row order and/or column order
    if (!identical(df1, df2)) { # or use all.equal(df1, df2) for less strict comparison
      print(paste("Sheet:", sheet_name, "has differences."))
      all_sheets_match <- FALSE
      # You can add more detailed comparisons here if needed
      # For example:
      # diff_result <- all_equal(df1, df2, ignore_row_order = TRUE)
      # print(diff_result)
    }
  }
  
  if (all_sheets_match) {
    print("All sheets have the same values.")
  } else {
    print("Some sheets have different values.")
  }
}

