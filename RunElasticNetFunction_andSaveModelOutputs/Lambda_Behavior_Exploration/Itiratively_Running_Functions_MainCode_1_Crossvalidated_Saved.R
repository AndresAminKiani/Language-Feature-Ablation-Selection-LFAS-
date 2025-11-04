## Specify the location (path and directory of the file) of the Montgomery database. 
library(easypackages)
libraries("readxl", "ggplot2", "ggalt", "openxlsx", "writexl", "rio", 
          "tidyverse", "pROC")
library(here)

find_file <- function(filename) {
  path <- list.files(path = here::here(), 
                     pattern = filename,
                     recursive = TRUE, 
                     full.names = TRUE)
  if (length(path) == 0) stop(paste("File", filename, "not found!"))
  if (length(path) > 1) warning("Multiple files found. Using the first one.")
  return(path[1])}

data <- find_file("Master9_Mtch_117x2_v13_with_PSs_1-1-18_Corrected634ID_zscore.xlsx")
pathname <- getwd()
#AllModels_Lambdas_Info <- data.frame()

## How many iterations of models do we need/want to run?
Num_of_iteration <- 200

## Grabbing models output for the number of iterations using the model function
Function_IterativelySaves_ModelOutput_3_Crossvalided_Saved(data, Num_of_iteration, pathname)




