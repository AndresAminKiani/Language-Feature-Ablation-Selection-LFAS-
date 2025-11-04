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
  return(path[1])
}
data <- find_file("Master9_Mtch_117x2_v13_with_PSs_1-1-18_Corrected634ID_zscore.xlsx")

## How many iterations of models do we need/want to run?
Num_of_iteration <- 200

pathname = '/Users/susmusharma/Desktop/Classes_Records/First_Year_Paper/R_Codes/New_R_Codes_for_AllChildren//RunElasticNetFunction_andSaveModelOutputs'
## Grabbing models output for the number of iterations using the model function
Function_IterativelySaves_ModelOutput_3(data, Num_of_iteration, pathname)




