rm(list=ls(all.names=TRUE))

#set the directory where the script is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)


namedataset <- "D4_analytical_dataset"

# Set seed for reproducibility
set.seed(1234)

# create base of exposed
df_size <- 100
data <- data.table::data.table(episode_id = 1:df_size #, 
                               # person_id
                               # gender
                               # ageband                         
                               # date_bleeding
                               # type_bleeding
                               # period
                               # outcome_AMI
                               # outcome_IS
                               # outcome_VTE
                               # outcome_TIA
                               # outcome_PE
                               # outcome_DIC
                               # outcome_DEATH
                               # covariate_1
                               # …
                               # covariate_26
)

# data <- ...



# saveRDS(data, file = paste0(thisdir, "/", namedataset,".rds"))
