library(deidentifyr)
library(tidyverse)
library(readxl)

setwd("~/6. research/covid student achievement")

## Read in raw excel file
##2020.03.03 - NS5341 SP81 2020 - Results.xlsx
files <- list.files(path='data/primarydata',full.names = TRUE)
FullData = vector('list', length(files))
i = 0
for (f in files) {
  i = i + 1
  print(paste0('file:',f))
  dat <- readxl::read_xlsx(f, trim_ws=TRUE) %>%
    dplyr::select(`Last Name`, `First Name`, `Student ID`, on_course, exam, 
                  task_change, total_achievement, uninvigilated, invigilated, 
                  pracvivaosce, `%inv`, DNS) %>%
    mutate(Subject=gsub('data/primarydata/([a-zA-Z]{2}[0-9]{4}).*','\\1',f),
           Year = as.numeric(gsub('data/primarydata/[a-zA-Z]{2}[0-9]{4}.SP[0-1]{1,2}.([0-9]{4}).*','\\1',f)),
           Study_period = gsub('data/primarydata/[a-zA-Z]{2}[0-9]{4}.(SP[0-1]{1,2}).[0-9]{4}.*','\\1',f)
    ) %>% dplyr::rename(
      First_name=`First Name`,
      Last_name=`Last Name`,
      Student_ID=`Student ID`) %>%
    deidentify(Student_ID, Last_name, First_name, warn_duplicates = FALSE, drop=FALSE) %>%
    dplyr::select(-Student_ID, -Last_name, -First_name)
  FullData[[i]] <- dat
}

FullData <- do.call('rbind', FullData)
if (!dir.exists('data/processed')) dir.create('data/processed')
save(FullData, file='data/processed/FullData.RData')
