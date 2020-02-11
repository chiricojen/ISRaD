library(testthat)
library(ISRaD)
library(openxlsx)
library(dplyr)
library(tidyr)

wd <- getwd()
setwd("..")
dataset_directory <- file.path(getwd(), "ISRaD_data_files") #Use absolute path where the data is stored

data_files <- list.files(dataset_directory, full.names = TRUE)
data_files <- data_files[grep("\\.xlsx", data_files)]
template_file <- system.file("extdata", "ISRaD_Master_Template.xlsx", package = "ISRaD")
template <- lapply(getSheetNames(template_file), function(s) read.xlsx(template_file , sheet=s))
names(template) <- getSheetNames(template_file)

template_info_file <- system.file("extdata", "ISRaD_Template_Info.xlsx", package = "ISRaD")
template_info <- lapply(getSheetNames(template_info_file), function(s) read.xlsx(template_info_file , sheet=s))
names(template_info) <- getSheetNames(template_info_file)

test_dir("testthat")
