# Runing tests in ISRaD
This folder contains scripts for testing data integrity and code functionality in ISRaD. Data integrity tests are based on the QA/QC function (QAQC.R), but are intended for use in development and recompiling the database following new data ingests. R code tests use R CMD check to ensure performance of the R package following building.

## Testing the integrity of the data
Data that lives outside the R package in the `ISRaD_data_files` folder can be tested using the file `ISRaD_data_test.R`. This is an R script that goes to the `testthat` folder and runs all tests that are there. They can be run from the command line as

```r
Rscript ISRaD_data_test.R
```
or directly inside R using devtools.

## Testing the R package
The R package is tested using built-in R tools for packages. The script `pkg_test.sh` is a bash script that builds the package from source files and tests the compiled package.

To run the script in the command line, simply type
```
./pkg_test.sh
```
