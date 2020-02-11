#!/bin/bash

rm *tar.gz
rm -r ISRaD.Rcheck
R CMD build ../Rpkg/
R CMD check *tar.gz
