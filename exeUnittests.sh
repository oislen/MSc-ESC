#!/bin/bash
LC_ALL=C.UTF-8 Rscript -e "renv::load(); testthat::test_dir('scripts/utilities/unittests');"