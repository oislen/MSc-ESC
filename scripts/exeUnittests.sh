#!/bin/bash
Rscript -e "source('../renv/activate.R'); testthat::test_dir('./utilities/unittests');"