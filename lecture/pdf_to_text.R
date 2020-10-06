##########################################
# Loren Collingwood                      #
# Converting PDF text to electronic text #
# POSC 207                               #
##########################################

options(repos = c(CRAN = "http://cran.rstudio.com"))
options(max.print=1000000)

# Set Directory
setwd("~/Dropbox/collingwood_research/posc_fall_20/POSC-207/data/tribes")
list.files()

#install.packages("pdftools")
library(pdftools)

# List out pdf files in directory and extract name #
files <- list.files(pattern = "pdf$")

# apply pdf_text() conversion function #
constitutions <- lapply(files, pdf_text)

# First Document #
paste(constitutions[[1]], collapse = " ")

# Second Document #
constitutions[[2]]

