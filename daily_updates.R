setwd("~/Documents/Centauri/fall part time/conjunction_analysis/")

Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/MacOS/pandoc")

rmarkdown::render("index.Rmd", output_dir = "docs")

quit(save="no")