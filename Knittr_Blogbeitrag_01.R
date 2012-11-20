library(devtools)
library(markdown)
library(knitr)

setwd("C:/Users/rroesler/R_Workspace/R_Blog/")

rmdFile <- "Blogbeitrag_MinimumWeightedBipartidMatching_01"
knit2html(paste(rmdFile,".Rmd",sep=""))
markdownToHTML(paste(rmdFile,".md",sep=""), 
               paste(rmdFile,".html",sep=""), fragment.only = TRUE)