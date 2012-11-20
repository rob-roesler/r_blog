library(devtools)
library(markdown)
library(knitr)

setwd("C:/Users/rroesler/R_Workspace/R_Blog/") # Setzen des Pfades

rmdFile <- "Blogbeitrag_MinimumWeightedBipartidMatching_01" # Blogbeitrag
knit2html(paste(rmdFile,".Rmd",sep=""))
markdownToHTML(paste(rmdFile,".md",sep=""), 
               paste(rmdFile,".html",sep=""), fragment.only = TRUE)