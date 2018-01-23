setwd("C:/Users/yzamriy/Documents/Tools and Methodology/DS/Git/Berkeley/W203-6-Lab1/Working_files/")

var_names <- colnames(read.csv("CandidateDebt.csv", nrows = 1))

var_names_corrected <- c(var_names[1:grep("position", var_names)],
                         "position2",
                         var_names[(grep("position", var_names) + 1):(length(var_names) - 1)])

CandidateDebt <- read.csv("CandidateDebt.csv", 
                          stringsAsFactors = FALSE,
                          col.names = var_names_corrected)