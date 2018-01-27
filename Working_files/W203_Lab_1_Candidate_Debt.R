#setwd("C:/Users/yzamriy/Documents/Tools and Methodology/DS/Git/Berkeley/W203-6-Lab1/Working_files/")
#setwd("C:/Users/Zamriyka/Documents/GitHub/W203-6-Lab1/Working_files/")

load("CandidateDebt.RData")

str(CandidateDebt)

summary(CandidateDebt$amount)

length(unique(CandidateDebt$reportnumber))
# 987
length(unique(CandidateDebt$filerid))
# 140
length(unique(CandidateDebt$filername))
# 133

aggr_filer <- aggregate(amount ~ filername + filerid, data = CandidateDebt, sum)
aggr_filer2 <- aggregate(filerid ~ filername, data = aggr_filer, length)
dups <- aggr_filer2$filername[aggr_filer2$filerid > 1]
CandidateDebt[CandidateDebt$filername == dups[3],]

aggr_office <- aggregate(amount ~ filerid + office, data = CandidateDebt, sum)
aggr_office2 <- aggregate(office ~ filerid, data = aggr_office, length)
table(aggr_office2$office)
#   1 
# 140 

aggr_party <- aggregate(amount ~ filerid + party, data = CandidateDebt, sum)
aggr_party2 <- aggregate(party ~ filerid, data = aggr_party, length)
table(aggr_party2$party)
# 1  2  3 
#50 63 27 

library(tidyverse)

aggr_cand <- CandidateDebt %>% 
  group_by(filerid) %>% 
  summarize(ttl_amount = sum(amount),
            office = first(office))
