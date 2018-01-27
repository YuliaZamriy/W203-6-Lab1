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
CandidateDebt[CandidateDebt$filername == dups[2],]

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

CandidateDebtAggr <- CandidateDebt %>% 
  mutate(from_thru = as.numeric(thrudate - fromdate),
         thru_debt = as.numeric(thrudate - debtdate)) %>% 
  group_by(filerid) %>% 
  summarise(ttl_amount = sum(amount),
            avg_amount = mean(amount),
            debt_count = n(),
            office_f = first(office),
            office_n = n_distinct(office),
            from_thru_avg = mean(from_thru),
            from_debt_avg = mean(from_debt),
            #legislativedistrict_f = first(legislativedistrict),
            legdist_n = n_distinct(legislativedistrict),
            #party_f = first(party),
            party_n = n_distinct(party),
            #jurisdiction_f = first(jurisdiction),
            jur_n = n_distinct(jurisdiction),
            #jurisdictiontype_f = first(jurisdictiontype),
            jurtype_n = n_distinct(jurisdictiontype),
            #jurisdictioncounty_f = first(jurisdictioncounty),
            jurcounty_n = n_distinct(jurisdictioncounty))

View(CandidateDebtAggr[CandidateDebtAggr$party_n == 1,])

table(CandidateDebt$party)
#table(CandidateDebtAggr$party_n)
table(CandidateDebtAggr$office_f)

debt_office <- aggregate(ttl_amount ~ office_f, data = CandidateDebtAggr, sum)

par(mar = c(8,4,2,1))
barplot(debt_office$ttl_amount/1000,
        names.arg = debt_office$office_f,
        cex.names = 0.5,
        cex.axis = 0.5,
        las = 2,
        ylab = "Debt (in $'000)",
        main = "Total Debt by Office",
        col = "darkgreen")


debt_office2 <- aggregate(filerid ~ office_f, data = CandidateDebtAggr, n_distinct)
debt_office3 <- cbind(debt_office, debt_office2)
debt_office3$amt_p_cand <- debt_office3$ttl_amount / debt_office3$filerid

barplot(debt_office3$filerid,
        names.arg = debt_office3$office_f,
        cex.names = 0.5,
        cex.axis = 0.5,
        las = 2,
        ylab = "Number of Candidates",
        main = "Number of Candidates (by Office)",
        col = "darkgreen")

barplot(debt_office3$amt_p_cand,
        names.arg = debt_office3$office_f,
        cex.names = 0.5,
        cex.axis = 0.5,
        las = 2,
        ylab = "Average Debt ($)",
        main = "Average Debt Per Candidate (by Office)",
        col = "darkgreen")

debt_descr <- aggregate(amount ~ description, data = CandidateDebt, sum)
debt_descr <- debt_descr[order(-debt_descr$amount),]

CandidateDebt$description_aggr[grepl("TREASURY", CandidateDebt$description, ignore.case = TRUE)] <-
  "TREASURY"
CandidateDebt$description_aggr[grepl("RETAINER", CandidateDebt$description, ignore.case = TRUE)] <-
  "RETAINER"
CandidateDebt$description_aggr[grepl("CAMPAIGN MANAGEMENT", CandidateDebt$description, ignore.case = TRUE)] <-
  "CAMPAIGN MANAGEMENT"
CandidateDebt$description_aggr[grepl("CONSULTING", CandidateDebt$description, ignore.case = TRUE)] <-
  "CONSULTING"
CandidateDebt$description_aggr[grepl("FUNDRAISING", CandidateDebt$description, ignore.case = TRUE)] <-
  "FUNDRAISING"

debt_descr2 <- aggregate(amount ~ description_aggr, data = CandidateDebt, sum)

debt_vendor <- aggregate(amount ~ vendorname, data = CandidateDebt, sum)
debt_vendor <- debt_vendor[order(-debt_vendor$amount),]

creditcard <- c("AM EX", "AMERICAN EXPRESS", "AMERICAN EXPRESS LOWES", "AMEX",
                "CITI MASTERCARD", "MASTERCARD", "VISA", "CAPITOL ONE")

CandidateDebt$vendorname_aggr[CandidateDebt$vendorname %in% creditcard] <- "CreditCard"
CandidateDebt$vendorname_aggr[grepl("CONSULTING", CandidateDebt$vendorname, ignore.case = TRUE)] <- 
  "Consulting"
CandidateDebt$vendorname_aggr[grepl("STRATEGIES", CandidateDebt$vendorname, ignore.case = TRUE)] <- 
  "Strategies"