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

summary(CandidateDebt$debtdate)
summary(CandidateDebt$fromdate)
summary(CandidateDebt$thrudate)

plot(CandidateDebt$debtdate, CandidateDebt$amount)

#CandidateDebt$monthsindebt <- round(max(CandidateDebt$debtdate) - CandidateDebt$debtdate) / 30)
CandidateDebt$weeksindebt <- round(difftime(max(CandidateDebt$debtdate), CandidateDebt$debtdate, units = "weeks"))
aggr_weeks <- aggregate(amount ~ weeksindebt, data = CandidateDebt, sum)

aggr_weeks <- aggr_weeks[order(-aggr_weeks$weeksindebt),]

par(mar = c(4,4,2,1))
barplot(aggr_weeks$amount/1000,
        names.arg = aggr_weeks$weeksindebt,
        cex.names = 0.5,
        cex.axis = 0.5,
        las = 2,
        xlab = "Weeks till election",
        ylab = "Debt (in $'000)",
        #main = "Debt Accumulation Before Election",
        col = "darkgreen")
title("Debt Accumulation Before Election",
      cex.main = 1)

library(tidyverse)

CandidateDebtAggr <- CandidateDebt %>% 
  group_by(filerid) %>% 
  summarise(ttl_amount = sum(amount),
            avg_amount = mean(amount),
            debt_count = n(),
            office_f = first(office),
            office_n = n_distinct(office),
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

table(CandidateDebt$party)
#table(CandidateDebtAggr$party_n)
table(CandidateDebtAggr$office_f)

debt_office <- aggregate(ttl_amount ~ office_f, data = CandidateDebtAggr, sum)

cbarplot(debt_office$ttl_amount/1000,
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

creditcard <- c("AM EX", "AMERICAN EXPRESS", "AMERICAN EXPRESS LOWES", "AMEX",
                "CITI MASTERCARD", "MASTERCARD", "VISA", "CAPITOL ONE",
                "MASTER CARD")

CandidateDebt$description_aggr[grepl("TREASURY", CandidateDebt$description, ignore.case = TRUE)] <-
  "TREASURY"
CandidateDebt$description_aggr[grepl("RETAINER", CandidateDebt$description, ignore.case = TRUE)] <-
  "RETAINER"
CandidateDebt$description_aggr[grepl("CAMPAIGN", CandidateDebt$description, ignore.case = TRUE)] <-
  "CAMPAIGN MANAGEMENT"
CandidateDebt$description_aggr[grepl("CONSULTING", CandidateDebt$description, ignore.case = TRUE)] <-
  "CONSULTING"
CandidateDebt$description_aggr[grepl("PER MONTH", CandidateDebt$description, ignore.case = TRUE)] <-
  "CONSULTING"
CandidateDebt$description_aggr[grepl("JANUARY SERVICES", CandidateDebt$description, ignore.case = TRUE)] <-
  "CONSULTING"
CandidateDebt$description_aggr[grepl("FUND", CandidateDebt$description, ignore.case = TRUE)] <-
  "FUNDRAISING"
CandidateDebt$description_aggr[grepl("PRINT", CandidateDebt$description, ignore.case = TRUE)] <-
  "PRINTING"
CandidateDebt$description_aggr[grepl("CARRY FORWARD", CandidateDebt$description, ignore.case = TRUE)] <-
  "CARRY FORWARD"
CandidateDebt$description_aggr[grepl("REIMB", CandidateDebt$description, ignore.case = TRUE)] <-
  "REIMBURSEMENT"
CandidateDebt$description_aggr[grepl("ACCOUNTING", CandidateDebt$description, ignore.case = TRUE)] <-
  "ACCOUNTING"
CandidateDebt$description_aggr[grepl("BONUS", CandidateDebt$description, ignore.case = TRUE)] <-
  "BONUS"
CandidateDebt$description_aggr[grepl("DESIGN", CandidateDebt$description, ignore.case = TRUE)] <-
  "DESIGN"
CandidateDebt$description_aggr[grepl("SHIRTS", CandidateDebt$description, ignore.case = TRUE)] <-
  "SWAG/SIGNAGE/MAIL"
CandidateDebt$description_aggr[grepl("STICKERS", CandidateDebt$description, ignore.case = TRUE)] <-
  "SWAG/SIGNAGE/MAIL"
CandidateDebt$description_aggr[grepl("SIGNS", CandidateDebt$description, ignore.case = TRUE)] <-
  "SWAG/SIGNAGE/MAIL"
CandidateDebt$description_aggr[grepl("MAIL", CandidateDebt$description, ignore.case = TRUE)] <-
  "SWAG/SIGNAGE/MAIL"
CandidateDebt$description_aggr[grepl("SUPPLIES", CandidateDebt$description, ignore.case = TRUE)] <-
  "SWAG/SIGNAGE/MAIL"
CandidateDebt$description_aggr[grepl("POSTAGE", CandidateDebt$description, ignore.case = TRUE)] <-
  "SWAG/SIGNAGE/MAIL"
CandidateDebt$description_aggr[grepl("STAMPS", CandidateDebt$description, ignore.case = TRUE)] <-
  "SWAG/SIGNAGE/MAIL"
CandidateDebt$description_aggr[grepl("DATA", CandidateDebt$description, ignore.case = TRUE)] <-
  "DATA/TECH/AD"
CandidateDebt$description_aggr[grepl("DISPLAY", CandidateDebt$description, ignore.case = TRUE)] <-
  "DATA/TECH/AD"
CandidateDebt$description_aggr[grepl("WEB", CandidateDebt$description, ignore.case = TRUE)] <-
  "DATA/TECH/AD"
CandidateDebt$description_aggr[grepl("ADVERTISEMENT", CandidateDebt$description, ignore.case = TRUE)] <-
  "DATA/TECH/AD"
CandidateDebt$description_aggr[grepl("TELEPHON", CandidateDebt$description, ignore.case = TRUE)] <-
  "DATA/TECH/AD"
CandidateDebt$description_aggr[grepl("COMPUTER", CandidateDebt$description, ignore.case = TRUE)] <-
  "DATA/TECH/AD"
CandidateDebt$description_aggr[grepl("POLLING", CandidateDebt$description, ignore.case = TRUE)] <-
  "POLLING"
CandidateDebt$description_aggr[grepl("CREDIT", CandidateDebt$description, ignore.case = TRUE)] <-
  "CREDIT CARD"
CandidateDebt$description_aggr[CandidateDebt$vendorname %in% creditcard] <-
  "CREDIT CARD"
CandidateDebt$description_aggr[is.na(CandidateDebt$description_aggr)] <- "OTHER"

table(CandidateDebt$description_aggr)

debt_descr <- aggregate(amount ~ description + description_aggr, data = CandidateDebt, sum)
debt_descr <- debt_descr[order(-debt_descr$amount),]
View(debt_descr[debt_descr$description_aggr == "OTHER",])
View(CandidateDebt[is.na(CandidateDebt$description),])

debt_descr2 <- aggregate(amount ~ description_aggr, data = CandidateDebt, sum)

debt_vendor <- aggregate(amount ~ vendorname, data = CandidateDebt, sum)
debt_vendor <- debt_vendor[order(-debt_vendor$amount),]

CandidateDebt$vendorname_aggr[CandidateDebt$vendorname %in% creditcard] <- "CreditCard"
CandidateDebt$vendorname_aggr[grepl("CONSULTING", CandidateDebt$vendorname, ignore.case = TRUE)] <- 
  "Consulting"
CandidateDebt$vendorname_aggr[grepl("STRATEGIES", CandidateDebt$vendorname, ignore.case = TRUE)] <- 
  "Strategies"