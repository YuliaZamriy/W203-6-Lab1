#setwd("C:/Users/yzamriy/Documents/Tools and Methodology/DS/Git/Berkeley/W203-6-Lab1/Working_files/")
#setwd("C:/Users/Zamriyka/Documents/GitHub/W203-6-Lab1/Working_files/")
setwd("/home/yulia/Documents/MIDS/W203-6-lab1/Working_files/")

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
aggr_office3 <- aggregate(filerid ~ office, data = aggr_office, length)
aggr_office3[aggr_office3$filerid > 3,]

aggr_party <- aggregate(amount ~ filerid + party, data = CandidateDebt, sum)
aggr_party2 <- aggregate(party ~ filerid, data = aggr_party, length)
table(aggr_party2$party)
# 1  2  3 
#50 63 27 

summary(CandidateDebt$debtdate)
summary(CandidateDebt$fromdate)
summary(CandidateDebt$thrudate)

plot(CandidateDebt$debtdate, CandidateDebt$amount)

CandidateDebt$weeksindebt <- round(difftime(max(CandidateDebt$debtdate), CandidateDebt$debtdate, units = "weeks"))
CandidateDebt$monthsindebt <- round(CandidateDebt$weeksindebt / 52 * 12)
CandidateDebt$monthsindebt <- as.numeric(CandidateDebt$monthsindebt)

aggr_weeks <- aggregate(amount ~ weeksindebt, data = CandidateDebt, sum)
aggr_weeks <- aggr_weeks[order(-aggr_weeks$weeksindebt),]

par(mar = c(4,4,2,1), 
    oma = c(1,1,1,1))
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

aggr_months <- aggregate(amount ~ monthsindebt, data = CandidateDebt, sum)
aggr_months <- aggr_months[order(-aggr_months$monthsindebt),]

barplot(aggr_months$amount/1000,
        names.arg = aggr_months$monthsindebt,
        cex.names = 0.8,
        cex.axis = 0.8,
        las = 2,
        xlab = "Months till election",
        ylab = "Debt (in $'000)",
        col = "darkgreen")
title("Debt Accumulation Before Election",
      cex.main = 1)

aggr_months_office <- aggregate(amount ~ monthsindebt + office, data = CandidateDebt, sum)
aggr_months_office <-
  reshape(aggr_months_office, 
          v.names = "amount", 
          idvar = "office",
          timevar = "monthsindebt",
          direction = "wide")

aggr_months_office[is.na(aggr_months_office)] <- 0

aggr_months_office$amount.13plus = aggr_months_office$amount.13 +
  aggr_months_office$amount.14 +
  aggr_months_office$amount.15 +
  aggr_months_office$amount.16 +
  aggr_months_office$amount.17 +
  aggr_months_office$amount.18 +
  aggr_months_office$amount.19 +
  aggr_months_office$amount.20 +
  aggr_months_office$amount.20 +
  aggr_months_office$amount.21 +
  aggr_months_office$amount.22 +
  aggr_months_office$amount.23 +
  aggr_months_office$amount.31 +
  aggr_months_office$amount.32 +
  aggr_months_office$amount.33 +
  aggr_months_office$amount.40 +
  aggr_months_office$amount.42 +
  aggr_months_office$amount.43 +
  aggr_months_office$amount.46
  
keep_vars <- c("office", "amount.13plus", "amount.12", "amount.11", "amount.10", "amount.9", "amount.8", "amount.7",
               "amount.6", "amount.5", "amount.4", "amount.3", "amount.2", "amount.1", "amount.0")  
new_vars <- c("office", "Month.13p", "Month.12", "Month.11", "Month.10", "Month.9", "Month.8", "Month.7",
              "Month.6", "Month.5", "Month.4", "Month.3", "Month.2", "Month.1", "Month.0")  

aggr_months_office <- aggr_months_office[, keep_vars]
colnames(aggr_months_office) <- new_vars
aggr_months_office2 <- aggr_months_office[,-1]
rownames(aggr_months_office2) <- aggr_months_office[, 1]

par(mar = c(4,4,2,1), 
    oma = c(1,1,1,1))
barplot(as.matrix(aggr_months_office2), 
        border="white", 
        space=0.04, 
        cex.names = 0.5,
        cex.axis = 0.8,
        col = rainbow(16),
        axes = FALSE,
        ylim = range(0, 500000),
        xlab = "Months till election",
        ylab = "Debt (in $'000)")
axis(2, at = seq(0, 500000, 50000),
     labels = seq(0, 5000, 500),
     las = 1)
legend("topright", 
       legend = rownames(aggr_months_office2), 
       fill = rainbow(16), 
       ncol = 1,
       cex = 0.75)
title("Debt Accumulation Before Election by Office",
      cex.main = 1)

selected_offices <- aggr_office3[aggr_office3$filerid > 3,1]
aggr_months_office3 <- aggr_months_office[aggr_months_office$office %in% selected_offices,][,-1]
rownames(aggr_months_office3) <- aggr_months_office[aggr_months_office$office %in% selected_offices,][, 1]

library(RColorBrewer)

barplot(as.matrix(aggr_months_office3), 
        border="white", 
        space=0.04, 
        cex.names = 0.5,
        cex.axis = 0.8,
        col = brewer.pal(6, "Dark2"),
        axes = FALSE,
        ylim = range(0, 500000),
        xlab = "Months till election",
        ylab = "Debt (in $'000)")
axis(2, at = seq(0, 500000, 50000),
     labels = seq(0, 5000, 500),
     las = 1)
legend("topright", 
       legend = rownames(aggr_months_office3), 
       fill = brewer.pal(6, "Dark2"), 
       ncol = 1,
       cex = 0.75)
title("Debt Accumulation Before Election by Office",
      cex.main = 1)

amount_by_office <- aggregate(amount ~ office, data = CandidateDebt, sum)
aggr_months_office4 <- amount_by_office[amount_by_office$office %in% selected_offices, ]
aggr_months_office4 <- cbind(aggr_months_office3, amount_by_office[amount_by_office$office %in% selected_offices, ])
aggr_months_office4$office <- NULL
aggr_months_office4$Month.13plus <- round(aggr_months_office4$Month.13plus / aggr_months_office4$amount, 2)
aggr_months_office4$Month.12 <- round(aggr_months_office4$Month.12 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.11 <- round(aggr_months_office4$Month.11 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.10 <- round(aggr_months_office4$Month.10 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.9 <- round(aggr_months_office4$Month.9 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.8 <- round(aggr_months_office4$Month.8 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.7 <- round(aggr_months_office4$Month.7 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.6 <- round(aggr_months_office4$Month.6 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.5 <- round(aggr_months_office4$Month.5 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.4 <- round(aggr_months_office4$Month.4 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.3 <- round(aggr_months_office4$Month.3 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.2 <- round(aggr_months_office4$Month.2 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.1 <- round(aggr_months_office4$Month.1 / aggr_months_office4$amount, 2)
aggr_months_office4$Month.0 <- round(aggr_months_office4$Month.0 / aggr_months_office4$amount, 2)

barplot(as.matrix(aggr_months_office4[,-15]), 
        border="white", 
        space=0.04, 
        cex.names = 0.5,
        cex.axis = 0.8,
        col = brewer.pal(6, "Dark2"),
        #axes = FALSE,
        #ylim = range(0, 500000),
        xlab = "Months till election",
        ylab = "Debt (in $'000)")
axis(2, at = seq(0, 500000, 50000),
     labels = seq(0, 5000, 500),
     las = 1)
legend("topright", 
       legend = rownames(aggr_months_office3), 
       fill = brewer.pal(6, "Dark2"), 
       ncol = 1,
       cex = 0.75)
title("Debt Accumulation Before Election by Office",
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

#View(CandidateDebtAggr[CandidateDebtAggr$party_n == 1,])

table(CandidateDebt$party)
table(CandidateDebtAggr$party_n)
table(CandidateDebtAggr$office_f)

debt_office <- aggregate(ttl_amount ~ office_f, data = CandidateDebtAggr, sum)

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

creditcard <- c("AM EX", "AMERICAN EXPRESS", "AMERICAN EXPRESS LOWES", "AMEX",
                "CITI MASTERCARD", "MASTERCARD", "VISA", "CAPITOL ONE",
                "MASTER CARD")

consulting <- c("CONSULTING", "JANUARY SERVICES", "$750 PER MONTH THROUGH OCTOBER")
swag <- c("RE-ORDER TEE SHIRTS", "BUMPER STICKERS/FLYERS", "CONSULTING/YARD SIGNS", 
          "YARD SIGNS", "OFFICE SUPPLIES/ WATER FOR KICKOFF")

CandidateDebt$description_aggr[grepl("TREASURY", CandidateDebt$description, ignore.case = TRUE)] <-
  "TREASURY"
CandidateDebt$description_aggr[grepl("RETAINER", CandidateDebt$description, ignore.case = TRUE)] <-
  "RETAINER"
CandidateDebt$description_aggr[grepl("CAMPAIGN", CandidateDebt$description, ignore.case = TRUE)] <-
  "CAMPAIGN MANAGEMENT"
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
CandidateDebt$description_aggr[grepl("POLLING", CandidateDebt$description, ignore.case = TRUE)] <-
  "POLLING"
CandidateDebt$description_aggr[grepl("CREDIT", CandidateDebt$description, ignore.case = TRUE)] <-
  "CREDIT CARD"
CandidateDebt$description_aggr[CandidateDebt$vendorname %in% creditcard] <-
  "CREDIT CARD"
CandidateDebt$description_aggr[CandidateDebt$description %in% consulting] <-
  "CONSULTING"
CandidateDebt$description_aggr[CandidateDebt$description %in% swag] <-
  "SWAG"
CandidateDebt$description_aggr[grepl("MAIL", CandidateDebt$description, ignore.case = TRUE)] <-
  "MAIL"
CandidateDebt$description_aggr[grepl("POSTAGE", CandidateDebt$description, ignore.case = TRUE)] <-
  "MAIL"
CandidateDebt$description_aggr[grepl("STAMPS", CandidateDebt$description, ignore.case = TRUE)] <-
  "MAIL"
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