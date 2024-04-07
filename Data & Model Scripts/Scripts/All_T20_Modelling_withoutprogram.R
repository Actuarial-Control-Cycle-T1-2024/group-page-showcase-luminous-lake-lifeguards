library(tidyverse)
library(bruceR)

########################################################################################################################################################################
# Assumptions 
########################################################################################################################################################################

# 1. Age is at start of policy year 
# 2. Interest rate assumption is 4% for years with no 1yr spot rate given interest_rate_assumption
interest_rate_assumption <- 0.04
investmentrate <- 0.06

########################################################################################################################################################################
# Importing Data
########################################################################################################################################################################
inforce_table <- read_csv("Data/updated_inforcetable.csv")
lapse_table <- read_csv("Data/lapse_rate.csv")
base_mortalitytable <- read_csv("Data/base_mortality.csv")
economictable <- read_csv("Data/economic_data.csv")
simplified_mpf <- read_csv("Data/simplified_mpf.csv")

#importing 2024 new business MPF
newbusiness_2024 <- read_csv("Data/2024_nb.csv")


adjustments_table <- data.frame(
  "Factor" = c(0.52846576, 4.98076474,	0.87463740,	4.35981729)
)
rownames(adjustments_table) <- c("FNS", "FS", "MNS", "MS")

commissionfactors <- data.frame(
  "Initial" = c(0.78826124606048000, 0.09853265575756000, 0.73899491818170000),
  "Renewal" = c(0.09853265575756, 0.00000000000000, 0.14779898363634)
)
rownames(commissionfactors) <- c("Agent", "Online", "Telemarketer")

reserverates <- data.frame(
  "Rate_per_1000" = c(0.8,0.2,0.25,0.3,0.35,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.4,0.35,0.3,0.25,0.2,0.15,0.1,0)
)

capital_per1000 <- 0.01

riskmargin <- data.frame(
  "Factor" = c(0,0,0.23,0.23)
)
rownames(riskmargin) <- c("very low risk", "low risk", "moderate risk", "high risk")

profitmargin <- data.frame(
  "Factor" = c(0.035,0.045,0.055,0.075)
)
rownames(profitmargin) <- c("very low risk", "low risk", "moderate risk", "high risk")

expensetable <- data.frame(
  "Expense" = c(2.05, 170)
)
rownames(expensetable) <- c("Acquisition", "Renewal")

interestdiscount <- data.frame(
  "Value" = c(interest_rate_assumption, ((1+interest_rate_assumption)*(1-(1+interest_rate_assumption)^(-20))/interest_rate_assumption), ((1+interest_rate_assumption)*(1-(1+interest_rate_assumption)^(-19))/interest_rate_assumption))
)
rownames(interestdiscount) <- c("Interest rate", "Premium annuity factor", "Renewal Commission annuity factor")


########################################################################################################################################################################
# Cleaning Data
########################################################################################################################################################################

#Creating table for T20 policy type only 
t20_inforcetable <- subset(inforce_table, Policy.type == "T20")

#Adjusting lapse table
lapse_table <- rbind(c(0,0), lapse_table)

#new business indicator effects 
nb_2024_t20 <- subset(newbusiness_2024, Policy.type == "T20")
simplified_mpf_newbusiness <- subset(simplified_mpf, Issue.year == "2001")
simplified_mpf_newbusiness[,"Issue.year"] <- rep("2024", nrow(simplified_mpf_newbusiness))

########################################################################################################################################################################
# Creating Base Functions and Adjusted Mortality Tables
########################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------
# Creating Current Processing Table (Extract information for policy being run) 
#------------------------------------------------------------------------------------------------------------------------------------------------
currentprocessing <- function(i){
  info <- simplified_mpf %>%
    filter(rownames(simplified_mpf) == i)
  info <- as.data.frame(info)
}

show(currentprocessing(1))

#------------------------------------------------------------------------------------------------------------------------------------------------
# Creating yield curve for T20 modelling 
#------------------------------------------------------------------------------------------------------------------------------------------------
yield_curve <- function(i){
  issueyear <- currentprocessing(i)["Issue.year"][1,1]
  curve <- matrix(c(issueyear:(issueyear+20)))
  colnames(curve) <- "Year"
  
  curve <- LOOKUP(as.data.frame(curve), "Year", economictable, "Year", "1yr")
  curve[is.na(curve)] <- interest_rate_assumption
  curve
}

show(yield_curve(1))
#------------------------------------------------------------------------------------------------------------------------------------------------
# Creating adjusted mortality table 
#------------------------------------------------------------------------------------------------------------------------------------------------
adjusted_mortality_table <- base_mortalitytable %>%
  mutate("FNS" = mortality_rate*adjustments_table["FNS","Factor"]) %>%
  mutate("FS" = mortality_rate*adjustments_table["FS","Factor"]) %>%
  mutate("MNS" = mortality_rate*adjustments_table["MNS","Factor"]) %>%
  mutate("MS" = mortality_rate*adjustments_table["MS","Factor"])

adjusted_mortality_table$FS <- replace(adjusted_mortality_table$FS, adjusted_mortality_table$FS > 1, 1)
adjusted_mortality_table$MS <- replace(adjusted_mortality_table$MS, adjusted_mortality_table$MS > 1, 1)

adjusted_mortality_table[120, 3:6] <- 1


########################################################################################################################################################################
# Creating Expected Claims 
########################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------
# Expected Claims Function
#------------------------------------------------------------------------------------------------------------------------------------------------
expected_claims <- function(currentpolicy, i){
  options(scipen=999)
  
  #defining basic info
  issueyear <- currentpolicy["Issue.year"][1,1]
  currentage <- currentpolicy["Issue.age"][1,1]
  gender <- currentpolicy["Sex"][1,1]
  smokerstatus <- currentpolicy["Smoker.Status"][1,1]
  index_gendersmoker <- paste(gender, smokerstatus, sep = '')
  
  
  #adding mortality and lapse
  output <- matrix(c(0:20))
  output <- cbind(output, c(0, c(issueyear:(issueyear+19))))
  output <- cbind(output, c(currentage:(currentage+20)))
  
  base_adjusted <- adjusted_mortality_table[currentage:(currentage+20), index_gendersmoker]
  
  output <- cbind(output, base_adjusted)
  output <- cbind(output, c(lapse_table[,2]))
  
  #renaming columns
  colnames(output) <- c("Policy_Year", "Calendar_Year", "Age", "Mortality_Rate", "Lapse_Rate")
  output$Mortality_Rate <- as.numeric(output$Mortality_Rate)
  
  #calculating end of year exposure 
  eoy_exp <- 1
  for (i in 2:21) {
    eoy_exp = c(eoy_exp, last(eoy_exp)*(1-output[i,"Mortality_Rate"]-output[i,"Lapse_Rate"]))
  }
  output <- cbind(output, "EOY_Exp" = eoy_exp)
  
  #calculating expected deaths 
  exp_deaths <- unlist(lapply(2:21, function(i) output[i-1, "EOY_Exp"]*output[i, "Mortality_Rate"]))
  output <- cbind(output, "Exp_Deaths" = c(0, exp_deaths))
  
  output <- output %>%
    mutate(Exp_Claims = 1*Exp_Deaths)
  
}

#------------------------------------------------------------------------------------------------------------------------------------------------
# Discounted expected claims value 
#------------------------------------------------------------------------------------------------------------------------------------------------
discounting_expclaims <- function(i, exp_claims, yieldcurve){
  discounting <- lapply(2:nrow(exp_claims), function(x) exp_claims[x,"Exp_Claims"]*prod(unlist(lapply(1:(x-1), function(y) 1/(1+yieldcurve[y,2]) ))) )
  output <- sum(unlist(discounting))
  output
}

########################################################################################################################################################################
# Creating Discount Exposure Factor
########################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------
# Exposure Function
#------------------------------------------------------------------------------------------------------------------------------------------------
exposure_factor <- function(currentpolicy, i){
  options(scipen=999)
  
  #defining basic info
  issueyear <- currentpolicy["Issue.year"][1,1]
  currentage <- currentpolicy["Issue.age"][1,1]
  gender <- currentpolicy["Sex"][1,1]
  smokerstatus <- currentpolicy["Smoker.Status"][1,1]
  index_gendersmoker <- paste(gender, smokerstatus, sep = '')
  
  
  #adding mortality and lapse
  output <- matrix(c(0:20))
  output <- cbind(output, c(0, c(issueyear:(issueyear+19))))
  output <- cbind(output, c(currentage:(currentage+20)))
  
  base_adjusted <- adjusted_mortality_table[currentage:(currentage+20), index_gendersmoker]
  
  output <- cbind(output, base_adjusted)
  output <- cbind(output, c(lapse_table[,2]))
  
  #renaming columns
  colnames(output) <- c("Policy_Year", "Calendar_Year", "Age", "Mortality_Rate", "Lapse_Rate")
  output$Mortality_Rate <- as.numeric(output$Mortality_Rate)
  
  #calculating end of year exposure 
  eoy_exp <- 1
  for (i in 2:21) {
    eoy_exp = c(eoy_exp, last(eoy_exp)*(1-output[i,"Mortality_Rate"]-output[i,"Lapse_Rate"]))
  }
  output <- cbind(output, "EOY_Exp" = eoy_exp)
  
}

#------------------------------------------------------------------------------------------------------------------------------------------------
# Discounted exposure factor function
#------------------------------------------------------------------------------------------------------------------------------------------------
discounting_exposure_factor <- function(i, projection, yieldcurve){
  discounting <- lapply(2:nrow(projection), function(x) projection[x,"EOY_Exp"]*prod(unlist(lapply(1:(x-1), function(y) 1/(1+yieldcurve[y,2]) ))) )
  output <- sum(unlist(discounting))
  output
}

########################################################################################################################################################################
# Creating Factors for profit margin calculation 
########################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------
# Projection Function
#------------------------------------------------------------------------------------------------------------------------------------------------
projecting_values <- function(currentpolicy, i){
  options(scipen=999)
  
  #defining basic info
  currentage <- currentpolicy["Issue.age"][1,1]
  gender <- currentpolicy["Sex"][1,1]
  smokerstatus <- currentpolicy["Smoker.Status"][1,1]
  index_gendersmoker <- paste(gender, smokerstatus, sep = '')
  
  
  #adding mortality and lapse
  output <- matrix(c(0:20))
  output <- cbind(output, c(currentage:(currentage+20)))
  
  base_adjusted <- adjusted_mortality_table[currentage:(currentage+20), index_gendersmoker]
  
  output <- cbind(output, base_adjusted)
  output <- cbind(output, c(lapse_table[,2]))
  
  #renaming columns
  colnames(output) <- c("Policy_Year", "Age", "Mortality_Rate", "Lapse_Rate")
  output$Mortality_Rate <- as.numeric(output$Mortality_Rate)
  
  #calculating end of year exposure 
  eoy_exp <- 1
  for (i in 2:21) {
    eoy_exp = c(eoy_exp, last(eoy_exp)*(1-output[i,"Mortality_Rate"]-output[i,"Lapse_Rate"]))
  }
  output <- cbind(output, "EOY_Exp" = eoy_exp)
  
  #calculating reserve + capital rate 
  reserve_and_capital <- reserverates[,"Rate_per_1000"]+capital_per1000
  output <- cbind(output, "R_n_Capital_Exp" = reserve_and_capital*output$EOY_Exp)
  
  #reserve increase
  rr <- reserverates[,"Rate_per_1000"]
  reserveincrease <- c(-rr[1], rr[1]-rr[2], unlist(lapply(3:nrow(output), function(x) rr[x-1]*output[x-2,"EOY_Exp"]-rr[x]*output[x-1,"EOY_Exp"])))
  output <- cbind(output, reserveincrease)
  
}

#------------------------------------------------------------------------------------------------------------------------------------------------
# Discounted exposure factor function
#------------------------------------------------------------------------------------------------------------------------------------------------
discounting_factor_reservecapital <- function(i, projection, yieldcurve){
  discounting_reservecapital_factor <- lapply(1:(nrow(projection)-1), function(x) projection[x,"R_n_Capital_Exp"]*prod(unlist(lapply(1:x, function(y) 1/(1+yieldcurve[y,2]) ))) )
  output <- sum(unlist(discounting_reservecapital_factor))
  output
}

discounting_factor_reserveincrease <- function(i, projection, yieldcurve){
  discounting_reserveincrease_factor <- lapply(2:nrow(projection), function(x) projection[x,"reserveincrease"]*prod(unlist(lapply(1:(x-1), function(y) 1/(1+yieldcurve[y,2]) ))) )
  output <- sum(unlist(discounting_reserveincrease_factor))+projection[1,"reserveincrease"]
  output
}








########################################################################################################################################################################
# Collecting all factors and values 
########################################################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------
# Creating table for factors 
#------------------------------------------------------------------------------------------------------------------------------------------------
simple_mpf_output <- simplified_mpf
simple_mpf_output[, "Discounted_Exp_Claims_Factor"] <- rep(0, nrow(simple_mpf_output))
simple_mpf_output[, "Discounted_Exposure_Factor"] <- rep(0, nrow(simple_mpf_output))
simple_mpf_output[, "Discounted_ReserveCapital_Factor"] <- rep(0, nrow(simple_mpf_output))
simple_mpf_output[, "Discounted_ReserveIncrease_Factor"] <- rep(0, nrow(simple_mpf_output))

#------------------------------------------------------------------------------------------------------------------------------------------------
# Running all the functions 
#------------------------------------------------------------------------------------------------------------------------------------------------
for (x in 1:nrow(simplified_mpf)){
  simple_mpf_output[x,"Discounted_Exp_Claims_Factor"] <- discounting_expclaims(x, expected_claims(currentprocessing(x),x), yield_curve(x))
  simple_mpf_output[x,"Discounted_ReserveCapital_Factor"] <- discounting_factor_reservecapital(x, projecting_values(currentprocessing(x),x), yield_curve(x))
  simple_mpf_output[x,"Discounted_ReserveIncrease_Factor"] <- discounting_factor_reserveincrease(x, projecting_values(currentprocessing(x),x), yield_curve(x))
  simple_mpf_output[x,"Discounted_Exposure_Factor"] <- discounting_exposure_factor(x, exposure_factor(currentprocessing(x),x), yield_curve(x))
}



########################################################################################################################################################################
# Extracting out simplified values back into original data-set
########################################################################################################################################################################
t20_discounted_factors <- t20_inforcetable %>%
  select(Policy.number:Distribution.Channel) 

t20_discounted_factors <- LOOKUP(t20_discounted_factors, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_Exp_Claims_Factor")
t20_discounted_factors <- LOOKUP(t20_discounted_factors, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_Exposure_Factor")
t20_discounted_factors <- LOOKUP(t20_discounted_factors, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_ReserveCapital_Factor")
t20_discounted_factors <- LOOKUP(t20_discounted_factors, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_ReserveIncrease_Factor")

t20_discounted_factors <- t20_discounted_factors %>%
  mutate(Discounted_Exp_Claims = Discounted_Exp_Claims_Factor*Face.amount)

########################################################################################################################################################################
# Calculating premium
########################################################################################################################################################################
t20_values <- t20_discounted_factors

t20_values <- t20_values %>%
  mutate("Annual_Premium" = (Discounted_Exp_Claims*(1+riskmargin[Underwriting.Class,"Factor"])+Discounted_Exposure_Factor*expensetable["Renewal","Expense"])/(1-(expensetable[1,1]+commissionfactors[Distribution.Channel, "Initial"])/interestdiscount[2,1]-interestdiscount[3,"Value"]*commissionfactors[Distribution.Channel,"Renewal"]/interestdiscount[2,"Value"]-profitmargin[Underwriting.Class,"Factor"])/interestdiscount[2,1])

withoutprogram_annualpremium = t20_values[,c("Policy.number", "Annual_Premium")]

########################################################################################################################################################################
# Calculating profit
########################################################################################################################################################################
#Calculate discounted interest 
t20_values <- t20_values %>%
  mutate(DiscountedInterestEarned = investmentrate*(Face.amount/1000*Discounted_ReserveCapital_Factor+Annual_Premium*(1-commissionfactors[Distribution.Channel,"Renewal"])*Discounted_Exposure_Factor))

#Calculate discounted profit 
t20_values <- t20_values %>%
  mutate(DiscountedProfit = Annual_Premium*(1+Discounted_Exposure_Factor)-(commissionfactors[Distribution.Channel,"Initial"]+commissionfactors[Distribution.Channel,"Renewal"]*Discounted_Exposure_Factor)*Annual_Premium-(expensetable[1,1]*Annual_Premium+expensetable[2,1]*Discounted_Exposure_Factor)-Discounted_Exp_Claims+DiscountedInterestEarned+Face.amount/1000*Discounted_ReserveIncrease_Factor)

t20_values <- t20_values %>%
  mutate(PVAccumPremium = Annual_Premium*interestdiscount[2,1])

profittable_withoutp <- t20_values %>%
  group_by(Underwriting.Class) %>%
  summarise("PVProfit"=sum(DiscountedProfit), "ProfitMargin" = round(100*sum(DiscountedProfit)/sum(PVAccumPremium),0)) %>%
  mutate("%ofProfit" = round(100*PVProfit/sum(PVProfit),0))


PVprofit_withoutprogram <- sum(t20_values$DiscountedProfit)
PVprofit_withoutprogram

PVexpclaims_withoutprogram <- sum(t20_values$Discounted_Exp_Claims)
PVexpclaims_withoutprogram

profitmargin_withoutprogram <- sum(t20_values$DiscountedProfit)/sum(t20_values$PVAccumPremium)

########################################################################################################################################################################
# Ratios
########################################################################################################################################################################
t20_values <- t20_values %>%
  mutate(PVRiskMargin = riskmargin[Underwriting.Class,1]*Discounted_Exp_Claims) 

t20_values <- t20_values %>%
  mutate(PVACQExpense = Annual_Premium*expensetable[1,1])

t20_values <- t20_values %>%
  mutate(PVRenewalExpense = Discounted_Exposure_Factor*expensetable[2,1])

t20_values <- t20_values %>%
  mutate(PVINITComm = Annual_Premium*commissionfactors[Distribution.Channel,"Initial"])

t20_values <- t20_values %>%
  mutate(PVRenewalComm = Annual_Premium*commissionfactors[Distribution.Channel,"Renewal"]*interestdiscount[3,1])

t20_values <- t20_values %>%
  mutate(PVProfitMargin = PVAccumPremium*profitmargin[Underwriting.Class,1])


# Calculating ratios
t20_ratios <- matrix(nrow = 7, ncol = 1)
rownames(t20_ratios) <- c("Commission", "Loss", "Expense","Combined_CLE","Risk", "Profit", "Total")
colnames(t20_ratios) <- "Ratios"

t20_ratios["Commission","Ratios"] <- (sum(t20_values$PVINITComm)+sum(t20_values$PVRenewalComm))/sum(t20_values$PVAccumPremium)
t20_ratios["Loss","Ratios"] <- sum(t20_values$Discounted_Exp_Claims)/sum(t20_values$PVAccumPremium)
t20_ratios["Expense","Ratios"] <- (sum(t20_values$PVACQExpense)+sum(t20_values$PVRenewalExpense))/sum(t20_values$PVAccumPremium)
t20_ratios["Combined_CLE","Ratios"] <- sum(t20_ratios[1:3,1])
t20_ratios["Risk","Ratios"] <- sum(t20_values$PVRiskMargin)/sum(t20_values$PVAccumPremium)
t20_ratios["Profit","Ratios"] <- sum(t20_values$PVProfitMargin)/sum(t20_values$PVAccumPremium) #this profit ratio doesn't make sense 
t20_ratios["Total","Ratios"] <- sum(t20_ratios[4:6,1])


################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
# THE FINAL OUTPUT IS t20_values !!!!!!!! YAY Everything is in one script. Also shouldn't take too long to run :) 
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
#Important outputs 
PVprofit_withoutprogram
PVexpclaims_withoutprogram
t20_ratios
profittable_withoutp
profitmargin_withoutprogram #overall profit margin of entire book 




##################
# Only needed to make sensitives easier to calculate (can ignore)
t20_sens <- data_frame(
  "VIF" = PVprofit_withoutprogram,
  "PVExpectedClaims" = PVexpclaims_withoutprogram,
  "PVProgramCosts" = 0,
  "LossRatio" = t20_ratios["Loss","Ratios"],
  "ExpenseRatio" = t20_ratios["Expense","Ratios"],
  "CommissionRatio" = t20_ratios["Commission","Ratios"],
  "CombinedRatio" = t20_ratios["Combined_CLE","Ratios"],
  "ProfitMargin" = profitmargin_withoutprogram
)

rownames(t20_sens) <- "WithoutProgram"
