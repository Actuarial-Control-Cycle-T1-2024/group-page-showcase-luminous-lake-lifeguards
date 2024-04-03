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
  "Initial" = c(0.08,0.03,0.06),
  "Renewal" = c(0.005,0,0.002)
)
rownames(commissionfactors) <- c("Agent", "Online", "Telemarketer")

reserverates <- data.frame(
  "FNS" = c(20),
  "MNS" = c(30),
  "FS" = c(200),
  "MS" = c(300)
  )

capital_per1000 <- 0.01

riskmargin <- data.frame(
  "Factor" = c(0,0,0.23,0.23)
)
rownames(riskmargin) <- c("very low risk", "low risk", "moderate risk", "high risk")

profitmargin <- data.frame(
  "Factor" = c(0.03,0.04,0.05,0.07)
)
rownames(profitmargin) <- c("very low risk", "low risk", "moderate risk", "high risk")

expensetable <- data.frame(
  "Expense" = c(0.2, 145)
)
rownames(expensetable) <- c("Acquisition", "Renewal")



########################################################################################################################################################################
# Cleaning Data
########################################################################################################################################################################

#Creating table for T20 policy type only 
spwl_inforcetable <- subset(inforce_table, Policy.type != "T20")

#new business indicator effects 
nb_2024_spwl <- subset(newbusiness_2024, Policy.type != "T20")
simplified_mpf_newbusiness <- subset(simplified_mpf, Issue.year == "2001")
simplified_mpf_newbusiness[,"Issue.year"] <- rep(2024, nrow(simplified_mpf_newbusiness))


########################################################################################################################################################################
# Creating Preliminary functions
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
  currentage <- currentprocessing(i)["Issue.age"][1,1]
  issueyear <- currentprocessing(i)["Issue.year"][1,1]
  curve <- matrix(c(issueyear:(issueyear+120-currentage-1)))
  colnames(curve) <- "Year"
  
  curve <- LOOKUP(as.data.frame(curve), "Year", economictable, "Year", "1yr")
  curve[is.na(curve)] <- interest_rate_assumption
  curve
}



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
# Creating Model for Expected Claims 
########################################################################################################################################################################
#------------------------------------------------------------------------------------------------------------------------------------------------
# Creating expected claims table 
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
  output <- matrix(c(0:(120-currentage)))
  output <- cbind(output, c(0, c(issueyear:(issueyear+120-currentage-1))))
  output <- cbind(output, c(currentage:120))
  
  base_adjusted <- adjusted_mortality_table[currentage:120, index_gendersmoker]
  
  output <- cbind(output, base_adjusted)
  
  #renaming columns
  colnames(output) <- c("Policy_Year", "Calendar_Year", "Age", "Mortality_Rate")
  output$Mortality_Rate <- as.numeric(output$Mortality_Rate)
  
  #calculating end of year exposure 
  eoy_exp <- 1
  for (i in 2:nrow(output)) {
    eoy_exp = c(eoy_exp, last(eoy_exp)*(1-output[i,"Mortality_Rate"]))
  }
  output <- cbind(output, "EOY_Exp" = eoy_exp)
  
  #calculating expected deaths 
  exp_deaths <- unlist(lapply(2:nrow(output), function(i) output[i-1, "EOY_Exp"]*output[i, "Mortality_Rate"]))
  output <- cbind(output, "Exp_Deaths" = c(0, exp_deaths))
  
  output <- output %>%
    mutate(Exp_Claims = 1*Exp_Deaths)
  
}





#------------------------------------------------------------------------------------------------------------------------------------------------
# Creating discounted expected claims value 
#------------------------------------------------------------------------------------------------------------------------------------------------
discounting_expclaims <- function(i, exp_claims, yieldcurve){
  discounting <- lapply(2:nrow(exp_claims), function(x) exp_claims[x,"Exp_Claims"]*prod(unlist(lapply(1:(x-1), function(y) 1/(1+yieldcurve[y,2]) ))) )
  output <- sum(unlist(discounting))
  output
}



########################################################################################################################################################################
# Creating function for exposure factor calculation
########################################################################################################################################################################
#------------------------------------------------------------------------------------------------------------------------------------------------
# Projection function
#------------------------------------------------------------------------------------------------------------------------------------------------
exposure_factors <- function(currentpolicy, i){
  options(scipen=999)
  
  #defining basic info
  issueyear <- currentpolicy["Issue.year"][1,1]
  currentage <- currentpolicy["Issue.age"][1,1]
  gender <- currentpolicy["Sex"][1,1]
  smokerstatus <- currentpolicy["Smoker.Status"][1,1]
  index_gendersmoker <- paste(gender, smokerstatus, sep = '')
  
  
  #adding mortality 
  output <- matrix(c(0:(120-currentage)))
  output <- cbind(output, c(0, c(issueyear:(issueyear+120-currentage-1))))
  output <- cbind(output, c(currentage:120))
  
  base_adjusted <- adjusted_mortality_table[currentage:120, index_gendersmoker]
  
  output <- cbind(output, base_adjusted)
  
  #renaming columns
  colnames(output) <- c("Policy_Year", "Calendar_Year", "Age", "Mortality_Rate")
  output$Mortality_Rate <- as.numeric(output$Mortality_Rate)
  
  #calculating end of year exposure 
  eoy_exp <- 1
  for (i in 2:nrow(output)) {
    eoy_exp = c(eoy_exp, last(eoy_exp)*(1-output[i,"Mortality_Rate"]))
  }
  output <- cbind(output, "EOY_Exp" = eoy_exp)
  
}

#------------------------------------------------------------------------------------------------------------------------------------------------
# Discounting function
#------------------------------------------------------------------------------------------------------------------------------------------------
discounting_exposure_factors <- function(i, exposure_factor, yieldcurve){
  discounting <- lapply(2:nrow(exposure_factor), function(x) exposure_factor[x,"EOY_Exp"]*prod(unlist(lapply(1:(x-1), function(y) 1/(1+yieldcurve[y,2]) ))) )
  output <- sum(unlist(discounting))
  output
}




########################################################################################################################################################################
# Creating function for profit calculation (reserves and capital)
########################################################################################################################################################################
#------------------------------------------------------------------------------------------------------------------------------------------------
# Projection function
#------------------------------------------------------------------------------------------------------------------------------------------------
projecting_values <- function(currentpolicy, i){
  options(scipen=999)
  
  #defining basic info
  currentage <- currentpolicy["Issue.age"][1,1]
  gender <- currentpolicy["Sex"][1,1]
  smokerstatus <- currentpolicy["Smoker.Status"][1,1]
  index_gendersmoker <- paste(gender, smokerstatus, sep = '')
  
  
  #adding mortality and lapse
  output <- matrix(c(0:(120-currentage)))
  output <- cbind(output, c(currentage:120))
  
  base_adjusted <- adjusted_mortality_table[currentage:120, index_gendersmoker]
  
  output <- cbind(output, base_adjusted)
  
  #renaming columns
  colnames(output) <- c("Policy_Year", "Age", "Mortality_Rate")
  output$Mortality_Rate <- as.numeric(output$Mortality_Rate)
  
  #calculating end of year exposure 
  eoy_exp <- 1
  for (i in 2:nrow(output)) {
    eoy_exp = c(eoy_exp, last(eoy_exp)*(1-output[i,"Mortality_Rate"]))
  }
  output <- cbind(output, "EOY_Exp" = eoy_exp)
  
  #calculating reserve + capital rate 
  reserverate <- c(seq(reserverates[1,index_gendersmoker],950,length.out=(85-currentage+1)),rep(950,120-85))
  reserve_and_capital <- reserverate+capital_per1000
  output <- cbind(output, "R_n_Capital_Exp" = reserve_and_capital*output$EOY_Exp)
  
  #reserve increase
  rr <- reserverate
  reserveincrease <- c(-rr[1], rr[1]-rr[2], unlist(lapply(3:nrow(output), function(x) rr[x-1]*output[x-2,"EOY_Exp"]-rr[x]*output[x-1,"EOY_Exp"])))
  output <- cbind(output, reserveincrease)
  
}

show(projecting_values(currentprocessing(1),1))

#------------------------------------------------------------------------------------------------------------------------------------------------
# Discounting function
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
# Running functions to calculate values 
########################################################################################################################################################################
simple_mpf_output <- simplified_mpf
simple_mpf_output[, "Discounted_Exp_Claims_Factor"] <- rep(0, nrow(simple_mpf_output))
simple_mpf_output[, "Discounted_Exposure_Factor"] <- rep(0, nrow(simple_mpf_output))
simple_mpf_output[, "Discounted_ReserveCapital_Factor"] <- rep(0, nrow(simple_mpf_output))
simple_mpf_output[, "Discounted_ReserveIncrease_Factor"] <- rep(0, nrow(simple_mpf_output))


for (x in 1:nrow(simplified_mpf)){
  simple_mpf_output[x,"Discounted_Exp_Claims_Factor"] <- discounting_expclaims(x, expected_claims(currentprocessing(x),x), yield_curve(x))
  simple_mpf_output[x,"Discounted_ReserveCapital_Factor"] <- discounting_factor_reservecapital(x, projecting_values(currentprocessing(x),x), yield_curve(x))
  simple_mpf_output[x,"Discounted_ReserveIncrease_Factor"] <- discounting_factor_reserveincrease(x, projecting_values(currentprocessing(x),x), yield_curve(x))
  simple_mpf_output[x,"Discounted_Exposure_Factor"] <- discounting_exposure_factors(x, exposure_factors(currentprocessing(x),x), yield_curve(x))
}

########################################################################################################################################################################
# Extracting out simplified values back into original dataset
########################################################################################################################################################################
spwl_discounted_values <- spwl_inforcetable %>%
  select(Policy.number:Distribution.Channel) 

spwl_discounted_values <- LOOKUP(spwl_discounted_values, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_Exp_Claims_Factor")
spwl_discounted_values <- LOOKUP(spwl_discounted_values, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_Exposure_Factor")
spwl_discounted_values <- LOOKUP(spwl_discounted_values, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_ReserveCapital_Factor")
spwl_discounted_values <- LOOKUP(spwl_discounted_values, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_ReserveIncrease_Factor")

spwl_discounted_values <- spwl_discounted_values %>%
  mutate(Discounted_Exp_Claims = Discounted_Exp_Claims_Factor*Face.amount)

########################################################################################################################################################################
# Calculating premium
########################################################################################################################################################################
spwl_values <- spwl_discounted_values

spwl_values <- spwl_values %>%
  mutate("Single_Premium" = (Discounted_Exp_Claims*(1+riskmargin[Underwriting.Class,"Factor"])+Discounted_Exposure_Factor*expensetable["Renewal","Expense"])/(1-expensetable[1,1]-commissionfactors[Distribution.Channel, "Initial"]-commissionfactors[Distribution.Channel,"Renewal"]*((1+interest_rate_assumption)*(1-(1+interest_rate_assumption)^-(119-Issue.age))/interest_rate_assumption)-profitmargin[Underwriting.Class,1]))

withoutprogram_premium_spwl = spwl_values[,c("Policy.number", "Single_Premium", "Issue.year", "Issue.age", "Sex", "Face.amount")]

########################################################################################################################################################################
# Calculating profit
########################################################################################################################################################################
#Calculate discounted interest 
spwl_values <- spwl_values %>%
  mutate(DiscountedInterestEarned = investmentrate*(Face.amount/1000*Discounted_ReserveCapital_Factor+Single_Premium*(0-commissionfactors[Distribution.Channel,"Renewal"])*Discounted_Exposure_Factor))

#Calculate discounted profit 
spwl_values <- spwl_values %>%
  mutate(DiscountedProfit = Single_Premium-(commissionfactors[Distribution.Channel,"Initial"]+commissionfactors[Distribution.Channel,"Renewal"]*Discounted_Exposure_Factor)*Single_Premium-(expensetable[1,1]*Single_Premium+expensetable[2,1]*Discounted_Exposure_Factor)-Discounted_Exp_Claims+DiscountedInterestEarned+Face.amount/1000*Discounted_ReserveIncrease_Factor)

spwl_values <- spwl_values %>%
  mutate(PVAccumPremium = Single_Premium)

profittable_withoutp <- spwl_values %>%
  group_by(Underwriting.Class) %>%
  summarise("PVProfit"=sum(DiscountedProfit), "ProfitMargin" = round(100*sum(DiscountedProfit)/sum(PVAccumPremium),0)) %>%
  mutate("%ofProfit" = round(100*PVProfit/sum(PVProfit),0))


PVprofit_withoutprogram <- sum(spwl_values$DiscountedProfit)
PVprofit_withoutprogram

PVexpclaims_withoutprogram <- sum(spwl_values$Discounted_Exp_Claims)
PVexpclaims_withoutprogram

profitmargin_withoutprogram <- sum(spwl_values$DiscountedProfit)/sum(spwl_values$PVAccumPremium)

########################################################################################################################################################################
# Ratios
########################################################################################################################################################################
spwl_values <- spwl_values %>%
  mutate(PVRiskMargin = riskmargin[Underwriting.Class,1]*Discounted_Exp_Claims) 

spwl_values <- spwl_values %>%
  mutate(PVACQExpense = Single_Premium*expensetable[1,1])

spwl_values <- spwl_values %>%
  mutate(PVRenewalExpense = Discounted_Exposure_Factor*expensetable[2,1])

spwl_values <- spwl_values %>%
  mutate(PVINITComm = Single_Premium*commissionfactors[Distribution.Channel,"Initial"])

spwl_values <- spwl_values %>%
  mutate(PVRenewalComm = Single_Premium*commissionfactors[Distribution.Channel,"Renewal"]*((1+interest_rate_assumption)*(1-(1+interest_rate_assumption)^-(119-Issue.age))/interest_rate_assumption))

spwl_values <- spwl_values %>%
  mutate(PVProfitMargin = PVAccumPremium*profitmargin[Underwriting.Class,1])


# Calculating ratios
spwl_ratios <- matrix(nrow = 7, ncol = 1)
rownames(spwl_ratios) <- c("Commission", "Loss", "Expense","Combined_CLE","Risk", "Profit", "Total")
colnames(spwl_ratios) <- "Ratios"

spwl_ratios["Commission","Ratios"] <- (sum(spwl_values$PVINITComm)+sum(spwl_values$PVRenewalComm))/sum(spwl_values$PVAccumPremium)
spwl_ratios["Loss","Ratios"] <- sum(spwl_values$Discounted_Exp_Claims)/sum(spwl_values$PVAccumPremium)
spwl_ratios["Expense","Ratios"] <- (sum(spwl_values$PVACQExpense)+sum(spwl_values$PVRenewalExpense))/sum(spwl_values$PVAccumPremium)
spwl_ratios["Combined_CLE","Ratios"] <- sum(spwl_ratios[1:3,1])
spwl_ratios["Risk","Ratios"] <- sum(spwl_values$PVRiskMargin)/sum(spwl_values$PVAccumPremium)
spwl_ratios["Profit","Ratios"] <- sum(spwl_values$PVProfitMargin)/sum(spwl_values$PVAccumPremium) #this profit ratio doesn't make sense 
spwl_ratios["Total","Ratios"] <- sum(spwl_ratios[4:6,1])

################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
# THE FINAL OUTPUT IS spwl_values !!!!!!!! 
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
################################################################################################################################################################################################################################################################################################################################################
#Important outputs 
PVprofit_withoutprogram
PVexpclaims_withoutprogram
spwl_ratios
profittable_withoutp
profitmargin_withoutprogram #overall profit margin of entire book 

##################
# Only needed to make sensitives easier to calculate (can ignore)
spwl_sens <- data_frame(
  "VIF" = PVprofit_withoutprogram,
  "PVExpectedClaims" = PVexpclaims_withoutprogram,
  "PVProgramCosts" = 0,
  "LossRatio" = spwl_ratios["Loss","Ratios"],
  "ExpenseRatio" = spwl_ratios["Expense","Ratios"],
  "CommissionRatio" = spwl_ratios["Commission","Ratios"],
  "CombinedRatio" = spwl_ratios["Combined_CLE","Ratios"],
  "ProfitMargin" = profitmargin_withoutprogram
)

rownames(spwl_sens) <- "WithoutProgram"
