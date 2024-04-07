################################################################################################################################################################
################################################################################################################################################################
# NOTE: Must run All_T20_Modelling_withoutprogram script first 
################################################################################################################################################################
################################################################################################################################################################

########################################################################################################################################################################
# Importing best worst middle cases 
########################################################################################################################################################################
programcosts_cases  <- read.csv('Data/program_costs.csv', row.names = 1)
lapse_cases <- read.csv('Data/lapse_adjustments.csv', row.names = 1)
mortality_cases <- read.csv('Data/mortality_adjustments.csv', row.names = 1)

programcosts_cases <- programcosts_cases*1000

# Change assumptions below to see output 

########################################################################################################################################################################
# Assumptions 
########################################################################################################################################################################
lapsecase_ind <- "middle_case"
mortalitycase_ind <- "middle_case"
programcostcase_ind <- "middle_case"
interest_rate_assumption <- 0.04
investmentrate <- 0.06
run_model_on_new_2024MPF <- "no" # either yes or no - this indicator determines whether to run the model on inforce book or 2024_newbusiness book 


#Don't change anything below this, All changes should be made in ASSUMPTIONS section ONLY 
########################################################################################################################################################################
########################################################################################################################################################################
if (run_model_on_new_2024MPF == "yes") {
  simplified_mpf <- simplified_mpf_newbusiness
}

#LAPSE table
lapse_table <- data.frame(
  "Policy_year" = seq(0,20),
  "Lapse_rate" = c(0, lapse_cases[,lapsecase_ind])
)


#Mortality adjustments
adjustments_table["FS","Factor"] <- mortality_cases["FS", mortalitycase_ind]
adjustments_table["FNS","Factor"] <- mortality_cases["FNS", mortalitycase_ind]
adjustments_table["MS","Factor"] <- mortality_cases["MS", mortalitycase_ind]
adjustments_table["MNS","Factor"] <- mortality_cases["MNS", mortalitycase_ind]


expensetable <- data.frame(
  "Expense" = c(2.05, 170, programcosts_cases["NS", programcostcase_ind], programcosts_cases["S", programcostcase_ind] )
)
rownames(expensetable) <- c("Acquisition", "Renewal", "NS", "S") #first two are acq and renewal expense which are applied to everyone. Second two are yearly expenses for program.



########################################################################################################################################################################
# Adjusted Mortality Table
########################################################################################################################################################################
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
t20_discounted_factors_withp <- t20_inforcetable %>%
  select(Policy.number:Distribution.Channel) 

if (run_model_on_new_2024MPF == "yes") {
  t20_discounted_factors_withp <- nb_2024_t20
}

t20_discounted_factors_withp <- LOOKUP(t20_discounted_factors_withp, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_Exp_Claims_Factor")
t20_discounted_factors_withp <- LOOKUP(t20_discounted_factors_withp, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_Exposure_Factor")
t20_discounted_factors_withp <- LOOKUP(t20_discounted_factors_withp, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_ReserveCapital_Factor")
t20_discounted_factors_withp <- LOOKUP(t20_discounted_factors_withp, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_ReserveIncrease_Factor")

t20_discounted_factors_withp <- t20_discounted_factors_withp %>%
  mutate(Discounted_Exp_Claims = Discounted_Exp_Claims_Factor*Face.amount)

########################################################################################################################################################################
# Calculating premium
########################################################################################################################################################################
t20_values_withp <- t20_discounted_factors_withp

t20_values_withp <- LOOKUP(t20_values_withp, c("Policy.number"), withoutprogram_annualpremium, c("Policy.number"), "Annual_Premium")

########################################################################################################################################################################
# Calculating profit
########################################################################################################################################################################
#Calculate discounted interest 
t20_values_withp <- t20_values_withp %>%
  mutate(DiscountedInterestEarned = investmentrate*(Face.amount/1000*Discounted_ReserveCapital_Factor+Annual_Premium*(1-commissionfactors[Distribution.Channel,"Renewal"])*Discounted_Exposure_Factor))

#Caculate discounted profit 
t20_values_withp <- t20_values_withp %>%
  mutate(DiscountedProfit = Annual_Premium*(1+Discounted_Exposure_Factor)-(commissionfactors[Distribution.Channel,"Initial"]+commissionfactors[Distribution.Channel,"Renewal"]*Discounted_Exposure_Factor)*Annual_Premium-(expensetable[1,1]*Annual_Premium+expensetable[Smoker.Status,1]+(expensetable[2,1]+expensetable[Smoker.Status,1])*Discounted_Exposure_Factor)-Discounted_Exp_Claims+DiscountedInterestEarned+Face.amount/1000*Discounted_ReserveIncrease_Factor)

t20_values_withp <- t20_values_withp %>%
  mutate(PVAccumPremium = Annual_Premium*interestdiscount[2,1])

profittable_withp <- t20_values_withp %>%
  group_by(Underwriting.Class) %>%
  summarise("PVProfit"=sum(DiscountedProfit), "ProfitMargin" = round(100*sum(DiscountedProfit)/sum(PVAccumPremium),0)) %>%
  mutate("%ofProfit" = round(100*PVProfit/sum(PVProfit),0))

PVprofit_withp <- sum(t20_values_withp$DiscountedProfit)
PVprofit_withp

PVexpclaims_withp <- sum(t20_values_withp$Discounted_Exp_Claims)
PVexpclaims_withp

profitmargin_withp <- sum(t20_values_withp$DiscountedProfit)/sum(t20_values_withp$PVAccumPremium)

########################################################################################################################################################################
# Ratios
########################################################################################################################################################################
t20_values_withp <- t20_values_withp %>%
  mutate(PVRiskMargin = riskmargin[Underwriting.Class,1]*Discounted_Exp_Claims) 

t20_values_withp <- t20_values_withp %>%
  mutate(PVAllACQExpense = Annual_Premium*expensetable[1,1]+expensetable[Smoker.Status,1])

t20_values_withp <- t20_values_withp %>%
  mutate(PVAllRenewalExpense = Discounted_Exposure_Factor*(expensetable[2,1]+expensetable[Smoker.Status,1]))

t20_values_withp <- t20_values_withp %>%
  mutate(PVProgramCosts = (1+Discounted_Exposure_Factor)*(expensetable[Smoker.Status,1]))

t20_values_withp <- t20_values_withp %>%
  mutate(PVINITComm = Annual_Premium*commissionfactors[Distribution.Channel,"Initial"])

t20_values_withp <- t20_values_withp %>%
  mutate(PVRenewalComm = Annual_Premium*commissionfactors[Distribution.Channel,"Renewal"]*interestdiscount[3,1])

t20_values_withp <- t20_values_withp %>%
  mutate(PVAccumPremium = Annual_Premium*interestdiscount[2,1])

t20_values_withp <- t20_values_withp %>%
  mutate(PVProfitMargin = PVAccumPremium*profitmargin[Underwriting.Class,1])


# Calculating ratios
t20_ratios_withp <- matrix(nrow = 7, ncol = 1)
rownames(t20_ratios_withp) <- c("Commission", "Loss", "Expense","Combined_CLE","Risk", "Profit", "Total")
colnames(t20_ratios_withp) <- "Ratios"

t20_ratios_withp["Commission","Ratios"] <- (sum(t20_values_withp$PVINITComm)+sum(t20_values_withp$PVRenewalComm))/sum(t20_values_withp$PVAccumPremium)
t20_ratios_withp["Loss","Ratios"] <- sum(t20_values_withp$Discounted_Exp_Claims)/sum(t20_values_withp$PVAccumPremium)
t20_ratios_withp["Expense","Ratios"] <- (sum(t20_values_withp$PVAllACQExpense)+sum(t20_values_withp$PVAllRenewalExpense))/sum(t20_values_withp$PVAccumPremium)
t20_ratios_withp["Combined_CLE","Ratios"] <- sum(t20_ratios_withp[1:3,1])
t20_ratios_withp["Risk","Ratios"] <- sum(t20_values_withp$PVRiskMargin)/sum(t20_values_withp$PVAccumPremium)
t20_ratios_withp["Profit","Ratios"] <- sum(t20_values_withp$PVProfitMargin)/sum(t20_values_withp$PVAccumPremium)
t20_ratios_withp["Total","Ratios"] <- sum(t20_ratios_withp[4:6,1])

########################################################################################################################################################################
# Outputs that should be seen
########################################################################################################################################################################
PVprofit_withp
PVexpclaims_withp
t20_ratios_withp
profittable_withp
profitmargin_withp #overall profit margin 
PVprofit_withp-PVprofit_withoutprogram


