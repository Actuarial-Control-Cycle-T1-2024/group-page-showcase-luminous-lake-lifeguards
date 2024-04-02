################################################################################################################################################################
################################################################################################################################################################
# NOTE: Must run All_spwl_Modelling_withprogram script first 
################################################################################################################################################################
################################################################################################################################################################

########################################################################################################################################################################
# Importing best worst middle cases 
########################################################################################################################################################################
programcosts_cases  <- read.csv('Data/program_costs.csv', row.names = 1)
mortality_cases <- read.csv('Data/mortality_adjustments.csv', row.names = 1)
sensitivity_cases_spwl <- read.csv('Data/sensitivity_cases_spwl.csv')

programcosts_cases <- programcosts_cases*1000

# Change assumptions below to see output 
running_sensitivities_spwl <- function(sen){
  
  s <- as.numeric(sen)
  ########################################################################################################################################################################
  # Assumptions 
  ########################################################################################################################################################################
  mortalitycase_ind <- sensitivity_cases_spwl[s,"Mortality"]
  programcostcase_ind <- sensitivity_cases_spwl[s,"Program_Costs"]
  interest_rate_assumption <- sensitivity_cases_spwl[s,"Discount"]
  investmentrate <- sensitivity_cases_spwl[s,"Asset_Earning_Rate"]
  run_model_on_new_2024MPF <- "no" # either yes or no - this indicator determines whether to run the model on inforce book or 2024_newbusiness book 
  
  
  
  #Don't change anything below this, All changes should be made in ASSUMPTIONS section ONLY 
  ########################################################################################################################################################################
  ########################################################################################################################################################################
  if (run_model_on_new_2024MPF == "yes") {
    simplified_mpf <- simplified_mpf_newbusiness
  }
  
  #Mortality adjustments
  adjustments_table["FS","Factor"] <- mortality_cases["FS", mortalitycase_ind]
  adjustments_table["FNS","Factor"] <- mortality_cases["FNS", mortalitycase_ind]
  adjustments_table["MS","Factor"] <- mortality_cases["MS", mortalitycase_ind]
  adjustments_table["MNS","Factor"] <- mortality_cases["MNS", mortalitycase_ind]
  
  
  expensetable <- data.frame(
    "Expense" = c(sensitivity_cases_spwl[s,"Acquisition_Expense"], sensitivity_cases_spwl[s,"Renewal_Expense"], programcosts_cases["NS", programcostcase_ind], programcosts_cases["S", programcostcase_ind] )
  )
  rownames(expensetable) <- c("Acquisition", "Renewal", "NS", "S") #first two are acq and renewal expense which are applied to everyone. Second two are yearly expenses for program.
  
  ########################################################################################################################################################################
  # Adjusted Mortality Tables 
  ########################################################################################################################################################################
  adjusted_mortality_table <- base_mortalitytable %>%
    mutate("FNS" = mortality_rate*adjustments_table["FNS","Factor"]) %>%
    mutate("FS" = mortality_rate*adjustments_table["FS","Factor"]) %>%
    mutate("MNS" = mortality_rate*adjustments_table["MNS","Factor"]) %>%
    mutate("MS" = mortality_rate*adjustments_table["MS","Factor"])
  
  adjusted_mortality_table$FS <- replace(adjusted_mortality_table$FS, adjusted_mortality_table$FS > 1, 1)
  adjusted_mortality_table$MS <- replace(adjusted_mortality_table$MS, adjusted_mortality_table$MS > 1, 1)
  
  adjusted_mortality_table[120, 3:6] <- 1
  
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
  spwl_discounted_values_withp <- spwl_inforcetable %>%
    select(Policy.number:Distribution.Channel) 
  
  if (run_model_on_new_2024MPF == "yes") {
    spwl_discounted_values_withp <- nb_2024_spwl
  }
  
  spwl_discounted_values_withp <- LOOKUP(spwl_discounted_values_withp, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_Exp_Claims_Factor")
  spwl_discounted_values_withp <- LOOKUP(spwl_discounted_values_withp, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_Exposure_Factor")
  spwl_discounted_values_withp <- LOOKUP(spwl_discounted_values_withp, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_ReserveCapital_Factor")
  spwl_discounted_values_withp <- LOOKUP(spwl_discounted_values_withp, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), simple_mpf_output, c("Issue.year", "Issue.age", "Sex", "Smoker.Status"), "Discounted_ReserveIncrease_Factor")
  
  spwl_discounted_values_withp <- spwl_discounted_values_withp %>%
    mutate(Discounted_Exp_Claims = Discounted_Exp_Claims_Factor*Face.amount)
  
  ########################################################################################################################################################################
  # Calculating premium
  ########################################################################################################################################################################
  spwl_values_withp <- spwl_discounted_values_withp
  
  spwl_values_withp <- LOOKUP(spwl_values_withp, c("Policy.number"), withoutprogram_premium_spwl, c("Policy.number"), "Single_Premium")
  
  
  ########################################################################################################################################################################
  # Calculating profit
  ########################################################################################################################################################################
  #Calculate discounted interest 
  spwl_values_withp <- spwl_values_withp %>%
    mutate(DiscountedInterestEarned = investmentrate*(Face.amount/1000*Discounted_ReserveCapital_Factor+Single_Premium*(0-commissionfactors[Distribution.Channel,"Renewal"])*Discounted_Exposure_Factor))
  
  #Calculate discounted profit 
  spwl_values_withp <- spwl_values_withp %>%
    mutate(DiscountedProfit = Single_Premium-(commissionfactors[Distribution.Channel,"Initial"]+commissionfactors[Distribution.Channel,"Renewal"]*Discounted_Exposure_Factor)*Single_Premium-(expensetable[1,1]*Single_Premium+expensetable[Smoker.Status,1]+(expensetable[2,1]+expensetable[Smoker.Status,1])*Discounted_Exposure_Factor)-Discounted_Exp_Claims+DiscountedInterestEarned+Face.amount/1000*Discounted_ReserveIncrease_Factor)
  
  spwl_values_withp <- spwl_values_withp %>%
    mutate(PVAccumPremium = Single_Premium)
  
  profittable_withp <- spwl_values_withp %>%
    group_by(Underwriting.Class) %>%
    summarise("PVProfit"=sum(DiscountedProfit), "ProfitMargin" = round(100*sum(DiscountedProfit)/sum(PVAccumPremium),0)) %>%
    mutate("%ofProfit" = round(100*PVProfit/sum(PVProfit),0))
  
  
  PVprofit_withprogram <- sum(spwl_values_withp$DiscountedProfit)
  PVprofit_withprogram
  
  PVexpclaims_withprogram <- sum(spwl_values_withp$Discounted_Exp_Claims)
  PVexpclaims_withprogram
  
  profitmargin_withprogram <- sum(spwl_values_withp$DiscountedProfit)/sum(spwl_values_withp$PVAccumPremium)
  
  ########################################################################################################################################################################
  # Ratios
  ########################################################################################################################################################################
  spwl_values_withp <- spwl_values_withp %>%
    mutate(PVRiskMargin = riskmargin[Underwriting.Class,1]*Discounted_Exp_Claims) 
  
  spwl_values_withp <- spwl_values_withp %>%
    mutate(PVAllACQExpense = Single_Premium*expensetable[1,1]+expensetable[Smoker.Status,1])
  
  spwl_values_withp <- spwl_values_withp %>%
    mutate(PVAllRenewalExpense = Discounted_Exposure_Factor*(expensetable[2,1]+expensetable[Smoker.Status,1]))
  
  spwl_values_withp <- spwl_values_withp %>%
    mutate(PVProgramCosts = (1+Discounted_Exposure_Factor)*expensetable[Smoker.Status,1])
  
  spwl_values_withp <- spwl_values_withp %>%
    mutate(PVINITComm = Single_Premium*commissionfactors[Distribution.Channel,"Initial"])
  
  spwl_values_withp <- spwl_values_withp %>%
    mutate(PVRenewalComm = Single_Premium*commissionfactors[Distribution.Channel,"Renewal"]*((1+interest_rate_assumption)*(1-(1+interest_rate_assumption)^-(119-Issue.age))/interest_rate_assumption))
  
  spwl_values_withp <- spwl_values_withp %>%
    mutate(PVProfitMargin = PVAccumPremium*profitmargin[Underwriting.Class,1])
  
  
  # Calculating ratios
  spwl_ratios_withp <- matrix(nrow = 7, ncol = 1)
  rownames(spwl_ratios_withp) <- c("Commission", "Loss", "Expense","Combined_CLE","Risk", "Profit", "Total")
  colnames(spwl_ratios_withp) <- "Ratios"
  
  spwl_ratios_withp["Commission","Ratios"] <- (sum(spwl_values_withp$PVINITComm)+sum(spwl_values_withp$PVRenewalComm))/sum(spwl_values_withp$PVAccumPremium)
  spwl_ratios_withp["Loss","Ratios"] <- sum(spwl_values_withp$Discounted_Exp_Claims)/sum(spwl_values_withp$PVAccumPremium)
  spwl_ratios_withp["Expense","Ratios"] <- (sum(spwl_values_withp$PVAllACQExpense)+sum(spwl_values_withp$PVAllRenewalExpense))/sum(spwl_values_withp$PVAccumPremium)
  spwl_ratios_withp["Combined_CLE","Ratios"] <- sum(spwl_ratios_withp[1:3,1])
  spwl_ratios_withp["Risk","Ratios"] <- sum(spwl_values_withp$PVRiskMargin)/sum(spwl_values_withp$PVAccumPremium)
  spwl_ratios_withp["Profit","Ratios"] <- sum(spwl_values_withp$PVProfitMargin)/sum(spwl_values_withp$PVAccumPremium) #this profit ratio doesn't make sense 
  spwl_ratios_withp["Total","Ratios"] <- sum(spwl_ratios_withp[4:6,1])
  
  ########################################################################################################################################################################
  # Outputs that should be seen
  ########################################################################################################################################################################
  PVprofit_withprogram
  PVexpclaims_withprogram
  spwl_ratios_withp
  profittable_withp
  profitmargin_withprogram #overall profit margin of entire book 
  PVprofit_withprogram - PVprofit_withoutprogram
  
  spwl_sens <- rbind(spwl_sens,c(PVprofit_withprogram,PVexpclaims_withprogram,sum(spwl_values_withp$PVProgramCosts),spwl_ratios_withp["Loss","Ratios"], spwl_ratios_withp["Expense","Ratios"], spwl_ratios_withp["Commission","Ratios"], spwl_ratios_withp["Combined_CLE","Ratios"], profitmargin_withprogram)) 
  rownames(spwl_sens)[s] <- sensitivity_cases_spwl$X[s]
  
  spwl_sens
}


for (s in 2:nrow(sensitivity_cases_spwl)){
  spwl_sens <- running_sensitivities_spwl(s)
}