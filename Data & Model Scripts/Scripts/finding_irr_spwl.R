

  IRR_rate <- 0.02
  
  yield_curve_IRR <- function(i){
    currentage <- currentprocessing(i)["Issue.age"][1,1]
    issueyear <- 9999
    curve <- matrix(c(issueyear:(issueyear+120-currentage-1)))
    colnames(curve) <- "Year"
    
    curve <- LOOKUP(as.data.frame(curve), "Year", economictable, "Year", "1yr")
    curve[is.na(curve)] <- IRR_rate
    curve
  }
  
  ########################################################################################################################################################################
  # Running functions to calculate values me
  ########################################################################################################################################################################
  simple_mpf_output <- simplified_mpf
  simple_mpf_output[, "Discounted_Exp_Claims_Factor"] <- rep(0, nrow(simple_mpf_output))
  simple_mpf_output[, "Discounted_Exposure_Factor"] <- rep(0, nrow(simple_mpf_output))
  simple_mpf_output[, "Discounted_ReserveCapital_Factor"] <- rep(0, nrow(simple_mpf_output))
  simple_mpf_output[, "Discounted_ReserveIncrease_Factor"] <- rep(0, nrow(simple_mpf_output))
  
  
  for (x in 1:nrow(simplified_mpf)){
    simple_mpf_output[x,"Discounted_Exp_Claims_Factor"] <- discounting_expclaims(x, expected_claims(currentprocessing(x),x), yield_curve_IRR(x))
    simple_mpf_output[x,"Discounted_ReserveCapital_Factor"] <- discounting_factor_reservecapital(x, projecting_values(currentprocessing(x),x), yield_curve_IRR(x))
    simple_mpf_output[x,"Discounted_ReserveIncrease_Factor"] <- discounting_factor_reserveincrease(x, projecting_values(currentprocessing(x),x), yield_curve_IRR(x))
    simple_mpf_output[x,"Discounted_Exposure_Factor"] <- discounting_exposure_factors(x, exposure_factors(currentprocessing(x),x), yield_curve_IRR(x))
  }
  
  ########################################################################################################################################################################
  # Extracting out simplified values back into original dataset
  ########################################################################################################################################################################
  spwl_discounted_values_withp <- spwl_inforcetable %>%
    select(Policy.number:Distribution.Channel) 
  
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





