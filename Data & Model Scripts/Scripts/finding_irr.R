#------------------------------------------------------------------------------------------------------------------------------------------------
# Creating yield curve for T20 modelling 
#------------------------------------------------------------------------------------------------------------------------------------------------


finding_IRR <- function(w) {
  
  IRR_rate <- w
  
  yield_curve_IRR <- function(i){
    issueyear <- 9999
    curve <- matrix(c(issueyear:(issueyear+20)))
    colnames(curve) <- "Year"
    
    curve <- LOOKUP(as.data.frame(curve), "Year", economictable, "Year", "1yr")
    curve[is.na(curve)] <- IRR_rate
    curve
  }
  
  
  
  #------------------------------------------------------------------------------------------------------------------------------------------------
  # Running all the functions 
  #------------------------------------------------------------------------------------------------------------------------------------------------
  for (x in 1:nrow(simplified_mpf)){
    simple_mpf_output[x,"Discounted_Exp_Claims_Factor"] <- discounting_expclaims(x, expected_claims(currentprocessing(x),x), yield_curve_IRR(x))
    simple_mpf_output[x,"Discounted_ReserveCapital_Factor"] <- discounting_factor_reservecapital(x, projecting_values(currentprocessing(x),x), yield_curve_IRR(x))
    simple_mpf_output[x,"Discounted_ReserveIncrease_Factor"] <- discounting_factor_reserveincrease(x, projecting_values(currentprocessing(x),x), yield_curve_IRR(x))
    simple_mpf_output[x,"Discounted_Exposure_Factor"] <- discounting_exposure_factor(x, exposure_factor(currentprocessing(x),x), yield_curve_IRR(x))
  }
  
  
  
  ########################################################################################################################################################################
  # Extracting out simplified values back into original data-set
  ########################################################################################################################################################################
  t20_discounted_factors_withp <- t20_inforcetable %>%
    select(Policy.number:Distribution.Channel) 
  
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
  
  show(PVprofit_withp)
}

finding_IRR(0.28)

lapply(seq(0.298,0.299,0.0001), function(x) finding_IRR(x))



