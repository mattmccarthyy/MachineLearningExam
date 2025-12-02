###################################################################################################################################################################################
#==================================================================================================================================================================================

##Severity GLM##
##Version 1: Uncapped Gross Claims

library(readr)
library(MASS)
library(splines)
library(ggplot2)

##Load in cleaned data set 
claims_severity = readRDS(url("https://github.com/mattmccarthyy/Statistical-Consulting-/raw/refs/heads/main/data/processed/claims_severity.rds"))
View(claims_severity)

par(mfrow=c(1,2))
hist(claims_severity$net_amount)
hist(claims_severity$gross_amount)
summary(claims_severity$net_amount)
summary(claims_severity$gross_amount)

#============================================================================================================================================================================================

##Split Data
split_data(claims_severity)

#=====================================================================================================

##Selection Loops - Use same spline for age as in frequency glm

#Load in library
library(MASS)
library(splines)

#Null model
null_gamma = glm(gross_amount ~1, family=Gamma(link="log"), data=train)

#Null model summary
summary(null_gamma) #AIC=991657

#Full model
full_gamma = glm(
  gross_amount~factor(usage)+factor(occasional_commercial)+factor(area)+factor(province)+
    factor(overnight_parking)+factor(security_device)+factor(body_type)+ factor(vehicle_power)+
    factor(fuel)+factor(transmission)+engine_cc+vehicle_value+factor(gender)+factor(ncd_level)+years_licensed+
    licensing_age+vehicle_age+reported_mileage+num_drivers+factor(employment_missing)+
    factor(reported_mileage_missing)+factor(engine_cc_missing)+ns(age, df=6),
  family=Gamma(link="log"), data=train
)

#Full model summary
summary(full_gamma) #AIC=987781

#Forward selection loop
forward_gamma = stepAIC(
  object = null_gamma, 
  scope = list(lower = formula(null_gamma), upper = formula(full_gamma)), 
  direction = "forward", 
  trace = TRUE  #Shows progress of variables included in each iteration
)

#Forward selection model summary
summary(forward_gamma) #AIC=987767

#Backward elimination loop
backward_gamma = stepAIC(
  object = full_gamma,
  direction = "backward", 
  trace = TRUE  #Shows progress of variables included in each iteration          
)

#Backward elimination model summary
summary(backward_gamma) #AIC=987767

#Bidirectional stepwise selection loop
stepwise_gamma = stepAIC(
  object = full_gamma,
  direction = "both",   #Allows both forward and backward selection
  trace = TRUE         
)

#Stepwise model summary
summary(stepwise_gamma) #AIC=987767

best_gamma = forward_gamma

best_gamma = glm(gross_amount ~ factor(vehicle_power) + factor(usage) + 
                   factor(area) + factor(fuel) + factor(overnight_parking) + 
                   ns(age, df = 6) + engine_cc + factor(security_device) + factor(occasional_commercial) + 
                   factor(reported_mileage_missing) + factor(ncd_level) + vehicle_age + 
                   factor(body_type) + factor(engine_cc_missing), family = Gamma(link = "log"), 
                 data = train)

#==========================================================================================================================

##Interactions

#Test reduction in AIC for each 2 way combination of predictors in best_gamma

#Define predictors
predictors_gamma = c("vehicle_power",
                    "usage",
                    "area",
                    "fuel",
                    "overnight_parking",
                    "age",
                    "engine_cc",
                    "security_device",
                    "occasional_commercial",
                    "ncd_level",
                    "vehicle_age",
                    "body_type",
                    "engine_cc_missing",
                    "reported_mileage_missing")

#Baseline model 
base_gamma = glm(gross_amount ~ 1, data = train, family = Gamma(link="log"))

#2-way combinations
combos_gamma = combn(predictors_gamma, 2, simplify = FALSE)

#Function to fit model and compute AIC reduction
test_combo_gamma = function(vars) {
  formula = as.formula(paste("gross_amount ~", paste(vars, collapse = " + ")))
  model = glm(formula, data = train, family = Gamma(link="log"))
  aic = AIC(model)
  reduction = AIC(base_gamma) - aic
  data.frame(var1 = vars[1],
             var2 = vars[2],
             AIC = aic,
             AIC_reduction = reduction)
}

#Apply across all combinations
results_gamma = do.call(rbind, lapply(combos_gamma, test_combo_gamma))

#Sort by largest AIC reduction
results_sorted_gamma = results_gamma[order(-results_gamma$AIC_reduction), ]
print(results_sorted_gamma) #Vehicle power appears in all of the top 13 interaction terms

#Use selection loops to determine which interactions to include

#Define candidate interactions 
candidates = paste(
  "factor(vehicle_power):factor(usage)",
  "factor(vehicle_power):factor(area)",
  "factor(vehicle_power):ns(age, df=6)",
  "factor(vehicle_power):factor(fuel)",
  "factor(vehicle_power):factor(body_type)",
  "factor(vehicle_power):factor(overnight_parking)",
  "factor(usage):engine_cc",
  "factor(vehicle_power):factor(security_device)",
  "factor(area):engine_cc",
  sep = " + "
)

scope_formula_gamma = as.formula(paste(". ~ . +", candidates))

#Forward Selection
forward_gamma = stepAIC(best_gamma, scope = list(upper = update(best_gamma, scope_formula_gamma)), 
                       direction = "forward", trace = TRUE)
summary(forward_gamma) #AIC=987760

#Bidirectional Stepwise Selection
stepwise_gamma = stepAIC(best_gamma, 
                    scope = list(upper = update(best_gamma, scope_formula)),
                    direction = "both",
                    trace = TRUE)
summary(stepwise_gamma)

##Best model including interactions: forward_gamma
gamma_model = forward_gamma

#Save for future use to avoid running loops
gamma_model = glm(gross_amount ~ factor(vehicle_power) + factor(usage) + 
                    factor(area) + factor(fuel) + factor(overnight_parking) + 
                    ns(age, df = 6) + engine_cc + factor(security_device) + factor(occasional_commercial) + 
                    factor(reported_mileage_missing) + factor(ncd_level) + vehicle_age + 
                    factor(body_type) + factor(engine_cc_missing) + factor(vehicle_power):factor(usage) + 
                    factor(usage):engine_cc, family = Gamma(link = "log"), data = train)
AIC(gamma_model) #987759.8

summary(gamma_model)

#LRT
anova(best_gamma, gamma_model, test="LRT") #Interactions Improve Fit

#Multi-collinearity
library(car)
vif(gamma_model)
alias(gamma_model)
##No evidence of multi-collinearity

#======================================================================================================================================

##Version 2: Capped Gross Claims
cap = 50000
claims_severity$gross = pmin(claims_severity$gross_amount,cap)
summary(claims_severity$gross)

##Selection Loops - Use same spline for age as in frequency glm

#Load in library
library(MASS)
library(splines)

#Null model
null_gamma_cap = glm(gross ~1, family=Gamma(link="log"), data=train)

#Null model summary
summary(null_gamma_cap) #AIC=987498

#Full model
full_gamma_cap = glm(
  gross~factor(usage)+factor(occasional_commercial)+factor(area)+factor(province)+
    factor(overnight_parking)+factor(security_device)+factor(body_type)+ factor(vehicle_power)+
    factor(fuel)+factor(transmission)+engine_cc+vehicle_value+factor(gender)+factor(ncd_level)+years_licensed+
    licensing_age+vehicle_age+reported_mileage+num_drivers+factor(employment_missing)+
    factor(reported_mileage_missing)+factor(engine_cc_missing)+ns(age, df=6),
  family=Gamma(link="log"), data=train
)

#Full model summary
summary(full_gamma_cap) #AIC=984160

#Forward selection loop
forward_gamma_cap = stepAIC(
  object = null_gamma_cap, 
  scope = list(lower = formula(null_gamma_cap), upper = formula(full_gamma_cap)), 
  direction = "forward", 
  trace = TRUE  #Shows progress of variables included in each iteration
)

#Forward selection model summary
summary(forward_gamma_cap) #AIC=984148

#Backward elimination loop
backward_gamma_cap = stepAIC(
  object = full_gamma_cap,
  direction = "backward", 
  trace = TRUE  #Shows progress of variables included in each iteration          
)

#Backward elimination model summary
summary(backward_gamma_cap) #AIC=984148

#Bidirectional stepwise selection loop
stepwise_gamma_cap = stepAIC(
  object = full_gamma_cap,
  direction = "both",   #Allows both forward and backward selection
  trace = TRUE         
)

#Stepwise model summary
summary(stepwise_gamma_cap) #AIC=984148

best_gamma_cap = forward_gamma_cap

best_gamma_cap = glm(gross ~ factor(vehicle_power) + factor(usage) + 
                   factor(area) + factor(fuel) + factor(overnight_parking) + 
                   ns(age, df = 6) + engine_cc + factor(security_device) + factor(occasional_commercial) + 
                   factor(reported_mileage_missing) + factor(ncd_level) + vehicle_age + 
                   factor(body_type) + factor(engine_cc_missing), family = Gamma(link = "log"), 
                 data = train)

#==========================================================================================================================

##Interactions

#Test reduction in AIC for each 2 way combination of predictors in best_gamma

#Define predictors
predictors_gamma_cap = c("vehicle_power",
                     "usage",
                     "area",
                     "fuel",
                     "overnight_parking",
                     "age",
                     "engine_cc",
                     "security_device",
                     "occasional_commercial",
                     "ncd_level",
                     "vehicle_age",
                     "body_type",
                     "engine_cc_missing",
                     "reported_mileage_missing")

#Baseline model 
base_gamma_cap = glm(gross ~ 1, data = train, family = Gamma(link="log"))

#2-way combinations
combos_gamma_cap = combn(predictors_gamma_cap, 2, simplify = FALSE)

#Function to fit model and compute AIC reduction
test_combo_gamma_cap = function(vars) {
  formula = as.formula(paste("gross ~", paste(vars, collapse = " + ")))
  model = glm(formula, data = train, family = Gamma(link="log"))
  aic = AIC(model)
  reduction = AIC(base_gamma_cap) - aic
  data.frame(var1 = vars[1],
             var2 = vars[2],
             AIC = aic,
             AIC_reduction = reduction)
}

#Apply across all combinations
results_gamma_cap = do.call(rbind, lapply(combos_gamma_cap, test_combo_gamma_cap))

#Sort by largest AIC reduction
results_sorted_gamma_cap = results_gamma_cap[order(-results_gamma_cap$AIC_reduction), ]
print(results_sorted_gamma_cap) #Vehicle power appears in all of the top 13 interaction terms

#Use selection loops to determine which interactions to include

#Define candidate interactions 
candidates = paste(
  "factor(vehicle_power):factor(usage)",
  "factor(vehicle_power):factor(area)",
  "factor(vehicle_power):ns(age, df=6)",
  "factor(vehicle_power):factor(fuel)",
  "factor(vehicle_power):factor(body_type)",
  "factor(vehicle_power):factor(overnight_parking)",
  "factor(usage):engine_cc",
  "factor(vehicle_power):factor(security_device)",
  "factor(area):engine_cc",
  sep = " + "
)

scope_formula_gamma_cap = as.formula(paste(". ~ . +", candidates))

#Forward Selection
forward_gamma_cap = stepAIC(best_gamma_cap, scope = list(upper = update(best_gamma_cap, scope_formula_gamma_cap)), 
                        direction = "forward", trace = TRUE)
summary(forward_gamma_cap) #AIC=984140

#Bidirectional Stepwise Selection
stepwise_gamma_cap = stepAIC(best_gamma_cap, 
                         scope = list(upper = update(best_gamma_cap, scope_formula)),
                         direction = "both",
                         trace = TRUE)
summary(stepwise_gamma_cap)

##Best model including interactions: forward_gamma
final_gamma = forward_gamma_cap
final_gamma
#Save for future use to avoid running loops
final_gamma = glm(gross ~ factor(vehicle_power) + factor(usage) + 
                    factor(area) + factor(fuel) + factor(overnight_parking) + 
                    ns(age, df = 6) + engine_cc + factor(security_device) + factor(occasional_commercial) + 
                    factor(reported_mileage_missing) + factor(ncd_level) + vehicle_age + 
                    factor(body_type) + factor(engine_cc_missing) + factor(vehicle_power):factor(usage) + 
                    factor(usage):engine_cc, family = Gamma(link = "log"), data = train)
AIC(final_gamma) #984141.2

summary(final_gamma)

#LRT
anova(best_gamma_cap, final_gamma, test="LRT") #Interactions Improve Fit

#Multi-collinearity
library(car)
vif(final_gamma)
alias(final_gamma)
##No evidence of multi-collinearity