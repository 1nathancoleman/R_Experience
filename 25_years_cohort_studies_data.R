###25 Years of Cohort Studies Meta Analysis 

#Import the data
setwd("C:/Users/nathancc/Box/PopeCAResearch/25 years of Cohort Studies")
library(readxl)
df <- read_excel("data.xlsx")

########################################################################################################
#################For each death type, convert HR and CI to balanced Coef and SE#########################
########################################################################################################
#Create All_mortality Data Frame
hr <- df$`AC- HR`
cl_1 <- df$`AC-CL`
ch_1 <- df$`AC-CH`
study <- df$Author
cohort <- df$Cohort
region <- df$Area
mean_pm2.5 <- df$`Mean PM2.5`
mean_age <- df$`Mean Age`
smoking <- df$`Individual Smoking`
all_mort <- data.frame(study, cohort, hr, cl_1,ch_1, region,smoking)
all_mort <- all_mort[complete.cases(all_mort), ]

all_mort$coef_1 <- log(all_mort$hr)
all_mort$se_lower <- (log(all_mort$hr) - log(all_mort$cl_1))/1.96
all_mort$se_upper <- (log(all_mort$ch_1) - log(all_mort$hr))/1.96
all_mort$se <- (all_mort$se_lower + all_mort$se_upper)/2

#now this dataframe has a coefficient and SE that works and is symetrical

#Create Cardio_mort data Frame
hr <- df$`Cardio - HR`
cl_1 <- df$`Cardio - CL`
ch_1 <- df$`Cardio - CH`
study <- df$Author
cohort <- df$Cohort
region <- df$Area
mean_pm2.5 <- df$`Mean PM2.5`
mean_age <- df$`Mean Age`
smoking <- df$`Individual Smoking`
cardio_mort <- data.frame(study, cohort, hr, cl_1,ch_1,region,smoking)
cardio_mort <- cardio_mort[complete.cases(cardio_mort), ]

cardio_mort$coef_1 <- log(cardio_mort$hr)
cardio_mort$se_lower <- (log(cardio_mort$hr) - log(cardio_mort$cl_1))/1.96
cardio_mort$se_upper <- (log(cardio_mort$ch_1) - log(cardio_mort$hr))/1.96
cardio_mort$se <- (cardio_mort$se_lower + cardio_mort$se_upper)/2

#Create resp_mort data Frame
hr <- df$`Resp- HR`
cl_1 <- df$`Resp- CL`
ch_1 <- df$`Resp- CH`
study <- df$Author
cohort <- df$Cohort
region <- df$Area
smoking <- df$`Individual Smoking`
mean_pm2.5 <- df$`Mean PM2.5`
mean_age <- df$`Mean Age`
resp_mort <- data.frame(study, cohort, hr, cl_1,ch_1,region,smoking)
resp_mort <- resp_mort[complete.cases(resp_mort), ]

resp_mort$coef_1 <- log(resp_mort$hr)
resp_mort$se_lower <- (log(resp_mort$hr) - log(resp_mort$cl_1))/1.96
resp_mort$se_upper <- (log(resp_mort$ch_1) - log(resp_mort$hr))/1.96
resp_mort$se <- (resp_mort$se_lower + resp_mort$se_upper)/2

#Create lung_mort data Frame
hr <- df$`Lung- HR`
cl_1 <- df$`Lung - CL`
ch_1 <- df$`Lung- CH`
study <- df$Author
cohort <- df$Cohort
region <- df$Area
smoking <- df$`Individual Smoking`
mean_pm2.5 <- df$`Mean PM2.5`
mean_age <- df$`Mean Age`
lung_mort <- data.frame(study, cohort, hr, cl_1,ch_1,region,smoking)
lung_mort <- lung_mort[complete.cases(lung_mort), ]

lung_mort$coef_1 <- log(lung_mort$hr)
lung_mort$se_lower <- (log(lung_mort$hr) - log(lung_mort$cl_1))/1.96
lung_mort$se_upper <- (log(lung_mort$ch_1) - log(lung_mort$hr))/1.96
lung_mort$se <- (lung_mort$se_lower + lung_mort$se_upper)/2


########################################################################################################
##################################Overall Random and Fixed Effect###################################### 
########################################################################################################      
library(metafor)


###ALL MORT###
#Fixed effects for all_mort
all_mort_fixed <- rma(yi = coef_1, sei = se,  data = all_mort, method = "FE")
#random effects (1) for all_mort
all_mort_random <- rma(yi = coef_1, sei = se, slab = study, data = all_mort)


#Export the coef, se, and I2
all_mort_fixed_results <- c(all_mort_fixed$b,all_mort_fixed$se,all_mort_fixed$I2)
all_mort_random_results <- c(all_mort_random$b,all_mort_random$se,all_mort_random$I2)

results <- data.frame(c("coef","se", "I2"), all_mort_fixed_results,all_mort_random_results)

###Cardio_Mort###
#Fixed effects 
cardio_mort_fixed <- rma(yi = coef_1, sei = se,  data = cardio_mort, method = "FE")
#random effects (1) 
cardio_mort_random <- rma(yi = coef_1, sei = se, slab = study, data = cardio_mort)


#Export the coef, se, and I2
cardio_mort_fixed_results <- c(cardio_mort_fixed$b,cardio_mort_fixed$se,cardio_mort_fixed$I2)
cardio_mort_random_results <- c(cardio_mort_random$b,cardio_mort_random$se,cardio_mort_random$I2)

results$cardio_mort_fixed_results <- cardio_mort_fixed_results
results$cardio_mort_random_results <- cardio_mort_random_results

###Resp_Mort###
#Fixed effects 
resp_mort_fixed <- rma(yi = coef_1, sei = se,  data = resp_mort, method = "FE")
#random effects (1) 
resp_mort_random <- rma(yi = coef_1, sei = se, slab = study, data = resp_mort)


#Export the coef, se, and I2
resp_mort_fixed_results <- c(resp_mort_fixed$b,resp_mort_fixed$se,resp_mort_fixed$I2)
resp_mort_random_results <- c(resp_mort_random$b,resp_mort_random$se,resp_mort_random$I2)

results$resp_mort_fixed_results <- resp_mort_fixed_results
results$resp_mort_random_results <- resp_mort_random_results

###Lung_Mort###
#Fixed effects 
lung_mort_fixed <- rma(yi = coef_1, sei = se,  data = lung_mort, method = "FE")
#random effects (1) 
lung_mort_random <- rma(yi = coef_1, sei = se, slab = study, data = lung_mort)


#Export the coef, se, and I2
lung_mort_fixed_results <- c(lung_mort_fixed$b,lung_mort_fixed$se,lung_mort_fixed$I2)
lung_mort_random_results <- c(lung_mort_random$b,lung_mort_random$se,lung_mort_random$I2)

results$lung_mort_fixed_results <- lung_mort_fixed_results
results$lung_mort_random_results <- lung_mort_random_results

#######################################################################################################
##########################################Regional Models##############################################
#######################################################################################################
#Subset each of the type of deaths into region (US, Canada, Europe, ASIA); obtain fixed and random effects

###North America
US_all_mort_fixed <- rma(yi = coef_1, sei = se,  data = all_mort[all_mort$region == "US",], method = "FE")
US_all_mort_random <- rma(yi = coef_1, sei = se,  data = all_mort[all_mort$region == "US",])
US_cardio_mort_fixed <- rma(yi = coef_1, sei = se,  data = cardio_mort[cardio_mort$region == "US",], method = "FE")
US_cardio_mort_random <- rma(yi = coef_1, sei = se,  data = cardio_mort[cardio_mort$region == "US",])
US_resp_mort_fixed <- rma(yi = coef_1, sei = se,  data = resp_mort[resp_mort$region == "US",], method = "FE")
US_resp_mort_random <- rma(yi = coef_1, sei = se,  data = resp_mort[resp_mort$region == "US",])
US_lung_mort_fixed <- rma(yi = coef_1, sei = se,  data = lung_mort[lung_mort$region == "US",], method = "FE")
US_lung_mort_random <- rma(yi = coef_1, sei = se,  data = lung_mort[lung_mort$region == "US",])

#Export to results
US_all_mort_fixed_results <- c(US_all_mort_fixed$b,US_all_mort_fixed$se,US_all_mort_fixed$I2)
US_all_mort_random_results <- c(US_all_mort_random$b,US_all_mort_random$se,US_all_mort_random$I2)
results$US_all_mort_fixed_results <- US_all_mort_fixed_results
results$US_all_mort_random_results <- US_all_mort_random_results
US_cardio_mort_fixed_results <- c(US_cardio_mort_fixed$b,US_cardio_mort_fixed$se,US_cardio_mort_fixed$I2)
US_cardio_mort_random_results <- c(US_cardio_mort_random$b,US_cardio_mort_random$se,US_cardio_mort_random$I2)
results$US_cardio_mort_fixed_results <- US_cardio_mort_fixed_results
results$US_cardio_mort_random_results <- US_cardio_mort_random_results
US_resp_mort_fixed_results <- c(US_resp_mort_fixed$b,US_resp_mort_fixed$se,US_resp_mort_fixed$I2)
US_resp_mort_random_results <- c(US_resp_mort_random$b,US_resp_mort_random$se,US_resp_mort_random$I2)
results$US_resp_mort_fixed_results <- US_resp_mort_fixed_results
results$US_resp_mort_random_results <- US_resp_mort_random_results
US_lung_mort_fixed_results <- c(US_lung_mort_fixed$b,US_lung_mort_fixed$se,US_lung_mort_fixed$I2)
US_lung_mort_random_results <- c(US_lung_mort_random$b,US_lung_mort_random$se,US_lung_mort_random$I2)
results$US_lung_mort_fixed_results <- US_lung_mort_fixed_results
results$US_lung_mort_random_results <- US_lung_mort_random_results

###Europe
EU_all_mort_fixed <- rma(yi = coef_1, sei = se,  data = all_mort[all_mort$region == "EUR",], method = "FE")
EU_all_mort_random <- rma(yi = coef_1, sei = se,  data = all_mort[all_mort$region == "EUR",])
EU_cardio_mort_fixed <- rma(yi = coef_1, sei = se,  data = cardio_mort[cardio_mort$region == "EUR",], method = "FE")
EU_cardio_mort_random <- rma(yi = coef_1, sei = se,  data = cardio_mort[cardio_mort$region == "EUR",])
EU_resp_mort_fixed <- rma(yi = coef_1, sei = se,  data = resp_mort[resp_mort$region == "EUR",], method = "FE")
EU_resp_mort_random <- rma(yi = coef_1, sei = se,  data = resp_mort[resp_mort$region == "EUR",])
EU_lung_mort_fixed <- rma(yi = coef_1, sei = se,  data = lung_mort[lung_mort$region == "EUR",], method = "FE")
EU_lung_mort_random <- rma(yi = coef_1, sei = se,  data = lung_mort[lung_mort$region == "EUR",])

#Export to results
EU_all_mort_fixed_results <- c(EU_all_mort_fixed$b,EU_all_mort_fixed$se,EU_all_mort_fixed$I2)
EU_all_mort_random_results <- c(EU_all_mort_random$b,EU_all_mort_random$se,EU_all_mort_random$I2)
results$EU_all_mort_fixed_results <- EU_all_mort_fixed_results
results$EU_all_mort_random_results <- EU_all_mort_random_results
EU_cardio_mort_fixed_results <- c(EU_cardio_mort_fixed$b,EU_cardio_mort_fixed$se,EU_cardio_mort_fixed$I2)
EU_cardio_mort_random_results <- c(EU_cardio_mort_random$b,EU_cardio_mort_random$se,EU_cardio_mort_random$I2)
results$EU_cardio_mort_fixed_results <- EU_cardio_mort_fixed_results
results$EU_cardio_mort_random_results <- EU_cardio_mort_random_results
EU_resp_mort_fixed_results <- c(EU_resp_mort_fixed$b,EU_resp_mort_fixed$se,EU_resp_mort_fixed$I2)
EU_resp_mort_random_results <- c(EU_resp_mort_random$b,EU_resp_mort_random$se,EU_resp_mort_random$I2)
results$EU_resp_mort_fixed_results <- EU_resp_mort_fixed_results
results$EU_resp_mort_random_results <- EU_resp_mort_random_results
EU_lung_mort_fixed_results <- c(EU_lung_mort_fixed$b,EU_lung_mort_fixed$se,EU_lung_mort_fixed$I2)
EU_lung_mort_random_results <- c(EU_lung_mort_random$b,EU_lung_mort_random$se,EU_lung_mort_random$I2)
results$EU_lung_mort_fixed_results <- EU_lung_mort_fixed_results
results$EU_lung_mort_random_results <- EU_lung_mort_random_results

###Asia
AS_all_mort_fixed <- rma(yi = coef_1, sei = se,  data = all_mort[all_mort$region == "ASIA",], method = "FE")
AS_all_mort_random <- rma(yi = coef_1, sei = se,  data = all_mort[all_mort$region == "ASIA",])
AS_cardio_mort_fixed <- rma(yi = coef_1, sei = se,  data = cardio_mort[cardio_mort$region == "ASIA",], method = "FE")
AS_cardio_mort_random <- rma(yi = coef_1, sei = se,  data = cardio_mort[cardio_mort$region == "ASIA",])
AS_resp_mort_fixed <- rma(yi = coef_1, sei = se,  data = resp_mort[resp_mort$region == "ASIA",], method = "FE")
AS_resp_mort_random <- rma(yi = coef_1, sei = se,  data = resp_mort[resp_mort$region == "ASIA",])
AS_lung_mort_fixed <- rma(yi = coef_1, sei = se,  data = lung_mort[lung_mort$region == "ASIA",], method = "FE")
AS_lung_mort_random <- rma(yi = coef_1, sei = se,  data = lung_mort[lung_mort$region == "ASIA",])

#Export to results
AS_all_mort_fixed_results <- c(AS_all_mort_fixed$b,AS_all_mort_fixed$se,AS_all_mort_fixed$I2)
AS_all_mort_random_results <- c(AS_all_mort_random$b,AS_all_mort_random$se,AS_all_mort_random$I2)
results$AS_all_mort_fixed_results <- AS_all_mort_fixed_results
results$AS_all_mort_random_results <- AS_all_mort_random_results
AS_cardio_mort_fixed_results <- c(AS_cardio_mort_fixed$b,AS_cardio_mort_fixed$se,AS_cardio_mort_fixed$I2)
AS_cardio_mort_random_results <- c(AS_cardio_mort_random$b,AS_cardio_mort_random$se,AS_cardio_mort_random$I2)
results$AS_cardio_mort_fixed_results <- AS_cardio_mort_fixed_results
results$AS_cardio_mort_random_results <- AS_cardio_mort_random_results
AS_resp_mort_fixed_results <- c(AS_resp_mort_fixed$b,AS_resp_mort_fixed$se,AS_resp_mort_fixed$I2)
AS_resp_mort_random_results <- c(AS_resp_mort_random$b,AS_resp_mort_random$se,AS_resp_mort_random$I2)
results$AS_resp_mort_fixed_results <- AS_resp_mort_fixed_results
results$AS_resp_mort_random_results <- AS_resp_mort_random_results
AS_lung_mort_fixed_results <- c(AS_lung_mort_fixed$b,AS_lung_mort_fixed$se,AS_lung_mort_fixed$I2)
AS_lung_mort_random_results <- c(AS_lung_mort_random$b,AS_lung_mort_random$se,AS_lung_mort_random$I2)
results$AS_lung_mort_fixed_results <- AS_lung_mort_fixed_results
results$AS_lung_mort_random_results <- AS_lung_mort_random_results


########################################################################################################
###########################################Smoking Control Models#######################################
########################################################################################################

Smoking_all_mort_fixed <- rma(yi = coef_1, sei = se,  data = all_mort[all_mort$smoking == 1,], method = "FE")
Smoking_all_mort_random <- rma(yi = coef_1, sei = se,  data = all_mort[all_mort$smoking == 1,])
Smoking_cardio_mort_fixed <- rma(yi = coef_1, sei = se,  data = cardio_mort[cardio_mort$smoking == 1,], method = "FE")
Smoking_cardio_mort_random <- rma(yi = coef_1, sei = se,  data = cardio_mort[cardio_mort$smoking == 1,])
Smoking_resp_mort_fixed <- rma(yi = coef_1, sei = se,  data = resp_mort[resp_mort$smoking == 1,], method = "FE")
Smoking_resp_mort_random <- rma(yi = coef_1, sei = se,  data = resp_mort[resp_mort$smoking == 1,])
Smoking_lung_mort_fixed <- rma(yi = coef_1, sei = se,  data = lung_mort[lung_mort$smoking == 1,], method = "FE")
Smoking_lung_mort_random <- rma(yi = coef_1, sei = se,  data = lung_mort[lung_mort$smoking == 1,])

#Export the results
Smoking_all_mort_fixed_results <- c(Smoking_all_mort_fixed$b,Smoking_all_mort_fixed$se,Smoking_all_mort_fixed$I2)
Smoking_all_mort_random_results <- c(Smoking_all_mort_random$b,Smoking_all_mort_random$se,Smoking_all_mort_random$I2)
Smoking_cardio_mort_fixed_results <- c(Smoking_cardio_mort_fixed$b,Smoking_cardio_mort_fixed$se,Smoking_cardio_mort_fixed$I2)
Smoking_cardio_mort_random_results <- c(Smoking_cardio_mort_random$b,Smoking_cardio_mort_random$se,Smoking_cardio_mort_random$I2)
Smoking_resp_mort_fixed_results <- c(Smoking_resp_mort_fixed$b,Smoking_resp_mort_fixed$se,Smoking_resp_mort_fixed$I2)
Smoking_resp_mort_random_results <- c(Smoking_resp_mort_random$b,Smoking_resp_mort_random$se,Smoking_resp_mort_random$I2)
Smoking_lung_mort_fixed_results <- c(Smoking_lung_mort_fixed$b,Smoking_lung_mort_fixed$se,Smoking_lung_mort_fixed$I2)
Smoking_lung_mort_random_results <- c(Smoking_lung_mort_random$b,Smoking_lung_mort_random$se,Smoking_lung_mort_random$I2)


results$smoking_all_mort_fixed_results <- Smoking_all_mort_random_results
results$smoking_all_mort_random_results <- Smoking_all_mort_random_results
results$smoking_cardio_mort_fixed_results <- Smoking_cardio_mort_fixed_results
results$smoking_cardio_mort_random_results <- Smoking_cardio_mort_random_results
results$smoking_resp_mort_fixed_results <- Smoking_resp_mort_fixed_results
results$smoking_resp_mort_random_results <- Smoking_resp_mort_random_results
results$smoking_lung_mort_fixed_results <- Smoking_lung_mort_fixed_results
results$smoking_lung_mort_random_results <- Smoking_lung_mort_random_results

########################################################################################################
###################################Export results and study wide estimates##############################
########################################################################################################
export_df <- data.frame(df$Author, df$Cohort, df$Area, df$`Mean PM2.5`, df$`Mean Age`, df$`Direct PM2.5 Exposure`, df$`Individual Smoking`)
export_df$all_mort_coef <- NA
export_df$all_mort_se <- NA
export_df$cardio_mort_coef <- NA
export_df$cardio_mort_se <- NA
export_df$resp_mort_coef <- NA
export_df$resp_mort_se <- NA
export_df$lung_mort_coef <- NA
export_df$lung_mort_se <- NA


#merge all_cause mort
for(i in 1:length(export_df$df.Author)){
  for(j in 1:length(all_mort$study)){
    if (export_df[i,1] == all_mort[j,1]){
      export_df[i,8] <- all_mort[j,8]
      export_df[i,9] <- all_mort[j,11]
    } 
  }
}

#merge cardio_mort
for(i in 1:length(df$Author)){
  for(j in 1:length(cardio_mort$study)){
    if (export_df[i,1] == cardio_mort[j,1]){
      export_df[i,10] <- cardio_mort[j,8]
      export_df[i,11] <- cardio_mort[j,11]
    } 
  }
}

#merge resp_mort
for(i in 1:length(df$Author)){
  for(j in 1:length(resp_mort$study)){
    if (export_df[i,1] == resp_mort[j,1]){
      export_df[i,12] <- resp_mort[j,8]
      export_df[i,13] <- resp_mort[j,11]
    } 
  }
}

#merge lung_mort
for(i in 1:length(df$Author)){
  for(j in 1:length(lung_mort$study)){
    if (export_df[i,1] == lung_mort[j,1]){
      export_df[i,14] <- lung_mort[j,8]
      export_df[i,15] <- lung_mort[j,11]
    } 
  }
}

###Export another dataframe with only hr and confidence interval instead of coef and SE
export_hr_df <- data.frame(export_df$df.Author, export_df$df.Cohort, export_df$df.Area, export_df$df..Mean.PM2.5., export_df$df..Mean.Age., export_df$df..Direct.PM2.5.Exposure., export_df$df..Individual.Smoking.)
export_hr_df$all_mort_hr <- exp(export_df$all_mort_coef)
export_hr_df$all_mort_LC <- exp(export_df$all_mort_coef - (1.96*export_df$all_mort_se))
export_hr_df$all_mort_HC <- exp(export_df$all_mort_coef + (1.96*export_df$all_mort_se))
export_hr_df$cardio_mort_hr <- exp(export_df$all_mort_coef)
export_hr_df$cardio_mort_LC <- exp(export_df$cardio_mort_coef - (1.96*export_df$cardio_mort_se))
export_hr_df$cardio_mort_HC <- exp(export_df$cardio_mort_coef + (1.96*export_df$cardio_mort_se))
export_hr_df$resp_mort_hr <- exp(export_df$resp_mort_coef)
export_hr_df$resp_mort_LC <- exp(export_df$resp_mort_coef - (1.96*export_df$resp_mort_se))
export_hr_df$resp_mort_HC <- exp(export_df$resp_mort_coef + (1.96*export_df$resp_mort_se))
export_hr_df$lung_mort_hr <- exp(export_df$lung_mort_coef)
export_hr_df$lung_mort_LC <- exp(export_df$lung_mort_coef - (1.96*export_df$lung_mort_se))
export_hr_df$lung_mort_HC <- exp(export_df$lung_mort_coef + (1.96*export_df$lung_mort_se))

###Export the csv of both the meta analysis results and the datasets with only coef and se
write.csv(results,'results.csv')
write.csv(export_df,'studies.csv')
write.csv(export_hr_df, 'Hazard_Ratios.csv')


