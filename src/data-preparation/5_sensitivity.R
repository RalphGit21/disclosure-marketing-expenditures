# Store the csv file stored from previous script (4_analysis.R)
df3 <- read.csv2("../../gen/output/dataset_end_4.csv")


############# Sensitivity (1/5): Alternative disclosure proxies #############

# More lenient method of defining IV (voluntary disclosure of marketing expenditures) - at least 1 component.
df3$dum_new <- ifelse(df3$adv_exp != "ND" | df3$com_exp != "ND" | df3$mkt_exp != "ND", 1, 0)

# Create a basic model (excl. year dummies and moderating effects). (Model 2.1.1 in Appendix A.2.1)
model_sens_1_basic <- lm(Q ~ dum_new + firm_size + leverage + revt_change + lag_Q, data = df3)
summary(model_sens_1_basic)

# Create the full model (Model 2.1.2 in Appendix A.2.1)
model_sens_1_full <- lm(Q ~ dum_new * quality_cent + B2B + dum_new:B2B  + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_1_full)


#############  Sensitivity (2/5): Varying disclosure quality constructs ############# 

# Scoring system changes: all components score equally
df3$com_q_new <- ifelse(df3$Commission == "ND", 0, ifelse(df3$Commission == "Low", 2, 4))

# Compute new disclosure quality score (sum of the components) and center at 8
df3 <- df3 %>% mutate(dq_quality_new_sum = adv_q + com_q_new + mkt_q)
df3$quality_cent_new_com <- df3$dq_quality_new_sum - 8

# Add this new measure of disclosure quality in the model (Model 2.2.1 in Appendix A.2.2)
model_sens_2equal <- lm(Q ~ dum * quality_cent_new_com + dum * B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_2equal)

# Compute new disclosure quality score based on the number of components that have been disclosed (ranging from 0 to 3) and create a new model (Model 2.2.2 in Appendix A.2.2)
df3 <- df3 %>% mutate(quality_sens2 = adv + com + mkt)
model_sens_2sum <- lm(Q ~ dum * quality_sens2 + dum * B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_2sum)

# Compute VIF values for this second measure
model_test <- lm(Q ~ dum + quality_sens2 +  B2B + firm_size + leverage + revt_change + lag_Q, data = df3)
vif(model_test)


#############  Sensitivity (3/5): Control variables ############# 

# Compute the new control variables (4 in total)
df3 <- df3 %>% mutate(firm_size2 = log(emp),
                      firm_size3 = log(prcc_f * csho),
                      leverage2 = dltt / (prcc_f * csho),
                      liquidity = act/lct)

# M3 including new firm size measure (log(emp)) --> Model 2.3.1 in Appendix A.2.3
model_sens_3_size2 <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + firm_size2 + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_size2)

# M3 including new firm size measure (log(prcc_f*csho)) --> Model 2.3.2 in Appendix A.2.3
model_sens_3_size3 <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + firm_size3 + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_size3)

# M3 including new financial leverage measure (dltt / (prcc_f * csho)) --> Model 2.3.3 in Appendix A.2.3
model_sens_3_lev2 <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + firm_size + leverage2 + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_lev2)

# M3 including liquidity as additional control variable (act/lct) --> Model 2.3.4 in Appendix A.2.3
model_sens_3_liq <- lm(Q ~  dum * quality_cent + B2B + dum:B2B + firm_size + leverage + liquidity + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_liq)


#############  Sensitivity (4/5): Industry fixed effects ############# 

# Add a new column and classify each company in their respective industry
df3 <- df3 %>% 
  mutate(industry = case_when(
    conm %in% c("TESLA INC") ~ 1,
    conm %in% c("AMAZON.COM INC", "EBAY INC", "WAYFAIR INC", "PC CONNECTION INC")  ~ 2,
    conm %in% c("SALESFORCE.COM INC", "CA INC", "ELECTRONIC ARTS INC", "ACTIVISION BLIZZARD INC", "SS&C TECHNOLOGIES HLDGS INC", "BLOCK INC") ~ 3,
    conm %in% c("DST SYSTEMS INC") ~ 4,
    conm %in% c("MARSH & MCLENNAN COS") ~ 5,
    TRUE ~ 6 ) )

# Add industry-fixed effects (Model 2.4.1 in Appendix A.2.4)
model_sens_4_fixedindustry <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear) + as.factor(industry), data = df3)
summary(model_sens_4_fixedindustry)


#############  Sensitivity (5/5): Omitted variables bias ############# 

# Replace commas with dots across all necessary  columns
df3$adv_exp_new <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$adv_exp)))
df3$com_exp_new <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$com_exp)))
df3$mkt_exp_new <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$mkt_exp)))

# Compute the expense ratios for each component
df3 <- df3 %>% mutate(adv_scaled = as.numeric(adv_exp_new) / xsga,
                      com_scaled = as.numeric(com_exp_new) / xsga,
                      mkt_scaled = as.numeric(mkt_exp_new) / xsga)

# Set all NAs equal to 0
df3$adv_scaled[is.na(df3$adv_scaled)] <- 0
df3$com_scaled[is.na(df3$com_scaled)] <- 0
df3$mkt_scaled[is.na(df3$mkt_scaled)] <- 0

# Compute the expense ratio, which is the sum of the component ratios
df3 <- df3 %>% mutate(total_scaled = adv_scaled + com_scaled + mkt_scaled)

# Include the expense ratio as an additional control variable (Model 2.5.1 in Appendix A.2.5)
model_sens_5_adv_exp <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + total_scaled + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_5_adv_exp)


# Store the final dataset
write.csv2(df3, "../../gen/output/full_dataset.csv", row.names = FALSE)

