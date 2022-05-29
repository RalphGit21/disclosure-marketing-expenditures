#df4 <- read.csv2("../../gen/output/final_dataset_new_inclyear3.csv") # In Excel, some lag_Q values are set to NA if gaps exist within companies (e.g., salesforce 2012, 2013, 2014 is missing, is lag_Q of 2015 should NOT be Q from 2011)
df3 <- read.csv2("../../gen/output/final_dataset_all_processed.csv")
df3 <- df3 %>% drop_na(lag_Q)

# Center disclosure quality
df3$quality_cent <- df3$dq_quality - 6 

# Windorize Q values
pct_Q <- quantile(df3$Q, c(.025, 0.975), na.rm = TRUE) 
df3$Q <- ifelse(df3$Q > pct_Q[2], pct_Q[2], df3$Q)
df3$Q <- ifelse(df3$Q < pct_Q[1], pct_Q[1], df3$Q)

# Sensitivity (1/5): Look at separate components as disclosure (resulting in 3 new models)

# More lenient method of defining IV (voluntary disclosure of marketing expenditures) - at least 1 component
df3$dum_new <- ifelse(df3$adv_exp != "ND" | df3$com_exp != "ND" | df3$mkt_exp != "ND", 1, 0)

model_sens_1_basic <- lm(Q ~ dum_new + firm_size + leverage + revt_change + lag_Q, data = df3)
summary(model_sens_1_basic)

model_sens_1_full <- lm(Q ~ dum_new * quality_cent + B2B + dum_new:B2B  + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_1_full)


# Sensitivity (2/5): Scoring system changes: all components score equally AND sum the numnber of components that are disclosed
df3$com_q_new <- ifelse(df3$Commission == "ND", 0, ifelse(df3$Commission == "Low", 2, 4))
df3 <- df3 %>% mutate(dq_quality_new_sum = adv_q + com_q_new + mkt_q)
df3$quality_cent_new_com <- df3$dq_quality_new_sum - 8 # Centering the moderator at 8

model_sens_2equal <- lm(Q ~ dum * quality_cent_new_com + dum * B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_2equal)


df3 <- df3 %>% mutate(quality_sens2 = adv + com + mkt)
model_sens_2sum <- lm(Q ~ dum * quality_sens2 + dum * B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_2sum)

model_test <- lm(Q ~ dum + quality_sens2 +  B2B + firm_size + leverage + revt_change + lag_Q, data = df3)
vif(model_test)

# Sensitivity (3/5): Firm size log(employees) & market value of equity, add leverage and liquidity

df3 <- df3 %>% mutate(firm_size2 = log(emp),
                      firm_size3 = log(prcc_f * csho),
                      leverage2 = dltt / (prcc_f * csho),
                      liquidity = act/lct)

model_sens_3_size2 <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + firm_size2 + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_size2)

model_sens_3_size3 <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + firm_size3 + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_size3)


model_sens_3_lev2 <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + firm_size + leverage2 + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_lev2)

model_sens_3_liq <- lm(Q ~  dum * quality_cent + B2B + dum:B2B + firm_size + leverage + liquidity + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_liq)

# Sensitivity (4/5): Firm-specific fixed effects and industry-specific fixed effects

df3 <- df3 %>% 
  mutate(industry = case_when(
    conm %in% c("TESLA INC") ~ 1,
    conm %in% c("AMAZON.COM INC", "EBAY INC", "WAYFAIR INC", "PC CONNECTION INC")  ~ 2,
    conm %in% c("SALESFORCE.COM INC", "CA INC", "ELECTRONIC ARTS INC", "ACTIVISION BLIZZARD INC", "SS&C TECHNOLOGIES HLDGS INC", "BLOCK INC") ~ 3,
    conm %in% c("DST SYSTEMS INC") ~ 4,
    conm %in% c("MARSH & MCLENNAN COS") ~ 5,
    TRUE ~ 6 ) )

model_sens_4_fixedindustry <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear) + as.factor(industry), data = df3)
summary(model_sens_4_fixedindustry)

# Sensitivity (5/5): Control for the exact expenses, scaled by SG&A and include as control variable

df3$adv_exp_new <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$adv_exp)))
df3$com_exp_new <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$com_exp)))
df3$mkt_exp_new <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$mkt_exp)))

df3 <- df3 %>% mutate(adv_scaled = as.numeric(adv_exp_new) / xsga,
                      com_scaled = as.numeric(com_exp_new) / xsga,
                      mkt_scaled = as.numeric(mkt_exp_new) / xsga)

df3$adv_scaled[is.na(df3$adv_scaled)] <- 0
df3$com_scaled[is.na(df3$com_scaled)] <- 0
df3$mkt_scaled[is.na(df3$mkt_scaled)] <- 0

df3 <- df3 %>% mutate(total_scaled = adv_scaled + com_scaled + mkt_scaled)

model_sens_5_adv_exp <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + total_scaled + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_5_adv_exp)

write.csv2(df3, "../../gen/output/full_dataset.csv", row.names = FALSE)

