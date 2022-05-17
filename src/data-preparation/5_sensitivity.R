df3 <- read.csv2("../../gen/output/final_dataset_new_inclyear3.csv") # In Excel, some lag_Q values are set to NA if gaps exist within companies (e.g., salesforce 2012, 2013, 2014 is missing, is lag_Q of 2015 should NOT be Q from 2011)
df3 <- df3 %>% drop_na(lag_Q)


# Sensitivity (1/5): Look at separate components as disclosure (resulting in 3 new models)
model_sens_1adv <- lm(log(Q) ~ Advertising * dq_quality + Advertising * B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_1adv)

model_sens_1com <- lm(log(Q) ~ Commission * dq_quality + Commission * B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_1com)

model_sens_1mkt <- lm(log(Q) ~ Marketing_expenses * dq_quality + Marketing_expenses * B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_1mkt)

# Sensitivity (2/5): Scoring system changes: all components score equally AND additional score for more disclosures
df3 <- df3 %>% 
  mutate(dq_quality_new = case_when(
    adv_q == 2 & com_q == 0 & mkt_q == 2 ~ 5, # If disclosures on 2 components, DQ + 1.
    adv_q == 2 & com_q == 0 & mkt_q == 4 ~ 7,
    adv_q == 2 & com_q == 1 & mkt_q == 2 ~ 7, # If disclosures on all 3 components, DQ + 2.
    adv_q == 2 & com_q == 1 & mkt_q == 4 ~ 9,
    adv_q == 2 & com_q == 2 & mkt_q == 2 ~ 8,
    adv_q == 2 & com_q == 2 & mkt_q == 4 ~ 10,
    adv_q == 4 & com_q == 0 & mkt_q == 2 ~ 7,
    adv_q == 4 & com_q == 0 & mkt_q == 4 ~ 9,
    adv_q == 4 & com_q == 1 & mkt_q == 2 ~ 9,
    adv_q == 4 & com_q == 1 & mkt_q == 4 ~ 11,
    adv_q == 4 & com_q == 2 & mkt_q == 2 ~ 10,
    adv_q == 4 & com_q == 2 & mkt_q == 4 ~ 12,
    adv_q == 2 & com_q == 0 & mkt_q == 0 ~ 2,
    adv_q == 4 & com_q == 0 & mkt_q == 0 ~ 4,
    adv_q == 0 & com_q == 1 & mkt_q == 0 ~ 1,
    adv_q == 0 & com_q == 2 & mkt_q == 0 ~ 2,
    adv_q == 0 & com_q == 0 & mkt_q == 2 ~ 2,
    adv_q == 0 & com_q == 0 & mkt_q == 4 ~ 4,
    adv_q == 2 & com_q == 1 & mkt_q == 0 ~ 4,
    adv_q == 2 & com_q == 2 & mkt_q == 0 ~ 5,
    adv_q == 4 & com_q == 1 & mkt_q == 0 ~ 6,
    adv_q == 4 & com_q == 2 & mkt_q == 0 ~ 7,
    adv_q == 0 & com_q == 1 & mkt_q == 2 ~ 4,
    adv_q == 0 & com_q == 2 & mkt_q == 2 ~ 5,
    adv_q == 0 & com_q == 1 & mkt_q == 4 ~ 6,
    adv_q == 0 & com_q == 2 & mkt_q == 4 ~ 7,
    TRUE ~ 0 ) )

model_sens_2syn <- lm(log(Q) ~ dum * dq_quality_new + dum * B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_2syn)


df3$com_q_new <- ifelse(df3$Commission == "ND", 0, ifelse(df3$Commission == "Low", 2, 4))
df3 <- df3 %>% mutate(dq_quality_new2 = adv_q + com_q_new + mkt_q)

model_sens_2equal <- lm(log(Q) ~ dum * dq_quality_new2 + dum * B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_2equal)


# Sensitivity (3/5): Firm size log(employees) & market value of equity, add leverage and liquidity

df3 <- df3 %>% mutate(firm_size2 = log(emp),
                      firm_size3 = log(prcc_f * csho),
                      leverage2 = dltt / (prcc_f * csho),
                      liquidity = act/lct)

model_sens_3_size2 <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size2 + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_size2)

model_sens_3_size3 <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size3 + leverage + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_size3)

# Firm size3 is best, so continue with that and add leverage2 and liquidity

model_sens_3_lev2 <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size3 + leverage2 + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_3_lev2)

model_sens_3_liq <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size3 + leverage2 + liquidity + revt_change + lag_Q + as.factor(fyear), data = df3)
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

model_sens_4_fixedconm <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size3 + leverage2 + revt_change + lag_Q + as.factor(fyear) + as.factor(conm), data = df3)
summary(model_sens_4_fixedconm)


model_sens_4_fixedindustry <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size3 + leverage2 + revt_change + lag_Q + as.factor(fyear) + as.factor(industry), data = df3)
summary(model_sens_4_fixedindustry)

# Sensitivity (5/5): Control for the exact expenses, scaled by SG&A and include as control variable

df3$adv_exp_new <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$adv_exp)))
df3$com_exp_new <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$com_exp)))
df3$mkt_exp_new <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$mkt_exp)))

df3 <- df3 %>% mutate(adv_scaled = as.numeric(adv_exp_new) / xsga,
                      com_scaled = as.numeric(com_exp_new) / xsga,
                      mkt_scaled = as.numeric(mkt_exp_new) / xsga)

model_sens_5_adv_exp <- lm(log(Q) ~ adv_scaled + firm_size3 + leverage2 + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_5_adv_exp)

model_sens_5_mkt_exp <- lm(log(Q) ~ mkt_scaled + firm_size3 + leverage2 + revt_change + lag_Q + as.factor(fyear), data = df3)
summary(model_sens_5_mkt_exp)
