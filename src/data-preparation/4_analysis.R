df2 <- read.csv2("../../gen/output/final_dataset_new_inclyear3.csv") # In Excel, some lag_Q values are set to NA if gaps exist within companies (e.g., salesforce 2012, 2013, 2014 is missing, is lag_Q of 2015 should NOT be Q from 2011)
library(tidyr)
df2 <- df2 %>% drop_na(lag_Q)

df_filt <- df2 %>% select(conm, fyear, at, Q, lag_Q, dum, B2B, dq_quality, firm_size, leverage, revt_change, t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, lag_Q)

model_simple <- lm(Q ~ dum * dq_quality + B2B + dum:B2B, data = df_filt)
summary(model_simple)


model_simple_control <- lm(Q ~ dum * dq_quality + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q, data = df_filt)
summary(model_simple_control)

model_simple_full <- lm(Q ~ dum * dq_quality + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df_filt)
summary(model_simple_full)


model_log_simple <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q, data = df_filt)
summary(model_log_simple)


model_log_control <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q, data = df_filt)
summary(model_log_control)


model_log_full <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df_filt)
summary(model_log_full)



model_1 <- lm(log(Q) ~ dum + firm_size + leverage + revt_change + lag_Q, data = df_filt)
summary(model_1)

model_2 <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q, data = df_filt)
summary(model_2)

model_3 <- lm(log(Q) ~ dum * dq_quality + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df_filt)
summary(model_3)



model_vif <- lm(Q ~ dum * dq_quality + B2B + firm_size + leverage + revt_change + lag_Q, data = df_filt)
summary(model_3)

vif(model_vif)

#Fitted vs residuals and 
plot(lm(df_filt$Q ~ df_filt$dum))
df_filt <- df_filt %>% mutate(log_Q = log(Q))

library(fixest)
library(modelsummary)

model_1 <- feols(Q ~ dum * dq_quality + B2B + dum:B2B + firm_size + lag_Q + leverage + revt_change
                 |
                 fyear, # Fixed effects (as fyear changes at constant rate over time)
                 data = df_filt) # Cluster for each company

summary(model_1)


#install.packages("sampleSelection")
library(sampleSelection)
model_3 <- selection(lag_Q ~ dq_quality + dum * B2B + CAGR + firm_size)





temp <- df_filt %>% mutate(extra = log(Q))
plot(lm(temp$extra ~ temp$dum))









# CORRELATION MATRIX
df_filt2 <- df_filt %>% select(Q, dum, B2B, dq_quality, firm_size, leverage, revt_change, lag_Q)

install.packages("rstatix")
library(rstatix)

cor.mat <- df_filt2 %>% cor_mat(); cor.mat
cor.mat %>% cor_get_pval()

sapply(df_filt2, mean, na.rm =TRUE)
sapply(df_filt2, sd, na.rm =TRUE)
sapply(df_filt2, min, na.rm =TRUE)
sapply(df_filt2, max, na.rm =TRUE)

## TEST AVERAGES
df3 <- df2 %>% filter(adv_exp > 0)
lapply(df3$adv_exp, integer)
df3$adv_exp <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$adv_exp)))
summary(df3$adv_exp)

df4 <- df2 %>% filter(mkt_exp != "ND")
df4$mkt_exp <- as.numeric(gsub(",", ".", gsub("\\.", "", df4$mkt_exp)))
summary(df4$mkt_exp)
