df2 <- read.csv2("../../gen/output/final_dataset_new3.csv") # In Excel, some lag_Q values are set to NA if gaps exist within companies (e.g., salesforce 2012, 2013, 2014 is missing, is lag_Q of 2015 should NOT be Q from 2011)

df_filt <- df2 %>% select(conm, fyear, at, Q, lag_Q, dum, B2B, dq_quality, firm_size, leverage, revt_change, lag_Q)

t <- lm(Q ~ dum * dq_quality + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q, data = df_filt)
summary(t)

library(fixest)
library(tidyr)
library(modelsummary)

model_1 <- feols(Q ~ dum * dq_quality + B2B + dum:B2B + firm_size + lag_Q + leverage + revt_change
                 |
                 fyear, # Fixed effects (as fyear changes at constant rate over time)
                 data = df_filt,
                 cluster = ~ {conm}) # Cluster for each company

summary(model_1)


model_2 <- lm(Q ~ dum * dq_quality + dum * B2B + CAGR + firm_size + lag_Q, data = df_filt)
summary(model_2)

model_3 <- lm(lag_Q ~ dum * dq_quality + dum * B2B, data = df_filt)
summary(model_3)

#install.packages("sampleSelection")
library(sampleSelection)
model_3 <- selection(lag_Q ~ dq_quality + dum * B2B + CAGR + firm_size)

















# CORRELATION MATRIX
df_filt2 <- df2 %>% select(lag_Q, dum, B2B, dq_quality, firm_size, leverage, revt_change, lag_Q)

install.packages("rstatix")
library(rstatix)

cor.mat <- df_filt2 %>% cor_mat(); cor.mat
cor.mat %>% cor_get_pval()

sapply(df_filt, mean, na.rm =TRUE)
sapply(df_filt, sd, na.rm =TRUE)
sapply(df_filt, min, na.rm =TRUE)
sapply(df_filt, max, na.rm =TRUE)

## TEST AVERAGES
df3 <- df2 %>% filter(adv_exp > 0)
lapply(df3$adv_exp, integer)
df3$adv_exp <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$adv_exp)))
summary(df3$adv_exp)

df4 <- df2 %>% filter(mkt_exp != "ND")
df4$mkt_exp <- as.numeric(gsub(",", ".", gsub("\\.", "", df4$mkt_exp)))
summary(df4$mkt_exp)
