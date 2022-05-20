df2 <- read.csv2("../../gen/output/final_dataset_all_processed.csv") # In Excel, some lag_Q values are set to NA if gaps exist within companies (e.g., salesforce 2012, 2013, 2014 is missing, is lag_Q of 2015 should NOT be Q from 2011)

library(tidyr)
df2 <- df2 %>% drop_na(lag_Q)

df_filt <- df2 %>% select(conm, fyear, at, Q, lag_Q, dum, B2B, dq_quality_new, firm_size, leverage, revt_change, lag_Q)

## VIF VALUES
library(car)
model_vif_old <- lm(Q ~ dum + dq_quality + B2B + firm_size + leverage + revt_change + lag_Q, data = df2)
vif(model_vif_old)

# Transform DQ variable:
df2$quality1 <- ifelse(df2$dum == 1, df2$dq_quality - 6, 0)
df2$quality2 <- ifelse(df2$dum == 0, df2$dq_quality, 0)

df2 <- df2 %>% mutate(dq_quality_new = quality1 + quality2)

model_vif_new <- lm(Q ~ dum + dq_quality_new + B2B + firm_size + leverage + revt_change + lag_Q, data = df2)
vif(model_vif_new)


#Fitted vs residuals and 
plot(lm(df_filt$Q ~ df_filt$dum))
df_filt <- df_filt %>% mutate(log_Q = log(Q))

temp <- df_filt %>% mutate(extra = log(Q))
plot(lm(temp$extra ~ temp$dum))



model_1 <- lm(log(Q) ~ dum + firm_size + leverage + revt_change + lag_Q, data = df_filt)
summary(model_1)

model_2 <- lm(log(Q) ~ dum * dq_quality_new + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q, data = df_filt)
summary(model_2)

model_3 <- lm(log(Q) ~ dum * dq_quality_new + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df_filt)
summary(model_3)




# CORRELATION MATRIX
df_filt2 <- df2 %>% select(Q, dum, B2B, dq_quality_new, firm_size, leverage, revt_change, lag_Q)

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
