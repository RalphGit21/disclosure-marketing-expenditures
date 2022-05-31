# Load the dataset. In Excel, some lag_Q values are set to NA if gaps exist within companies (e.g., salesforce 2012, 2013, 2014 is missing, is lag_Q of 2015 should NOT be Q from 2011)
df2 <- read.csv2("../../gen/output/final_dataset_all_processed.csv")

# Remove the rows where the lagged Q is NA.
library(tidyr)
df2 <- df2 %>% drop_na(lag_Q)

df_filt <- df2 %>% select(conm, fyear, at, Q, lag_Q, dum, B2B, dq_quality, firm_size, leverage, revt_change, lag_Q)

## Compute VIF values for main model
library(car)
model_vif_old <- lm(Q ~ dum + dq_quality + B2B + firm_size + leverage + revt_change + lag_Q, data = df2)
vif(model_vif_old)

# Winsorize Q values
pct_Q <- quantile(df2$Q, c(.025, 0.975), na.rm = TRUE) 
df2$Q <- ifelse(df2$Q > pct_Q[2], pct_Q[2], df2$Q)
df2$Q <- ifelse(df2$Q < pct_Q[1], pct_Q[1], df2$Q)

# Center disclosure quality
df2$quality_cent <- df2$dq_quality - 6 

# Fitted vs residuals and 
plot(lm(df2$Q ~ df2$dum))


# Model M1, excluding moderators and year dummies
model_1 <- lm(Q ~ dum + firm_size + leverage + revt_change + lag_Q, data = df2)
summary(model_1)

# Model M2, including moderators but excluding year dummies
model_2 <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q, data = df2)
summary(model_2)

# Model M3, including moderators and year dummies (Full model)
model_3 <- lm(Q ~ dum * quality_cent + B2B + dum:B2B + firm_size + leverage + revt_change + lag_Q + as.factor(fyear), data = df2)
summary(model_3)


# CORRELATION MATRIX
df_filt2 <- df2 %>% select(Q, dum, B2B, quality_cent, firm_size, leverage, revt_change, lag_Q)

# Compute correlation matrix using rstatix library. Moreover, compute the p-values to see which correlations are significant
library(rstatix)
cor.mat <- df_filt2 %>% cor_mat(); cor.mat
cor.mat %>% cor_get_pval()

# Compute the mean, sd, min and max values for each of the included variable.
sapply(df_filt2, mean, na.rm =TRUE)
sapply(df_filt2, sd, na.rm =TRUE)
sapply(df_filt2, min, na.rm =TRUE)
sapply(df_filt2, max, na.rm =TRUE)

# Save the dataset
write.csv2(df2, "../../gen/output/dataset_end_4.csv", row.names = FALSE)


## TEST AVERAGES
df3 <- df2 %>% filter(adv_exp > 0)
lapply(df3$adv_exp, integer)
df3$adv_exp <- as.numeric(gsub(",", ".", gsub("\\.", "", df3$adv_exp)))
summary(df3$adv_exp)

df4 <- df2 %>% filter(mkt_exp != "ND")
df4$mkt_exp <- as.numeric(gsub(",", ".", gsub("\\.", "", df4$mkt_exp)))
summary(df4$mkt_exp)
