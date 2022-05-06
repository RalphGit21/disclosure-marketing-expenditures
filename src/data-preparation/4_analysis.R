df2 <- read.csv2("../../gen/output/final_dataset2.csv") # In Excel, some lag_Q values are set to NA if gaps exist within companies (e.g., salesforce 2012, 2013, 2014 is missing, is lag_Q of 2015 should NOT be Q from 2011)

df_filt <- df2 %>% select(conm, fyear, at, Q, lag_Q, dum, B2B, dq_quality, interest, CAGR, firm_size)




t <- lm(lag_Q ~ dum * dq_quality + dum * B2B, data = df_filt)
summary(t)

library(fixest)
model_1 <- feols(lag_Q ~ dum * dq_quality + dum * B2B,
                 data = df_filt,
                 cluster = ~ conm)

summary(model_1)


t2 <- lm(lag_Q ~ dum + dq_quality + B2B + interest + CAGR + firm_size, data = df_filt)
summary(t2)



# CORRELATION MATRIX
df_filt <- df2 %>% select(lag_Q, dum, B2B, dq_quality, interest, CAGR, firm_size)

install.packages("rstatix")
library(rstatix)

cor.mat <- df_filt %>% cor_mat(); cor.mat
cor.mat %>% cor_get_pval()

sapply(df_filt, mean, na.rm =TRUE)
sapply(df_filt, sd, na.rm =TRUE)
sapply(df_filt, min, na.rm =TRUE)
sapply(df_filt, max, na.rm =TRUE)
