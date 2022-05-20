# Import the transformed dataset (previously joined_data, renamed to final_dataset)
df <- read.csv2("../../gen/temp/processed_dataset.csv")
df <- df %>% mutate(Q = (at + csho * prcc_f - ceq - txdb) / at)
#df <- na.omit(df) #Last 5 rows are all NAs, 220 --> 215 observations

#Make dummies for each component
df$adv <- ifelse(df$adv_exp != "ND", 1, 0)
df$com <- ifelse(df$com_exp != "ND", 1, 0)
df$mkt <- ifelse(df$mkt_exp != "ND", 1, 0)

#Summarize the components to measure the IV (at least 2 of the components --> 1, 0 otherwise)
df$dum <- ifelse(df$adv_exp != "ND" & df$com_exp != "ND" |
                        df$adv_exp != "ND" & df$mkt_exp != "ND" |
                        df$com_exp != "ND" & df$mkt_exp != "ND" |
                        df$adv_exp != "ND" & df$com_exp != "ND" & df$mkt_exp != "ND", 1, 0)
 #Add ratings
df$adv_q <- ifelse(df$Advertising == "ND", 0, ifelse(df$Advertising == "Low", 2, 4))
df$com_q <- ifelse(df$Commission == "ND", 0, ifelse(df$Commission == "Low", 1, 2))
df$mkt_q <- ifelse(df$Marketing_expenses == "ND", 0, ifelse(df$Marketing_expenses == "Low", 2, 4))

df <- df %>% mutate(dq_quality = adv_q + com_q + mkt_q)


################# CONTROL VARIABLE 1: firm size

df <- df %>% mutate(firm_size = log(at))
################# CONTROL VARIABLE 2: financial leverage

df <- df %>% mutate(leverage = dltt/at)
quantile(df$leverage, c(.01, 0.99), na.rm = TRUE) 
df$leverage <- ifelse(df$leverage > 0.5397315, 0.5397315, df$leverage) #Setting high outliers to 99th percentile (3 outliers)


################# CONTROL VARIABLE 3: revenue growth
df <- df %>% group_by(conm) %>% mutate(revt_change = ((revt - lag(revt, n = 1, order_by = fyear)) / lag(revt, n = 1, order_by = fyear)) * 100)
quantile(df$revt_change, c(.01, 0.99), na.rm = TRUE) 
df$revt_change <- ifelse(df$revt_change < -29.62341, -29.62341, df$revt_change) #Setting low outliers to 1st percentile (2 outliers)
df$revt_change <- ifelse(df$revt_change > 197.91255, 197.91255, df$revt_change) #Setting high outliers to 99th percentile (2 outliers)

################# CONTROL VARIABLE 4: Lagged DV

df <- df %>% group_by(conm) %>% mutate(lag_Q = lag(Q, n = 1, order_by = fyear))

dir.create("../../gen/output")
write.csv2(df, "../../gen/output/final_dataset_all.csv", row.names = FALSE)

