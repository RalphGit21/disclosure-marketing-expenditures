# Import the transformed dataset (previously joined_data, renamed to processed_dataset) after hand-coding and inspecting all of the 10-K reports.
df <- read.csv2("../../gen/temp/processed_dataset.csv")
df <- df %>% mutate(Q = (at + csho * prcc_f - ceq - txdb) / at)


# Make dummies for each component
df$adv <- ifelse(df$adv_exp != "ND", 1, 0)
df$com <- ifelse(df$com_exp != "ND", 1, 0)
df$mkt <- ifelse(df$mkt_exp != "ND", 1, 0)

# Summarize the components to measure the IV (at least 2 of the components --> 1, 0 otherwise)
df$dum <- ifelse(df$adv_exp != "ND" & df$com_exp != "ND" |
                 df$adv_exp != "ND" & df$mkt_exp != "ND" |
                 df$com_exp != "ND" & df$mkt_exp != "ND" |
                 df$adv_exp != "ND" & df$com_exp != "ND" & df$mkt_exp != "ND", 1, 0)

# Add ratings (ND = 0, Low = 1 for commission and 2 for advertising and marketing, High = 2 for commission and 4 for advertising and marketing)
df$adv_q <- ifelse(df$Advertising == "ND", 0, ifelse(df$Advertising == "Low", 2, 4))
df$com_q <- ifelse(df$Commission == "ND", 0, ifelse(df$Commission == "Low", 1, 2))
df$mkt_q <- ifelse(df$Marketing_expenses == "ND", 0, ifelse(df$Marketing_expenses == "Low", 2, 4))

# Compute disclosure quality (moderator), which is the sum of the scores of the components for each observation
df <- df %>% mutate(dq_quality = adv_q + com_q + mkt_q)

# Compute the control variables:

################# CONTROL VARIABLE 1: firm size

df <- df %>% mutate(firm_size = log(at))

################# CONTROL VARIABLE 2: financial leverage

# First, compute leverage. Then, compute the 1st and 99th percentiles and cap outliers (1% is 0.00, so does not need to be capped)
df <- df %>% mutate(leverage = dltt/at)
pct_lev <- quantile(df$leverage, c(.01, 0.99), na.rm = TRUE) 
df$leverage <- ifelse(df$leverage > pct_lev[2], pct_lev[2], df$leverage) #Setting high outliers to 99th percentile (3 outliers)


################# CONTROL VARIABLE 3: revenue growth

# First, lag revenue change. Then, compute the year-over-year revenue growth, which is the revenue in (year t - revenue in year t-1) / revenue in year t-1
df <- df %>% group_by(conm) %>% mutate(revt_change = ((revt - lag(revt, n = 1, order_by = fyear)) / lag(revt, n = 1, order_by = fyear)) * 100)

# Then, cap the min and max values as done for financial leverage.
pct_revt <- quantile(df$revt_change, c(.01, 0.99), na.rm = TRUE) 
df$revt_change <- ifelse(df$revt_change < pct_revt[1], pct_revt[1], df$revt_change) #Setting low outliers to 1st percentile (2 outliers)
df$revt_change <- ifelse(df$revt_change > pct_revt[2], pct_revt[2], df$revt_change) #Setting high outliers to 99th percentile (2 outliers)

################# CONTROL VARIABLE 4: Lagged DV

df <- df %>% group_by(conm) %>% mutate(lag_Q = lag(Q, n = 1, order_by = fyear))

# Create an output directory and store the dataset in the the output folder.
dir.create("../../gen/output")
write.csv2(df, "../../gen/output/final_dataset_all.csv", row.names = FALSE)

