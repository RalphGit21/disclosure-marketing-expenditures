# Import the transformed dataset (previously joined_data, renamed to final_dataset)
df <- read.csv2("../../gen/temp/processed_dataset.csv")
df <- df %>% mutate(Q = (at + csho * prcc_f - ceq - txdb) / at)
df <- na.omit(df) #Last 5 rows are all NAs, 220 --> 215 observations

#Make dummies for each component
df$adv <- ifelse(df$adv_exp != "ND", 1, 0)
df$com <- ifelse(df$com_exp != "ND", 1, 0)
df$mkt <- ifelse(df$mkt_exp != "ND", 1, 0)

#Summarize the components to measure the IV (1 of any of components is 1, 0 otherwise)
df$dum <- ifelse(df$adv_exp != "ND" | df$com_exp != "ND" | df$mkt_exp != "ND", 1, 0)

 #Add ratings
df$adv_q <- ifelse(df$Advertising == "ND", 0, ifelse(df$Advertising == "Low", 1, 3))
df$com_q <- ifelse(df$Commission == "ND", 0, ifelse(df$Commission == "Low", 1, 2))
df$mkt_q <- ifelse(df$Marketing_expenses == "ND", 0, ifelse(df$Marketing_expenses == "Low", 1, 3))

df <- df %>% mutate(dq_quality = adv_q + com_q + mkt_q)


#data_main_filt2 <- data_main %>% select(conm, act, at, cogs, dltt, ebit, emp, ib, invt, lct, lt, oancf, ppent, revt, xad, xopr, xrd, xsga)
#df <- inner_join(data_main_filt2, df , by = "at")
#df$conm.y <- NULL
#names(df)[1] <- 'conm'

################# CONTROL VARIABLE 1: INTEREST RATES
df <- df %>% 
  mutate(interest = case_when(
    fyear == "2000" ~ 6.8,
    fyear == "2001" ~ 4.6,
    fyear == "2002" ~ 3, 
    fyear == "2003" ~ 2.2,
    fyear == "2004" ~ 1.6,
    fyear == "2005" ~ 3, 
    fyear == "2006" ~ 4.8,
    fyear == "2007" ~ 5.2,
    fyear == "2008" ~ 3.1, 
    fyear == "2009" ~ 2.5,
    fyear == "2010" ~ 2.1,
    fyear == "2011" ~ 1.1, 
    fyear == "2012" ~ 1.3,
    fyear == "2013" ~ 1.5,
    fyear == "2014" ~ 1.4, 
    fyear == "2015" ~ 2.3,
    fyear == "2016" ~ 2.4,
    fyear == "2017" ~ 2.2, 
    fyear == "2018" ~ 2.4,
    fyear == "2019" ~ 3.4,
    fyear == "2020" ~ 2.3, 
    TRUE ~ 0 ) )

summary(df$fyear)

# SRC: https://data.worldbank.org/indicator/FR.INR.RINR?locations=US

################# CONTROL VARIABLE 2: MARKET COMPOUNDED ANNUAL RETURN RATE

df <- df %>% 
  mutate(CAGR = case_when(
    fyear == "2000" ~ -9.10,
    fyear == "2001" ~ -11.89,
    fyear == "2002" ~ -22.10, 
    fyear == "2003" ~ 28.68,
    fyear == "2004" ~ 10.88,
    fyear == "2005" ~ 4.91, 
    fyear == "2006" ~ 15.79,
    fyear == "2007" ~ 5.49,
    fyear == "2008" ~ -37, 
    fyear == "2009" ~ 26.46,
    fyear == "2010" ~ 15.06,
    fyear == "2011" ~ 2.11, 
    fyear == "2012" ~ 16,
    fyear == "2013" ~ 32.39,
    fyear == "2014" ~ 13.69, 
    fyear == "2015" ~ 1.38,
    fyear == "2016" ~ 11.96,
    fyear == "2017" ~ 21.83, 
    fyear == "2018" ~ -4.38,
    fyear == "2019" ~ 31.49,
    fyear == "2020" ~ 18.4, 
    TRUE ~ 28.71 ) )

# SRC: https://ycharts.com/indicators/sp_500_total_return_annual


################# CONTROL VARIABLE 3: LAG TOBIN Q

df <- df %>% group_by(conm) %>% mutate(lag_Q = lag(Q, n = 1, order_by = fyear))

################# CONTROL VARIABLE 4: SG&A SCALED BY TOTAL ASSETS

df <- df %>% mutate(firm_size = xsga / at)

dir.create("../../gen/output")
write.csv2(df, "../../gen/output/final_dataset.csv", row.names = FALSE)
