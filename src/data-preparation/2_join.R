# Call the second  dataset (generated from Compustat)
tobin <- read_sav("../../data/bnoi5iqhjoxaudyz.sav")

tobin <- tobin %>% mutate(Q = (at + csho * prcc_f - ceq - txdb) / at)
data_main_filt <- data_main %>% select(at)

data_join <- inner_join(data_main_filt, tobin , by = "at")

write.csv2(data_join, "../../gen/temp/joined_data.csv", row.names = FALSE)

# Within Excel, deal with some NAs, add all the columns for IVs, and moderators.