# Call the second  dataset (generated from Compustat)
tobin <- read_sav("../../data/bnoi5iqhjoxaudyz.sav")

# Compute tobin's Q
tobin <- tobin %>% mutate(Q = (at + csho * prcc_f - ceq - txdb) / at)
data_main_filt <- read.csv2("../../gen/temp/data_new.csv") %>% select(at)

# Join the dataset from step 1 with this new dataset
data_join <- inner_join(data_main_filt, tobin , by = "at")

# Store this new dataset in a new csv file and store it in the temp folder
write.csv2(data_join, "../../gen/temp/joined_data.csv", row.names = FALSE)

# Within Excel, deal with some NAs, add all the columns for IVs, and moderators.