library(haven)
library(dplyr)

dir.create("../../data")
data <- read_sav("../../data/CSTAT_annual_2000-2021_naics336-541.sav")
# Later on, values will be lagged. So need at least 2 observations in the dataset:
data <- data %>% group_by(conm) %>% filter(n() > 1)

# Only include companies whose stock price is greater than $5 due to penny stocks being more volatile
data <- data %>% filter(prcc_f >= 5)


# Select the ciks of the firms that are within the top 10 in terms of annual sales within each industry
top_336111 = c("0001467858",
               "0001094517",
               "0001318605",
               "0001094517",
               "0000807707",
               "0001755456")

top_454110 = c("0001018724",
               "0001794309",
               "0001254699",
               "0001065088",
               "0001616707",
               "0001050377",
               "0000885721",
               "0001513842")

top_511210 = c("0001108524",
               "0000796434",
               "0001124610",
               "0000718877",
               "0000042542",
               "0000712515",
               "0001800227",
               "0000356028",
               "0001402436",
               "0001512673")

top_518210 = c("0001724670",
               "0001062047",
               "0001492714",
               "0000714603",
               "0000885617",
               "0001355470")

top_524210 = c("0000937834",
               "0000070696",
               "0000062709",
               "0000900390",
               "0001333986",
               "0000021175",
               "0000045939",
               "0000277795")

top_541511 = c("0001058290",
               "0001087423",
               "0001556148",
               "0001367535",
               "0001319085",
               "0001327811",
               "0001579086",
               "0000946581",
               "0001352010",
               "0001207074")

# Merge these 6 industries in one list
top_an <- c(top_336111, top_454110, top_511210, top_518210, top_524210, top_541511)

# create an additional column that is 1 if the cik is in the top_an list and only keep those that are in the new dataset
data$top <- as.numeric(data$cik %in% top_an)
data_main <- data %>% filter(top == 1)

# Output the newly created csv file 
write.csv2(data_main, "../../gen/temp/data_new.csv", row.names = FALSE)

# In addition, create a csv file that stores all the ciks of the firms included in this dataset and store it as companies_cik.csv in the temp folder
companies_list <- data_main %>% select(cik) %>% summarize_all(last)
write.csv(companies_list, "../../gen/temp/companies_cik.csv", row.names = FALSE)
