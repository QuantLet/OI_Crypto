# Note: Without data access you cannot source the main function!


# Set Working Directory to Repo and Load Main 
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/data")
path = file.path(wd, paste0("impl/Main.R"))

source(path)


library("xtable", quietly = TRUE)


sum.names = c("Order Imbalance", "Returns")



btc.sum.table = xtable(descriptive.section$`btc-summary`,
                       caption = "Bitcoin Location Parameters")
names(btc.sum.table) = sum.names

ltc.sum.table = xtable(descriptive.section$`ltc-summary`,
                       caption = "Litecoin Location Parameters")
names(ltc.sum.table) = sum.names

etc.sum.table = xtable(descriptive.section$`etc-summary`,
                       caption = "Etherium Location Parameters")
names(etc.sum.table) = sum.names



nrow.idx = nrow(btc.sum.table)


print(btc.sum.table, hline.after = c(0,0, nrow.idx, nrow.idx))
print(ltc.sum.table, hline.after = c(0,0, nrow.idx, nrow.idx))
print(etc.sum.table, hline.after = c(0,0, nrow.idx, nrow.idx))