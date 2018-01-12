# Note: Without data access you cannot source the main function!


# Set Working Directory to Repo and Load Main 
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/data")
path = file.path(wd, paste0("impl/Main.R"))

source(path)


cor.names = c("Order Imbalance", "Returns")


btc.cor.table = xtable(cor(main.sub.btc, method = "pearson"),
                       caption = "Bitcoin Pearson Correlation")
names(btc.cor.table) = cor.names
rownames(btc.cor.table) = cor.names

ltc.cor.table = xtable(cor(main.sub.ltc, method = "pearson"),
                       caption = "Litecoin Pearson Correlation")
names(ltc.cor.table) = cor.names
rownames(ltc.cor.table) = cor.names

etc.cor.table = xtable(cor(main.sub.etc, method = "pearson"),
                       caption = "Etherium Pearson Correlation")
names(etc.cor.table) = cor.names
rownames(etc.cor.table) = cor.names

nrow.idx = nrow(btc.cor.table)




print(btc.cor.table, hline.after = c(0,0, nrow.idx, nrow.idx))
print(ltc.cor.table, hline.after = c(0,0, nrow.idx, nrow.idx))
print(etc.cor.table, hline.after = c(0,0, nrow.idx, nrow.idx))
