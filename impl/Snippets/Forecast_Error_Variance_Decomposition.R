# Note: Without data access you cannot source the main function!


# Set Working Directory to Repo and Load Main 
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/data")
path = file.path(wd, paste0("impl/Main.R"))

source(path)

fevdlist = lapply(var.section, function(z){
  
  return(fevd(z))
  
})

# win.graph(width = 15, height = 10)
# for(i in 1:length(fevdlist)){
# 
# titlename = paste0(names(fevdlist[i]))
# plot(fevdlist[[i]], main = titlename)
# }




# Bitcoin
btc.voi.fevd = xtable(fevd(var.section$voi.btc)[[1]],
                      caption = "Bitcoin: Forecast Error Variance Decomposition of Order  Imbalance")

btc.ret.fevd = xtable(fevd(var.section$voi.btc)[[2]],
                      caption = "Bitcoin: Forecast Error Variance Decomposition of Returns")

btc.fevd.colnames = c("Order Imbalance", "Returns")
names(btc.voi.fevd) = btc.fevd.colnames
names(btc.ret.fevd) = btc.fevd.colnames
nrow.fevd.table = 10

print(btc.voi.fevd, hline.after = c(0,0, nrow.fevd.table, nrow.fevd.table))
print(btc.ret.fevd, hline.after = c(0,0, nrow.fevd.table, nrow.fevd.table))




# Litecoin
ltc.voi.fevd = xtable(fevd(var.section$voi.ltc)[[1]],
                      caption = "Litecoin: Forecast Error Variance Decomposition of Order Imbalance")

ltc.ret.fevd = xtable(fevd(var.section$voi.ltc)[[2]],
                      caption = "Litecoin: Forecast Error Variance Decomposition of Returns")

ltc.fevd.colnames = c("Order Imbalance", "Returns")
names(ltc.voi.fevd) = ltc.fevd.colnames
names(ltc.ret.fevd) = ltc.fevd.colnames
nrow.fevd.table = 10

print(ltc.voi.fevd, hline.after = c(0,0, nrow.fevd.table, nrow.fevd.table))
print(ltc.ret.fevd, hline.after = c(0,0, nrow.fevd.table, nrow.fevd.table))



# Etherium
etc.voi.fevd = xtable(fevd(var.section$voi.etc)[[1]],
                      caption = "Etherium: Forecast Error Variance Decomposition of Order  Imbalance")

etc.ret.fevd = xtable(fevd(var.section$voi.etc)[[2]],
                      caption = "Etherium: Forecast Error Variance Decomposition of Returns")

etc.fevd.colnames = c("Order Imbalance", "Returns")
names(etc.voi.fevd) = etc.fevd.colnames
names(etc.ret.fevd) = etc.fevd.colnames
nrow.fevd.table = 10

print(etc.voi.fevd, hline.after = c(0,0, nrow.fevd.table, nrow.fevd.table))
print(etc.ret.fevd, hline.after = c(0,0, nrow.fevd.table, nrow.fevd.table))

