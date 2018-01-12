# Note: Without data access you cannot source the main function!


# Set Working Directory to Repo and Load Main 
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/data")
path = file.path(wd, paste0("impl/Main.R"))

source(path)


library("vars", quietly = TRUE)

selectionlist = list()
maindfs = list(main.sub.btc, main.sub.ltc, main.sub.etc)
aiclist = lapply(maindfs, function(z){
  
  return(VARselect(z, type = "none", lag.max = 15)$selection[1])
  
  
})


rm(maindfs)

aicdf = do.call(rbind.data.frame, aiclist)
rownames(aicdf) = c("Bitcoin", "Litecoin", "Etherium")
colnames(aicdf) = c("AIC Selection")

aicxtab = xtable(aicdf, caption = "Model Order P as suggested by Akaike Information Criterion")
nrow.aic.idx = nrow(aicdf)

print(aicxtab, hline.after = c(0,0, nrow.aic.idx, nrow.aic.idx))