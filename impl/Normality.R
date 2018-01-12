# Note: Without data access you cannot source the main function!


# Set WD 
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/impl")
path = file.path(wd, paste0("Main.R"))

source(path)


normal = lapply(normality.section$test, function(z){
  
  jbextr = z$jb.mul$JB
  
  pval = jbextr$p.value
  stat = jbextr$statistic
  
  return(list(pval, stat))
  
})

normal.data = do.call(rbind.data.frame, normal)
normal.data.names = c("P-Value", "JB Test Statistic")
rownames(normal.data) = c("Litecoin", "Etherium", "Bitcoin")
#rownames(normal.data) = c("OI BTC", "VOI LTC", "OI ETC", "VOI ETC", "OI LTC", "VOI LTC")

#knitr::kable(normal.data)

normal.table = xtable(normal.data,
                      caption = "Jarque Bera Test on Non-normality of Residuals")
names(normal.table) = normal.data.names

nrow.idx.normal = nrow(normal.table)

print(normal.table, hline.after = c(0,0, nrow.idx.normal, nrow.idx.normal))