# Note: Without data access you cannot source the main function!


# Set Working Directory to Repo and Load Main 
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/data")
path = file.path(wd, paste0("impl/Main.R"))

source(path)



pt.list = lapply(portmanteau.section$test, function(z){
  
  # BG
  es = z[[1]]$serial
  es.statistic = es$statistic
  es.pval = es$p.value
  
  # Portmanteau Results
  pt = z[[2]]$serial
  pt.statistic = pt$statistic
  pt.pval = pt$p.value
  
  out.list = list("BG Statistic" = es$statistic,
                  "BG P Value" = es$p.value,
                  "PT Statistic" = pt$statistic,
                  "PT P Value" = pt$p.value
  )
  
  return(out.list)
  
})


serial.df = do.call(rbind.data.frame, pt.list)


serial.names = c("BG Statistic", "BG P-Value", "PT Statistic", "PT P-Value")
serial.rown = c("Litecoin", "Etherium", "Bitcoin")

serial.table = xtable(serial.df,
                      caption = "Breusch Godfrey LM Test and Portmanteau Test for serial residual       Correlation")
names(serial.table) = serial.names
rownames(serial.table) = serial.rown

nrow.idx.serial = nrow(serial.table)

print(serial.table, hline.after = c(0,0, nrow.idx.serial, nrow.idx.serial))
