# Note: Without data access you cannot source the main function!


# Set WD 
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/impl")
path = file.path(wd, paste0("Main.R"))

source(path)



library("vars", quietly = TRUE)




unidirectional.causality = lapply(granger.section$test, function(z){
  
  round(z[[2]]$Granger$p.value, 4)
  #z[[1]]$Granger$statistic
  
})

p.val.uni = do.call(rbind.data.frame, unidirectional.causality)
names(p.val.uni) = "P-Value"
rownames(p.val.uni) = c("Litecoin", "Etherium", "Bitcoin")

granger.table = xtable(p.val.uni, caption = "Unidirectional Causality. Null Hypotheses: Order Imbalance does not cause Returns")
nrow.idx.granger = nrow(granger.table)
print(granger.table, hline.after= c(0,0,nrow.idx.granger, nrow.idx.granger))



#  Returns are cause below

unidirectional.causality2 = lapply(granger.section$test, function(z){
  
  round(z[[1]]$Granger$p.value, 4)
})

p.val.uni2 = do.call(rbind.data.frame, unidirectional.causality2)
names(p.val.uni2) = "P-Value"
rownames(p.val.uni2) = c("Litecoin", "Etherium", "Bitcoin")

granger.table2 = xtable(p.val.uni2, caption = "Unidirectional Causality. Null Hypotheses: Returns do not cause Order Imbalance")
nrow.idx.granger2 = nrow(granger.table2)
print(granger.table2, hline.after= c(0,0,nrow.idx.granger2, nrow.idx.granger2))


instantaneous.causality = lapply(granger.section$test, function(z){
  
  round(z[[1]]$Instant$p.value, 4) 
})


p.val.inst = do.call(rbind.data.frame, instantaneous.causality)
names(p.val.inst) = "P-Value"
rownames(p.val.inst) = c("Litecoin", "Etherium", "Bitcoin")

granger.table3 = xtable(p.val.inst, caption = "Instantaneous Causality between Order Imbalance and Returns")
nrow.idx.granger3 = nrow(granger.table3)
print(granger.table3, hline.after= c(0,0,nrow.idx.granger3, nrow.idx.granger3))
