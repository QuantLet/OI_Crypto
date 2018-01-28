# Impulse Response Functions between Order Imbalances and Returns

# Note: Without data access you cannot source the main function!


# Set WD 
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/impl")
path = file.path(wd, paste0("Main.R"))

source(path)

library("plotly", quietly = TRUE)
library(plyr, quietly = TRUE)

wd2 = paste0(wd, "/Include")
setwd(wd2)



# IRF

gen.irf.plot = function(varmodel, responsechar1, responsechar2, exportbool, cryptoname){ # only for k = 2
  
  v = varmodel
  titlename = "" #paste0(cryptoname, " Impulse Response")
  
  
  i = irf(v, response = "ret", boot = TRUE)
  df <- ldply(i, data.frame)[1:33,]
  names(df)[3] = "voi"
  df$Steps = rep(0:10, 3)
  
  p <- plot_ly(data = df, x = ~Steps[1:11], y = ~voi[1:11], name = 'Return Response', type = 'scatter', mode = 'lines',
               line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
    add_trace(y = ~voi[.id == "Lower"], name = 'LowerBound', line = list(color = 'rgb(22, 96, 167)', width = 4), legendgroup = 'one') %>%
    add_trace(y = ~voi[.id == "Upper"], name = 'UpperBound', line = list(color = 'rgb(0, 0, 0)', width = 4, legendgroup = 'one', dash = 'dash')) %>%
    layout(title = titlename,
           xaxis = list(title = "Steps"),
           yaxis = list (title = "Return"))
  
  
  i2 = irf(v, response = "voi", boot = TRUE)
  df <- ldply(i2, data.frame)[1:33,]
  names(df)[3] = "ret"
  df$Steps = rep(0:10, 3)
  
  p2 <- plot_ly(data = df, x = ~Steps[1:11], y = ~ret[1:11], name = 'Order Imbalance Response', type = 'scatter', mode = 'lines',
                line = list(color = 'rgb(205, 12, 24)', width = 4), legendgroup = 'one') %>%
    add_trace(y = ~ret[.id == "Lower"], name = 'LowerBound', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), legendgroup = 'one') %>%
    add_trace(y = ~ret[.id == "Upper"], name = 'UpperBound', line = list(color = 'rgb(0, 0, 0)', width = 4, dash = 'dash'), legendgroup = 'one') %>%
    layout(title = titlename,
           xaxis = list(title = "Steps"),
           yaxis = list (title = "Order Imbalance"))
  
  if(exportbool == TRUE){
    
    plotly::export(p, paste0(cryptoname, "-IRF-RET.png"))
    plotly::export(p2, paste0(cryptoname, "-IRF-VOI.png"))
    
  }
  
  
}

gen.irf.plot(var.section$voi.btc, "ret", "voi", exportbool = TRUE, cryptoname = "Bitcoin")
gen.irf.plot(var.section$voi.ltc, "ret", "voi", exportbool = TRUE, cryptoname = "Litecoin")
gen.irf.plot(var.section$voi.etc, "ret", "voi", exportbool = TRUE, cryptoname = "Etherium")


