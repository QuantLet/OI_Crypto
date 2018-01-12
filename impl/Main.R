# Tests

# Run this script for each currency in a seperate environment for simultaneous imports via Rmarkdown. 
# style: for (i in c(BTC_EUR, LTC_EUR, ETC_EUR)) OR use lists and the analysis section as a function

# Detach all Packages
#lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
#warning("Cleaning Session!\nAll packages are being detached!")

#rm(list = ls())

# Set Working Directory to Repo
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/data")
setwd(wd)


# Define and load necessary packages
packages = c("RMySQL", "vars", "stats", "Hmisc", "keyringr", "data.table", "tseries", "quantmod", "anytime", "xtable", "psd")

lapply(packages, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(packages, library, quietly = TRUE, character.only = TRUE)




###################################################### Define input data set #################################################

# Import data
firstwave.data = new.env()
secondwave.data = new.env()

# Demo Data
#load("demo/ETC_EUR.RData", envir = firstwave.data) # skip hourly data
load("demo/BTC_EUR.RData", envir = firstwave.data)
load("demo/LTC_EUR.RData", envir = firstwave.data)

load("demo/ETC_EUR_ext.RData", envir = secondwave.data)
load("demo/BTC_EUR_ext.RData", envir = secondwave.data)
load("demo/LTC_EUR_ext.RData", envir = secondwave.data)

# Combine data input
ETC_EUR = data.frame(secondwave.data$ETC_EUR_extended) #rbind(firstwave.data$ETC_EUR, secondwave.data$ETC_EUR_extended)
BTC_EUR = rbind(firstwave.data$BTC_EUR, secondwave.data$BTC_EUR_extended)
LTC_EUR = rbind(firstwave.data$LTC_EUR, secondwave.data$LTC_EUR_extended)

# Clean up
rm(firstwave.data)
rm(secondwave.data)


### Input data set
# replacing dat = LTC_EUR executes script for LTC data set...

currencies = list(BTC_EUR, LTC_EUR, ETC_EUR)
names(currencies) = c("BTC_EUR", "LTC_EUR", "ETC_EUR")
symbols = c("btc", "ltc", "etc")

rm(BTC_EUR)
rm(LTC_EUR)
rm(ETC_EUR)

# timeRange = lapply(currencies, function(z){
#   
#   unixtime = range(z$timestamp)
#   conv = as.POSIXct(unixtime, origin = "1970-01-01")
#   return(conv)
#   
# })


# Initialize Environment for VARs
var.section = new.env()
adf.section = new.env()
acf.section = new.env()
descriptive.section = new.env()




# Remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 30 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

for(i in 1:3)
{
  
  
  # Define data set
  dat = currencies[[i]]
  symbol = symbols[i]
  
  
  
  ################################################### Section Start: Extract Order Imbalance ###############################################
  
  
  # Order by timestamp
  dat.ord = dat[order(dat$timestamp),]
  
  
  # Show unique timestamps
  # Split by timestamp and type in order to reconstruct orderbook
  dt       = data.table(dat.ord)
  dt$id = substr(anytime(dt$timestamp), 1, 16) # create identifier for time - minute
  dat.subs = by(dt, list(dt$id), subset)
  
  unique.dat.subs = lapply(dat.subs, function(z){
    
    un = unique(z)
    
    # kick z if bid and ask are not both available
    if(any(un$type == "bid") && any(un$type == "ask")) return(un)
    else return(NULL)
    
  })
  
  rm(dat.subs)
  
  #Data cleaning: omit all subs that are NULL
  clean.dat.subs = Filter(Negate(is.null), unique.dat.subs)
  
  rm(unique.dat.subs)
  
  print("date range")
  startdat = clean.dat.subs[[1]]
  enddat = clean.dat.subs[[length(clean.dat.subs)]]
  print(startdat)
  print(enddat)
  
  # Main Function
  # Calculate Supply and Demand Shifts 
  # Define Order Flow Imbalance (OFI)
  
  # Use Volume Order Imbalance, n = 20
  # nn = 5
  # nn2 = 10
  
   if(symbol == "etc"){
     nn = 3
     }else(nn = 10)

   n = nn - 1
     weight.vec = exp(-0.5 * 0:n)

  
  voi.test = lapply(clean.dat.subs, function(z){
    
    setDT(z)
    
    # For nn
    z.bid = subset(z, type == "bid")
    bid_vec = z.bid[order(quote, decreasing = T)[1:nn]]
    
    z.ask = subset(z, type == "ask")
    ask_vec = z.ask[order(quote, decreasing = F)[1:nn]]
    
    # Weights
    bid_vec$qv = bid_vec$volume * weight.vec # nn:1
    ask_vec$qv = ask_vec$volume * weight.vec #1:nn # reverse order
    
    # For all
    best_bid = bid_vec$quote[1]
    best_ask = ask_vec$quote[1]
    
    if(best_bid > best_ask) stop("bid higher than ask")
    
    # Remove best bids that are higher than asks ans best asks that are lower than bids
    
    Midprice = (best_bid + best_ask)/2
    
    # Only volume
    #voi = ((sum(ask_vec$volume) - sum(bid_vec$volume))) / ((sum(ask_vec$volume) + sum(bid_vec$volume)))
    
    # For weighted vector
    voi = ((sum(ask_vec$qv) - sum(bid_vec$qv))) / ((sum(ask_vec$qv) + sum(bid_vec$qv)))
    
    return(list("VOI" = voi,
                "Midprice" = Midprice))
    
    
  })
  
  
  voi.bound = do.call(rbind.data.frame, voi.test)
  names(voi.bound) = c("voi", "midpoint_price")
  voi.bound$timestamp = rownames(voi.bound)
  
  voi.bound$ret = Delt(voi.bound$midpoint_price, type = "arithmetic")
  #voi.bound$ret = c(0,diff(log(voi.bound$midpoint_price)))
  setDT(na.omit(voi.bound))
  
  
  main.df = data.table(voi.bound)
  main.df.name = paste0("main.df-", symbol)
  assign(main.df.name, main.df)
  


  
} # end main loop

rm(currencies)
rm(dat)
rm(dat.ord)

SubAndClean = function(main){
  
  s = data.table(na.omit(main[,c("voi", "ret")]))
  sub = subset(s, s$ret >= -0.3 & s$ret <= 0.3)
  return(sub)
}

main.sub.btc = SubAndClean(`main.df-btc`)
main.sub.ltc = SubAndClean(`main.df-ltc`)
main.sub.etc = SubAndClean(`main.df-etc`)


### Descriptive Analysis
maindfs = list(main.sub.btc, main.sub.ltc, main.sub.etc)
for(i in 1:3){
  
  main.df = maindfs[[i]]
  symbol = symbols[i]

# Autocorrelation
acf.singles = apply(main.df, 2, function(z){
  acf(z, ci = 0.95)
})


# Save Autocorrelation
for(k in 1:2){
  acfs.name = paste0(symbol,"-acf")
  assign(acfs.name, acf.singles, envir = acf.section)
}


# Correlation
cors.pearson = cor(main.df)
cors.spearman = cor(main.df, method = "spearman")

# Save Correlations - Pearson
cors.name.pearson = paste0(symbol,"-pearson.cor")
assign(cors.name.pearson, cors.pearson, envir = descriptive.section)

# Save Correlations - Spearman
cors.name.spearman = paste0(symbol,"-spearman.cor")
assign(cors.name.spearman, cors.spearman, envir = descriptive.section)


# Summary Statistics
summaries = sapply(main.df, function(z) base::summary(z))

# Save Summary Statistics
summary.name = paste0(symbol,"-summary")
assign(summary.name, summaries, envir = descriptive.section)


}

### End Descriptive Analysis




criterion = "AIC"

varlag = 15

voi.var.btc = VAR(main.sub.btc, ic = criterion, type = "none", lag.max = varlag)
voi.var.ltc = VAR(main.sub.ltc, ic = criterion, type = "none", lag.max = varlag) # 9
voi.var.etc = VAR(main.sub.etc, ic = criterion, type = "none", lag.max = varlag) #  10 no serial corr, 15 sign.


# Save VAR in according environment
assign("voi.btc", voi.var.btc, envir = var.section)
assign("voi.ltc", voi.var.ltc, envir = var.section)
assign("voi.etc", voi.var.etc, envir = var.section)


# ADF TEST
adflag = 12
ur.df(main.sub.btc$voi, type = "none", lags = adflag)
ur.df(main.sub.btc$ret, type = "none", lags = adflag)

ur.df(main.sub.ltc$voi, type = "none", lags = adflag)
ur.df(main.sub.ltc$ret, type = "none", lags = adflag)

ur.df(main.sub.etc$voi, type = "none", lags = adflag)
ur.df(main.sub.etc$ret, type = "none", lags = adflag)



################################################### Section Start: Vector Autoregression ###############################################


# Save results in new environment
print("Relevant objects are stored in the new environment var.section")
print(ls(var.section))
lapply(var.section, summary) # summaries





################################################### Section End: Vector Autoregression ###############################################



################################################### Section Start: Check Stability ###############################################


# New Section: Check if VAR is stable by examining the modulus of the eigenvalues of the characteristic
# polynomial

root.list = lapply(var.section, roots)
root = unlist(root.list)
if(any(root > 1))
{
  root.location = which(root > 1)
  root.message = paste0("Root > 1 detected at ", root.location)
  print(root.message)
} else
{
  print("Congratulations. All VARs are stable.")  
}





################################################### Section End: Check Stability ###############################################



################################################### Section Start: Analyzing Residuals / Portmanteau Test ###############################################

# Environment: portmanteau
portmanteau.section = new.env()

# use VAR objects from var.section
portmanteau.section$test = lapply(var.section, function(z)
{
  s.es = serial.test(z, type = "BG") # or ES
  s.pt = serial.test(z, type = "PT.asymptotic", lags.pt = 20)
  
  return(list(s.es, s.pt))
})

# Save results in new environment
print("Relevant objects are stored in the new environment portmanteau.section")
print(ls(portmanteau.section))
portmanteau.section$test # Output


# Exclude serially correlated error models from further analysis
################################################### Section End: Analyzing Residuals / Portmanteau Test ###############################################



################################################### Section Start: Analyzing Residuals / Normality Test ###############################################

library("DescTools")

# New Environment
normality.section = new.env()

# Again use VAR objects from var.section
normality.section$test = lapply(var.section, function(z)
{
  n = normality.test(z)
  return(n)
})


# Save results in new environment
print("Relevant objects are stored in the new environment normality.section")
print(ls(normality.section))
normality.section$test # Output




################################################### Section End: Analyzing Residuals / Normality Test ###############################################




################################################### Section Start: Granger Causality ###############################################

# New Environment
granger.section = new.env()

# Again use VAR objects from var.section
granger.section$test = lapply(var.section, function(z)
{
  # returns are always first column
  cause.column.one = colnames(z$y)[1] # VOI
  cause.column.two = colnames(z$y)[2] 
  
  c.one = causality(z, cause = c(cause.column.two)) # ret are cause
  c.two = causality(z, cause = c(cause.column.one)) # VOI are cause
  
  out.list = list(c.one, c.two)
  
  return(out.list)
})


# Save results in new environment
print("Relevant objects are stored in the new environment granger.section")
print(ls(granger.section))
granger.section$test # Output











library("plotly", quietly = TRUE)
library(plyr, quietly = TRUE)

wd2 = paste0(wd, "/Include")
setwd(wd2)



# IRF

gen.irf.plot = function(varmodel, responsechar1, responsechar2, exportbool, cryptoname){ # only for k = 2

  # to delete
# responsechar1 = "ret"  
# responsechar2 = "voi"
# varmodel = var.section$voi.btc  
# cryptoname = "Bitcoin"
#

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


png(file = "btc-acf.png")
plot(acf.section$`btc-acf`$voi, main = "")
dev.off()

png(file = "ltc-acf.png")
plot(acf.section$`ltc-acf`$voi, main = "")
dev.off()


png(file = "etc-acf.png")
plot(acf.section$`etc-acf`$voi, main = "")
dev.off()



# To do: fix layout, export for all currencies


# Forecast Error Variance Decomposition
#
# v = var.section$voi.btc
#
# f = fevd(v)
# data <- ldply(f, data.frame)
# data$Steps = rep(1:10, 2)
#
# p <- plot_ly(data, x = ~Steps[.id == "voi"], y = ~voi[.id == "voi"], color = I("red"),type = 'bar', name = 'Order Imbalance', height = 300, legendgroup = "one") %>%
#   add_trace(y = ~ret[.id == "voi"], name = 'Returns', color = I("black"), legendgroup = "one") %>%
#   layout(yaxis = list(title = 'Percentage'), xaxis = list(title = 'Steps'), barmode = 'stack', title = "FEVD for Order Imbalance")
#
# p2 <- plot_ly(data, x = ~Steps[.id == "ret"], y = ~voi[.id == "ret"], color = I("red"), type = 'bar', name = 'Order Imbalance', height = 300, legendgroup = "one", showlegend = FALSE) %>%
#   add_trace(y = ~ret[.id == "ret"], name = 'Returns', color = I("black"), showlegend = FALSE) %>%
#   layout(yaxis = list(title = 'Percentage'), xaxis = list(title = 'Steps'), barmode = 'stack', title = "FEVD for Returns")
#
#
# subplot(p, p2, nrows = 2, shareX = TRUE, shareY = TRUE)
#
#
# # FEVD Function
# 
# gen.fevd.plot = function(varmodel, cryptoname){
# 
# 
# v = varmodel
# 
# f = fevd(v)
# data <- ldply(f, data.frame)
# data$Steps = rep(1:10, 2)
# 
# p <-  plot_ly(data, x = ~Steps[.id == "voi"], y = ~voi[.id == "voi"], color = I("red"),type = 'bar', name = 'Order Imbalance') %>%
#       add_trace(y = ~ret[.id == "voi"], name = 'Returns', color = I("black")) %>%
#   
#       layout(yaxis = list(title = 'Percentage'), xaxis = list(title = 'Steps'),
#       barmode = 'stack', title = paste0(cryptoname, " - FEVD for Order Imbalance"), showlegend = TRUE)
# 
# p2 <- plot_ly(data, x = ~Steps[.id == "ret"], y = ~voi[.id == "ret"], color = I("red"), type = 'bar', name = 'Order Imbalance') %>%
#   add_trace(y = ~ret[.id == "ret"], name = 'Returns', color = I("black")) %>%
#   
#   layout(yaxis = list(title = 'Percentage'), xaxis = list(title = 'Steps'),
#          barmode = 'stack', title = paste0(cryptoname, " - FEVD for Returns"),showlegend = FALSE)
# 
# 
# p3 = subplot(p, p2, nrows = 2, which_layout = 1, heights = c(0.3, 0.3), margin = 0.1)
# p3
# 
# 
# 
# p.comb = subplot(p, p2, heights = 0.5) %>%
#   layout(annotations = list(
#   list(x = 0.2 , y = 1.05, text = "AA", showarrow = F, xref='paper', yref='paper'),
#   list(x = 0.8 , y = 1.05, text = "BB", showarrow = F, xref='paper', yref='paper')))
# 
# 
# p.comb = subplot(p, p2, nrows = 1,heights = c(0.5))
# # p.comb = subplot(p, p2, nrows = 2, heights = c(0.5, 0.5))
# 
# return(p.out)
# 
# }
# 
#  gen.fevd.plot(var.section$voi.btc, "Bitcoin")
#
# # 
# # 
# # # Litecoin
# # 
# # ltc.voi.irf = irf(var.section$voi.ltc, response = "ret", boot = TRUE)
# # ltc.voi = data.frame(ltc.voi.irf$irf$voi, ltc.voi.irf$irf$voi2, 1:11)
# # names(ltc.voi) = c("VOI", "VOI2", "Steps")
# # 
# # p2 = plot_ly(ltc.voi, x = ~Steps, y = ~VOI, name = "VOI --> Returns", type = "scatter", mode = "lines+markers", yaxis = "Returns") %>% 
# #   add_trace(y = ~VOI2, name = "VOI2 --> Returns",mode = "lines+markers") %>%
# #   layout(title = "Litecoin IRF: VOI & VOI2 --> Returns",
# #          yaxis = list("title" = "Returns"),
# #          xaxis = list("title" = "Steps"))
# # 
# 
# plotly::export(p2, "LTC-IRF-VOI.png")
# 
# # Bitcoin
# 
# btc.voi.irf = irf(var.section$voi.btc, response = "ret", boot = TRUE)
# btc.voi = data.frame(btc.voi.irf$irf$voi, btc.voi.irf$irf$voi2, 1:11)
# names(btc.voi) = c("VOI", "VOI2", "Steps")
# 
# p3 = plot_ly(btc.voi, x = ~Steps, y = ~VOI, name = "VOI --> Returns", type = "scatter", mode = "lines+markers") %>% 
#   add_trace(y = ~VOI2, name = "VOI2 --> Returns", mode = "lines+markers") %>%
#   layout(title = "Bitcoin IRF: VOI & VOI2 --> Returns",
#          yaxis = list("title" = "Returns"),
#          xaxis = list("title" = "Steps"))
# 
# plotly::export(p3, "BTC-IRF-VOI.png")
# 
# 
# btc.oi.irf = irf(var.section$oi.btc, response = "ret", boot = TRUE)
# btc.oi = data.frame(btc.oi.irf$irf$oi, btc.oi.irf$irf$oi2, 1:11)
# names(btc.oi) = c("OI", "OI2", "Steps")
# 
# p4 = plot_ly(btc.oi, x = ~Steps, y = ~OI, name = "OI --> Returns", type = "scatter", mode = "lines+markers") %>% 
#   add_trace(y = ~OI2, name = "OI2 --> Returns", mode = "lines+markers") %>%
#   layout(title = "Bitcoin IRF: OI & OI2 --> Returns",
#          yaxis = list("title" = "Returns"),
#          xaxis = list("title" = "Steps"))
# 
# plotly::export(p4, "BTC-IRF-OI.png")
# 
# 
# # Etherium
# 
# etc.voi.irf = irf(var.section$voi.etc, response = "ret", boot = TRUE, n.ahead = 10, cumulative = TRUE)
# etc.voi = data.frame(etc.voi.irf$irf$voi, etc.voi.irf$irf$voi2, 1:11)
# names(etc.voi) = c("VOI", "VOI2", "Steps")
# 
# p5 = plot_ly(etc.voi, x = ~Steps, y = ~VOI, name = "VOI --> Returns", type = "scatter", mode = "lines+markers") %>% 
#   add_trace(y = ~VOI2, name = "VOI2 --> Returns", mode = "lines+markers") %>%
#   layout(title = "Etherium IRF: VOI & VOI2 --> Returns",
#          yaxis = list("title" = "Returns"),
#          xaxis = list("title" = "Steps"))
# 
# plotly::export(p5, "ETC-IRF-VOI.png")
# 
# 
# etc.oi.irf = irf(var.section$oi.etc, response = "ret", boot = TRUE)
# etc.oi = data.frame(etc.oi.irf$irf$oi, etc.oi.irf$irf$oi2, 1:11)
# names(etc.oi) = c("OI", "OI2", "Steps")
# 
# p6 = plot_ly(etc.oi, x = ~Steps, y = ~OI, name = "OI --> Returns", type = "scatter", mode = "lines+markers") %>% 
#   add_trace(y = ~OI2, name = "OI2 --> Returns", mode = "lines+markers") %>%
#   layout(title = "Etherium IRF: OI & OI2 --> Returns",
#          yaxis = list("title" = "Returns"),
#          xaxis = list("title" = "Steps"))
# 
# plotly::export(p6, "ETC-IRF-OI.png")
# setwd(wd)
# 

