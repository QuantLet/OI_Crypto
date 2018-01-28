# Create Order Imbalance Autocorrelation Plots  

# Note: Without data access you cannot source the main function!


# Set Working Directory to Repo and Load Main 
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/impl")
path = file.path(wd, paste0("Main.R"))

source(path)


wd2 = paste0(wd, "/Include")
setwd(wd2)

# ACF
png(file = "btc-acf.png")
plot(acf.section$`btc-acf`$voi, main = "")
dev.off()

png(file = "ltc-acf.png")
plot(acf.section$`ltc-acf`$voi, main = "")
dev.off()

png(file = "etc-acf.png")
plot(acf.section$`etc-acf`$voi, main = "")
dev.off()
