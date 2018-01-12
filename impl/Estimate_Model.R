# Note: Without data access you cannot source the main function!


# Set WD 
wd = paste0(Sys.getenv("USERPROFILE"),"/OI_Crypto/impl")
path = file.path(wd, paste0("Main.R"))

source(path)



library("stargazer", quietly = TRUE)

# Fix Labels
caption_at_bottom <- function(expr) {
  x <- capture.output(expr)
  cap <- grep("\\\\caption", x)
  lab <- grep("\\\\label", x)
  last <- grep("\\\\end\\{table", x)
  cat(
    paste(
      c(x[-last], x[cap], x[lab], x[last])[-c(cap, lab)]
      , collapse = "\n")
    , "\n")
}



# Prepare Bitcoin

btc.var.i = var.section$voi.btc$varresult$voi
btc.var.r = var.section$voi.btc$varresult$ret

names.vec.btc = rep(c("Order Imbalance", "Returns"), 10)
names.num.btc = paste0(names.vec.btc, " Lag ")
names.comp.btc = paste0(names.num.btc, rep(1:10, each = 2))

attr(btc.var.i$coefficients, "names") = names.comp.btc
attr(btc.var.r$coefficients, "names") = names.comp.btc

# Bitcoin
caption_at_bottom(stargazer(btc.var.i,
                            btc.var.r,
                            type = "latex",
                            single.row = TRUE,
                            summary = TRUE,
                            title = "Bitcoin VAR - Coefficient Estimates",
                            column.labels = c("Order Imbalance", "Returns"),
                            dep.var.caption = "",
                            report=('vc*'),
                            keep.stat = c("n", "adj.rsq"),
                            no.space = TRUE,
                            model.numbers = FALSE,
                            header = FALSE))


# Prepare Litecoin

ltc.var.i = var.section$voi.ltc$varresult$voi
ltc.var.r = var.section$voi.ltc$varresult$ret

names.vec.ltc = rep(c("Order Imbalance", "Returns"), 4)
names.num.ltc = paste0(names.vec.ltc, " Lag ")
names.comp.ltc = paste0(names.num.ltc, rep(1:4, each = 2))

attr(ltc.var.i$coefficients, "names") = names.comp.ltc
attr(ltc.var.r$coefficients, "names") = names.comp.ltc



# Litecoin
caption_at_bottom(stargazer(ltc.var.i,
                            ltc.var.r,
                            type = "latex",
                            summary = TRUE,
                            title = "Litecoin VAR - Coefficient Estimates",
                            column.labels = c("Order Imbalance", "Returns"),
                            dep.var.caption = "",
                            report=('vc*'),
                            keep.stat = c("n", "adj.rsq" ),
                            no.space = TRUE,
                            model.numbers = FALSE,
                            header = FALSE))

# Prepare Etherium

etc.var.i = var.section$voi.etc$varresult$voi
etc.var.r = var.section$voi.etc$varresult$ret

names.vec.etc = rep(c("Order Imbalance", "Returns"), 12)
names.num.etc = paste0(names.vec.etc, " Lag ")
names.comp.etc = paste0(names.num.etc, rep(1:12, each = 2))

attr(etc.var.i$coefficients, "names") = names.comp.etc
attr(etc.var.r$coefficients, "names") = names.comp.etc


# Etherium
caption_at_bottom(stargazer(etc.var.i,
                            etc.var.r,
                            type = "latex",
                            summary = TRUE,
                            title = "Etherium VAR - Coefficient Estimates",
                            column.labels = c("Order Imbalance", "Returns"),
                            dep.var.caption = "",
                            report=('vc*'),
                            keep.stat = c("n", "adj.rsq" ),
                            no.space = TRUE,
                            model.numbers = FALSE,
                            header = FALSE))
