## Not run:
## ------------------------------------------------------------
## multivariate mixed forests
## lipids used as the multivariate y-responses
## ------------------------------------------------------------
## load the data
data(nutrigenomic, package = "randomForestSRC")
## nice wrapper for making multivariate formula
mvrfsrc.f <- function(ynames, dat) {
  as.formula(paste("Multivar(", paste(ynames, collapse = ","),paste(") ~ ."), sep = ""))
}
## multivariate mixed forest call
mv.obj <- rfsrc(mvrfsrc.f(colnames(nutrigenomic$lipids)),
                data.frame(do.call(cbind, nutrigenomic)), nsplit = 10)
## ------------------------------------------------------------
## nice wrappers to extract standardized performance values
## works for all forests - including multivariate forests
## ------------------------------------------------------------

## pull the standardized error from a forest object
get.error <- function(obj) {
  100 * c(sapply(obj$yvar.names, function(nn) {
    o.coerce <- randomForestSRC:::coerce.multivariate(obj, nn)
    if (o.coerce$family == "class") {
      tail(o.coerce$err.rate[, 1], 1)
    }
    else {
      tail(o.coerce$err.rate, 1) / var(o.coerce$yvar, na.rm = TRUE)
    }
  }))
}
## pull the standardized VIMP from a forest object
get.vimp <- function(obj) {
  vimp <- 100 * do.call(cbind, lapply(obj$yvar.names, function(nn) {
    o.coerce <- randomForestSRC:::coerce.multivariate(obj, nn)
    if (o.coerce$family == "class") {
      o.coerce$importance[, 1]
    }
    else {
      o.coerce$importance / var(o.coerce$yvar, na.rm = TRUE)
    }
  }))
  colnames(vimp) <- obj$yvar.names
  vimp
}
## ------------------------------------------------------------
## plot the standarized performance and VIMP values
## ------------------------------------------------------------
## acquire the error rate for each of the 21-coordinates
## standardize to allow for comparison across coordinates
serr <- get.error(mv.obj)
## acquire standardized VIMP
svimp <- get.vimp(mv.obj)
par(mfrow = c(1,2))
plot(serr, xlab = "Lipids", ylab = "Standardized Performance")
matplot(svimp, xlab = "Genes/Diet/Genotype", ylab = "Standardized VIMP")
## End(Not run)










library(randomForestSRC)
## ------------------------------------------------------------
## interactions for multivariate mixed forests
## ------------------------------------------------------------
mtcars.new <- mtcars
mtcars.new$cyl <- factor(mtcars.new$cyl)
mtcars.new$carb <- factor(mtcars.new$carb, ordered = TRUE)
mv.obj <- rfsrc(cbind(carb, mpg, cyl) ~., data = mtcars.new)
find.interaction(mv.obj, method = "vimp", outcome.target = "carb")
find.interaction(mv.obj, method = "vimp", outcome.target = "mpg")
find.interaction(mv.obj, method = "vimp", outcome.target = "cyl")



## ------------------------------------------------------------
## multivariate regression
## ------------------------------------------------------------
mtcars.mreg <- rfsrc(Multivar(mpg, cyl) ~., data = mtcars)
plot.variable(mtcars.mreg, outcome.target = "mpg", partial = TRUE, nvar = 1)
plot.variable(mtcars.mreg, outcome.target = "cyl", partial = TRUE, nvar = 1)
