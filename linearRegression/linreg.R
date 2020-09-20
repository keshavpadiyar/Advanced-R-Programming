  linreg <- function(formula, data, ...) UseMethod("linreg")

  lreg <- function(x, y) {
    coef <- (solve(t(x) %*% x)) %*% (t(x) %*% y)
    fittedval <- (x %*% coef)
    residuals <- (y - fittedval)
    df <- nrow(x) - ncol(x)
    variance <- ((t(residuals) %*% residuals) / df)
    covar <- variance[1] * (solve(t(x) %*% x))
    se <- sqrt(diag(covar))
    StdErr <- round(as.matrix(se), 5)
    colnames(StdErr) <- c("Std. Error")
    tval <- round(t(t(coef) / se), 2)
    colnames(tval) <- c("t value")
    pval <- as.matrix(format.pval(2 * pt(-abs(tval), 
                df = df),digits = 1))
    pval <- as.matrix(paste(pval, as.matrix(lapply(pval,significant_stars))))
    colnames(pval) <- c("Pr(>|t|)")
    Estimate <- round(as.matrix(coef), 5)
    colnames(Estimate) = c("Estimate")
    list(
      coefficients = structure(as.vector(t(coef)), names = colnames(t(coef))),
      covar = covar,
      residuals = structure(as.vector(t(residuals)), names = 1:nrow(residuals)),
      fitted.values = structure(as.vector(t(fittedval)),
                              names = colnames(t(fittedval))),
      df = df,
      variance = variance,
      StdErr = StdErr,
      tval = tval,
      pval = pval,
      Estimate = Estimate
    )
  }
  significant_stars <- function(pval) {
    stars = ""
    if(pval <= 0.001)
      stars = "***"
    if(pval > 0.001 & pval <= 0.01)
      stars = "**"
    if(pval > 0.01 & pval <= 0.05)
      stars = "*"
    if(pval > 0.05 & pval <= 0.1)
      stars = "."
    stars
  }
  lm <- function(formula, data, ...) {
    x <- model.matrix(formula, data)
    y <- as.matrix(data[which(colnames(data) == (all.vars(formula)[1]))])
    est <- lreg(x, y)
    est$call <- match.call()
    class(est) <- "linreg"
    return(est)
  }
  predict <- function(x, ...){
    return(x$fitted.values)
  }
  print.linreg <- function(x, ...) {
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print(x$coefficients)
  }
  summary.linreg <- function (x, ...){
      mat_val <- cbind(
      Estimate = x$Estimate,
      "Std. Err" = x$StdErr,
      t.value = x$tval,
      p.value = x$pval
      )
    cat("Call:\n")
    print(x$call)
    cat("\nResduals:\n")
    print(summary(x$residuals)[-4])
    cat("\ncoefficients:\n")
    print(mat_val)
    cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  }
  model <- lm(Petal.Length ~ Species, data = iris)