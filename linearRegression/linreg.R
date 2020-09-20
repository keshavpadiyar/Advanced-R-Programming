linreg <- function(formula,data,...) {
    x <- model.matrix(formula,data)
    y <- as.matrix(data[which(colnames(data) == (all.vars(formula)[1]))])
    est <- lnreg(x, y)
    est$call <- match.call()
    class(est) <- "linreg"
    return(est)
}

lnreg <- function(x, y) {
    coef <- (solve(t(x) %*% x)) %*% (t(x) %*% y)
    fittedval <- (x %*% coef)
    residuals <- (y - fittedval)
    df <- nrow(x) - ncol(x)
    sigma2 <- ((t(residuals) %*% residuals) / df)
    covar <- sigma2[1] * (solve(t(x) %*% x))
    se <- sqrt(diag(covar))
    stderr <- round(as.matrix(se),5)
    colnames(stderr) <- c("Std.Error")
    tval <- round(t(t(coef) / se),2)
    colnames(tval) <- c("t value")
    pval <- as.matrix(format.pval(2 * pt(-abs(tval),
              df = df
                ), digits = 1))
    pval <- as.matrix(paste(pval, as.matrix(lapply(pval, significant_stars))))
    colnames(pval) <- c("p value")
    estimate <- round(as.matrix(coef),5)
    colnames(estimate) <- c("Estimate")
    list(
    coefficients = structure(as.vector(t(coef)), names = colnames(t(coef))),
    covar = covar,
    residuals = structure(as.vector(t(residuals)), names = 1:nrow(residuals)),
    fitted.values = structure(as.vector(t(fittedval)),
                              names = colnames(t(fittedval))
                              ),
    df = df,
    sigma2 = sigma2,
    stdErr = stderr,
    tval = tval,
    pval = pval,
    Estimate = estimate
  )
}

significant_stars <- function(pval) {
    stars <- ""
    if (pval <= 0.001) {
        stars <- "***"
    }
    if (pval > 0.001 & pval <= 0.01) {
        stars <- "**"
    }
    if (pval > 0.01 & pval <= 0.05) {
        stars <- "*"
    }
    if (pval > 0.05 & pval <= 0.1) {
        stars <- "."
    }
    stars
}

print.linreg <- function(x,...) {
    cat("Custom Print Built As a part of Lab 4\n")
    cat("Call:\n")
    base::print(x$call)
    cat("\nCoefficients:\n")
    base::print(x$coefficients)
}

summary.linreg <- function(x,...) {
    mat_val <- data.frame(
    Estimate = x$Estimate,
    "Std. Err" = x$stdErr,
    t.value = x$tval,
    p.value = x$pval
  )
    cat("Custom Summary Built As a part of Lab 4\n")
    cat("Call:\n")
    base::print(x$call)
    cat("\nResduals:\n")
    base::print(base::summary(x$residuals)[-4])
    cat("\ncoefficients:\n")
    base::print(mat_val)
    cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    cat(
    "Residual standard error:",
        round(sqrt(diag(x$sigma2)[1]), 4), "on",
            x$df, "degrees of freedom"
        )
}

coef.linreg <- function(x, ...) {
    cat("Custom Coef Built As a part of Lab 4\n")
    return(x$coefficients)
}

resid <- function(x, ...) UseMethod("linreg")
resid <- function(x, ...) {
    cat("Custom Resid Built As a part of Lab 4\n")
    return(x$residuals)
}

pred <- function(x, ...) UseMethod("linreg")
pred <- function(x, ...) {
    cat("Custom Pred Built As a part of Lab 4\n")
    return(x$fitted.values)
}


plot.linreg <- function(x, ...) {
    library(ggplot2)
    qplot(x = x$fitted.values, y = x$residuals,
               xlab = "Fitted Values", ylab = "Residuals") +
    geom_point() +
    stat_summary(fun = median, color = "red", geom = "line", size = 1)

    stdresid = sqrt(abs(as.vector(x$residuals) - mean(as.vector(x$residuals))
                    / as.vector(sqrt(x$sigma2[1])))
                    )
    qplot(x = x$fitted.values, y = stdresid,
               xlab = "Fitted Values", ylab = "Standardized Reciduals") +
    geom_point() +
    stat_summary(fun = mean, color = "red", geom = "line", size = 1)

}