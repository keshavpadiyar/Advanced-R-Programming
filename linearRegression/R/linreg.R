#' Linear Regression S3 class- linreg
#'
#' @param formula Equation with dependent and Independent variables.
#' @param data Input data.frame
#'
#' @return Returns the linreg class object of list containing 
#'                         1> Regression Coefficients, 
#'                         2> Co-variance matrix, 
#'                         3> residuals, 
#'                         4> fitted values, 
#'                         5> degrees of freedom, 
#'                         6> variance (sigma2),
#'                         7> Standard Error,
#'                         8> T value
#'                         9> P value
#'                         10> Function call
#'  
#'                         
#' @export
#'
#' @examples
#' \dontrun{ 
#' data(iris)
#' mod_object <- linreg(Petal.Length~Species, data = iris)
#' print(mod_object)
#' summary(mod_objet)
#' plot(mod_object)
#' }
#' @details This linreg class uses S3 class as object oriented programming. Uses the function lnreg as
#'           regression computation engine. 
#'           
#'Methods associated with linreg class:
#'1> print(mod_object): Prints the regression formula and data along with the coefficients.
#'
#'2> Summary(mod_object): Returns a similar printout as printed for lm objects, but includes only the coefficients with 
#'                        their standard error, t-value and p-value as well as the estimate of residual variance
#'                        and the degrees of freedom in the model.
#'
#'3> coef(mod_object): Returns the coefficients as a named vector.
#
#'4> resid(mod_object): Returns the vector of residuals e.
#'
#'5> pred(mod_object): Returns the predicted values y.
#'
#'6> plot(mod_object): Plots the two plots (1. Residuals vs Fitted, 2. Scale-Location) using ggplot2.
#' 
#' 
linreg <- function(formula, data) {
  #  Fetching the formula and data as matrix
  x <- model.matrix(formula, data)

  #  Fetch the Dependent Variable from the matrix
  y <- as.matrix(data[which(colnames(data) == (all.vars(formula)[1]))])

  #  Calling the method lnreg that computes the regression coefficients and required calculations
  est <- lnreg(x, y)
  est$call <- match.call()
  class(est) <- "linreg"
  return(est)
}

#  Function to compute the regression - regression computation engine.
lnreg <- function(x, y) {

  #  Calculate the Regression Coefficients using the formula : BETA^ = [t(x)*t] * [t(x)*y]
  coef <- (solve(t(x) %*% x)) %*% (t(x) %*% y)

  #  Get Predicted values based on the regression coefficients: y^ = XBETA^
  fittedval <- (x %*% coef)

  #  Get the residuals:  = y - y^ = y - XBETA^
  residuals <- (y - fittedval)

  #  Calculate the degrees of freedom: number of rows - total number of variables
  df <- nrow(x) - ncol(x)

  #  Calculate teh varience of the residuals: [t(residuals)*residuals]/ degrees of freedom
  sigma2 <- ((t(residuals) %*% residuals) / df)

  #  Getting the covarience matrix: variance of the residuals * [t(x)*x]
  covar <- sigma2[1] * (solve(t(x) %*% x))

  #  Standard Error = Sqaure root (diagonal values of covarience matrix)
  se <- sqrt(diag(covar))

  #  Rounding off the Standard Error Values to 5 decimal values
  stderr <- round(as.matrix(se), 5)
  colnames(stderr) <- c("Std.Error")

  #  Calculate the t value: coefficients/ standard error and rouding the values to 2 decimal points
  tval <- round(t(t(coef) / se), 2)
  colnames(tval) <- c("t value")

  #  Calculate the p value using pt() function, getting the lower tail value 
  pval <- as.matrix(format.pval(2 * pt(-abs(tval),
                                          df = df
                                       ), digits = 1)
                    )
  pval <- as.matrix(paste(pval, as.matrix(lapply(pval, significant_stars))))
  colnames(pval) <- c("p value")

  estimate <- round(as.matrix(coef), 5)
  colnames(estimate) <- c("Estimate")

  #  The lnreg function returns the below list
  list(
    coefficients = structure(as.vector(t(coef)), names = colnames(t(coef))),
    
    covar = covar,
    
    residuals = structure(as.vector(t(residuals)), names = 1:nrow(residuals)),
    
    fittedvalues = structure(as.vector(t(fittedval)),
                          names = colnames(t(fittedval))
                          ),
    df = df,
    
    sigma2 = sigma2,
    
    stdErr = stderr,
    
    tval = tval,
    
    pval = pval,
    
    Estimate = estimate
  )
}#  End lnreg


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
}#  End significant_stars


#' Print method of linreg class
#'
#' @param x Linreg class object
#' @param ... additional parameter 
#'
#' @return Prints out the regression formula and data along with the coefficients
#' @export
#'
print.linreg <- function(x, ...) {
  cat("Call:\n")
  base::print(x$call)
  cat("\nCoefficients:\n")
  base::print(x$coefficients)
}# End print

#summary <- function(x, ...) UseMethod("linreg")
#' Summary method of linreg class
#'
#' @param object Linreg class object
#' @param ... additional parameter 
#'
#' @return Returns a similar printout as printed for lm objects, but includes only the coefficients with 
#'                        their standard error, t-value and p-value as well as the estimate of residual variance
#'                        and the degrees of freedom in the model
#'
#' @export
summary.linreg <- function(object, ...) {
  mat_val <- data.frame(
    Estimate = object$Estimate,
    "Std. Err" = object$stdErr,
    t.value = object$tval,
    p.value = object$pval
  )
  cat("Call:\n")
  base::print(object$call)
  cat("\nResduals:\n")
  base::print(base::summary(object$residuals)[-4])
  cat("\ncoefficients:\n")
  base::print(mat_val)
  cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  cat("Residual standard error:",
      round(sqrt(diag(object$sigma2)[1]), 4), "on",
      object$df, "degrees of freedom"
     )
}# End summary

#coef <- function(x, ...) UseMethod("linreg")
#' Coef method of linreg class
#' @param object Linreg class object
#' @param ... additional parameter 
#' 
#' @return Returns the coefficients as a named vector.
#' @export
coef.linreg <- function(object, ...) {
  return(object$coefficients)
}# End coef


resid <- function(object, ...) UseMethod("resid")
#' Resid method of linreg class
#'
#' @param object Linreg class object
#' @param ... additional parameter
#'
#' @return Returns the vector of residuals e.
#' @export
resid.linreg <- function(object, ...) {
  return(object$residuals)
}# End resid

pred <- function(object) UseMethod("linreg")
#' pred method of linreg class
#'
#' @param object Linreg class object
#'
#' @return Returns the predicted values y.
#' @export

pred <- function(object) {
  return(object$fittedvalues)
}# End pred


#' Plot method of linreg class
#'
#' @param x Linreg class object
#' @param ... additional parameter
#'
#' @return Plots the two plots (1. Residuals vs Fitted, 2. Scale - Location) using ggplot2.
#' 
#' @import ggplot2 
#' @import gridExtra
#' 
#' @export
#'
plot.linreg <- function(x, ...) {
  
  liu_theme <- theme(
    
        panel.background = element_rect(fill="white"),
        panel.border = element_rect(fill= NA, colour = 'black'),
        aspect.ratio = 0.5:0.5,
        
        #grid elements
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_line(colour = 'black'),
        axis.title = element_text(face = "bold", size = 10),
        axis.line = element_line(color = 'black'),
        axis.line.x.top = element_line(color = 'black'),
        
        plot.title = element_text(             
          size = 12,
          face = "bold",
          hjust = 0.5,
          vjust = 2),
        
        plot.caption = element_text(
          size = 12,                 
          hjust = 0.5),               
        )
  
  p1 <- qplot(
    x = x$fittedvalues, y = x$residuals,
    xlab = "Fitted Values", ylab = "Residuals",
  ) +
    geom_point() +
    stat_summary(fun = median, color = "red", geom = "line", size = 1) +
    labs( title = "Residuals vs Fitted", 
          caption = "lm(Petal.Length ~ Species)"
        ) +
    liu_theme

       
  stdresid <- sqrt(abs(scale(as.vector(x$residuals))))
  p2 <- qplot(
              x = x$fittedvalues, y = stdresid,
              xlab = "Fitted Values", ylab = "Standardized Residuals"
              ) +
    geom_point() +
    stat_summary(fun = mean, color = "red", geom = "line", size = 1) +
    labs( title = "Scale-Location", 
          caption = "lm(Petal.Length ~ Species)"
         ) +
    liu_theme
  
  gridExtra::grid.arrange(p1,p2, nrow = 2)

}#  End plot