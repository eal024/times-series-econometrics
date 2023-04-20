
# browseVignettes("gets")

# GETS: Genral to spesific modeling and indicator saturation method

#' arx(): ar-x with the log arch-x error
#' getsm(): gets modeling of the mean specificatio of an arx  
#' getsv(): gets of the log-variance specification of an arx
#' isat(): 
#' 
#' Backward elimination of single and mulitple hypotesis testing., goodness to fit and diagnose tests

# GETS: automated genral to specific modeling and IS

#Campos J, Hendry DF, Ericsson NR (eds.) (2005). General-to-Specific Modeling. Volumes 1 and 2. Edward Elgar Publishing, Cheltenham.

# The main model class under consideration is the autoregressive (AR) model with exponential autoregressive conditional heteroscedastic (ARCH) variance,

# LDGP (Hendry and Doornik, 2014)

# y = BX
# Bi is relevant if B != 0, 
# GETS: Try to find as many as possible variable that corresponds to a signifiancant level ALFA

# Backwards elimintaion, test on the B, diagnose mm.

# Tree steps
#1) Formualte GUM: Genreal unrestricetde model -- passes a set of chosen dignostics test
# A regressior is non significant if t-test (two-sided) is lower than the chosen signif. level alfa

# 2) Undertake the backwards elimination along multiple paths. This by
# removing one-by-one non-significant regressors.
# Each removal is checked for validy aginst the chosen set of diagnositc test, and
# and for parsimonious encompassing against the GUM

# 3) Select among the terminal models, the spesification with the best fit according to fit-criterion (information criterion)

# For k candidate variables, there are 2^k possible models.
# as k-beckome large, the number become computtationally infeasible. A structhttp://127.0.0.1:31505/graphics/5e3a1da4-516d-49f2-9c7e-f390ef25d712.pngural search is therefore done by the GETS, 
# removing variables along a seasch path while checking the diagnostics at each removal.


# Alternatives to gets: stas (core Team) -- search for the best AR(P), using AIC.
# step (stats) both forward -- backward steps-wise seach. Lars (Hastie and Efron, 2013) and glmnet (Friedman)
# shrinkage based searh methods


# Example -----------------------------------------------------------------

library(lgarch)

set.seed(123)

y <- arima.sim( list( ar = 0.4), 100)

eps <- lgarchSim(100, arch = 0.3, garch = 0)


yy <- arima.sim( list(ar = 0.4), 100, innov = eps)

plot(as.zoo(cbind(y,yy,eps) ))


## GETS
library(gets)

mod01 <- arx(y, ar = 1)

# Two diagnostic test are of standardized residuals z.
# AR and ARCH test are Ljung and Box (1978) test for serial correlation in z and z2

# mc = mean constant -- include a constant
# ar = ar = 1, ar = c(2,4), 1:4 osv
# mxreg: attitional regresors
# vcov.type: "ordinary" white (hetrocedasticity and autocrorrelation robust variance covariance matric) Newey and West (1987)

mX <- matrix( rnorm(100*5), 100, 5)

model02 <- arx(y, ar = 1:2, mxreg = mX, vcov.type = "white")

# Estimate the log-variance specification
mod03 <- arx(eps, mc = F, arch = 1)

## Extracting information
fitted(model02)
coef(model02)
logLik(model02)
recursive(model02)
sigma(model02)
toLatex(model02)


# we fit an AR(4)-X-log-ARCH(4)-X model to a quarterly inflation series, and show that 14 
# the conditional variance specification provides a substantial improvement in terms of fit and
#diagnostics. The following

data("infldata", package = "gets")
infldata <- zooreg(infldata[, -1], frequency = 4, start = c(1989, 1))

head(infldata)

inflMod01 <- arx(infldata[, "infl"], ar = 1:4, mxreg = infldata[, 2:4],
                 vcov.type = "white")


# AR(4)-X dummies in the X part.
# Hetro robust st. errors
inflMod01

# We need a time-varying conditional vaiance -- this is because:
# The diagnostic suggest that standard. residual are autocorrelated and hetroscedastic, since the
# test for auto and hetro yield p value of 0.6% and 1,5% resepectively.
# Next we spesfy the condational variance

# The spesification the conditional vaiance as a log-arch(4)-X

inflMod02 <- arx(infldata[, "infl"], ar = 1:4, mxreg = infldata[, 2:4],
                 arch = 1:4, vxreg = infldata[, 2:4], vcov.type = "white")


inflMod02
# The diagnostics imporve substantially from inflMod01
# AR and ARCH test of standard z suggest standd erros z is uncorralted and homo at the usual significans levels (1%,5%,10%)
# Jarque and Bera (1980) test suggest z is normal

resid(inflMod02) 
qqnorm(resid(inflMod02) )
qqline(resid(inflMod02) )


qqnorm(resid(inflMod01) )
qqline(resid(inflMod01) )
hist(resid(inflMod01))

# THe model also improve form estimating the variance. 
# See the loglik
inflMod01
logLik(inflMod01)/104 # n =104

inflMod02
logLik(inflMod02)/100 # n = 100

info.criterion(as.numeric(logLik(inflMod01)), n = 104, k = 8 + 1)
info.criterion(as.numeric(logLik(inflMod02)), n = 100, k = 8 + 8)

# falls form 2.53, to 2.38 in the model2
# Average log-likehollds is neceassary -- the models are estimating diff. number of obs.
# This (sentence above) is the main diff. between AIC, BIC and info criterion.

# Together the enhanced fit and diagnostics indicarte the log-variance specifiation provides 
# a notable improvemnt.

## The gets modeling

infMod03 <- getsm(inflMod02)

infMod03

# OBS! See seach path AR(4) is re introduced
# This is beacuase the deletion leads to a violation of one or sefvral of the diagnostics test
# The AR(4) is therefore kept, even it has no significants


#Next, we use the residuals of the simplified model to develop a parsimonious model of
#the log-variance, storing the results in inflMod05:
inflMod04 <- arx(residuals(infMod03), mc = FALSE, arch = 1:4,
                   vxreg = infldata[, 2:4])

# Lag is spesf. to 5 to keep the same number as earlier
inflMod05 <- getsv(inflMod04, ar.LjungB = list(lag = 5, pval = 0.025))

inflMod05

# arch1 and 2 suggest high impact of the ARCH(1) and ARCH(2) terms.


# End: reestimate the full, simplifed model, and generate out-of-sample forecast of the contional st.dev up to four quartes ahead

inflMod6 <- arx(infldata[, "infl"], ar = c(1,4),
                arch = 1:2, vxreg = infldata[, 2:4], vcov.type = "white")

# Model
inflMod6

# out of sample reg: 4 quarter ahead
newvxreg <- matrix( 0,4,3)

colnames(newvxreg) <- paste0("q",2:4, "dum")

newvxreg[2,"q2dum"] <- 1
newvxreg[3,"q3dum"] <- 1
newvxreg[4,"q4dum"] <- 1

set.seed(123)

predict( inflMod6, n.ahead = 4, spec = "variance", newvxreg = newvxreg)
# The bootstrap procedure is used to generate varinace forecast two or more steps ahead

# In other words, the conditional variance is forecasted to be about four times higher in 2016(1)
# than in 2016(4). This has notable economic consequences. For example, if the forecasted
# inflation in 2016(1) is 2%, then an approximate 95% prediction interval computed as 2±2×bσn+1
# is given by the range 0% to 4%, which is large. By contrast, an approximate 95% prediction
# interval for 2016(4) computed as 2 ± 2 × bσn+4 is given by the range 1.1% to 2.9%, which is
# much tighter.


















