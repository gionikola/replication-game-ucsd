### DUMMY VAR ANALYSIS 

###############################################
### Load packages & FRED API key 

## Load and install relevant packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, 
               lubridate,
               reshape2,
               data.table,
               tidyr,
               stats,
               fredr,
               BVAR,
               vars, 
               tseries,
               ggplot2,
               expm) 
# Set FRED API key 
fredr_set_key("cbd3662cc6a06560acacf323fe5ae37a")

###############################################
### Import data

# Read data
data <- read.csv("data/var_data.csv")

# Clean up names 
names(data) <- c("date", 
                 "tfp", 
                 "output", 
                 "investment", 
                 "consumption", 
                 "hours",
                 "sp500",
                 "gdp_deflator",
                 "ebp",
                 "gz_spread",
                 "default_risk",
                 "rmv_banks",
                 "looss",
                 "baa")

# Remove redundant rows at the end
data <- data[1:133,]

###############################################
### Estimate VAR

# Select VAR-relevant series
data_var <- data %>%
  dplyr::select(tfp, output, consumption, hours, gz_spread, sp500, gdp_deflator)

# Select optimal lag order
# lagorders <- VARselect(data_var)$selection
# lagorders
# lagorder <- lagorders[3]

# Estimate VAR
estim <- VAR(data_var, p = 4, type = "const")
print("Estimated model roots: ")
print(summary(estim)$roots)

###############################################
### Obtain all necessary (reduced-form) matrix objects 

# A_p matrices 
get_A <- function(varest, H){
  
  # Store lag coefficient matrices 
  lagcoefmats <- Acoef(varest)
  x <- varest
  params <- ncol(x$datamat[, -c(1:x$K)])
  sigma.u <- crossprod(resid(x)) / (x$obs - params)
  
  # Store necessary object dimensions
  numvars <- dim(lagcoefmats[[1]])[1]
  numlags <- length(lagcoefmats) 
  numdims <- numvars * numlags 
  
  # Create VAR(1) (companion form) lag coefficient matrix 
  Anorm <- matrix(0 * (1:numdims^2), nrow = numdims, ncol = numdims)
  for (n in 1:numlags){
    Anorm[1:numvars, (1+(n-1)*numvars):(n*numvars)] <- lagcoefmats[[n]]
  }
  Anorm[(numvars+1):(numdims), 1:(numvars * (numlags - 1))] <- diag(numvars * (numlags - 1))
  
  # Compute PTIRF
  A_list <- list()
  for (h in 1:H){
    response <- Anorm %^% h
    response <- response[1:varest$K, 1:varest$K]
    A_list[[h]] <- response
  }
  
  return(A_list)
} 
A_mats <- get_A(estim, 40)

# Sigma (error cov mat)
get_sigma <- function(varest){
  
  lagcoefmats <- Acoef(varest)
  params <- ncol(varest$datamat[, -c(1:varest$K)])
  sigma <- crossprod(resid(varest)) / (varest$obs - params)
  
  return(sigma)
} 
sigma_mat <- get_sigma(estim)
impact_mat <- t(chol(sigma_mat))


