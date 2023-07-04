###############################################
###############################################
### Load packages & Create functions for analysis

###############################################
### Load packages 

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

###############################################
### Retrieve VAR MA-form coefficient matrices

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

###############################################
### Retrieve VAR error covariance matrix 

get_sigma <- function(varest){
  
  lagcoefmats <- Acoef(varest)
  params <- ncol(varest$datamat[, -c(1:varest$K)])
  sigma <- crossprod(resid(varest)) / (varest$obs - params)
  
  return(sigma)
} 

###############################################
### Map to forecast error variance of i-th variable

forecast_err_var <- function(gamma,i,A,Sigma,h) {
  # gamma = variable maximized over
  # choice of j var, start guess is 
  # A = list of A_tau matrices
  # 
  
  numer_temp = 0
  denom_temp = 0
  
  
  ## decompose B0, into Btil_0, D
  B0 <- t(chol(Sigma))
  B_til_0 <- B0
  
  
  ## create ei vector
  e_i = numeric(nrow(A[[1]]))
  for(k in 1:nrow(A[[1]])) {
    if(k == i){
      e_i[k] = 1
    }
  }
  
  #loop over to sum across A matrices
  numer_temp <- B_til_0 %*% gamma %*% t(gamma) %*% t(B_til_0)
  denom_temp <- Sigma
  for(k in 1:h) {
    numer_temp =  numer_temp +  A[[k]] %*% B_til_0 %*% gamma %*% t(gamma) %*% t(B_til_0) %*% t(A[[k]])
    denom_temp =  denom_temp +  A[[k]] %*% Sigma %*% t(A[[k]])
  }
  
  #do final multiplication and combine numer, denom
  numer = t(e_i) %*% numer_temp %*% e_i
  denom = t(e_i) %*% denom_temp %*% e_i
  
  fev = numer/denom
  
  return(fev)
}

###################################################
### Generate IRF

irf <- function(A_mats, impact_mat, gamma_opt, h){
  
  responses <- matrix(, nrow = h+1, ncol = dim(impact_mat)[1])
  responses[1,] <- impact_mat %*% gamma_opt 
  
  for(i in 1:(h)){
    responses[i+1,] <- A_mats[[i]] %*% impact_mat %*% gamma_opt 
    
  }
  
  return(responses)
}