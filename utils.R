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
               ggpubr,
               expm, 
               readr,
               forcats) 

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

###################################################
### Plot IRF

plot_irf <- function(data, endog_vars, response_var, irf_horizon, fev_horizon, flip=FALSE){
  
  h = irf_horizon
  
  # Select VAR-relevant series
  data_var <- data %>%
    dplyr::select(any_of(endog_vars))
  
  # Estimate VAR
  estim <- VAR(data_var, p = 5, type = "const")
  print("Estimated model roots: ")
  print(summary(estim)$roots)
  
  ### Obtain all necessary (reduced-form) matrix objects 
  A_mats <- get_A(estim, h)
  sigma_mat <- get_sigma(estim)
  impact_mat <- t(chol(sigma_mat))
  
  ### Find news shock
  output_optim_gamma = optim(par = c(1,1,1,1,1,1,1), 
                             fn = forecast_err_var, 
                             i = 1, 
                             A = A_mats, 
                             Sigma = sigma_mat, 
                             h=fev_horizon, 
                             control = list(fnscale=-1), 
                             method = "L-BFGS-B",
                             lower=c(-0.000000000000001,rep(-Inf,6)), 
                             upper=c(0.000000000000001, rep(Inf,6)))
  gamma_opt <- -output_optim_gamma$par/norm(output_optim_gamma$par,type="2")
  if(gamma_opt[1] < 0){
    gamma_opt = -gamma_opt
  }
  if(flip == TRUE){
    gamma_opt = -gamma_opt
  }
  
  ### Generate IRFs
  irfs <- irf(A_mats, impact_mat, gamma_opt, h) * 100
  
  irfs_df <- data.frame(horizon = c(0:h),
                        irfs[,1],
                        irfs[,2],
                        irfs[,3],
                        irfs[,4],
                        irfs[,5],
                        irfs[,6],
                        irfs[,7])
  names(irfs_df) = c("horizon",endog_vars)
  
  irfs_df_long <- pivot_longer(irfs_df, cols = -c("horizon"), names_to = "variable", values_to = "response") 
  
  irfs_df_long$variable <- factor(irfs_df_long$variable, levels = endog_vars)
  
  if(response_var[1] == "bank_equity"){
    irfs_df_long %>% mutate(response = -response)
  }
  
  ggplot(irfs_df_long %>% dplyr::filter(variable %in% response_var), 
         aes(x=horizon,y=response)) +
    geom_line(linetype= "solid", size = 1) +
    theme_bw() + 
    xlab("Quarters")
}