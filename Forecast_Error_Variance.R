#### Inputs: B0 (Sigma = B0*B0'), A_tau, choice of j
#### Outputs: Vij(h)


#### base assumption: i=j=1 

forecast_err_var <- function(gamma,i,j,A,Sigma,h) {
    # gamma = variable maximized over
    # choice of j var, start guess is 
    # A = list of A_tau matrices
    # 
  
    numer_temp = 0
    denom_temp = 0
    
    
    ## decompose B0, into Btil_0, D
    B0 <- t(chol(Sigma))
    B_til_0 <- B0
    
    
    ## create ei, ej vectors
    e_i = numeric(nrow(A[[1]]))
    for(k in 1:nrow(A[[1]])) {
          if(k == i){
                e_i[k] = 1
          }
    }
    
    e_j = numeric(nrow(A[[1]]))
    for(k in 1:nrow(A[[1]])) {
      if(k == j){
        e_j[k] = 1
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



### now run maximization over different
output_optim_gamma = optim(par = c(1,1,10,0,0,10,0), fn = forecast_err_var, i = 1, j = 1, A = A_mats, Sigma = sigma_mat, h=40 , control = list(fnscale=-1), method = "L-BFGS-B",lower=c(-0.000000000000001,rep(-Inf,6)), upper=c(0.000000000000001, Inf, Inf, Inf, Inf, Inf, Inf))
gamma_opt <- -output_optim_gamma$par/norm(output_optim_gamma$par,type="2")
