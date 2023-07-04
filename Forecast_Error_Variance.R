
### now run maximization over different
output_optim_gamma = optim(par = c(1,1,10,0,0,10,0), fn = forecast_err_var, i = 1, j = 1, A = A_mats, Sigma = sigma_mat, h=40 , control = list(fnscale=-1), method = "L-BFGS-B",lower=c(-0.000000000000001,rep(-Inf,6)), upper=c(0.000000000000001, Inf, Inf, Inf, Inf, Inf, Inf))
gamma_opt <- -output_optim_gamma$par/norm(output_optim_gamma$par,type="2")
