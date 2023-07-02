source("var_estimation.R")
source("Forecast_Error_Variance.R")


###################################################
### Create IRF function
irf <- function(A_mats, impact_mat, gamma_opt, h){
  
  responses <- matrix(, nrow = h+1, ncol = dim(impact_mat)[1])
  responses[1,] <- impact_mat %*% gamma_opt 
    
  for(i in 1:(h)){
    responses[i+1,] <- A_mats[[i]] %*% impact_mat %*% gamma_opt 
    
  }
  
  return(responses)
}

###################################################
### Generate IRFs
irfs <- irf(A_mats, impact_mat, -gamma_opt, h) * 100

irfs_df <- data.frame(horizon = c(0:h),
                      tfp = irfs[,1],
                      output = irfs[,2],
                      consumption = irfs[,3],
                      hours = irfs[,4],
                      gz_spread = irfs[,5],
                      sp500 = irfs[,6],
                      gdp_deflator = irfs[,7])

irfs_df_long <- pivot_longer(irfs_df, cols = -c("horizon"), names_to = "variable", values_to = "response") 

irfs_df_long$variable <- factor(irfs_df_long$variable, levels = c("tfp","output","consumption","hours","gz_spread","sp500","gdp_deflator"))

####################################################
### Plot IRFs
ggplot(irfs_df_long %>% filter(variable == names(irfs_df)[2]), aes(x=horizon, y=response, linetype=variable)) +
  geom_line() +
  theme_bw()
ggplot(irfs_df_long %>% filter(variable == names(irfs_df)[3]), aes(x=horizon, y=response, linetype=variable)) +
  geom_line() +
  theme_bw()
ggplot(irfs_df_long %>% filter(variable == names(irfs_df)[4]), aes(x=horizon, y=response, linetype=variable)) +
  geom_line() +
  theme_bw()
ggplot(irfs_df_long %>% filter(variable == names(irfs_df)[5]), aes(x=horizon, y=response, linetype=variable)) +
  geom_line() +
  theme_bw()
ggplot(irfs_df_long %>% filter(variable == names(irfs_df)[6]), aes(x=horizon, y=response, linetype=variable)) +
  geom_line() +
  theme_bw()
ggplot(irfs_df_long %>% filter(variable == names(irfs_df)[7]), aes(x=horizon, y=response, linetype=variable)) +
  geom_line() +
  theme_bw()
ggplot(irfs_df_long %>% filter(variable == names(irfs_df)[8]), aes(x=horizon, y=response, linetype=variable)) +
  geom_line() +
  theme_bw()


ggplot(irfs_df_long, aes(x=horizon,y=response)) +
  geom_line() +
  facet_wrap(. ~ variable, scale = "free", nrow=2) +
  theme_bw()
