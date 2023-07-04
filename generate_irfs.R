source("utils.R")

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

# Estimate VAR
estim <- VAR(data_var, p = 4, type = "const")
print("Estimated model roots: ")
print(summary(estim)$roots)

###############################################
### Obtain all necessary (reduced-form) matrix objects 
A_mats <- get_A(estim, 40)
sigma_mat <- get_sigma(estim)
impact_mat <- t(chol(sigma_mat))

###############################################
### Find news shock
output_optim_gamma = optim(par = c(1,1,1,1,1,1,1), 
                           fn = forecast_err_var, 
                           i = 1, 
                           A = A_mats, 
                           Sigma = sigma_mat, 
                           h=40, 
                           control = list(fnscale=-1), 
                           method = "L-BFGS-B",
                           lower=c(-0.000000000000001,rep(-Inf,6)), 
                           upper=c(0.000000000000001, rep(Inf,6)))
gamma_opt <- -output_optim_gamma$par/norm(output_optim_gamma$par,type="2")

###################################################
### Generate IRFs
h=30
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
ggplot(irfs_df_long, aes(x=horizon,y=response)) +
  geom_line() +
  facet_wrap(. ~ variable, scale = "free", nrow=2) +
  theme_bw()
