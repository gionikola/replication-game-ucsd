source("utils.R")

# Set seed 
set.seed(070720232)

###############################################
### Import data

# Read data
data <- read.csv("data/var_data_update.csv")

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
                 "bank_equity",
                 "sloos",
                 "baa")

# Remove redundant rows at the end
data <- data[1:133,]

###############################################
###############################################
###############################################
### REPLICATE FIG 1
###############################################
###############################################
###############################################

# Set IRF length
h=40

# Set VAR number of lags 
p=5

# Select VAR-relevant series
data_var <- data %>%
  dplyr::select(tfp, output, consumption, hours, gz_spread, sp500, gdp_deflator)

# Estimate VAR
estim <- VAR(data_var, p = p, type = "const")
print("Estimated model roots: ")
print(summary(estim)$roots)

### Obtain all necessary (reduced-form) matrix objects 
A_mats <- get_A(estim, 40)
sigma_mat <- get_sigma(estim)
impact_mat <- t(chol(sigma_mat))

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
if(gamma_opt[1] < 0){
  gamma_opt = -gamma_opt
}

### Generate IRFs
irfs <- irf(A_mats, impact_mat, gamma_opt, h) * 100

irfs_df <- data.frame(horizon = c(0:h),
                      tfp = irfs[,1],
                      output = irfs[,2],
                      consumption = irfs[,3],
                      hours = irfs[,4],
                      gz_spread = irfs[,5],
                      sp500 = irfs[,6],
                      gdp_deflator = irfs[,7])
names(irfs_df) <- c("horizon", 
                    "TFP",
                    "Output",
                    "Consumption",
                    "Hours",
                    "GZ spread",
                    "S&P 500",
                    "Inflation")

irfs_df_long <- pivot_longer(irfs_df, cols = -c("horizon"), names_to = "variable", values_to = "response") 

irfs_df_long$variable <- factor(irfs_df_long$variable, levels = c("TFP",
                                                                  "Output",
                                                                  "Consumption",
                                                                  "Hours",
                                                                  "GZ spread",
                                                                  "S&P 500",
                                                                  "Inflation"))
GZ_Spread_IRF_TFP_news_fig1 <- readr::read_csv("GZ Spread_IRF_TFP_news_fig1.csv") %>% 
  rename(horizon = Horizon,
         response = Resp,) %>% 
  mutate(Variable = case_when(Variable == "GZ Spread" ~ "GZ spread", TRUE ~ Variable)) %>% 
  mutate(variable = forcats::as_factor(Variable))

### Plot IRFs (Fig 1)
fig1 <- ggplot(irfs_df_long, aes(x=horizon, y=response)) +
  geom_line(data = GZ_Spread_IRF_TFP_news_fig1, aes(x = horizon, y = response), linetype = "dashed")+
  geom_ribbon(data = GZ_Spread_IRF_TFP_news_fig1, aes(ymin = LowerCI, ymax = HigherCI), alpha = 0.5)+
  geom_line(size = 1) +
  facet_wrap(. ~ variable, scale = "free", nrow=2) +
  theme_bw() +
  xlab("Quarters") + ylab(NULL)
fig1 


### Save Fig 1
ggsave(filename = "figures/fig1_alt.png", fig1,
       width = 8, height = 4, dpi = 300, units = "in", device='png')

###############################################
###############################################
###############################################
### REPLICATE FIG 2
###############################################
###############################################
###############################################

### import responses from replication files

GZ_Spread_IRF_TFP_news_fig1 <- readr::read_csv("GZ Spread_IRF_TFP_news_fig1.csv") %>% 
  rename(horizon = Horizon,
         response = Resp,) %>% 
  mutate(Variable = case_when(Variable == "GZ Spread" ~ "GZ spread", TRUE ~ Variable)) %>% 
  mutate(variable = forcats::as_factor(Variable))
DR_IRF_TFP_news_fig1 <- readr::read_csv("Default Risk_IRF_TFP_news_fig1.csv") %>% 
  rename(horizon = Horizon,
         response = Resp,) %>% 
  #mutate(Variable = case_when(Variable == "GZ Spread" ~ "GZ spread", TRUE ~ Variable)) %>% 
  mutate(variable = forcats::as_factor(Variable))
BE_IRF_TFP_news_fig1 <- readr::read_csv("Bank Equity_IRF_TFP_news_fig1.csv") %>% 
  rename(horizon = Horizon,
         response = Resp,) %>% 
  #mutate(Variable = case_when(Variable == "GZ Spread" ~ "GZ spread", TRUE ~ Variable)) %>% 
  mutate(variable = forcats::as_factor(Variable))
EBP_IRF_TFP_news_fig1 <- readr::read_csv("Excess Bond Premium_IRF_TFP_news_fig1.csv") %>% 
  rename(horizon = Horizon,
         response = Resp,) %>% 
  #mutate(Variable = case_when(Variable == "GZ Spread" ~ "GZ spread", TRUE ~ Variable)) %>% 
  mutate(variable = forcats::as_factor(Variable))
SLOOS_IRF_TFP_news_fig1 <- readr::read_csv("SLOOS_IRF_TFP_news_fig1.csv") %>% 
  rename(horizon = Horizon,
         response = Resp,) %>% 
  #mutate(Variable = case_when(Variable == "GZ Spread" ~ "GZ spread", TRUE ~ Variable)) %>% 
  mutate(variable = forcats::as_factor(Variable))

### GZ spread (facet 1)
p_gz_spread <- plot_irf(data, 
                        c("tfp","output","consumption","hours","gz_spread","sp500","gdp_deflator"), 
                        c("gz_spread"), 
                        40,
                        40) + ylab(NULL)  + theme(legend.position="none") + ggtitle("GZ spread")+
  geom_line(data = filter(GZ_Spread_IRF_TFP_news_fig1, variable == "GZ spread"),  linetype = "dashed")+
  geom_ribbon(data = filter(GZ_Spread_IRF_TFP_news_fig1, variable == "GZ spread"), aes(ymin = LowerCI, ymax = HigherCI), alpha = 0.5)


### Excess bond premium (facet 2)

p_ebp <- plot_irf(data, 
                  c("tfp","output","consumption","hours","ebp","sp500","gdp_deflator"), 
                  c("ebp"), 
                  40,
                  40) + ylab(NULL)  + theme(legend.position="none") + ggtitle("Excess bond premium")+
  geom_line(data = filter(EBP_IRF_TFP_news_fig1, variable == "Excess Bond Premium"),  linetype = "dashed")+
  geom_ribbon(data = filter(EBP_IRF_TFP_news_fig1, variable == "Excess Bond Premium"), aes(ymin = LowerCI, ymax = HigherCI), alpha = 0.5)


### Default risk (facet 3)

p_default_risk<- plot_irf(data, 
                          c("tfp","output","consumption","hours","default_risk","sp500","gdp_deflator"), 
                          c("default_risk"), 
                          40,
                          40) + ylab(NULL)  + theme(legend.position="none") + ggtitle("Default risk")+
  geom_line(data = filter(DR_IRF_TFP_news_fig1, variable == "Default Risk"),  linetype = "dashed")+
  geom_ribbon(data = filter(DR_IRF_TFP_news_fig1, variable == "Default Risk"), aes(ymin = LowerCI, ymax = HigherCI), alpha = 0.5)


### Bank equity (facet 4)

p_bank_equity <- plot_irf(data, 
                          c("tfp","output","consumption","hours","bank_equity","sp500","gdp_deflator"), 
                          c("bank_equity"), 
                          40,
                          40,
                          TRUE) + ylab(NULL)  + theme(legend.position="none") + ggtitle("Bank equity")+
  geom_line(data = filter(BE_IRF_TFP_news_fig1, variable == "Bank Equity"),  linetype = "dashed")+
  geom_ribbon(data = filter(BE_IRF_TFP_news_fig1, variable == "Bank Equity"), aes(ymin = LowerCI, ymax = HigherCI), alpha = 0.5)


### SLOOS (facet 5)

p_sloos <- plot_irf(data %>% filter(is.na(sloos)==FALSE), 
                    c("tfp","output","consumption","hours","sloos","sp500","gdp_deflator"), 
                    c("sloos"), 
                    40,
                    40,
                    TRUE) + ylab(NULL)  + theme(legend.position="none") + ggtitle("SLOOS")+
  geom_line(data = filter(SLOOS_IRF_TFP_news_fig1, variable == "SLOOS"),  linetype = "dashed")+
  geom_ribbon(data = filter(SLOOS_IRF_TFP_news_fig1, variable == "SLOOS"), aes(ymin = LowerCI, ymax = HigherCI), alpha = 0.5)


### Plot all facets in same fig (Fig 2)
fig2 <- ggarrange(p_gz_spread, p_ebp, p_default_risk, p_bank_equity, p_sloos, nrow = 1, ncol = 5)
fig2

### Save Fig 2
ggsave(filename = "figures/fig2_alt.png", fig2,
       width = 12, height = 3, dpi = 300, units = "in", device='png')

### Save Fig 2

###############################################
###############################################
###############################################
### REPLICATE FIG 3
###############################################
###############################################
###############################################

# Set IRF length
h=40

# Select VAR-relevant series
data_var <- data %>%
  dplyr::select(tfp, output, consumption, hours, ebp, sp500, gdp_deflator)

# Estimate VAR
estim <- VAR(data_var, p = p, type = "const")
print("Estimated model roots: ")
print(summary(estim)$roots)

### Obtain all necessary (reduced-form) matrix objects 
A_mats <- get_A(estim, 40)
sigma_mat <- get_sigma(estim)
impact_mat <- t(chol(sigma_mat))

### Find max FEV EBP shock
output_optim_gamma = optim(par = c(1,1,1,1,1,1,1), 
                           fn = forecast_err_var, 
                           i = 5, 
                           A = A_mats, 
                           Sigma = sigma_mat, 
                           h=40, 
                           control = list(fnscale=-1), 
                           method = "L-BFGS-B",
                           lower=c(-0.000000000000001,rep(-Inf,6)), 
                           upper=c(0.000000000000001, rep(Inf,6)))
gamma_opt <- -output_optim_gamma$par/norm(output_optim_gamma$par,type="2")
if(gamma_opt[1] < 0){
  gamma_opt = -gamma_opt
}

### Generate IRFs
irfs <- irf(A_mats, impact_mat, gamma_opt, h) * 100

irfs_df <- data.frame(horizon = c(0:h),
                      tfp = irfs[,1],
                      output = irfs[,2],
                      consumption = irfs[,3],
                      hours = irfs[,4],
                      ebp = irfs[,5],
                      sp500 = irfs[,6],
                      gdp_deflator = irfs[,7])
names(irfs_df) <- c("horizon", 
                    "TFP",
                    "Output",
                    "Consumption",
                    "Hours",
                    "Excess bond premium",
                    "S&P 500",
                    "Inflation")

irfs_df_long <- pivot_longer(irfs_df, cols = -c("horizon"), names_to = "variable", values_to = "response") 

irfs_df_long$variable <- factor(irfs_df_long$variable, levels = c("TFP",
                                                                  "Output",
                                                                  "Consumption",
                                                                  "Hours",
                                                                  "Excess bond premium",
                                                                  "S&P 500",
                                                                  "Inflation"))
irfs_df_long = irfs_df_long %>% 
  mutate(shock = "Max FEV EBP")

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
if(gamma_opt[1] < 0){
  gamma_opt = -gamma_opt
}

### Generate IRFs
irfs <- irf(A_mats, impact_mat, gamma_opt, h) * 100

irfs_df <- data.frame(horizon = c(0:h),
                      tfp = irfs[,1],
                      output = irfs[,2],
                      consumption = irfs[,3],
                      hours = irfs[,4],
                      ebp = irfs[,5],
                      sp500 = irfs[,6],
                      gdp_deflator = irfs[,7])
names(irfs_df) <- c("horizon", 
                    "TFP",
                    "Output",
                    "Consumption",
                    "Hours",
                    "Excess bond premium",
                    "S&P 500",
                    "Inflation")

irfs_df_long2 <- pivot_longer(irfs_df, cols = -c("horizon"), names_to = "variable", values_to = "response") 

irfs_df_long2$variable <- factor(irfs_df_long2$variable, levels = c("TFP",
                                                                    "Output",
                                                                    "Consumption",
                                                                    "Hours",
                                                                    "Excess bond premium",
                                                                    "S&P 500",
                                                                    "Inflation"))
irfs_df_long2 = irfs_df_long2 %>% 
  mutate(shock = "News shock")

### Combine IRF datasets
irfs_df_long = rbind(irfs_df_long2, irfs_df_long)

### Reorder shocks
irfs_df_long$shock <- factor(irfs_df_long$shock, levels = c("News shock", "Max FEV EBP"))

### Plot IRFs (Fig 3)
fig3 <- ggplot(irfs_df_long, aes(x=horizon, y=response, linetype=shock)) +
  geom_line() +
  facet_wrap(. ~ variable, scale = "free", nrow=2) +
  theme_bw() +
  xlab("Quarters") + ylab(NULL) + theme(legend.position="none")


### Save Fig 3
ggsave(filename = "figures/fig3_alt.png", fig3,
       width = 8, height = 4, dpi = 300, units = "in", device='png')
fig3