## replicate and update data set for the VAR in Christoph Görtz, John D. Tsoukalas, and Francesco Zanetti, 
## "News Shocks under Financial Frictions", AEJ 2022
## SD Replication Games July 1 2023

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)
library(readxl)
library(fredr)
library(purrr)
library(forcats)


fredr_set_key("266c597cbf3d25f366b082b4bf5161fe")

# NOTE: the PCDGCC96  PCESVC96 PCNDGC96 series (real chain weighted PCE components) used in the paper are not published for years prior to 2002 any longer
# They're apparently used to calculate growth rates of real consumption; I'm substituting the quantity indexes
# Durable goods: DDURRA3Q086SBEA
# Nondurable goods: DNDGRA3Q086SBEA
# Services: DSERRA3Q086SBEA


params <- list(
  series_id = c("GDP","GPDI","GPDIC1","PCDG", "PCDGCC96","DDURRA3Q086SBEA", "PCESV",
                "PCESVC96","DSERRA3Q086SBEA", "PCND","PCNDGC96","DNDGRA3Q086SBEA","CNP16OV","COMPNFB","HOANBS","FEDFUNDS", "GDPDEF", "CPIAUCSL","BAA10YM", "DRTSCILM"),
  frequency = "q"
)


FRED_data <- pmap_dfr(
  .l = params,
  .f = ~ fredr(series_id = .x, frequency = .y, aggregation_method = "avg", observation_start = as.Date("1960-01-01"))
)

macro_data <- FRED_data %>% 
  select(date:value) %>% 
  pivot_wider(names_from = series_id)

transformed_macro_data <- macro_data %>% 
  mutate(gdpdef_infl = 100*(log(GDPDEF) - log(lag(GDPDEF)))) %>% 
  mutate(consumption = PCESV + PCND,
         investment = GPDI + PCDG) %>% 
  mutate(nom_c_share = consumption/(consumption + investment),
         nom_i_share = investment/(consumption + investment),
         i_c_ratio = investment/consumption) %>% 
  mutate(nom_nd_share = PCND/consumption,
         nom_serv_share = PCESV/consumption) %>% 
  arrange(date) %>% 
  mutate(avg_share_nd = (nom_nd_share + lag(nom_nd_share))/2,
         avg_share_sv = (nom_serv_share + lag(nom_serv_share))/2) %>% 
  ## this is where we use the quantity index instead of the chained growth used in the OG paper
  mutate(real_nd_growth = 100*(DNDGRA3Q086SBEA/lag(DNDGRA3Q086SBEA) - 1),
         real_s_growth = 100*(DSERRA3Q086SBEA/lag(DSERRA3Q086SBEA) -1) ) %>% 
  # chain weighted measures of growth
  mutate(total_c_growth_cw = avg_share_nd*real_nd_growth +avg_share_sv*real_s_growth) %>% 
  mutate(real_consumption = NA)

base_period_loc <- which(transformed_macro_data$date == "2004-10-01")

transformed_macro_data$real_consumption[base_period_loc] <- transformed_macro_data$consumption[base_period_loc]


# chain weighting is a little goofy 

for (i in seq(from =(base_period_loc-1), to =1)) {
  transformed_macro_data$real_consumption[[i]] <- transformed_macro_data$real_consumption[[i+1]] /(1 + transformed_macro_data$total_c_growth_cw[[i]]/100)
}
for (i in seq(from =(base_period_loc+1), to =nrow(transformed_macro_data))) {
  transformed_macro_data$real_consumption[[i]] <- transformed_macro_data$real_consumption[[i-1]] *(1 + transformed_macro_data$total_c_growth_cw[[i]]/100)
}

transformed_macro_data <- transformed_macro_data %>% 
  mutate(c_deflator = consumption/real_consumption ) %>% 
  mutate(c_inflation = 100*(log(c_deflator) - log(lag(c_deflator))))

# investment shares
transformed_macro_data <- transformed_macro_data %>% 
  mutate(gdpi_share = GPDI/investment,
         dg_share = PCDG/investment) %>% 
  mutate(avg_share_gdpi = (gdpi_share + lag(gdpi_share))/2,
         avg_share_dg = (dg_share + lag(dg_share))/2) %>% 
  ## this is where we use the quantity index instead of the chained growth used in the OG paper
  mutate(real_gdpi_growth = 100*(GPDIC1/lag(GPDIC1) - 1),
         real_dg_growth = 100*(DDURRA3Q086SBEA/lag(DDURRA3Q086SBEA) -1) ) %>% 
  # chain weighted measures of growth
  mutate(nom_total_i_growth_cw = avg_share_gdpi*real_gdpi_growth +avg_share_dg*real_dg_growth) %>% 
  mutate(real_investment = NA)


transformed_macro_data$real_investment[base_period_loc] <- transformed_macro_data$investment[base_period_loc]


# chain weighting is a little goofy 

for (i in seq(from =(base_period_loc-1), to =1)) {
  transformed_macro_data$real_investment[[i]] <- transformed_macro_data$real_investment[[i+1]] /(1 + transformed_macro_data$nom_total_i_growth_cw[[i]]/100)
}
for (i in seq(from =(base_period_loc+1), to =nrow(transformed_macro_data))) {
  transformed_macro_data$real_investment[[i]] <- transformed_macro_data$real_investment[[i-1]] *(1 + transformed_macro_data$nom_total_i_growth_cw[[i]]/100)
}





transformed_macro_data <- transformed_macro_data %>% 
  mutate(i_deflator = investment/real_investment ) %>% 
  mutate(i_inflation = 100*(log(i_deflator) - log(lag(i_deflator)))) %>% 
  mutate(RPI = i_deflator/c_deflator)  # relative price of investment




# real GDP, I, C per capita
## Per-capita stuff is divided by a scaled version of population where pop is scaled to Q3 1992
# This is not mentioned anywhere that I can find, and it's not obvious why they pick that year
base_period_pop <- transformed_macro_data$CNP16OV[which(transformed_macro_data$date == "1992-07-01")]


transformed_macro_data <- transformed_macro_data %>% 
  mutate(civpop_index = CNP16OV/base_period_pop) %>% 
  mutate(real_gdp_pc = log((GDP/c_deflator)/civpop_index),
         real_c_pc = log(real_consumption/civpop_index),
         real_i_pc = log(real_investment/civpop_index),
         hours_pc = log(HOANBS/civpop_index))
#       real_wage = log(COMPNFB/c_deflator))x``
## most recent Fernald TFP vintage; they normalize the level to 1 in Q4 1983, and then take logs

## TODO: is there a way to avoid hard-coding to skip the averages at the end of the sheet?

fernald_tfp <- read_excel("data/fernald_vintages/data_quarterly_2023.02.14.xlsx", 
                          sheet = "quarterly", range ="A2:V306") %>% 
  #select(date,dtfp_util, dtfp_I_util, dtfp_C_util) %>% 
  select(date,dtfp_util) %>% 
  mutate(date = yq(date)) %>% 
  mutate(tfp_util = NA) 



base_period_tfp <- which(fernald_tfp$date == "1983-10-01")


fernald_tfp$tfp_util[base_period_tfp] <- 1



for (i in seq(from =(base_period_tfp-1), to =1)) {
  fernald_tfp$tfp_util[[i]] <- fernald_tfp$tfp_util[[i+1]] /(1 + fernald_tfp$dtfp_util[[i]]/100)
}
for (i in seq(from =(base_period_tfp+1), to =nrow(fernald_tfp))) {
  fernald_tfp$tfp_util[[i]] <- fernald_tfp$tfp_util[[i-1]] *(1 + fernald_tfp$dtfp_util[[i]]/100)
}


fernald_tfp$tfp_util_norm <- log(fernald_tfp$tfp_util)

#### FINANCIAL DATA

ebp_csv <- read_csv("data/ebp_csv.csv", col_types = cols(date = col_date(format = "%m/%d/%Y")))

ebp_q <- ebp_csv %>% 
  group_by(year(date), quarter(date)) %>% 
  summarise(ebp = mean(ebp), gz_spread = (mean(gz_spread)), defaultrisk = mean(est_prob) ) %>% 
  mutate(date = make_date(year = `year(date)`, month = 3*`quarter(date)`-2)) %>% 
  ungroup() %>% 
  select(date, ebp, gz_spread, defaultrisk)

defl_pop <- transformed_macro_data %>% 
  select(date,civpop_index, c_deflator ) 

# Shiller, downloaded from http://www.econ.yale.edu/~shiller/data/ie_data.xls on July 1 2023

shiller_ie_data <- read_excel("data/shiller_ie_data.xls", 
                              sheet = "Data", range = "A9:B1835", col_names = c("date", "sp500"), col_types = c("numeric", "numeric")) %>% 
  mutate(yr = round(date)) %>% 
  mutate(mth = round(100*(date - yr))) %>% 
  mutate(date = make_date(year = yr, month = mth, day = 1)) %>% 
  select(date, sp500) %>% 
  group_by(year(date), quarter(date)) %>% 
  summarise(sp500 = mean(sp500)) %>% 
  mutate(date = make_date(year = `year(date)`, month = 3*`quarter(date)`-2)) %>% ungroup() %>% 
  select(date, sp500) #%>% 
  #mutate(sp500 = log(sp500))

shiller_ie_data <- right_join(shiller_ie_data,defl_pop ) %>% 
  mutate(sp500 = log( (sp500/c_deflator)/civpop_index)) %>% 
  select(date, sp500)
  
#mutate(date = as_date(date, format = "%Y.%mm"))

## their nominal bank value

nom_bank_value <- read_excel("data/finaldata072015.xlsx", 
                              sheet = "DSGE Model data", range = "DT8:DT148", 
                              col_names = "nom_bank_val") %>% 
  mutate(date = seq(from = ymd("1982-01-01"), to = ymd("2017-01-01"), by = "3 months"))



bank_val <- left_join(nom_bank_value, defl_pop) %>% 
  mutate(real_bank_val_pc = log( (nom_bank_val/c_deflator)/civpop_index)) %>% 
  select(date, real_bank_val_pc)


### put it all together; this corresponds to the 13 variables on the sheet "84Q1-2017Q1" in the replication excel file

var_data_update <- list(transformed_macro_data, bank_val, shiller_ie_data,fernald_tfp,ebp_q) %>% reduce(full_join, by = "date") %>% 
  select(date,tfp_util_norm,real_gdp_pc, real_i_pc, real_c_pc, hours_pc,sp500, gdpdef_infl, ebp, gz_spread,defaultrisk, real_bank_val_pc,DRTSCILM,BAA10YM)


var_data_update_samp <- var_data_update %>% 
  filter(date >= yq("1984:Q1") & date <= yq("2017:Q1")) %>% 
#rescaling
  mutate(defaultrisk = defaultrisk/100, gdpdef_infl = 4*gdpdef_infl/100, gz_spread = gz_spread/100,ebp = ebp/100,DRTSCILM = DRTSCILM/100,BAA10YM = BAA10YM/100)

colnames(var_data_update_samp) <- c("date", "TFP",	"Real GDP per capita",	"Real Investment per capita",	"Real consumption per capita"	,"Hours per capita","SP500", "Inflation (Implicit GDP deflator)",	"ebp",	"gz_spr",	"defaultrisk","RMV Banks",	"LOOS: Reporting tighter standards","BAA SPREAD")

write_csv(var_data_update_samp, "data/var_data_update.csv")

# compare updated to in the replication file
finaldata072015 <- read_excel("data/finaldata072015.xlsx", 
                              col_names = FALSE, range = "A3:N135")
colnames(finaldata072015) <- c("date", "TFP",	"Real GDP per capita",	"Real Investment per capita",	"Real consumption per capita"	,"Hours per capita","SP500", "Inflation (Implicit GDP deflator)",	"ebp",	"gz_spr",	"defaultrisk","RMV Banks",	"LOOS: Reporting tighter standards","BAA SPREAD")
finaldata072015$date <-as_date(finaldata072015$date)
var_data_update_samp_tidy <- var_data_update_samp %>% 
  pivot_longer(cols = -date) %>% 
  mutate(name = as_factor(name)) %>% 
  ungroup()

finaldata072015_tidy <- finaldata072015 %>% 
  pivot_longer(cols = -date)
 
 ggplot(var_data_update_samp_tidy, aes(x = date, y = value)) +
   geom_line(color = "black", linewidth = 1)+
   facet_wrap(facets = vars(forcats::as_factor(name)), scales = "free")+
   geom_line(data = finaldata072015_tidy, color = "blue", linetype = "dashed")+
   theme_classic()+
   ggtitle("Updated data (black, solid) and replication package data (blue, dashed)")
 
 ggsave(filename = "figures/data_replication.png", bg = "white",dpi = "retina", scale = 2, width=160*.9, height =90*.9, units = "mm")
 
 
 
 ## sources of differences:
 # RMV is because of population scaling, which I changed to make similar to the other per-capita variables
 # default risk -- not sure, perhaps change in source data handling.  it's not a matter of annualizing.  They don't report any transformations
