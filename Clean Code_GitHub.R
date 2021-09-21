# Analysis code

# Loading packages
extrafont::loadfonts(device = "win")

require(tidyverse)
require(tidylog)
require(readxl)
require(DataExplorer)
require(caret)
require(kohonen)
require(aweSOM)
require(ParBayesianOptimization)
require(caret)
require(doParallel)
require(doSNOW)
require(iml)
require(future)
require(future.callr)
require(cowplot)
require(GGally)
require(fpc)


# Auxiliary function

# Estimate season of the year
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  factor(ifelse (d >= WS | d < SE, "Winter",
                 ifelse (d >= SE & d < SS, "Spring",
                         ifelse (d >= SS & d < FE, "Summer", "Fall"))),
         levels = c("Winter", "Spring", "Summer", "Fall"))
}



# Loading data ####
pa <- readRDS("~/proAction_anon_hrd_tbl.rds")
lpl <- readRDS("~/prod_lifetime_anon_anm_tbl.rds")
research_data <- readRDS("~/res_data_anon_anm_tbl.rds")
hsi <- readRDS("~/whi_indicators_anon_tbl.rds")


# Generating a more readable herd ID and animal ID 
unique_hrd_id <- pa %>% 
  select(id) %>% 
  drop_na() %>% 
  rbind(lpl %>% select(id),
        research_data %>% select(id),
        hsi %>%  select(id)) %>% 
  distinct(id) %>% 
  mutate(hrd_id = seq_along(id))


# Cleaning data ####

# DHI

# Replacing herd and animal ids to more readable characters
hsi <- hsi %>% 
  drop_na(id) %>% 
  left_join(unique_hrd_id, by = "id") %>% 
  select(hrd_id, setdiff(names(hsi), c("id")))


# Keeping only herds with at least 9 tests over the three-year period
# Creating variables to calculate the index

hsi1 <- hsi %>% 
  filter(cnttests >=9) %>% 
  
  mutate(sold_dairy_pcntg = (sold_dairy_cnt / avg_num_cows)*100,
         involuntary_pcntg = avg_pcntg_trnvr - sold_dairy_pcntg,
         avg_mortality_pcntg = (num_cow_dead_yr/avg_num_cows)*100,
         
         # Converting age at first calving from days to months
         age1stcalv = (age1stcalv/365.25)*12) %>%  
  
  select(hrd_id, avg_num_cows, cnttests, avg_pcntg_lgvt, involuntary_pcntg,
         avg_mortality_pcntg, pctmun5, pctpfratio11, enercorr_mi, avg_tci_yr,
         day_life_value_rank, pctcalfdead, age1stcalv, pct_lact_start_abortion,
         pctbhb20, pctccs400)



# Exploratory data analysis
plot_missing(hsi1)
plot_bar(hsi1)
plot_histogram(hsi1)


summary(hsi1)



# Identifying and removing outliers

# Average percentage of cows on 3+ lactation

hsi1 %>% 
  pull(avg_pcntg_lgvt) %>% 
  hist()

hsi1 %>% 
  pull(avg_pcntg_lgvt) %>% 
  summary()

# Not much indication of problem here!


# Involuntary culling
hsi1 %>% 
  pull(involuntary_pcntg) %>% 
  hist()

hsi1 %>% 
  pull(involuntary_pcntg) %>% 
  summary()




hsi1 %>% 
  filter(involuntary_pcntg >= 0, involuntary_pcntg <= 100) %>% 
  pull(involuntary_pcntg) %>% 
  hist()

hsi1 %>% 
  filter(involuntary_pcntg >= 0, involuntary_pcntg <= 100) %>% 
  pull(involuntary_pcntg) %>% 
  summary()

# Removing observations where percentage of involuntary culling is higher than 100%
# and lower than 0%
hsi2 <- hsi1 %>% 
  filter(involuntary_pcntg >= 0, involuntary_pcntg <= 100)




# Average percentage of mortality
hsi2 %>% 
  pull(avg_mortality_pcntg) %>% 
  hist()

hsi2 %>% 
  pull(avg_mortality_pcntg) %>% 
  summary()
# No indication of problem here!


# Milk urea nitrogen

hsi2 %>% 
  pull(pctmun5) %>% 
  hist()

hsi2 %>% 
  pull(pctmun5) %>% 
  summary()
# Data is very skewed, but there is not much justifiable reason to remove observations!


# Protein to fat ratio

hsi2 %>%  
  pull(pctpfratio11) %>% 
  hist()

hsi2 %>% 
  pull(pctpfratio11) %>% 
  summary()
# Data is very skewed, but there is not much justifiable reason to remove data!


# Energy corrected milk

hsi2 %>%  
  pull(enercorr_mi) %>% 
  hist()

hsi2 %>% 
  pull(enercorr_mi) %>% 
  summary()
# No indication of problem here!


# Transition cow index

hsi2 %>%  
  pull(avg_tci_yr) %>% 
  hist()

hsi2 %>% 
  pull(avg_tci_yr) %>% 
  summary()
# No indication of problem here!


# day_life_value_rank

hsi2 %>%  
  pull(day_life_value_rank) %>% 
  hist()

hsi2 %>% 
  pull(day_life_value_rank) %>% 
  summary()
# No indication of problem here!


# Calf mortality

hsi2 %>%  
  pull(pctcalfdead) %>% 
  hist()

hsi2 %>% 
  pull(pctcalfdead) %>% 
  summary()
# Data is very skewed, but there is not much justifiable reason to remove data!


# Age at 1st calving

hsi2 %>% 
  pull(age1stcalv) %>% 
  hist()

hsi2 %>% 
  pull(age1stcalv) %>% 
  summary()


# Impossible age at first calving is impossible. Removing those observations
hsi3 <- hsi2 %>% 
  filter(age1stcalv > 0)


hsi3 %>% 
  pull(age1stcalv) %>% 
  hist()

hsi3 %>% 
  pull(age1stcalv) %>% 
  summary()


# Abortion

hsi3 %>%  
  pull(pct_lact_start_abortion) %>% 
  hist()

hsi3 %>% 
  pull(pct_lact_start_abortion) %>% 
  summary()
# Data is very skewed, but there is not much justifiable reason to remove data!


# beta hydroxybutyrate

hsi3 %>%  
  pull(pctbhb20) %>% 
  hist()

hsi3 %>% 
  pull(pctbhb20) %>% 
  summary()
# No indication of problem here!


# Somatic cell count

hsi3 %>%  
  pull(pctccs400) %>% 
  hist()

hsi3 %>% 
  pull(pctccs400) %>% 
  summary()
# No indication of problem here!



# Calculating a final average per herd and removing herds with more than 3 variables
# with missing observations

hsi4 <- hsi3 %>%
  select(-cnttests) %>% 
  group_by(hrd_id) %>% 
  summarise_if(is.numeric, mean, na.rm = T) %>% 
  ungroup() %>% 
  mutate(na = rowSums(is.na(.))) %>%
  filter(na <= 3) %>% 
  select(-na)
  


# Cleaning welfare outcome measures data
# Replacing herd and animal ids to more readable characters

pa <- pa %>% 
  drop_na(id) %>% 
  left_join(unique_hrd_id, by = "id") %>% 
  select(hrd_id, setdiff(names(pa), c("id")))


# Removing herds with two obs on the same day (duplicated observations)
# and keeping the most recent evaluation for herds with more than one 
# observations but not on the same day

pa1 <- pa %>% 
  mutate(visit_date = as.Date(visit_date, format = "%B %d, %Y")) %>% 
  
  # Duplicated observations
  group_by(hrd_id, visit_date) %>%
  add_count() %>%
  ungroup() %>%
  filter(n == 1) %>%
  
  # Keeping the most recent
  group_by(hrd_id) %>% 
  arrange(desc(visit_date), .by_group = TRUE) %>% 
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  filter(n == 1) %>% 
  select(-n)



# Welfare responses indicate the percentage of animals where the welfare measure
# was not an issue. This could be harder to understand. So I will convert it to indicate 
# the percentage of animals where the welfare measure indicated an issue.
# Also calculating season of the assessment


pa2 <- pa1 %>% 
  
  mutate(season = getSeason(visit_date),
         year = lubridate::year(visit_date),
         BCS = 100 - bcs_percent,
         HOCK = 100 - hock_percent,
         KNEE = 100- knee_percent, 
         NECK = 100 - neck_percent,
         LAME = 100 - lame_percent) %>% 
  
  # Removing negative values for prevalence
  filter(BCS >= 0, HOCK >= 0, KNEE >= 0, NECK >= 0, LAME >= 0) %>%
  
  # Keeping only free-stall and tie-stall barns
  filter(barn_type %in% c("Freestall", "Tiestall")) %>% 
  
  mutate_if(is.character, as.factor) %>% 
  select(hrd_id, visit_date, milking, season, year, barn_type, BCS, HOCK,
         KNEE, NECK, LAME)



# Merging DHI and proAction datasets

wd1 <- inner_join(pa2, hsi4, by = "hrd_id")



# Calculating the Herd Status Index (HSI)

rank <- c("avg_pcntg_lgvt", "enercorr_mi", "avg_tci_yr")

recp.rank <- c("involuntary_pcntg", "avg_mortality_pcntg", "pctmun5",
               "pctpfratio11", "day_life_value_rank", "pctcalfdead",
               "age1stcalv", "pct_lact_start_abortion", "pctbhb20",
               "pctccs400" )




wd2 <- wd1 %>% 
  
  # Getting number of indicators with no missing observations
  select(rank, recp.rank) %>% 
  mutate(n_indexes = 13-rowSums(is.na(.))) %>% 
  select(n_indexes) %>% 
  bind_cols(wd1, .) %>% 
  
  # Percentile ranking indicators
  mutate_at(rank, percent_rank) %>% 
  
  # Reciprocal of percentile ranking indicators
  mutate_at(recp.rank, ~ 1 - percent_rank(.)) %>% 
  
  # Calculating the Index
  mutate(agreg_ind = rowSums(select(., rank, recp.rank), na.rm = TRUE),
         HSI = agreg_ind/n_indexes) %>% 
  
  # Dropping intermediary variables
  select(-n_indexes, -agreg_ind)



# Data file with proAction, HSI, and HSI indicators
wd3 <- wd2 %>% 
  select(hrd_id, HSI) %>% 
  left_join(wd1, by = "hrd_id") %>% 
  mutate(hrd_id = as.factor(hrd_id))



# Performance data
lpl1 <- lpl %>% 
  drop_na(id) %>% 
  left_join(unique_hrd_id, by = "id") %>% 
  
  # Remove negative values on length of productive life (LPL)
  filter(prod_lt > 0) %>% 
  
  # Convert LPL to year
  mutate(lpl = prod_lt/365.25) %>% 
  select(hrd_id, lpl) %>% 
  
  # Aggregate LPL to herd level
  group_by(hrd_id) %>% 
  summarise(lpl = mean(lpl))


# On research data, production and economics are provided by lactation
# Have to calculate the sum for the each animal to get a herd average
# Some animals moved between herds. For those animals, I am keeping
# production to the herd in which they finished the current lactation


# Getting number of unique cows per herd to calculate the average of the sum 
anm_herd_n <- research_data %>% 
  
  # Replacing herd and animal ids to more readable characters
  drop_na(id) %>% 
  left_join(unique_hrd_id, by = "id") %>% 
  
  # Removing NAs on production variables because I need complete 
  # obs to calculate ECM. Also, removing production with Zero values
  
  # There are also some Zeros and NAs on milk value, despite having info
  # on production. Removing these observations as well.
  
  drop_na(c(lact_date_yld_milk, lact_date_yld_fat,
            lact_date_yld_prot, cumul_milk_value)) %>% 
  
  filter(lact_date_yld_milk != 0,
         lact_date_yld_fat != 0,
         lact_date_yld_prot != 0,
         cumul_milk_value != 0) %>% 
  
  group_by(hrd_id) %>% 
  summarise(n_unique_anm = n_distinct(anm_id_anon))




wd4 <- research_data %>% 
  
  # Replacing herd and animal ids to more readable characters
  drop_na(id) %>% 
  left_join(unique_hrd_id, by = "id") %>% 
  
  
  # Removing NAs on production variables because I need complete 
  # obs to calculate ECM. Also, removing production values of ZERO
  
  # There are also some Zeros and NAs on milk value, despite having info
  # on production. Removing this observations as well.
  
  drop_na(c(lact_date_yld_milk, lact_date_yld_fat,
            lact_date_yld_prot, cumul_milk_value)) %>% 
  
  filter(lact_date_yld_milk != 0,
         lact_date_yld_fat != 0,
         lact_date_yld_prot != 0,
         cumul_milk_value != 0) %>% 
  
  select(hrd_id, lact_date_yld_milk, lact_date_yld_fat, 
         lact_date_yld_prot, cumul_milk_value) %>% 
  
  # Calculate ECM
  mutate(ecm = 12.55*lact_date_yld_fat + 
           7.39*lact_date_yld_prot + 
           0.2595*lact_date_yld_milk) %>%
  
  # Calculate cumulative sum
  group_by(hrd_id) %>% 
  summarise(across(everything(), sum)) %>% 
  
  
  # Calculate cumulative herd average
  left_join(anm_herd_n, by = "hrd_id") %>% 
  mutate(lact_date_yld_milk = lact_date_yld_milk/n_unique_anm,
         lact_date_yld_fat = lact_date_yld_fat/n_unique_anm,
         lact_date_yld_prot = lact_date_yld_prot/n_unique_anm,
         ecm = ecm/n_unique_anm,
         cumul_milk_value = cumul_milk_value/n_unique_anm) %>% 
  
  
  # Merge with LPL data based on herd ID
  inner_join(lpl1,
             by = "hrd_id") %>% 
  
  
  # Get final working data by merging with welfare data
  
  mutate(hrd_id = as.factor(hrd_id)) %>% 
  inner_join(wd3 %>% select(hrd_id, visit_date, milking, 
                            avg_pcntg_lgvt, barn_type, season, year, 
                            BCS, HOCK, KNEE, NECK, LAME, HSI),
             by = "hrd_id")



# Exploratory analysis of final working data ####
summary(wd4$visit_date)

# Getting descriptive stats
wd4 %>% 
  select(BCS, HOCK, KNEE, NECK, LAME, HSI, ecm, cumul_milk_value, lpl, avg_pcntg_lgvt) %>% 
  summarize_if(is.numeric, mean) %>% 
  as.data.frame() %>% 
  round(2)

wd4 %>% 
  select(BCS, HOCK, KNEE, NECK, LAME, HSI, ecm, cumul_milk_value, lpl, avg_pcntg_lgvt) %>% 
  summarize_if(is.numeric, sd) %>% 
  as.data.frame() %>% 
  round(2)

wd4 %>% 
  select(BCS, HOCK, KNEE, NECK, LAME, HSI, ecm, cumul_milk_value, lpl, avg_pcntg_lgvt) %>% 
  summarize_if(is.numeric, min) %>% 
  as.data.frame() %>% 
  round(1)

wd4 %>% 
  select(BCS, HOCK, KNEE, NECK, LAME, HSI, ecm, cumul_milk_value, lpl, avg_pcntg_lgvt) %>% 
  summarize_if(is.numeric, max) %>% 
  as.data.frame() %>% 
  round(1)



plot_missing(wd4)
plot_bar(wd4)
plot_histogram(wd4)



# Self-Organizing Map (SOM) ####

# Preparing data for SOM

# Creating a SOM with two layers
# 1) First layer with descriptive supplementary variables: barn, year, season
# 2) Second layer with the welfare outcome measures

# Euclidean distance will be used


# Treating year as numeric, one-hot encoding barn type, and cyclic encoding season

dummy <- dummyVars(" ~ barn_type",
                   data = wd4)

data <- wd4 %>% 
  
  # One-hot encoding barn type
  predict(dummy, .) %>%
  cbind(wd4 %>% 
          select(season, year, BCS, HOCK, KNEE, NECK, LAME, HSI)) %>% 
  
  # Encoding season as cyclic variable
  mutate(season_cyclic = ifelse(season == "Winter", 1,
                                ifelse(season == "Spring", 2,
                                       ifelse(season == "Summer", 3,
                                              ifelse(season == "Fall", 4, 99999999)))),
         sin_season = sin((2*pi*season_cyclic)/length(unique(season))),
         cos_season = cos((2*pi*season_cyclic)/length(unique(season)))) %>%
  
  select(-season, -season_cyclic)



# Creating data and distance lists to use on SOM
supplement <- c("barn_type.Freestall", "barn_type.Tiestall", "sin_season", 
                "cos_season", "year")

welfare <- setdiff(names(data), supplement)


data_list <- list()
distances <- vector()


data_list[["supplement"]] <- scale(data[, supplement])
distances <- c(distances, "euclidean")


data_list[["welfare"]] <- scale(data[ ,welfare])
distances <- c(distances, "euclidean")


# SOM hyperparameters

# Number of units will be 5 times the squared root of number of observations (DOI: 10.1007/978-3-642-56927-2)

n_units <- function(data) {
  
  nVariables <- length(data)
  n <- length(row.names(data))
  
  grid <- 5*sqrt(n)
  
  short <- round(sqrt(grid/1.5))
  long <- round(1.5*short)
  
  n_units <- short*long
  
  return(list(short, long, n_units))
  
}

n_units(data)


# Creating the SOM grid

som_grid <- somgrid(xdim = n_units(data)[[1]],
                    ydim = n_units(data)[[2]],
                    neighbourhood.fct = "gaussian",
                    topo = "hexagonal")


# Number of iterations is 500 times the number of units; Kohonen rule of thumb (DOI: 10.1007/978-3-642-56927-2)
n_iterations <- n_units(data)[[3]] * 500




# Will be using the pbatch learning algorithm because it can be run in parallel 
# Defying the number of cores available

n_cores <- parallel::detectCores(); n_cores


## Bayesian optimization of layers' weights ####

# Layers weight will be defined using Bayesian Optimization
# Necessary to make sure that both layers are equally represented

# The SOM function already apply a internal weighting scheme, but it was not enough in this case.

# The quantization error will be used as the base to measure the quality of the SOM.
# It represent the average squared distance between the data points and 
# the map's prototypes to which they are mapped. Lower is better.

# The quantization error is calculated for each layer individually, but the Bayesian 
# optimization accept one loss value to be minimized. Average alone of both layers was 
# not working. Supplement layer was too low and the welfare layer was not that good.
# Tried different weighted averages (variance layers and number of variables), 
# but none worked well.

# The approach with the best results was using am adaption of the root mean squared error.
# Since this metric considered both layers at the same time in the calculation and it is
# adjusted by the overall mean (I also tried Mean Absolute Error, but it was not good!!)


# The following function calculates the quantization error. It was adapted based 
# on the function somQuality from the package aweSOM (version 1.2), which only 
# works for SOM with a single layer (traditional SOM)

somQuantError <- function (som, traindat, layer) {
  
  bmu <- som$unit.classif
  sqdist <- rowSums((traindat - layer[bmu, ])^2, na.rm = TRUE)
  err.quant <- mean(sqdist)
  
  res <- list(err.quant = err.quant)
  res
}



# The following function calculates the "adapted" RMSE to be used as a loss function.
cost_Funct <- function(welfare, supplement, mean){
  
  welf_power_error <- (welfare - mean)^2
  sup_power_error <- (supplement - mean)^2
  
  sqrt((welf_power_error + sup_power_error)/2)
  
}

# Creating function to run the weights optimization 

som_bayes <- function(supplement.weight, welfare.weight) {
  
  set.seed(1801)
  m1 <- supersom(data = data_list,
                 grid = som_grid,
                 rlen = 1e4, # 10k Fast learning (Kohonen, 2001; SOM Book; DOI: 10.1007/978-3-642-56927-2)
                 user.weights = c(supplement.weight, 
                                  welfare.weight),
                 mode = "pbatch",
                 cores = n_cores,
                 dist.fcts = distances,
                 whatmap = c("supplement", "welfare"),
                 normalizeDataLayers = TRUE)
  
  
  supplement_quant_error <- somQuantError(som = m1,
                                          traindat = data_list[["supplement"]],
                                          layer = m1$codes$supplement)$err.quant
  
  welfare_quant_error <- somQuantError(som = m1,
                                       traindat = data_list[["welfare"]],
                                       layer = m1$codes$welfare)$err.quant
  
  
  # The optimization maximizes a loss function, but in this case the objective is to 
  # minimize the adapted RMSE. Therefore, I will be adding a negative sign to it
  
  mean_error <- (supplement_quant_error + welfare_quant_error)/2
  RMSE_cost_value <- -cost_Funct(welfare = welfare_quant_error, 
                                 supplement = supplement_quant_error,
                                 mean = mean_error)
  
  return(list(Score = RMSE_cost_value))
  
}


# Setting the search grid of the hyperparameters based on a uniform distribution

size <- 30

set.seed(1801)
search_grid <- data.frame(supplement.weight = runif(size, 1, 20),
                          welfare.weight = runif(size, 1, 20))


# Running Bayesian optimization

start.opt <- Sys.time()
start.opt

set.seed(1801)
OPT_Res <- bayesOpt(FUN = som_bayes,
                    bounds = list(supplement.weight = c(1, 20), 
                                  welfare.weight = c(1, 20)),
                    initGrid = search_grid,
                    iters.n = 20, 
                    acq = "ei",
                    eps = 0.01, # Based on DOI: 10.23915/distill.00026
                    verbose = 1,
                    plotProgress = TRUE)

OPT_Res1 <-  addIterations(OPT_Res,
                           iters.n = 20,
                           verbose = 2,
                           plotProgress = TRUE)



end.opt <-Sys.time()

getBestPars(OPT_Res1)



## Training FINAL SOM ####

start <- Sys.time()
start

set.seed(1801)
m1 <- supersom(data = data_list,
               grid = som_grid,
               rlen = n_iterations,
               user.weights = c(getBestPars(OPT_Res1)$supplement.weight,
                                getBestPars(OPT_Res1)$welfare.weight), # Suppl = 5.769437, Welfare = 15.54552
               mode = "pbatch",
               cores = n_cores,
               dist.fcts = distances,
               whatmap = c("supplement", "welfare"),
               normalizeDataLayers = TRUE)

end <- Sys.time()


## Evaluating the SOM ####

{
  par(mfrow = c(1, 3),
      mar=c(5.1, 4.1, 4.1, 2.1))
  
  plot(m1, type="changes")
  plot(m1, type="dist.neighbours", whatmap = "welfare",
       shape = "straight",
       palette.name = function(n) 
         gray.colors(n, start = 0, end = 1, gamma = 0.5)[n:1])
  plot(m1, type = "count")
  
  par(mfrow = c(1, 1),
      mar=c(5.1, 4.1, 4.1, 2.1))
}


# Error of the different layers

supplement_quant_error <-  somQuantError(som = m1,
                                         traindat = data_list[["supplement"]],
                                         layer = m1$codes$supplement)$err.quant

welfare_quant_error <- somQuantError(som = m1,
                                     traindat = data_list[["welfare"]],
                                     layer = m1$codes$welfare)$err.quant

supplement_quant_error; welfare_quant_error


mean(c(supplement_quant_error, welfare_quant_error))




## SOM heatmaps of welfare outcome measures  ####

filtering.function <- function(x, y) {
  x[!(x %in% y)]
}

coolBlueHotRed <- function(n, alpha = 1) 
{
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}



{
  par(mfrow = c(2, 3),
      mar=c(5.1, 4.1, 4.1, 2.1),
      family = "Times New Roman")
  
  border <- "black"
  
  var_unscaled <- wd4 %>% 
    select(BCS) %>% 
    mutate(unit = m1$unit.classif) %>% 
    group_by(unit) %>% 
    summarise(BCS = mean(BCS)) %>% 
    ungroup() %>% 
    
    # Only needed if there are empty units in the SOM
    rbind(data.frame(unit = filtering.function(x = seq(1, n_units(data)[[3]], by = 1),
                                               y = m1$unit.classif),
                     BCS = NA)) %>%
    arrange(unit) %>% 
    pull(BCS)
  
  
  
  plot(m1, type = "property", 
       property = var_unscaled,
       main = bquote("Body condition score "<= 2 ~"(%)"),
       palette.name = coolBlueHotRed,
       border = border,
       shape = "straight",
       na.color = "white")
  
  
  
  var_unscaled <- wd4 %>% 
    select(HOCK) %>% 
    mutate(unit = m1$unit.classif) %>% 
    group_by(unit) %>% 
    summarise(HOCK = mean(HOCK)) %>% 
    ungroup() %>% 
    
    # Only needed if there are empty units in the SOM
    rbind(data.frame(unit = filtering.function(x = seq(1, n_units(data)[[3]], by = 1),
                                               y = m1$unit.classif),
                     HOCK = NA)) %>%
    arrange(unit) %>% 
    pull(HOCK)
  
  plot(m1, type = "property", 
       property = var_unscaled,
       main = "Hock lesions (%)", 
       palette.name = coolBlueHotRed,
       border = border,
       shape = "straight",
       na.color = "white")
  
  
  var_unscaled <- wd4 %>% 
    select(KNEE) %>% 
    mutate(unit = m1$unit.classif) %>% 
    group_by(unit) %>% 
    summarise(KNEE = mean(KNEE)) %>% 
    ungroup() %>% 
    
    # Only needed if there are empty units in the SOM
    rbind(data.frame(unit = filtering.function(x = seq(1, n_units(data)[[3]], by = 1),
                                               y = m1$unit.classif),
                     KNEE = NA)) %>%
    arrange(unit) %>% 
    pull(KNEE)
  
  plot(m1, type = "property", 
       property = var_unscaled,
       main = "Knee injuries (%)", 
       palette.name = coolBlueHotRed,
       border = border,
       shape = "straight",
       na.color = "white")
  
  
  var_unscaled <- wd4 %>% 
    select(NECK) %>% 
    mutate(unit = m1$unit.classif) %>% 
    group_by(unit) %>% 
    summarise(NECK = mean(NECK)) %>% 
    ungroup() %>% 
    
    # Only needed if there are empty units in the SOM
    rbind(data.frame(unit = filtering.function(x = seq(1, n_units(data)[[3]], by = 1),
                                               y = m1$unit.classif),
                     NECK = NA)) %>%
    arrange(unit) %>% 
    pull(NECK)
  
  plot(m1, type = "property", 
       property = var_unscaled,
       main = "Neck injuries (%)", 
       palette.name = coolBlueHotRed,
       border = border,
       shape = "straight",
       na.color = "white")
  
  
  var_unscaled <- wd4 %>% 
    select(LAME) %>% 
    mutate(unit = m1$unit.classif) %>% 
    group_by(unit) %>% 
    summarise(LAME = mean(LAME)) %>% 
    ungroup() %>% 
    
    # Only needed if there are empty units in the SOM
    rbind(data.frame(unit = filtering.function(x = seq(1, n_units(data)[[3]], by = 1),
                                               y = m1$unit.classif),
                     LAME = NA)) %>%
    arrange(unit) %>% 
    pull(LAME)
  
  plot(m1, type = "property", 
       property = var_unscaled,
       main = "Lameness (%)", 
       palette.name = coolBlueHotRed,
       border = border,
       shape = "straight",
       na.color = "white")
  
  var_unscaled <- wd4 %>% 
    select(HSI) %>% 
    mutate(unit = m1$unit.classif) %>% 
    group_by(unit) %>% 
    summarise(HSI = mean(HSI)) %>% 
    ungroup() %>% 
    
    # Only needed if there are empty units in the SOM
    rbind(data.frame(unit = filtering.function(x = seq(1, n_units(data)[[3]], by = 1),
                                               y = m1$unit.classif),
                     HSI = NA)) %>%
    arrange(unit) %>% 
    pull(HSI)
  
  plot(m1, type = "property", 
       property = var_unscaled,
       main = "Herd Status Index", 
       palette.name = coolBlueHotRed,
       border = border,
       shape = "straight",
       na.color = "white")
  
  
  par(mfrow = c(1, 1),
      mar=c(5.1, 4.1, 4.1, 2.1))
}



# Clustering ####

## Distance matrix  ####


# Getting only the layer with welfare indicators
codes <- tibble( layers = names(m1$codes),
                 codes = m1$codes ) %>%
  dplyr::filter(layers == c("welfare")) %>% 
  mutate( codes = purrr::map(codes, as_tibble) ) %>%
  spread( key = layers, value = codes) %>%
  apply(1, bind_cols) %>%
  .[[1]] %>%
  as_tibble()

# generate distance matrix from codes
dist_m <- stats::dist(codes,
                      method = "euclidean") %>%  
  as.matrix()


# generate separate distance matrix from map location
dist_on_map <- kohonen::unit.distances(som_grid)


# multiply the euclidean distance by the distance on map to adjust the distance to the map topology
dist_adj <- dist_m * dist_on_map


## Evaluating different cluster approaches ####


# Jaccard's bootstrap distance to evaluate cluster stability

options(digits = 3)

# Manually tried k varying from 2 to 7 and cluster methods = claraCBI, disthclustCBI, and distnoisemclustCBI 

k <- 2
cbv <- clusterboot(data = as.dist(dist_adj),
                   distances = TRUE,
                   B = 100,
                   bootmethod = "boot",
                   clustermethod = distnoisemclustCBI,
                   noisemethod = FALSE,
                   k = k,
                   # method = "ward.D",
                   # usepam = TRUE,
                   # diss = TRUE,
                   seed = 1801,
                   count = TRUE)

cbv



# Calculating some additional validation methods to compare clusters options that were overall stable
options(digits = 2)

set.seed(1801)
cluster.stats(d = as.dist(dist_adj), 
              clustering = claraCBI(data = as.dist(dist_adj),
                                    k = 2,
                                    usepam = TRUE)$partition,
              noisecluster = FALSE,
              silhouette = TRUE,
              wgap = FALSE, sepindex = FALSE,
              sepwithnoise = TRUE,
              aggregateonly = TRUE)

set.seed(1801)
cluster.stats(d = as.dist(dist_adj), 
              clustering = claraCBI(data = as.dist(dist_adj),
                                    k = 6,
                                    usepam = TRUE)$partition,
              noisecluster = FALSE,
              silhouette = TRUE,
              wgap = FALSE, sepindex = FALSE,
              sepwithnoise = TRUE,
              aggregateonly = TRUE)


set.seed(1801)
cluster.stats(d = as.dist(dist_adj), 
              clustering = disthclustCBI(dmatrix = as.dist(dist_adj),
                                         k = 2,
                                         method = "ward.D")$partition,
              noisecluster = FALSE,
              silhouette = TRUE,
              wgap = FALSE, sepindex = FALSE,
              sepwithnoise = TRUE,
              aggregateonly = TRUE)




# Additionally, following methodology proposed by Akhanli and Hennig (2020) and calculating A1 and A2 indexes
# doi.org/10.1007/s11222-020-09958-2

clustermethodpars <- list()

clustermethodpars[[2]] <- list()
clustermethodpars[[2]]$method <- "ward.D"


bootclassif <- c("centroid","averagedist")

set.seed(1801)
val_boostcluster_stats <- clusterbenchstats(data = as.dist(dist_adj),
                                            G = c(2, 6),
                                            diss = TRUE,
                                            distmethod = rep(TRUE, 2),
                                            scaling = FALSE,
                                            clustermethod = c("claraCBI", "disthclustCBI"),
                                            methodnames = c("PAM", "HC"),
                                            clustermethodpars = clustermethodpars,
                                            useboot = TRUE,
                                            bootclassif = bootclassif,
                                            bootruns = 100,
                                            useallg = TRUE)



# Index A1 in Akhanli and Hennig (2020) (need these weights choices):
# A1 puts more emphasis on cluster homogeneity
print(val_boostcluster_stats$sstat,
      aggregate = TRUE,
      weights = c(1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0))


# Index A2 in Akhanli and Hennig (2020) (need these weights choices):
# A2 puts more emphasis on cluster separation
print(val_boostcluster_stats$sstat,
      aggregate = TRUE,
      weights = c(0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,1,0))


# Return the digits number to default
options(digits = 7)



## Partitioning around medoids (PAM) clustering ####
# PAM clustering with 6 clusters was the best choice.

set.seed(1801)
som_cluster_adj <- claraCBI(data = as.dist(dist_adj),
                            k = 6,
                            usepam = TRUE)


# Visualizing the clusters using classic MDS
cluster_mds <- cmdscale(d = as.dist(dist_adj),
                        k = 2, # First two dimensions
                        eig = TRUE)

ggplot(mapping = aes(x = cluster_mds$points[,1],
                     y = cluster_mds$points[,2],
                     color = as.factor(som_cluster_adj$partition))) +
  geom_point(show.legend = FALSE) +
  tidyquant::theme_tq() +
  labs(x = "Discriminant coordinate 1",
       y = "Discriminant coordinate 2")



# Plotting the clusters in the SOM

# Setting a color pallet
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


pretty_palette <- gg_color_hue(6)


# Getting number of farms to include in the graph

link <- tibble(map_loc = names(som_cluster_adj$partition) %>% as.integer(),
               cluster = som_cluster_adj$partition)

pred <- tibble(map_loc = m1$unit.classif) %>%
  left_join(link)



wd5 <- wd4 %>%
  bind_cols(pred) %>% 
  mutate(cluster = as.factor(cluster)) %>% 
  
  # Changing cluster coding so it can work nicer with caret during modeling
  mutate(cluster = as.factor(ifelse(cluster == 1, "Cluster.1",
                                    ifelse(cluster == 2, "Cluster.2",
                                           ifelse(cluster == 3, "Cluster.3",
                                                  ifelse(cluster == 4, "Cluster.4",
                                                         ifelse(cluster == 5, "Cluster.5",
                                                                ifelse(cluster == 6, "Cluster.6", "who knows"))))))))


n_herds <- table(wd5$cluster)
n_herds

perc_herds <- (table(wd5$cluster)/nrow(wd5) *100) %>%  round(1)
perc_herds



bgcol <- pretty_palette[som_cluster_adj$partition]

# Needed if there are empty units
bgcol[c(filtering.function(x = seq(1, n_units(data)[[3]], by = 1),
                           y = m1$unit.classif))] <- NA



plot(m1, type = "mapping", bgcol = bgcol,
     keepMargins = TRUE,
     shape = "straight",
     border = "white",
     pchs = NA,
     main = "",
     na.color = "white")


add.cluster.boundaries(m1, som_cluster_adj$partition)


text(x = 4.5, y = -0.2, labels = c("Cluster 1 (N = 281; 9.42%)"),
     adj = 0.5,
     cex = 1.3, col = pretty_palette[1], family = "Times New Roman", font = 2)

text(x = 11, y = -0.2, labels = c("Cluster 2 (N = 411; 13.8%)"),
     adj = 0.5,
     cex = 1.3, col = pretty_palette[2], family = "Times New Roman", font = 2)

text(x = 0, y = 8.3, labels = c("Cluster 3 (N = 283; 9.49%)"),
     adj = 0.5,
     cex = 1.3, col = pretty_palette[3], family = "Times New Roman", font = 2,
     srt = 90)

text(x = 14.6, y = 7, labels = c("Cluster 4 (N = 600; 20.1%)"),
     adj = 0.5,
     cex = 1.3, col = pretty_palette[4], family = "Times New Roman", font = 2,
     srt = 90)

text(x = 14.6, y = 13.9, labels = c("Cluster 5 (N = 576; 19.3%)"),
     adj = 0.5,
     cex = 1.3, col = pretty_palette[5], family = "Times New Roman", font = 2,
     srt = 90)

text(x = 5, y = 18.3, labels = c("Cluster 6 (N = 831; 27.9%)"),
     adj = 0.5,
     cex = 1.3, col = pretty_palette[6], family = "Times New Roman", font = 2)




# Describing clusters ####

# Getting descriptive stats

wd5 %>% 
  select(cluster, BCS, HOCK, KNEE, NECK, LAME, HSI, ecm, cumul_milk_value, 
         lpl, avg_pcntg_lgvt) %>% 
  group_by(cluster) %>% 
  summarize_if(is.numeric, mean) %>% 
  as.data.frame() %>% 
  mutate_if(is.numeric, round, 2)

wd5 %>% 
  select(cluster, BCS, HOCK, KNEE, NECK, LAME, HSI, ecm, cumul_milk_value,
         lpl, avg_pcntg_lgvt) %>% 
  group_by(cluster) %>% 
  summarize_if(is.numeric, sd) %>% 
  as.data.frame() %>% 
  mutate_if(is.numeric, round, 2)


# Correlation matrix
wd5 %>%
  select(BCS, HOCK, KNEE, NECK, LAME, HSI, ecm, cumul_milk_value,
         lpl, avg_pcntg_lgvt) %>%
  cor() %>%
  round(2)



## Splitting data ####
# Splitting data into training and validation files (75:25 ratio)
# based on the outcome variable


set.seed(1810)
splitIndex <- splitTools::partition(wd5$cluster,
                                    p = c(train = 0.75, valid = 0.25))

train.cluster <- wd5[splitIndex$train,] %>% 
  select(cluster, BCS, HOCK, KNEE, NECK, LAME, HSI)

valid.cluster <- wd5[splitIndex$valid,] %>% 
  select(cluster, BCS, HOCK, KNEE, NECK, LAME, HSI)



# Creating folds for 10 fold cross-validation
set.seed(1801)
folds <- createFolds(train.cluster$cluster, k = 10, list=TRUE)

set.seed(1801)
fit_control <- trainControl(method = "adaptive_cv",
                            search = "grid",
                            index = folds,
                            adaptive = list(min = 5, alpha = 0.05, 
                                            method = "gls", 
                                            complete = TRUE),
                            allowParallel = TRUE,
                            verboseIter = TRUE,
                            classProbs = TRUE)


cl <- makePSOCKcluster(14)
registerDoParallel(cl, cores = 14)

getDoParWorkers()


## Training machine learning models ####

# RPART: Recursive partitioning tree
set.seed(1801)
rpart_cluster <- train(cluster ~ ., 
                       data = train.cluster, 
                       method = "rpart",
                       metric = "Accuracy",
                       trControl = fit_control,
                       tuneLength = 5)


# Gradient boosting machine
set.seed(1801)
gbm_cluster <- train(cluster ~ ., 
                     data = train.cluster, 
                     method = "gbm",
                     metric = "Accuracy",
                     trControl = fit_control,
                     tuneLength = 5)


# Extreme gradient boosting machine
set.seed(1801)
xgbm_cluster <- train(cluster ~ .,
                      data = train.cluster,
                      method = "xgbTree",
                      metric = "F1",
                      trControl = fit_control,
                      tuneLength = 5)


# Random Forest
set.seed(1801)
rf_cluster <- train(cluster ~ ., 
                    data = train.cluster, 
                    method = "ranger",
                    metric = "Accuracy",
                    trControl = fit_control,
                    tuneLength = 5)

# Support vector machine
set.seed(1801)
svm_cluster <- train(cluster ~ ., 
                     data = train.cluster, 
                     method = "svmRadial",
                     metric = "Accuracy",
                     trControl = fit_control,
                     tuneLength = 5)


stopCluster(cl)




## Evaluating models ####

confusionMatrix(data = predict(rpart_cluster, valid.cluster), 
                reference = valid.cluster$cluster,
                mode = "everything")

confusionMatrix(data = predict(gbm_cluster, valid.cluster), 
                reference = valid.cluster$cluster,
                mode = "everything")


confusionMatrix(data = predict(xgbm_cluster, valid.cluster), 
                reference = valid.cluster$cluster,
                mode = "everything")


confusionMatrix(data = predict(rf_cluster, valid.cluster), 
                reference = valid.cluster$cluster,
                mode = "everything")


confusionMatrix(data = predict(svm_cluster, valid.cluster), 
                reference = valid.cluster$cluster,
                mode = "everything")



## Interpreting model with highest accuracy  ####

predictor.cluster <- Predictor$new(model = xgbm_cluster, 
                                   data = wd5 %>% 
                                     select(cluster, BCS, HOCK, KNEE, NECK, LAME, HSI),
                                   y = "cluster")



# Creates a PSOCK cluster with 10 cores
future::plan(multisession, workers = 10)


# Partial dependency plots
pdp.BCS.cluster <- FeatureEffect$new(predictor.cluster, 
                                     feature = "BCS", method = "pdp")

pdp.BCS.plot <- plot(pdp.BCS.cluster) +
  geom_smooth(method = "loess", se = TRUE,
              formula =  'y ~ x',
              size = 0.5) +
  facet_wrap(~ .class,
             scales = "free",
             labeller = as_labeller(c ("Cluster.1" = "Cluster 1",
                                       "Cluster.2" = "Cluster 2",
                                       "Cluster.3" = "Cluster 3",
                                       "Cluster.4" = "Cluster 4",
                                       "Cluster.5" = "Cluster 5",
                                       "Cluster.6" = "Cluster 6")),
             nrow = 2) +
  theme_classic(base_family = "Times New Roman") + 
  scale_y_continuous(limits = c(-0.25, 1.08),
                     breaks = seq(0, 1, by = 0.25)) +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line("black"), 
        axis.title = element_text(size = 10),
        axis.title.x = element_text(size = 10,
                                    margin = margin(t = 5, # top
                                                    r = 0, # right 
                                                    b = 0, # bottom
                                                    l = 0)),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, # top
                                                    r = 10, # right 
                                                    b = 0, # bottom
                                                    l = 0))) + 
  labs( y = "Prediction probability",
        x = bquote("Body condition score "<= 2 ~"(%)"))


pdp.BCS.plot



pdp.HOCK.cluster <- FeatureEffect$new(predictor.cluster, 
                                      feature = "HOCK", method = "pdp")

pdp.HOCK.plot <- plot(pdp.HOCK.cluster) +
  geom_smooth(method = "loess", se = TRUE,
              formula =  'y ~ x',
              size = 0.5) +
  facet_wrap(~ .class,
             scales = "free",
             labeller = as_labeller(c ("Cluster.1" = "Cluster 1",
                                       "Cluster.2" = "Cluster 2",
                                       "Cluster.3" = "Cluster 3",
                                       "Cluster.4" = "Cluster 4",
                                       "Cluster.5" = "Cluster 5",
                                       "Cluster.6" = "Cluster 6")),
             nrow = 2) +
  theme_classic(base_family = "Times New Roman") + 
  scale_y_continuous(limits = c(-0.25, 1.08),
                     breaks = seq(0, 1, by = 0.25)) +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line("black"), 
        axis.title = element_text(size = 10),
        axis.title.x = element_text(size = 10,
                                    margin = margin(t = 5, # top
                                                    r = 0, # right 
                                                    b = 0, # bottom
                                                    l = 0)),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, # top
                                                    r = 10, # right 
                                                    b = 0, # bottom
                                                    l = 0))) + 
  labs( y = "Prediction probability",
        x = "Hock lesion (%)")

pdp.HOCK.plot



pdp.KNEE.cluster <- FeatureEffect$new(predictor.cluster, 
                                      feature = "KNEE", method = "pdp")

pdp.KNEE.plot <- plot(pdp.KNEE.cluster) +
  geom_smooth(method = "loess", se = TRUE,
              formula =  'y ~ x',
              size = 0.5) +
  facet_wrap(~ .class,
             scales = "free",
             labeller = as_labeller(c ("Cluster.1" = "Cluster 1",
                                       "Cluster.2" = "Cluster 2",
                                       "Cluster.3" = "Cluster 3",
                                       "Cluster.4" = "Cluster 4",
                                       "Cluster.5" = "Cluster 5",
                                       "Cluster.6" = "Cluster 6")),
             nrow = 2) +
  theme_classic(base_family = "Times New Roman") + 
  scale_y_continuous(limits = c(-0.25, 1.08),
                     breaks = seq(0, 1, by = 0.25)) +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line("black"), 
        axis.title = element_text(size = 10),
        axis.title.x = element_text(size = 10,
                                    margin = margin(t = 5, # top
                                                    r = 0, # right 
                                                    b = 0, # bottom
                                                    l = 0)),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, # top
                                                    r = 10, # right 
                                                    b = 0, # bottom
                                                    l = 0))) + 
  labs( y = "Prediction probability",
        x = "Knee lesion (%)")

pdp.KNEE.plot



pdp.NECK.cluster <- FeatureEffect$new(predictor.cluster, 
                                      feature = "NECK", method = "pdp")

pdp.NECK.plot <- plot(pdp.NECK.cluster) +
  geom_smooth(method = "loess", se = TRUE,
              formula =  'y ~ x',
              size = 0.5) +
  facet_wrap(~ .class,
             scales = "free",
             labeller = as_labeller(c ("Cluster.1" = "Cluster 1",
                                       "Cluster.2" = "Cluster 2",
                                       "Cluster.3" = "Cluster 3",
                                       "Cluster.4" = "Cluster 4",
                                       "Cluster.5" = "Cluster 5",
                                       "Cluster.6" = "Cluster 6")),
             nrow = 2) +
  theme_classic(base_family = "Times New Roman") + 
  scale_y_continuous(limits = c(-0.25, 1.08),
                     breaks = seq(0, 1, by = 0.25)) +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line("black"), 
        axis.title = element_text(size = 10),
        axis.title.x = element_text(size = 10,
                                    margin = margin(t = 5, # top
                                                    r = 0, # right 
                                                    b = 0, # bottom
                                                    l = 0)),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, # top
                                                    r = 10, # right 
                                                    b = 0, # bottom
                                                    l = 0))) + 
  labs( y = "Prediction probability",
        x = "Neck lesion (%)")

pdp.NECK.plot



pdp.LAME.cluster <- FeatureEffect$new(predictor.cluster, 
                                      feature = "LAME", method = "pdp")

pdp.LAME.plot <- plot(pdp.LAME.cluster) +
  geom_smooth(method = "loess", se = TRUE,
              formula =  'y ~ x',
              size = 0.5) +
  facet_wrap(~ .class,
             scales = "free",
             labeller = as_labeller(c ("Cluster.1" = "Cluster 1",
                                       "Cluster.2" = "Cluster 2",
                                       "Cluster.3" = "Cluster 3",
                                       "Cluster.4" = "Cluster 4",
                                       "Cluster.5" = "Cluster 5",
                                       "Cluster.6" = "Cluster 6")),
             nrow = 2) +
  theme_classic(base_family = "Times New Roman") + 
  scale_y_continuous(limits = c(-0.25, 1.08),
                     breaks = seq(0, 1, by = 0.25)) +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line("black"), 
        axis.title = element_text(size = 10),
        axis.title.x = element_text(size = 10,
                                    margin = margin(t = 5, # top
                                                    r = 0, # right 
                                                    b = 0, # bottom
                                                    l = 0)),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, # top
                                                    r = 10, # right 
                                                    b = 0, # bottom
                                                    l = 0))) + 
  labs( y = "Prediction probability",
        x = "Lameness (%)")

pdp.LAME.plot


pdp.HSI.cluster <- FeatureEffect$new(predictor.cluster, 
                                     feature = "HSI", method = "pdp")

pdp.HSI.plot <- plot(pdp.HSI.cluster) +
  geom_smooth(method = "loess", se = TRUE,
              formula =  'y ~ x',
              size = 0.5) +
  facet_wrap(~ .class,
             scales = "free",
             labeller = as_labeller(c ("Cluster.1" = "Cluster 1",
                                       "Cluster.2" = "Cluster 2",
                                       "Cluster.3" = "Cluster 3",
                                       "Cluster.4" = "Cluster 4",
                                       "Cluster.5" = "Cluster 5",
                                       "Cluster.6" = "Cluster 6")),
             nrow = 2) +
  theme_classic(base_family = "Times New Roman") + 
  scale_y_continuous(limits = c(-0.25, 1.08),
                     breaks = seq(0, 1, by = 0.25)) +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.line = element_line("black"), 
        axis.title = element_text(size = 10),
        axis.title.x = element_text(size = 10,
                                    margin = margin(t = 5, # top
                                                    r = 0, # right 
                                                    b = 0, # bottom
                                                    l = 0)),
        axis.title.y = element_text(size = 10,
                                    margin = margin(t = 0, # top
                                                    r = 10, # right 
                                                    b = 0, # bottom
                                                    l = 0))) + 
  labs( y = "Prediction probability",
        x = "Herd status index")

pdp.HSI.plot



# Analyzing production and performance data ####


## Splitting data ####
# Using the same splitting and folds as from when analyzing the welfare outcome data


train.dhi <- wd5[splitIndex$train,] %>% 
  select(cluster, ecm, cumul_milk_value, lpl, avg_pcntg_lgvt)

valid.dhi <- wd5[splitIndex$valid,] %>% 
  select(cluster, ecm, cumul_milk_value, lpl, avg_pcntg_lgvt)


set.seed(1801)
fit_control.dhi <- trainControl(method = "adaptive_cv",
                                search = "grid",
                                index = folds,
                                adaptive = list(min = 5, alpha = 0.05, 
                                                method = "gls", 
                                                complete = TRUE),
                                allowParallel = TRUE,
                                verboseIter = TRUE,
                                classProbs = TRUE)



cl <- makePSOCKcluster(14)
registerDoParallel(cl, cores = 14)

getDoParWorkers()


## Training machine learning models ####

# RPART: Recursive partitioning tree
set.seed(1801)
rpart_dhi <- train(cluster ~ ., 
                   data = train.dhi, 
                   method = "rpart",
                   metric = "Accuracy",
                   trControl = fit_control.dhi,
                   tuneLength = 5)



# Gradient boosting machine
set.seed(1801)
gbm_dhi <- train(cluster ~ ., 
                 data = train.dhi, 
                 method = "gbm",
                 metric = "Accuracy",
                 trControl = fit_control.dhi,
                 tuneLength = 5)


# Extreme gradient boosting machine
set.seed(1801)
xgbm_dhi <- train(cluster ~ .,
                  data = train.dhi,
                  method = "xgbTree",
                  metric = "Accuracy",
                  trControl = fit_control.dhi,
                  tuneLength = 5)


# Random Forest
set.seed(1801)
rf_dhi <- train(cluster ~ .,
                data = train.dhi,
                method = "ranger",
                metric = "Accuracy",
                trControl = fit_control.dhi,
                tuneLength = 5)


# Support vector machine
set.seed(1801)
svm_dhi <- train(cluster ~ .,
                 data = train.dhi,
                 method = "svmRadial",
                 metric = "Accuracy",
                 trControl = fit_control.dhi,
                 tuneLength = 5)



stopCluster(cl)


## Evaluating models ####
confusionMatrix(data = predict(rpart_dhi, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")

confusionMatrix(data = predict(gbm_dhi, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(xgbm_dhi, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(rf_dhi, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(svm_dhi, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")



# Low accuracy could be happening because of unbalanced distribution of cluster labels.
# Trying some balancing options

## Weighted models ####
# Weights will be defined as the multiplicative inverse of the number of observations from each cluster


weights.dhi <- train.dhi %>% 
  group_by(cluster) %>%
  add_count() %>%
  mutate(weights.cluster = 1/(n / nrow(train.cluster))) %>% 
  ungroup() %>% 
  pull(weights.cluster)



set.seed(1801)
fit_control.dhi <- trainControl(method = "adaptive_cv",
                                search = "grid",
                                index = folds,
                                adaptive = list(min = 5, alpha = 0.05, 
                                                method = "gls", 
                                                complete = TRUE),
                                allowParallel = TRUE,
                                verboseIter = TRUE,
                                classProbs = TRUE)



cl <- makePSOCKcluster(14)
registerDoParallel(cl, cores = 14)

getDoParWorkers()


### Training machine learning models ####

# RPART: Recursive partitioning tree
set.seed(1801)
rpart_dhi_wght <- train(cluster ~ ., 
                        data = train.dhi, 
                        method = "rpart",
                        metric = "Accuracy",
                        weights = weights.dhi,
                        trControl = fit_control.dhi,
                        tuneLength = 5)



# Gradient boosting machine
set.seed(1801)
gbm_dhi_wght <- train(cluster ~ ., 
                      data = train.dhi, 
                      method = "gbm",
                      metric = "Accuracy",
                      weights = weights.dhi,
                      trControl = fit_control.dhi,
                      tuneLength = 5)


# Extreme gradient boosting machine
set.seed(1801)
xgbm_dhi_wght <- train(cluster ~ .,
                       data = train.dhi,
                       method = "xgbTree",
                       metric = "Accuracy",
                       weights = weights.dhi,
                       trControl = fit_control.dhi,
                       tuneLength = 5)


# Random Forest
set.seed(1801)
rf_dhi_wght <- train(cluster ~ .,
                     data = train.dhi,
                     method = "ranger",
                     metric = "Accuracy",
                     weights = weights.dhi,
                     trControl = fit_control.dhi,
                     tuneLength = 5)


# Support vector machine does not have a weight argument!

stopCluster(cl)


### Evaluating models ####
confusionMatrix(data = predict(rpart_dhi_wght, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")

confusionMatrix(data = predict(gbm_dhi_wght, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(xgbm_dhi_wght, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(rf_dhi_wght, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")



## Synthetic Minority Oversampling Technique (SMOTE) ####

set.seed(1801)
train.dhi.bal <- UBL::SmoteClassif(form = cluster ~ .,
                                   dat = train.dhi %>% 
                                     as.data.frame(),
                                   C.perc = "balance")

summary(train.dhi.bal)


# Creating folds for 10 fold cross-validation
set.seed(1801)
folds.dhi.bal <- createFolds(train.dhi.bal$cluster, 
                             k = 10,
                             list=TRUE)

set.seed(1801)
fit_control.dhi.bal <- trainControl(method = "adaptive_cv",
                                    search = "grid",
                                    index = folds.dhi.bal,
                                    adaptive = list(min = 5, alpha = 0.05, 
                                                    method = "gls", 
                                                    complete = TRUE),
                                    allowParallel = TRUE,
                                    verboseIter = TRUE,
                                    classProbs = TRUE)




cl <- makePSOCKcluster(14)
registerDoParallel(cl, cores = 14)

getDoParWorkers()


### Training machine learning models ####

# RPART: Recursive partitioning tree
set.seed(1801)
rpart_dhi.bal <- train(cluster ~ ., 
                       data = train.dhi.bal, 
                       method = "rpart",
                       metric = "Accuracy",
                       trControl = fit_control.dhi.bal,
                       tuneLength = 5)



# Gradient boosting machine
set.seed(1801)
gbm_dhi.bal <- train(cluster ~ ., 
                     data = train.dhi.bal, 
                     method = "gbm",
                     metric = "Accuracy",
                     trControl = fit_control.dhi.bal,
                     tuneLength = 5)


# Extreme gradient boosting machine
set.seed(1801)
xgbm_dhi.bal <- train(cluster ~ .,
                      data = train.dhi.bal,
                      method = "xgbTree",
                      metric = "Accuracy",
                      trControl = fit_control.dhi.bal,
                      tuneLength = 5)

# Random Forest
set.seed(1801)
rf_dhi.bal <- train(cluster ~ .,
                    data = train.dhi.bal,
                    method = "ranger",
                    metric = "Accuracy",
                    trControl = fit_control.dhi.bal,
                    tuneLength = 5)


# Support vector machine
set.seed(1801)
svm_dhi.bal <- train(cluster ~ .,
                     data = train.dhi.bal,
                     method = "svmRadial",
                     metric = "Accuracy",
                     trControl = fit_control.dhi.bal,
                     tuneLength = 5)

stopCluster(cl)




### Evaluating models ####
confusionMatrix(data = predict(rpart_dhi.bal, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(gbm_dhi.bal, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(xgbm_dhi.bal, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(rf_dhi.bal, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(svm_dhi.bal, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


## UP Sampling ####

set.seed(1801)
train.up <- upSample(x = train.dhi %>%
                         select(ecm, cumul_milk_value, lpl, avg_pcntg_lgvt) %>%
                         as.data.frame(),
                       y = train.dhi$cluster,
                       yname = "cluster")


# Creating folds for 10 fold cross-validation
set.seed(1801)
folds.up <- createFolds(train.up$cluster, k = 10, list=TRUE)


set.seed(1801)
fit_control.up <- trainControl(method = "adaptive_cv",
                                 search = "grid",
                                 index = folds.up,
                                 adaptive = list(min = 5, alpha = 0.05, 
                                                 method = "gls", 
                                                 complete = TRUE),
                                 allowParallel = TRUE,
                                 verboseIter = TRUE,
                                 classProbs = TRUE)



cl <- makePSOCKcluster(14)
registerDoParallel(cl, cores = 14)

getDoParWorkers()

### Training machine learning models ####

# RPART: Recursive partitioning tree
set.seed(1801)
rpart_dhi.up <- train(cluster ~ ., 
                        data = train.up, 
                        method = "rpart",
                        metric = "Accuracy",
                        trControl = fit_control.up,
                        tuneLength = 5)


# Gradient boosting machine
set.seed(1801)
gbm_dhi.up <- train(cluster ~ ., 
                      data = train.up, 
                      method = "gbm",
                      metric = "Accuracy",
                      trControl = fit_control.up,
                      tuneLength = 5)


# Extreme gradient boosting machine
set.seed(1801)
xgbm_dhi.up <- train(cluster ~ .,
                       data = train.up,
                       method = "xgbTree",
                       metric = "Accuracy",
                       trControl = fit_control.up,
                       tuneLength = 5)


# Random Forest
set.seed(1801)
rf_dhi.up <- train(cluster ~ ., 
                     data = train.up, 
                     method = "ranger",
                     metric = "Accuracy",
                     trControl = fit_control.up,
                     tuneLength = 5)


# Support vector machine
set.seed(1801)
svm_dhi.up <- train(cluster ~ ., 
                      data = train.up, 
                      method = "svmRadial",
                      metric = "Accuracy",
                      trControl = fit_control.up,
                      tuneLength = 5)


stopCluster(cl)


### Evaluating models ####

confusionMatrix(data = predict(rpart_dhi.up, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(gbm_dhi.up, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(xgbm_dhi.up, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(rf_dhi.up, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


confusionMatrix(data = predict(svm_dhi.up, valid.dhi), 
                reference = valid.dhi$cluster,
                mode = "everything")


## Interpreting the model with the best accuracy and that predicted all cluster labels ####

predictor.dhi <- Predictor$new(model = rf_dhi, 
                               data = wd5 %>% 
                                 select(cluster, ecm, cumul_milk_value, lpl, avg_pcntg_lgvt),
                               y = "cluster")


# Creates a PSOCK cluster with 10 cores
plan("callr", workers = 10)


# Accumulated local effect plots
ale.ecm.dhi <- FeatureEffect$new(predictor.dhi, 
                                 feature = "ecm", method = "ale")

ale.ecm.plot <- plot(ale.ecm.dhi) +
  geom_smooth(method = "loess", se = TRUE,
              formula =  'y ~ x',
              size = 0.5) +
  facet_wrap(~ .class, 
             scales = "free",
             labeller = as_labeller(c ("Cluster.1" = "Cluster 1",
                                       "Cluster.2" = "Cluster 2",
                                       "Cluster.3" = "Cluster 3",
                                       "Cluster.4" = "Cluster 4",
                                       "Cluster.5" = "Cluster 5",
                                       "Cluster.6" = "Cluster 6")),
             nrow = 2) +
  theme_classic(base_family = "Times New Roman") + 
  scale_y_continuous(limits = c(-0.55, 0.9),
                     breaks = seq(-0.50, 0.85, by = 0.22)) +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.line = element_line("black"), 
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 5, # top
                                                    r = 0, # right 
                                                    b = 0, # bottom
                                                    l = 0)),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, # top
                                                    r = 10, # right 
                                                    b = 0, # bottom
                                                    l = 0))) + 
  labs( y = "Accumulated local effect",
        x = bquote("Cumulative energy corrected milk (kg)"))




ale.cumul_milk_value.dhi <- FeatureEffect$new(predictor.dhi, 
                                              feature = "cumul_milk_value", method = "ale")

ale.cumul_milk_value.plot <- plot(ale.cumul_milk_value.dhi) +
  geom_smooth(method = "loess", se = TRUE,
              formula =  'y ~ x',
              size = 0.5) +
  facet_wrap(~ .class, 
             scales = "free",
             labeller = as_labeller(c ("Cluster.1" = "Cluster 1",
                                       "Cluster.2" = "Cluster 2",
                                       "Cluster.3" = "Cluster 3",
                                       "Cluster.4" = "Cluster 4",
                                       "Cluster.5" = "Cluster 5",
                                       "Cluster.6" = "Cluster 6")),
             nrow = 2) +
  theme_classic(base_family = "Times New Roman") + 
  scale_y_continuous(limits = c(-0.55, 0.9),
                     breaks = seq(-0.50, 0.85, by = 0.22)) +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.line = element_line("black"), 
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 5, # top
                                                    r = 0, # right 
                                                    b = 0, # bottom
                                                    l = 0)),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, # top
                                                    r = 10, # right 
                                                    b = 0, # bottom
                                                    l = 0))) + 
  labs( y = "Accumulated local effect",
        x = bquote("Cumulative milk value ($CAD)"))




ale.lpl.dhi <- FeatureEffect$new(predictor.dhi, 
                                 feature = "lpl", method = "ale")

ale.lpl.plot <- plot(ale.lpl.dhi) +
  geom_smooth(method = "loess", se = TRUE,
              formula =  'y ~ x',
              size = 0.5) +
  facet_wrap(~ .class, 
             scales = "free",
             labeller = as_labeller(c ("Cluster.1" = "Cluster 1",
                                       "Cluster.2" = "Cluster 2",
                                       "Cluster.3" = "Cluster 3",
                                       "Cluster.4" = "Cluster 4",
                                       "Cluster.5" = "Cluster 5",
                                       "Cluster.6" = "Cluster 6")),
             nrow = 2) +
  theme_classic(base_family = "Times New Roman") + 
  scale_y_continuous(limits = c(-0.55, 0.9),
                     breaks = seq(-0.50, 0.85, by = 0.22)) +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.line = element_line("black"), 
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 5, # top
                                                    r = 0, # right 
                                                    b = 0, # bottom
                                                    l = 0)),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, # top
                                                    r = 10, # right 
                                                    b = 0, # bottom
                                                    l = 0))) + 
  labs( y = "Accumulated local effect",
        x = bquote("Length of productive life (years)"))



ale.avg_pcntg_lgvt.dhi <- FeatureEffect$new(predictor.dhi, 
                                            feature = "avg_pcntg_lgvt", method = "pdp")

ale.avg_pcntg_lgvt.plot <- plot(ale.avg_pcntg_lgvt.dhi) +
  geom_smooth(method = "loess", se = TRUE,
              formula =  'y ~ x',
              size = 0.5) +
  facet_wrap(~ .class, 
             scales = "free",
             labeller = as_labeller(c ("Cluster.1" = "Cluster 1",
                                       "Cluster.2" = "Cluster 2",
                                       "Cluster.3" = "Cluster 3",
                                       "Cluster.4" = "Cluster 4",
                                       "Cluster.5" = "Cluster 5",
                                       "Cluster.6" = "Cluster 6")),
             nrow = 2) +
  theme_classic(base_family = "Times New Roman") + 
  scale_y_continuous(limits = c(-0.55, 0.9),
                     breaks = seq(-0.50, 0.85, by = 0.22)) +
  theme(axis.text = element_text(size = 10, color = "black"),
        axis.ticks.x = element_line(color = "black"),
        axis.line = element_line("black"), 
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 5, # top
                                                    r = 0, # right 
                                                    b = 0, # bottom
                                                    l = 0)),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, # top
                                                    r = 10, # right 
                                                    b = 0, # bottom
                                                    l = 0))) + 
  labs( y = "Accumulated local effect",
        x = bquote("3+ lactation (%)"))


plot_grid(ale.ecm.plot,
          ale.cumul_milk_value.plot,
          ale.lpl.plot,
          ale.avg_pcntg_lgvt.plot,
          
          label_fontfamily = "Times New Roman",
          ncol = 2,
          labels = c("A", "B", "C", "D"),
          label_size = 20)


