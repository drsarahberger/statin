# Load necessary library
library(dplyr)

# Set the number of samples
n <- 519

# Quantitative Features: mean and sd
age_mean <- 57.9
age_sd <- 8.5
bmi_mean <- 29.2
bmi_sd <- 5.5

# Simulate Quantitative Data
age_yr <- rnorm(n, mean = age_mean, sd = age_sd)
body_mass_index <- rnorm(n, mean = bmi_mean, sd = bmi_sd)

#qualitative features: percentage
psex_m <- 386/n
prace_w <- 500/n
pdiabetes <- 87/n
phypertension <- 367/n
pcurrent_smoking <- 157/n
pprevious_mi <- 137/n
pprevious_pci <-112/n
pprior_statin_use <- 319/n
pantiplatelet_agent <- 508/n
pbeta_blocker <- 317/n
pace_inhibitor <- 231/n
pangiotensin_receptor_blocker <- 82/n

# Qualitative Features: categories and proportions
sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(psex_m, 1-psex_m))
race <- sample(c("White", "Other"), n, replace = TRUE, prob = c(prace_w, 1-prace_w))
diabetes <- sample(c("1", "0"), n, replace = TRUE, prob = c(pdiabetes, 1-pdiabetes))
hypertension <- sample(c("1", "0"), n, replace = TRUE, prob = c(phypertension, 1-phypertension))
current_smoking <- sample(c("1", "0"), n, replace = TRUE, prob = c(pcurrent_smoking, 1- pcurrent_smoking))
previous_mi <- sample(c("1", "0"), n, replace = TRUE, prob = c(pprevious_mi, 1-pprevious_mi))
previous_pci <- sample(c("1", "0"), n, replace = TRUE, prob = c(pprevious_pci, 1-pprevious_pci))
prior_statin_use <- sample(c("1", "0"), n, replace = TRUE, prob = c(pprior_statin_use, 1-pprior_statin_use))
antiplatelet_agent <- sample(c("1", "0"), n, replace = TRUE, prob = c(pantiplatelet_agent, 1-pantiplatelet_agent))
beta_blocker <- sample(c("1", "0"), n, replace = TRUE, prob = c(pbeta_blocker, 1-pbeta_blocker))
ace_inhibitor <- sample(c("1", "0"), n, replace = TRUE, prob = c(pace_inhibitor, 1-pace_inhibitor))
angiotensin_receptor_blocker <- sample(c("1", "0"), n, replace = TRUE, prob = c(pangiotensin_receptor_blocker, 1-pangiotensin_receptor_blocker))

# Combine into a Data Frame
data_frame_atorvastatine <- data.frame(group = "atorvastatin", age_yr, body_mass_index, sex, race, diabetes, hypertension, current_smoking, previous_mi, previous_pci, prior_statin_use, antiplatelet_agent, beta_blocker, ace_inhibitor, angiotensin_receptor_blocker)

# --------------------------------------------
# Rosuvastatin

# Set the number of samples
n <- 520

# Quantitative Features: mean and sd
age_mean <- 57.4
age_sd <- 8.6
bmi_mean <- 28.9
bmi_sd <- 5.0

# Simulate Quantitative Data
age_yr <- rnorm(n, mean = age_mean, sd = age_sd)
body_mass_index <- rnorm(n, mean = bmi_mean, sd = bmi_sd)

#qualitative features: percentage
psex_m <- 379/n
prace_w <- 496/n
pdiabetes <- 72/n
phypertension <- 364/n
pcurrent_smoking <- 179/n
pprevious_mi <- 117/n
pprevious_pci <- 131/n
pprior_statin_use <- 303/n
pantiplatelet_agent <- 507/n
pbeta_blocker <- 315/n
pace_inhibitor <- 226/n
pangiotensin_receptor_blocker <- 87/n
  
  
# Qualitative Features: categories and proportions
sex <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(psex_m, 1-psex_m))
race <- sample(c("White", "Other"), n, replace = TRUE, prob = c(prace_w, 1-prace_w))
diabetes <- sample(c("1", "0"), n, replace = TRUE, prob = c(pdiabetes, 1-pdiabetes))
hypertension <- sample(c("1", "0"), n, replace = TRUE, prob = c(phypertension, 1-phypertension))
current_smoking <- sample(c("1", "0"), n, replace = TRUE, prob = c(pcurrent_smoking, 1- pcurrent_smoking))
previous_mi <- sample(c("1", "0"), n, replace = TRUE, prob = c(pprevious_mi, 1-pprevious_mi))
previous_pci <- sample(c("1", "0"), n, replace = TRUE, prob = c(pprevious_pci, 1-pprevious_pci))
prior_statin_use <- sample(c("1", "0"), n, replace = TRUE, prob = c(pprior_statin_use, 1-pprior_statin_use))
antiplatelet_agent <- sample(c("1", "0"), n, replace = TRUE, prob = c(pantiplatelet_agent, 1-pantiplatelet_agent))
beta_blocker <- sample(c("1", "0"), n, replace = TRUE, prob = c(pbeta_blocker, 1-pbeta_blocker))
ace_inhibitor <- sample(c("1", "0"), n, replace = TRUE, prob = c(pace_inhibitor, 1-pace_inhibitor))
angiotensin_receptor_blocker <- sample(c("1", "0"), n, replace = TRUE, prob = c(pangiotensin_receptor_blocker, 1-pangiotensin_receptor_blocker))

# Combine into a Data Frame
data_frame_rosuvastatine <- data.frame(group = "rosuvastatin",age_yr, body_mass_index, sex, race, diabetes, hypertension, current_smoking, previous_mi, previous_pci, prior_statin_use, antiplatelet_agent, beta_blocker, ace_inhibitor, angiotensin_receptor_blocker)

# Display
df_final <- rbind(data_frame_atorvastatine, data_frame_rosuvastatine)
write.csv(df_final, "data/simulated_statin_dataset.csv", row.names = FALSE)