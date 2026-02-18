# A lot of this is a copy of Shubh's code and data cleaning, but I am trying new models
library(haven)      # To read .dta files
library(tidyverse)  # For data manipulation (dplyr) and plotting (ggplot2)
library(lme4)       # For Random Coefficient Models (mixed models)
library(lmerTest)   # Gives the p-values for lme4 models
library(sjPlot)     # Great for visualizing mixed model results

# Load the data
data <- read_dta("data/family asthma p1 data version2.dta")

# Inspect
summary(data)

# 1. Reshape using pivot_longer
# We use a special regex pattern to strip the numbers (1, 2, 3...) from the variable names
long_data <- data %>%
  pivot_longer(
    cols = matches("\\d+$"),  # Selects columns ending in numbers (e.g., age1, fev12)
    names_to = c(".value", "Survey_Wave"), # Splits into variable name + survey number
    names_pattern = "([a-zA-Z]+)(\\d+)"    # Regex: Letters capture name, Digits capture wave
  ) %>%
  
  # 2. Clean up the data
  mutate(
    Survey_Wave = as.numeric(Survey_Wave), # Convert wave number to numeric
    
    # Ensure ID and Family are factors for the model
    id = as.factor(id),
    family = as.factor(family)
  ) %>%
  
  # 3. Remove rows where the outcome (FEV) is missing 
  # (Mixed models handle missing data well, but rows with no outcome provide no info)
  filter(!is.na(fev)) %>%
  
  # 4. Arrange by Family, then ID, then Time (optional, but good for inspection)
  arrange(family, id, Survey_Wave)

# Check the result
head(long_data)

# Data Cleaning steps
cleaned_data <- long_data %>%
  filter(!is.na(fev))
# 1. Remove rows where the outcome variable (FEV) is missing

# 2. Factorize categorical variables
cleaned_data <- cleaned_data %>%
  mutate(
    # Fixed Variables
    sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female")),
    asthma = factor(asthma, levels = c(0, 1), labels = c("Non-Asthma", "Asthma")),
    smkever = factor(smkever, levels = c(0, 1), labels = c("Never-Smoker", "Ever-Smoker")),
    
    # Time-Dependent Variables
    # Note: Ensure 'smk' is actually 1, 2, 3 in the data. 
    smk = factor(smk, levels = c(1, 2, 3), labels = c("Current", "Ex", "Never"))
  )

# 3. Create Quartiles for 'Cncig' (Option 3 for smoking variables)
cleaned_data <- cleaned_data %>%
  # Optional: Handle NAs if necessary (e.g., if non-smokers are NA, set to 0)
  # mutate(cncig = replace_na(cncig, 0)) %>% 
  
  mutate(
    # Create quartiles for cigarettes per day
    # We use 'ntile' to split data into 4 groups
    cncig_quartile = ntile(cncig, 4), 
    
    # Convert that new variable into a factor
    cncig_cat = as.factor(cncig_quartile)
  )

# 4. Centering Age (Crucial for convergence in Mixed Models)
# It makes the intercept meaningful (e.g., baseline lung function at age 20 or mean age)
cleaned_data$age_centered <- cleaned_data$age - mean(cleaned_data$age, na.rm = TRUE)

# Explanatory Analysis
# Sample 50 random IDs to keep the plot readable
set.seed(2026)
sample_ids <- sample(unique(cleaned_data$id), 50)
subset_data <- subset(cleaned_data, id %in% sample_ids)

# Spaghetti Plot
ggplot(subset_data, aes(x = age, y = fev, group = id)) +
  geom_line(alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Individual FEV1 Trajectories (Spaghetti Plot)",
       x = "Age (Years)", y = "FEV1")

# Visualizing Group Differences (Asthma vs. Non-Asthma)
ggplot(cleaned_data, aes(x = age, y = fev, color = asthma)) +
  # Add smooth trend lines (linear method because we assume linear decline)
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Mean Rate of Decline: Asthma vs Non-Asthma",
       x = "Age", y = "FEV1")

summary(cleaned_data$fev)
summary(cleaned_data$age)

# Boxplot to catch extreme FEV outliers
boxplot(cleaned_data$fev, main = "Boxplot of FEV1 Values")

# Correlation of Predictors
cor(cleaned_data[, c("fev", "age", "mht", "mwt")], use = "complete.obs")

## Research Question 1

# 1. Fit the Random Coefficient Model (RCM)
# We control for Sex, Height (mht), and Smoking History (smkever)
# Random Effects: 
#   - Family: Random Intercept only (1 | family)
#   - Subject: Random Intercept AND Random Slope for Age (1 + age_centered | id)

model_rq1 <- lmer(fev ~ age_centered * asthma + sex + mht + smkever + 
                    (1 | family) + (1 + age_centered | id),
                  data = cleaned_data,
                  REML = TRUE,
                  control = lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)))

# 2. View the Results
summary(model_rq1)

# A. Residuals vs Fitted Plot (Check for homoscedasticity)
plot(model_rq1, main = "Residuals vs Fitted Values")


# B. Normal Q-Q Plot of Residuals (Check normality)
qqnorm(resid(model_rq1))
qqline(resid(model_rq1), col = "red")

# C. Check Random Effects Normality
# Extract random effects
re <- ranef(model_rq1)

# Plot Subject Random Effects (Intercept and Slope)
par(mfrow=c(1,2))
qqnorm(re$id$`(Intercept)`, main = "Random Intercept (Subject)")
qqline(re$id$`(Intercept)`, col = "red")

qqnorm(re$id$age_centered, main = "Random Slope (Age)")
qqline(re$id$age_centered, col = "red")
par(mfrow=c(1,1)) # Reset plotting area

## Research Question 2

# Model 2: Three-way interaction (Age * Asthma * Smoking)
# We use the optimizer control to prevent the convergence warning.

model_rq2 <- lmer(fev ~ age_centered * asthma * smkever + sex + mht + 
                    (1 | family) + (1 + age_centered | id),
                  data = cleaned_data,
                  REML = TRUE,
                  control = lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)))

# View the results
summary(model_rq2)

summary(cleaned_data$cncig)

model_rq3 <- lmer(fev ~ age_centered * asthma * cncig + sex + mht + 
                    (1 | family) + (1 + age_centered | id),
                  data = cleaned_data,
                  REML = TRUE,
                  control = lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)))
summary(model_rq3)

# Plotting the 3-way interaction
plot_model(model_rq3, type = "pred", terms = c("age_centered", "asthma", "cncig")) +
  labs(
    title = "Lung Function Decline: Asthma x Cncig Interaction",
    y = "Predicted FEV1 (Liters)",
    x = "Years from Mean Age",
    color = "Asthma Status"
  ) +
  theme_minimal()

# FVC Models ----------------
model_aq1 <- lmer(fvc ~ age_centered * asthma + sex + mht + smkever + 
                              (1 | family) + (1 + age_centered | id),
                            data = cleaned_data,
                            REML = TRUE,
                            control = lmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 2e5)))

# 2. View the Results
summary(model_aq1)

model_aq3 <- lmer(fvc ~ age_centered * asthma * cncig + sex + mht + 
                    (1 | family) + (1 + age_centered | id),
                  data = cleaned_data,
                  REML = TRUE,
                  control = lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)))
summary(model_aq3)

# Plotting the 3-way interaction
plot_model(model_aq3, type = "pred", terms = c("age_centered", "asthma", "cncig")) +
  labs(
    title = "Lung Function Decline: Asthma x Cncig Interaction",
    y = "Predicted FVC (Liters)",
    x = "Years from Mean Age",
    color = "Asthma Status"
  ) +
  theme_minimal()

# Make FEV1/FVC Ratio column and try this
# Create a new column 'ratio_col' which is the ratio of 'col1' to 'col2'
cleaned_data <- cleaned_data %>% 
  mutate(fev_fvc_ratio = fev / fvc) #

# View the updated data frame
print(cleaned_data)

# Models for ratio
# FVC Models ----------------
model_bq1 <- lmer(fev_fvc_ratio ~ age_centered * asthma + sex + mht + smkever + 
                    (1 | family) + (1 + age_centered | id),
                  data = cleaned_data,
                  REML = TRUE,
                  control = lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)))
summary(model_bq1)

model_bq3 <- lmer(fev_fvc_ratio ~ age_centered * asthma * cncig + sex + mht + 
                    (1 | family) + (1 + age_centered | id),
                  data = cleaned_data,
                  REML = TRUE,
                  control = lmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun = 2e5)))
summary(model_bq3)

# Plotting the 3-way interaction
plot_model(model_bq3, type = "pred", terms = c("age_centered", "asthma", "cncig")) +
  labs(
    title = "Lung Function Decline: Asthma x Cncig Interaction",
    y = "Predicted FEV1/FVC Ratio",
    x = "Years from Mean Age",
    color = "Asthma Status"
  ) +
  theme_minimal()
