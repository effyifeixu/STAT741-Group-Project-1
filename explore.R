library(tidyverse)
library(haven)
library(ggplot2)
library(lme4)


# Load the data
asthma_df <- read_dta("data/family asthma p1 data version2.dta")

# Reshape the data
asthma_tidy_df <- asthma_df %>%
  pivot_longer(
    cols = matches("\\d+$"),           
    names_to = c(".value", "visit"),  
    names_pattern = "([a-z]+)(\\d+)"   
  ) %>%
  select(family, id, visit, everything())

# Print out the columns
columns = colnames(asthma_tidy_df)
columns

# See what is in each of the columns
dim(asthma_tidy_df)
str(asthma_tidy_df)
summary(asthma_tidy_df)

#Cleaning up the data and applying factors
asthma_tidy_df$id <- factor(asthma_tidy_df$id)
asthma_tidy_df$family <- factor(asthma_tidy_df$family)
asthma_tidy_df$sex <- factor(asthma_tidy_df$sex)
asthma_tidy_df$smk <- factor(asthma_tidy_df$smk)
asthma_tidy_df$smkever <- factor(asthma_tidy_df$smkever,
                                 levels = c(0,1),
                                 labels = c("never","ever"))


#Making numeric
asthma_tidy_df$visit <- as.numeric(asthma_tidy_df$visit)

#Confirm
str(asthma_tidy_df$id)
str(asthma_tidy_df$family)
str(asthma_tidy_df$sex)
str(asthma_tidy_df$smk)
str(asthma_tidy_df$visit)

# Cleaning the data, ensures model will view data correctly
asthma_tidy_df <- asthma_tidy_df |>
  dplyr::mutate(
    id = factor(id),
    family = factor(family),
    visit = as.numeric(visit),
    sex = factor(sex),
    smk = factor(smk)
  )


# NA Diagnostics before fitting data, amount of NAs in the data
vars <- c("fev","visit","age","sex","smk","id")
colSums(is.na(asthma_tidy_df[vars]))
nrow(na.omit(asthma_tidy_df[vars]))

sapply(asthma_tidy_df, class)
dplyr::glimpse(asthma_tidy_df)




#Research Question Drives what I should look at
#Question 1 has to do with if asthmatics suffer steeper rates of decline or lower levels of lung function independent of smoking?
#First, I think I should visualize and record asthma and smoking data.

#First, how many unique participants do I have
num_unique_participants <- n_distinct(asthma_tidy_df$id)
cat("The number of unique participants is", num_unique_participants, "\n")

# Ok, now lets take a look at the asthma column
ggplot(asthma_tidy_df, aes(x = factor(asthma))) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Proportion of Participants by Asthma Status",
    x = "Group", 
    y = "Proportion"
    )

# Ok, now, let's take a look at the smoking data
ggplot(asthma_tidy_df, aes(x = factor(smkever))) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Proportion of Participants by Smoker Status",
    x = "Group", 
    y = "Proportion"
  )

# Ok, just for the sake of simplicity, I'm going to build a fixed linear model 
# just so I know how to do it for now 
# Fixed Linear Model
fixed_model <- lm(fev ~ 1 + age + mht + mwt + asthma,data=asthma_tidy_df)
summary(fixed_model)   # coefficients, SEs, t-tests, R^2
coef(fixed_model)      # beta-hat
fitted(fixed_model)    # fitted values
residuals(fixed_model) # residuals
confint(fixed_model)   # confidence intervals


# Simple Data Visulizations
# Is there a trend of FEV with Age?
ggplot(asthma_tidy_df, aes(x = age, y = fev)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(title = "FEV vs Age with Linear Fit",
       x = "Age",
       y = "FEV")

# Is there a trend of FVC with Age?
ggplot(asthma_tidy_df, aes(x = age, y = fvc)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(title = "FVC vs Age with Linear Fit",
       x = "Age",
       y = "FVC")

# Is there a trend of FVC with weight?
ggplot(asthma_tidy_df, aes(x = mwt, y = fev)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(title = "FEV vs MWT with Linear Fit",
       x = "MWT",
       y = "FEV")

# Is there a trend of FEV with height?
ggplot(asthma_tidy_df, aes(x = mht, y = fev)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(title = "FEV vs MHT with Linear Fit",
       x = "MHT",
       y = "FEV")

# Is there a trend of FEV with number of cigaretts smoked per day?
ggplot(asthma_tidy_df, aes(x = cncig, y = fev)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(title = "FEV vs CNCIG with Linear Fit",
       x = "CNCIG",
       y = "FEV")

# First mixed affects model below. 
# Response: 2 models. FEV reponse, and FVC response 
# Fixed Effects: asthma, mht, mwt, age 
# Random Effects: (1|id/family), visit (this is time variable), asthma, smkever, smk, cncig 
# The line I would enter in lmer would be 
# fev ~ asthma + mht + mwt + age+ visit + (1|id/family) + (visit|id) + (asthma|id) + (smkever|id) + (smk|id) + (cncig|id)
# Ok, I learned that this model above doesn't make sense for a few reasons.
# Should be (1|family/id), or individuals are nested within families, not the other way around
# (visit|id) can be clarified as (1 + visit|id)
# We don't expect asthma to vary by id (probably not, though some people might be cured), do don't allow it to vary by id at least at this point of basic modeling
# (smkever|id) would not be varying for each patient, though they could start smoking at some point, so leave it out. It is not random effect
# (smk|id) and (cncig|id) doesn't make sense to include as random effect because we don't want to be modeling its variance
# 

# Another mistake I made was that I thought that controlling for smoking meant making it as
# a random effect. After that I thought that leaving it out would be a way to control for it.
# The answer is that I need to leave in the smoking data as fixed effects so that we can see
# how it effects the FEV/FVC.

# New try:
# Response: 2 models. FEV reponse, and FVC response 
# Fixed Effects: asthma, mht, mwt, age, visit, smk, smkever, cncig 
# Random Effects: (1|family/id), visit (this is time variable)
# The line I would enter in lmer would be 
# fev ~ asthma + mht + mwt + age+ visit + smk + smkever + cncig + (1|family/id) + (1+visit|id)
# I found out I also need to include an interaction, asthma*visit is shorthand for asthma + visit + asthma:visit
# fev ~ asthma*visit + mht + mwt + age + smk + smkever + cncig + (1|family/id) + (1+visit|id)

# This model didn't work because the amount of parameters, which I will investigate with the code below
# fit <- lmer(
#   fev ~ asthma*visit + mht + mwt + age + smk + smkever + cncig + (1|family/id) + (1+visit|id),
#   data = asthma_tidy_df,
#   REML = TRUE
# )

# summary(fit)

length(unique(asthma_tidy_df$id))              # how many ids
table(table(asthma_tidy_df$id))                # how many ids have 1 obs, 2 obs, etc.
is.factor(asthma_tidy_df$visit); nlevels(asthma_tidy_df$visit)

nrow(asthma_tidy_df)
lme4::nobs(your_model_fit)  # after fitting
colSums(is.na(model.frame(your_formula, asthma_tidy_df)))

# First, set up simplest model and work from there
m0 <- lmer(fev ~ visit + (1 | id), data = asthma_tidy_df, REML = TRUE)
summary(m0)


#Plot the raw trajectories
ggplot(asthma_tidy_df, aes(x = visit, y = fev, group = id)) +
  geom_line(alpha = 0.1) +
  geom_smooth(aes(group = 1), color = "red", se = FALSE) +
  labs(title = "FEV Trajectories by Patient")

#Plot subject specific fitted lines
ranef_df <- ranef(m0)$id
ranef_df$id <- rownames(ranef_df)
colnames(ranef_df)[1] <- "u_i"

#Plot a few individual fitted trajectories
ggplot(ranef_df, aes(x = u_i)) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Random Intercepts",
       x = "Subject-specific deviation")





#Compare models code
#m_ri_ML  <- update(m_ri, REML = FALSE)
#m_rs0_ML <- update(m_rs0, REML = FALSE)
#anova(m_ri_ML, m_rs0_ML)








