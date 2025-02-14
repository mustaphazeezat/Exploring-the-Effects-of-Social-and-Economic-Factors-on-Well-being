setwd('/Users/azeezatmustapha/Desktop/school/winter4/case-study2/')

library(dplyr)
library(ggplot2)
library(brms)
library(xtable)
library(knitr)
library(forcats)
library(zoo)


theme_set(
  theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Title size, bold, centered
      axis.title.x = element_text(size = 16),  # X-axis label size
      axis.title.y = element_text(size = 16),  # Y-axis label size
      axis.text.x = element_text(size = 13),   # X-axis tick labels size
      axis.text.y = element_text(size = 13)    # Y-axis tick labels size
    )
)

df <- read.csv("germany.csv", sep=';')
df_USA <- read.csv("usa.csv", sep=';')
df_china <- read.csv("china.csv", sep=';')
df_trend <- read.csv("filtered_germany_data.csv")
View(df)


# Select columns by name
df_germ <- df[, c( "Q288R", "Q53", "Q49", "Q47", "Q50")]
df_usa <- df_USA[, c("Q49", "Q47", "Q50")]
df_chi <- df_china[, c("Q49", "Q47", "Q50")]

colnames(df_germ) <-c('income_level', 'access_to_health', 'swl', 'soh', 'sfs')
colnames(df_usa) <-c('swl', 'soh', 'sfs')
colnames(df_chi) <-c( 'swl', 'soh', 'sfs')

# Check for missing data
sum(is.na(df_germ))
sum(is.na(df_usa))
sum(is.na(df_chi))
summary(df_germ)

# Remove rows with any negative values
df_germ <- df_germ[apply(df_germ, 1, function(row) all(row >= 0)), ]
df_usa <- df_usa[apply(df_usa, 1, function(row) all(row >= 0)), ]
df_chi <- df_chi[apply(df_chi, 1, function(row) all(row >= 0)), ]
df_trend <- df_trend[apply(df_trend, 1, function(row) all(row >= 0)), ]

summary(df_germ)

# recoding variables Access to health and state of health

#df_germ <- df_germ %>%
#  mutate(access_to_health = case_when(
#    access_to_health == 4 ~ 1,
#    access_to_health == 3 ~ 2,
#    access_to_health == 2 ~ 3,
#   access_to_health == 1 ~ 4,
#    TRUE ~ access_to_health # Preserve any other values if they exist
#  ))


# Replace values in the 'soh' column using case_when
df_germ <- df_germ %>%
  mutate(soh = case_when(
    soh == 5 ~ 1,
    soh == 4 ~ 2,
    soh == 3 ~ 3,
    soh == 2 ~ 4,
    soh == 1 ~ 5,
    TRUE ~ soh # Preserve any other values if they exist
  ))

df_usa <- df_usa %>%
  mutate(soh = case_when(
    soh == 5 ~ 1,
    soh == 4 ~ 2,
    soh == 3 ~ 3,
    soh == 2 ~ 4,
    soh == 1 ~ 5,
    TRUE ~ soh # Preserve any other values if they exist
  ))

df_chi <- df_chi %>%
  mutate(soh = case_when(
    soh == 5 ~ 1,
    soh == 4 ~ 2,
    soh == 3 ~ 3,
    soh == 2 ~ 4,
    soh == 1 ~ 5,
    TRUE ~ soh # Preserve any other values if they exist
  ))

# Check the summary to confirm the transformation
summary(df_germ)

str(df_germ)


######################### DESCRIPTIVE ANALYSIS #####################################

## Relationship between satisfaction with life and income
ggplot(df_germ, aes(x = factor(income_level), y = swl, fill = factor(income_level))) +
  geom_boxplot() +
  labs(title = "", x = "Income Level", y = "Satisfaction with Life", fill = "Income level") 

cor(df_germ$swl, df_germ$income_level, method = 'spearman')
 
# Relationship between satisfaction with life and Access to healthcare

ggplot(df_germ, aes(x = factor(access_to_health), y = swl, fill = factor(access_to_health))) +
  geom_boxplot() +
  labs(title = "", x = "Access to healthcare", y = "Satisfaction with Life", fill = "Access to healthcare") 

cor(df_germ$swl, df_germ$access_to_health, method = 'spearman')

# Relationship between satisfaction with life and state of health
ggplot(df_germ, aes(x = factor(soh), y = swl, fill = factor(soh))) +
  geom_boxplot() +
  labs(title = "", x = "State of health", y = "Satisfaction with Life", fill = "State of health") 

cor(df_germ$swl, df_germ$soh, method = 'spearman')

# Relationship between satisfaction with life and Satisfaction with financial situation

ggplot(df_germ, aes(x = factor(sfs), y = swl, fill = factor(sfs))) +
  geom_boxplot() +
  labs(title = "", x = "Satisfaction with financial situation", y = "Satisfaction with Life", fill = "SFS Levels") 

cor(df_germ$swl, df_germ$sfs, method = 'spearman')
  

## Relationship between satisfaction with life and highest level of education

# Create a contingency table
table_data <- as.data.frame(table(df_germ$income_level, df_germ$swl))

# Heatmap
ggplot(table_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(x = "Level of income", y = "Satisfaction with life", fill = "Frequency") 



cor(df_germ$swl, df_germ$income_level, method = 'spearman')

## Relationship between satisfaction with life and social class

# Create a contingency table
table_data1 <- as.data.frame(table(df_germ$access_to_health, df_germ$swl))

# Heatmap
ggplot(table_data1, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(x = "Access to healthcare", y = " Satisfaction with life", fill = "Frequency") 

cor(df_germ$swl, df_germ$ access_to_health, method = 'spearman')

## Relationship between satisfaction with life and state of health

# Create a contingency table
table_data2 <- as.data.frame(table(df_germ$soh, df_germ$swl))

# Heatmap
ggplot(table_data2, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(x = "State of health", y = "Satisfaction with life", fill = "Frequency")


cor(df_germ$swl, df_germ$soh, method = 'spearman')

## Relationship between satisfaction with life and Satisfaction with financial situation

# Create a contingency table
table_data3 <- as.data.frame(table(df_germ$sfs, df_germ$swl))

# Heatmap
ggplot(table_data3, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(x = "Satisfaction with financial situation ", y = "Satisfaction with life", fill = "Frequency") 

cor(df_germ$swl, df_germ$sfs, method = 'spearman')


############################### USA #############################################


## Relationship between satisfaction with life and highest level of education

# Create a contingency table
table_data5 <- as.data.frame(table(df_usa$swl, df_usa$income_level))

# Heatmap
ggplot(table_data5, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Satisfaction with life", y = "Highest level of education", fill = "Frequency") +
  theme_minimal()

cor(df_usa$swl, df_usa$income_level, method = 'spearman')

## Relationship between satisfaction with life and state of health

# Create a contingency table
table_data6 <- as.data.frame(table(df_usa$swl, df_usa$soh))

# Heatmap
ggplot(table_data6, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Satisfaction with life", y = "State of health", fill = "Frequency") +
  theme_minimal()

cor(df_usa$swl, df_usa$soh, method = 'spearman')

## Relationship between satisfaction with life and state of health

# Create a contingency table
table_data6 <- as.data.frame(table(df_usa$swl, df_usa$sfs))

# Heatmap
ggplot(table_data6, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Satisfaction with life", y = "Satisfaction with financial situation", fill = "Frequency") +
  theme_minimal()

cor(df_usa$swl, df_usa$sfs, method = 'spearman')


############################### CHINA #############################################

## Relationship between satisfaction with life and highest level of education

# Create a contingency table
table_data7 <- as.data.frame(table(main_df$country, main_df$swl))

# Heatmap
ggplot(table_data7, aes(Var2,Var1, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Countries", y = "Satisfaction with life", fill = "Frequency") 

cor(df_chi$swl, df_chi$income_level, method = 'spearman')

## Relationship between satisfaction with life and state of health

# Create a contingency table
table_data8 <- as.data.frame(table(main_df$country, main_df$soh))

# Heatmap
ggplot(table_data8, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Countries", y = "State of health", fill = "Frequency")

cor(df_chi$swl, df_chi$soh, method = 'spearman')

## Relationship between satisfaction with life and state of health

# Create a contingency table
table_data9 <- as.data.frame(table(main_df$country, main_df$sfs))

# Heatmap
ggplot(table_data9, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Countries", y = "Satisfaction with financial situation", fill = "Frequency")

cor(df_chi$swl, df_chi$sfs, method = 'spearman')

############## OBJECTIVE 1 #######################

### Transform variables to appropriate type.
df_germ$income_level <- as.factor(df_germ$income_level)
df_germ$access_to_health <- as.factor(df_germ$access_to_health)
df_germ$soh <- as.factor(df_germ$soh)
df_germ$sfs <- as.factor(df_germ$sfs)

str(df_germ)

set.seed(200)
#### PRIOR DEFINITION #####
prior_def <- prior(normal(0, 2), class = "b") +
  prior(normal(0, 1), class = "Intercept")

# Define weakly informative priors for the ordinal regression model
prior_def2 <- c(
  # Prior for the intercept (weakly informative normal prior with a large SD)
  prior(normal(0, 5), class = "Intercept"), 
  
  # Priors for the effects of predictors (income_level, access_to_health, etc.)
  prior(normal(0, 2), class = "b"),  # Weakly informative prior for slopes
  
  # Prior for the scale parameter (variance in the model)
  prior(normal(0, 2), class = "sigma")
)

### Bayesian analysis ####

fit_cm <- brm(
  swl ~ 1 + income_level + access_to_health + soh + sfs,
  data = df_germ,
  family = cumulative("probit"),
  prior = prior_def,
  cores = 4
)

summary(fit_cm)
plot(conditional_effects(fit_cm, 'income_level', categorical = TRUE))
plot(conditional_effects(fit_cm, 'access_to_health', categorical = TRUE))
plot(conditional_effects(fit_cm, 'soh', categorical = TRUE))
plot(conditional_effects(fit_cm, 'sfs', categorical = TRUE))
marginal_effects(fit_cm, 'access_to_health', categorical = TRUE)

fit_acm <- brm(
  formula = swl ~ 1 + income_level + access_to_health + soh + sfs,
  data = df_germ,
  family = acat("probit"),
  prior = prior_def,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  iter = 4000,
  chains = 4,
  cores = 4
)

pp_check(fit_acm)

summary(fit_acm)
plot(conditional_effects(fit_acm, 'access_to_health', categorical = TRUE))
plot(conditional_effects(fit_acm, 'soh', categorical = TRUE), xlab = "State of Health")
plot(conditional_effects(fit_acm, 'sfs', categorical = TRUE))
df_trend$year <- as.factor(df_trend$year)

fit_cmv <- brm(
  formula = bf(swl ~ 1 + income_level + access_to_health + soh + sfs) +
    lf(disc ~ 0 + income_level + access_to_health + soh + sfs, cmc = FALSE),
  data = df_germ,
  family = cumulative("probit"),
  init_r = 0.1,
  cores = 4
)
 
summary(fit_cmv)
plot(conditional_effects(fit_cmv, 'access_to_health', categorical = TRUE))
plot(conditional_effects(fit_cmv, 'soh', categorical = TRUE), xlab = "State of Health")
plot(conditional_effects(fit_cmv, 'sfs', categorical = TRUE))

get_prior(fit_cmv) 

bayesian_trend_model <- brm(
  formula = satisfaction_life ~ 1 + year,                            # Satisfaction with life as a function of time (wave)
  data = df_trend,                   # Filtered data for Germany
  family = cumulative(link = "probit"),  # Cumulative ordinal family with probit link
  chains = 4,                            # Number of Markov chains
  iter = 2000,                           # Iterations per chain
  warmup = 1000,                         # Warmup iterations
  cores = 4                           # Reproducibility
)

summary(bayesian_trend_model)
plot(conditional_effects(bayesian_trend_model, "year", categorical = TRUE))
trend_plot <- conditional_effects(bayesian_trend_model, "year", categorical = TRUE)

trend_plot <- conditional_effects(bayesian_trend_model, "year", categorical = TRUE)

# Create the plot
plot(trend_plot, xlab = "Wave")
help(plot)
# Modify the x-axis label
title(xlab = "Wave")

# Convert the plot to a ggplot object
trend_plot_gg <- plot(trend_plot, plot = FALSE)[[1]] + 
  ggtitle("Trend of Satisfaction with Life in Germany Across WVS Waves") +
  xlab("World Values Survey Wave") +
  ylab("Predicted Satisfaction with Life") +
  theme_minimal(base_size = 14)

# Show the plot
print(trend_plot_gg)
# Change x-axis label afterwards
title(xlab = "State of Health")

plot(conditional_effects(fit_cm, 'swl', categorical = TRUE))

### Model comparison
loo(fit_cm, fit_acm, fit_cmv)


############### OBJECTIVE 2 ###########################

###MERGE ALL DATA FRAME
sub_germ <- df_germ[, c('swl', 'soh', 'sfs')]
sub_germ$country <- 'Germany'
df_usa$country <- 'USA'
df_chi$country <- 'China'
list_of_dfs <- list(sub_germ, df_usa, df_chi)
main_df <- do.call(rbind, list_of_dfs)
summary(df_usa)


# Calculate medians by Country without converting to numeric
median_table <- main_df %>%
  group_by(country) %>%
  summarize(
    Median_swl = levels(as.factor(swl))[median(as.numeric(swl), na.rm = TRUE)],
    Median_soh = levels(as.factor(soh))[median(as.numeric(soh), na.rm = TRUE)],
    Median_sfs = levels(as.factor(sfs))[median(as.numeric(sfs), na.rm = TRUE)]
)
# Display the table

# Group by 'Country' and calculate quantiles
quantiles <- main_df %>%
  group_by(country) %>%
  summarise(
    `25th Percentile` = quantile(soh, 0.25),
    `Median (50th)` = quantile(soh, 0.5),
    `75th Percentile` = quantile(soh, 0.75)
  )

print(quantiles)

summary(main_df)

# Basic boxplot with ordinal labels
ggplot(main_df, aes(x = country, y = swl, fill = country)) +
  geom_boxplot() +
  labs(title = "",
       x = "Country",
       y = "Satisfaction with Life") 

# Basic boxplot with ordinal labels
ggplot(main_df, aes(x = country, y = sfs, fill = country)) +
  geom_boxplot() +
  labs(title = "",
       x = "Country",
       y = "Satisfaction with financial situation") 

ggplot(main_df, aes(x = country, y = soh, fill = country)) +
  geom_boxplot() +
  labs(title = "",
       x = "Country",
       y = "State of health") 

main_df$soh <- as.factor(main_df$soh)
main_df$sfs <- as.factor(main_df$sfs)

fit_multilevel <- brm(
  formula = swl ~ 1 + soh + sfs + (1 | country),
  data = main_df,  # Your dataset
  family = cumulative(link = "probit"),  # Use cumulative model for ordinal data
  prior = c(
    prior(normal(0, 2), class = "b"),         # Priors for fixed effects
    prior(normal(0, 2), class = "Intercept"), # Prior for intercept
    prior(normal(0, 1), class = "sd")       # Prior for random effects
  ), 
  control = list(adapt_delta = 0.99),
  chains = 4, iter = 2000, cores = 4
)

pp_check(fit_multilevel)

summary(fit_multilevel)

plot(conditional_effects(fit_multilevel, 'soh', categorical = TRUE))
plot(conditional_effects(fit_multilevel, 'sfs', categorical = TRUE))
plot(conditional_effects(fit_multilevel, 'country', categorical = TRUE))

# Conditional effects for 'country'
cond_effects_country <- conditional_effects(
  fit_multilevel, 
  effects = "country", 
  categorical = TRUE, 
  re_formula = NULL  # Includes random effects
)
plot(cond_effects_country)
##7. Interpret Results
##After fitting the models, extract and interpret the results:
  
# Summary of the model
summary(fit_multilevel)

# Posterior intervals
posterior_summary(fit_multilevel)

# Plot fixed and random effects
plot(fit_multilevel)

# Random effects for countries
ranef(fit_multilevel)

# Posterior predictive checks
pp_check(fit_multilevel)

################## OBJECTIVE 3 ##########################
df_trend <- df_trend[apply(df_trend, 1, function(row) all(row >= 0)), ]

ggplot(df_trend, aes(x = factor(year), y = satisfaction_life, fill = factor(year))) +
  geom_boxplot() +
  labs(title = "",
       x = "Years",
       y = "Satisfaction with Life", fill='Year') 

# Summarize Q49 over time
trend_data <- df_trend %>%
  group_by(year) %>%
  summarize(
    Mean_Q49 = mean(satisfaction_life, na.rm = TRUE),
    Median_Q49 = median(satisfaction_life, na.rm = TRUE),
    MA_Mean_Q49 = rollmean(Mean_Q49, k = 3, fill = NA)
  )
trend_data

ggplot(trend_data, aes(x = year)) +
  geom_line(aes(y = Mean_Q49, color = "Mean Satisfaction with Life"), size = 1) +
  geom_line(aes(y = Median_Q49, color = "Median Satisfaction with Life"), linetype = "dashed", size = 1) +
  labs(
    title = "Development of Satisfaction with Life Over Time",
    x = "Year",
    y = "Satisfaction with Life",
    color = "Metric"
  ) +
  theme_minimal()



# Add moving averages
trend_data <- trend_data %>%
  mutate(
    MA_Mean_Q49 = rollmean(Mean_Q49, k = 3, fill = NA)
  )

# Plot with moving average
ggplot(trend_data, aes(x = year)) +
  geom_line(aes(y = Mean_Q49, color = "Mean Satisfaction with Life"), size = 1) +
  geom_line(aes(y = MA_Mean_Q49, color = "3-Year Moving Average"), linetype = "dotted", size = 1) +
  labs(
    title = "Smoothed Development of Satisfaction with Life Over Time",
    x = "Year",
    y = "Satisfaction with Life",
    color = "Metric"
  ) +
  theme_minimal()


ggplot(df_trend, aes(x = year, y = satisfaction_life)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", linetype = "dashed") +
  labs(
    title = "Trend of Satisfaction with Life Over Time",
    x = "Year",
    y = "Satisfaction with Life"
  ) +
  theme_minimal()





correlation <- df_trend %>%
  summarise(
    correlation_financial = cor(satisfaction_life, financial, method = "spearman", use = "complete.obs")
  )

print(correlation)

# Summarize financial satisfaction and health over time
factors_summary <- df_trend %>%
  group_by(year) %>%
  summarise(
    mean_satisfaction_life = mean(satisfaction_life, na.rm = TRUE),
    median_satisfaction_life = median(satisfaction_life, na.rm = TRUE),
    mean_financial = mean(financial, na.rm = TRUE),
    median_financial = median(financial, na.rm = TRUE),
    
    count = n() # Total observations per year
  )

# View the summarized data
print(factors_summary)


ggplot(df_germ, aes(x = factor(income_level, ordered = TRUE), y = swl)) +
  geom_boxplot() +
  facet_wrap(~ sfs) +
  labs(x = "Income Level", y = "Satisfaction with Life") 



# Stacked bar plot

ggplot(main_df, aes(x = country, fill = factor(swl))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) + # Percentage scale
  labs(
    title = "Well-Being Distribution Across Countries (Stacked Bar Plot)",
    x = "Country",
    y = "Proportion",
    fill = "Well-Being Levels"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) # Center the title

# Calculate proportions for each country and satisfaction level
df_prop <- main_df %>%
  group_by(country, swl) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ungroup()

df_prop2 <- main_df %>%
  group_by(country, sfs) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ungroup()

df_prop3 <- main_df %>%
  group_by(country, soh) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ungroup()

# Create heatmap based on proportions
ggplot(df_prop, aes(x = country, y = factor(swl), fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", name = "Proportion") +
  labs(
    title = "",
    x = "Countries",
    y = "Satisfaction with Life"
  ) 

# Create heatmap based on proportions
ggplot(df_prop2, aes(x = country, y = factor(sfs), fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", name = "Proportion") +
  labs(
    title = "",
    x = "Countries",
    y = "Satisfaction with financial situation"
  )

# Create heatmap based on proportions
ggplot(df_prop3, aes(x = country, y = factor(soh), fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkblue", name = "Proportion") +
  labs(
    title = "",
    x = "Countries",
    y = "State of health"
  )

table_dataa <- as.data.frame(table(df_trend$year, df_trend$satisfaction_life))

df_propa <- df_trend %>%
  group_by(year, satisfaction_life) %>%
  summarise(Frequency = n()) %>%
  mutate(Proportion = Frequency / sum(Frequency)) %>%
  ungroup()

ggplot(df_propa, aes(x = factor(year), y = factor(satisfaction_life), fill = Proportion)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue") +
  labs(
    title = "",
    x = "Years",
    y = "Satisfaction with Life",
    fill = "Proportion"
  ) 





