# ============================================================================
# PREDICTIVE ANALYSIS: Testing "Emotional Connection Predicts Satisfaction"
# Using Combined Data from Movies, Music, and Social Media
# All variables on 1-5 scale for consistency
# ============================================================================

# Load required libraries
library(tidyverse)
library(caret)
library(glmnet)
library(car)
library(effectsize)
library(ggplot2)
library(meta)
library(pROC)

# Set seed for reproducibility
set.seed(123)


# ============================================================================
# PART 1: LOAD AND COMBINE DATASETS 
# ============================================================================

cat("LOADING AND COMBINING DATASETS\n")
setwd("G:\\TPSM_Assignment_2026\\data\\processed\\musicEntertainmentType\\sample")

# Load your three datasets (adjust paths as needed)
movies_data <- read.csv("sample_movie_dataset.csv")        
social_data <- read.csv("sample_social_media_dataset.csv")        
music_data <- read.csv("sample_music_dataset.csv")   

cat("\nDataset sizes:\n")
cat(" Social Media:", nrow(social_data), "observations\n")  #1500 observations
cat(" Movies:", nrow(movies_data), "observations\n")        #1500 observations
cat(" Music:", nrow(music_data), "observations\n")          #1500 observations

#---------------------------------------------------------------------------
# 1.1 Standardize variable names across datasets
#---------------------------------------------------------------------------

# Movies dataset preparation
#-----------------------------------------------------------------
movies_combined <- movies_data %>%
  mutate(
    activity = "Movies",
    emotional_intensity_original = emotional_intensity,
    satisfaction = satisfaction,  # 1-5 scale
    
    # Create binary satisfaction based on median
    satisfaction_binary = ifelse(satisfaction > median(satisfaction, na.rm = TRUE), 1, 0),
    
    # Format demographics
    gender = case_when(
      gender == "Female" ~ "Female",
      gender == "Male" ~ "Male",
      TRUE ~ "Other/Not Specified"
    ),
    
    age_group = factor(age_group), 
    emotional_group = ifelse(emotional_intensity_original > median(emotional_intensity_original, na.rm = TRUE), 
                             "High", "Low"),
    
    #Standardize emotional intensity
    emotional_intensity_std = as.numeric(scale(emotional_intensity_original))
  ) %>%
  select(activity, 
         emotional_intensity = emotional_intensity_std,  
         emotional_intensity_original,  # Keep for reference
         satisfaction, satisfaction_binary, gender, age_group, emotional_group)


# Music dataset preparation
#-------------------------------------------------------------------------
music_combined <- music_data %>%
  mutate(
    activity = "Music",
    emotional_intensity_original = emotional_intensity,  # Original 1-5 scale
    satisfaction_binary = case_when(
      liked == 1 & disliked == 0 ~ 1,  # Satisfied
      liked == 0 & disliked == 1 ~ 0,  # Dissatisfied
      liked == 1 & disliked == 1 ~ NA_real_,  # Mixed - exclude
      liked == 0 & disliked == 0 ~ NA_real_,  # Neutral - exclude
      TRUE ~ NA_real_
    ),
    
    # Demographics
    gender = gender,
    age_group = cut(age,
                    breaks = c(0, 12, 18, 25, 35, 50, Inf),
                    labels = c("0-12", "13-18", "19-25", "26-35", "36-50", "50+"),
                    include.lowest = TRUE),
    
    emotional_group = ifelse(emotional_intensity > median(emotional_intensity, na.rm = TRUE), 
                             "High", "Low"),
    emotional_intensity_std = as.numeric(scale(emotional_intensity_original))
  ) %>%
  # Remove rows with NA satisfaction
  filter(!is.na(satisfaction_binary)) %>%
  select(activity, emotional_intensity = emotional_intensity_std,
         emotional_intensity_original, satisfaction_binary, gender, age_group, emotional_group)


# Social media dataset preparation
#---------------------------------------------------------------------
social_combined <- social_data %>%
  mutate(
    activity = "Social Media",
    emotional_intensity_original = emotional_intensity,
    # Satisfaction is already 0-10 scale
    satisfaction = satisfaction, 
    # Create binary satisfaction based on median
    satisfaction_binary = ifelse(satisfaction > median(satisfaction, na.rm = TRUE), 1, 0),
    
    # Format demographics properly
    gender = case_when(
      audience_gender_distribution == "female" ~ "Female",
      audience_gender_distribution == "male" ~ "Male",
      TRUE ~ "Other/Not Specified"
    ),
    
    # Age group - ensure it's a factor with proper levels
    age_group = factor(audience_age_distribution, 
                       levels = c("13-18", "19-25", "26-35", "36-50", "50+")),
    
    emotional_group = ifelse(emotional_intensity_original > median(emotional_intensity_original, na.rm = TRUE), 
                             "High", "Low"),
    
    # Standardize emotional intensity LAST (for regression)
    emotional_intensity_std = as.numeric(scale(emotional_intensity_original))
  ) %>%
  select(activity, 
         emotional_intensity = emotional_intensity_std,  # Standardized for analysis
         emotional_intensity_original,  # Keep for reference
         satisfaction, satisfaction_binary, gender, age_group, emotional_group)


#---------------------------------------------------------------------------
# 1.2 Combine all datasets
#---------------------------------------------------------------------------

all_data_combined <- bind_rows(social_combined, movies_combined, music_combined)


cat("\nCombined dataset size: n =", nrow(all_data_combined), "\n")  #n = 4500 
cat("\nDistribution by activity:\n")
print(table(all_data_combined$activity))   # Movies -1500 Music - 1500  Social Media - 1500 


# Save combined dataset
setwd("G:\\TPSM_Assignment_2026\\data\\combined")
saveRDS(all_data_combined, "combined_predictive_data.rds")
write.csv(all_data_combined, "combined_predictive_data.csv", row.names = FALSE)

cat("\n✓ Combined dataset saved with", ncol(all_data_combined), "variables\n")   #8 variables


# ============================================================================
# PART 2: DESCRIPTIVE OVERVIEW OF COMBINED DATA
# ============================================================================

# Summary by activity
activity_summary <- all_data_combined %>%
  group_by(activity) %>%
  summarise(
    n = n(),
    mean_sat = round(mean(satisfaction_binary), 2),
    sd_sat = round(sd(satisfaction_binary), 2),
    mean_emo = round(mean(emotional_intensity), 2),
    sd_emo = round(sd(emotional_intensity), 2),
    correlation = round(cor(emotional_intensity, satisfaction_binary), 3)
  )

cat("\nSummary by Activity:\n")
print(activity_summary)
# activity         n mean_sat sd_sat mean_emo sd_emo correlation
# <chr>        <int>    <dbl>  <dbl>    <dbl>  <dbl>       <dbl>
#  Movies        1500     0.49   0.5         0      1       0.054
#  Music         1500     0.67   0.47        0      1       0.249
#  Social Media  1500     0.49   0.5         0      1       0.021

# Emotional Intensity by Dataset
emo_by_dataset <- all_data_combined %>%
  group_by(activity) %>%
  summarise(
    n = n(),
    mean_emo = round(mean(emotional_intensity, na.rm = TRUE), 2),
    sd_emo = round(sd(emotional_intensity, na.rm = TRUE), 2),
    min_emo = round(min(emotional_intensity, na.rm = TRUE), 2),
    max_emo = round(max(emotional_intensity, na.rm = TRUE), 2),
    .groups = 'drop'
  )

cat("\nEmotional Intensity by Dataset:\n")
print(emo_by_dataset)
# activity         n mean_emo sd_emo min_emo max_emo
# <chr>        <int>    <dbl>  <dbl>   <dbl>   <dbl>
#  Movies        1500        0      1   -2.88    2.23
#  Music         1500        0      1   -2.28    3.45
#  Social Media  1500        0      1   -0.92    4.96

# Satisfaction by Dataset
sat_binary <- all_data_combined %>%
  group_by(activity) %>%
  summarise(
    n = n(),
    satisfaction_rate = round(mean(satisfaction_binary, na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  )

cat("\nBinary Satisfaction Rate by Dataset:\n")
print(sat_binary)
# activity         n satisfaction_rate
# <chr>        <int>             <dbl>
#  Movies        1500              49.4
#  Music         1500              67  
#  Social Media  1500              49.3



# Correlation Analysis
#-----------------------------------------------------
# Social Media Correlation
cor_social <- cor.test(social_combined$emotional_intensity,
                       social_combined$satisfaction_binary,
                       method = "pearson")

# Movies Correlation
cor_movie <- cor.test(movies_combined$emotional_intensity,
                      movies_combined$satisfaction_binary,
                      method = "pearson")

# Music Correlation (point-biserial, since satisfaction is binary)
cor_music <- cor.test(music_combined$emotional_intensity,
                      music_combined$satisfaction_binary,
                      method = "pearson")

# Summary Table
correlation_summary <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  Correlation = c(
    round(cor_social$estimate, 3),
    round(cor_movie$estimate, 3),
    round(cor_music$estimate, 3)
  ),
  CI_Lower = c(
    round(cor_social$conf.int[1], 3),
    round(cor_movie$conf.int[1], 3),
    round(cor_music$conf.int[1], 3)
  ),
  CI_Upper = c(
    round(cor_social$conf.int[2], 3),
    round(cor_movie$conf.int[2], 3),
    round(cor_music$conf.int[2], 3)
  ),
  P_value = c(
    round(cor_social$p.value, 4),
    round(cor_movie$p.value, 4),
    round(cor_music$p.value, 4)
  ),
  Effect_Size = c(
    ifelse(cor_social$p.value < 0.05, "Significant", "Not significant"),
    ifelse(cor_movie$p.value < 0.05, "Significant", "Not significant"),
    ifelse(cor_music$p.value < 0.05, "Significant", "Not significant")
  ),
  R_squared = c(
    round(cor_social$estimate^2, 3),
    round(cor_movie$estimate^2, 3),
    round(cor_music$estimate^2, 3)
  )
)

cat("\nCorrelation Summary Across Datasets:\n")
print(correlation_summary)
# Dataset         Correlation CI_Lower CI_Upper P_value     Effect_Size     R_squared
# Social Media      0.021   -0.030    0.071     0.4173     Not significant     0.000
# Movies            0.054    0.004    0.104     0.0359     Significant         0.003
# Music             0.249    0.201    0.295     0.0000     Significant         0.062

# Overall correlation
cor_overall <- cor.test(all_data_combined$emotional_intensity, all_data_combined$satisfaction_binary)
cat("\nOverall correlation: r =", round(cor_overall$estimate, 3), 
    ", p =", round(cor_overall$p.value, 4), "\n")  #r = 0.104 , p = 0 


setwd("G:\\TPSM_Assignment_2026\\outputs\\descriptive\\combined")

# Calculate satisfaction rates by activity
sat_rates <- all_data_combined %>%
  group_by(activity) %>%
  summarise(
    satisfaction_rate = mean(satisfaction_binary, na.rm = TRUE) * 100,
    se = sd(satisfaction_binary, na.rm = TRUE) / sqrt(n()),
    ci_lower = satisfaction_rate - 1.96 * se,
    ci_upper = satisfaction_rate + 1.96 * se
  )

p1 <- ggplot(sat_rates, aes(x = activity, y = satisfaction_rate, fill = activity)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_text(aes(label = paste0(round(satisfaction_rate, 1), "%")), 
            vjust = -0.5, size = 4) +
  labs(title = "Satisfaction Rate by Activity",
       subtitle = "Error bars show 95% Confidence Intervals",
       x = "Activity", 
       y = "Satisfaction Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, 100)

ggsave("satisfaction_rate_by_activity.png", p1, width = 8, height = 5)


# T-TEST COMPARISON (High vs Low Emotional Groups)
social_high <- social_combined %>% filter(emotional_group == "High") %>% pull(satisfaction_binary)
social_low <- social_combined %>% filter(emotional_group == "Low") %>% pull(satisfaction_binary)
t_social <- t.test(social_high, social_low)
d_social <- cohens_d(satisfaction_binary ~ emotional_group, data = social_combined)

movie_high <- movies_combined %>% filter(emotional_group == "High") %>% pull(satisfaction_binary)
movie_low <- movies_combined %>% filter(emotional_group == "Low") %>% pull(satisfaction_binary)
t_movie <- t.test(movie_high, movie_low)
d_movie <- cohens_d(satisfaction_binary ~ emotional_group, data = movies_combined)

music_high <- music_combined %>% filter(emotional_group == "High") %>% pull(satisfaction_binary)
music_low <- music_combined %>% filter(emotional_group == "Low") %>% pull(satisfaction_binary)
t_music <- t.test(music_high, music_low)
d_music <- cohens_d(satisfaction_binary ~ emotional_group, data = music_combined)


group_stats_combined <- bind_rows(
  social_combined %>% group_by(emotional_group) %>% summarise(
    dataset = "Social Media",
    n = n(),
    mean_sat = mean(satisfaction_binary, na.rm = TRUE),
    sd_sat = sd(satisfaction_binary, na.rm = TRUE),
    .groups = 'drop'
  ),
  movies_combined %>% group_by(emotional_group) %>% summarise(
    dataset = "Movies",
    n = n(),
    mean_sat = mean(satisfaction_binary, na.rm = TRUE),
    sd_sat = sd(satisfaction_binary, na.rm = TRUE),
    .groups = 'drop'
  ),
  music_combined %>% group_by(emotional_group) %>% summarise(
    dataset = "Music",
    n = n(),
    mean_sat = mean(satisfaction_binary, na.rm = TRUE),
    sd_sat = sd(satisfaction_binary, na.rm = TRUE),
    .groups = 'drop'
  )
)

cat("\nGroup Statistics by Dataset:\n")
print(group_stats_combined)
# emotional_group dataset          n mean_sat sd_sat
# <chr>           <chr>        <int>    <dbl>  <dbl>
#  High            Social Media   250    0.512  0.501
#  Low             Social Media  1250    0.490  0.500
#  High            Movies         748    0.517  0.500
#  Low             Movies         752    0.471  0.499
#  High            Music          536    0.795  0.404
#  Low             Music          964    0.601  0.490



ttest_summary <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  t_value = c(
    round(t_social$statistic, 3),
    round(t_movie$statistic, 3),
    round(t_music$statistic, 3)
  ),
  df = c(
    round(t_social$parameter, 1),
    round(t_movie$parameter, 1),
    round(t_music$parameter, 1)
  ),
  p_value = c(
    round(t_social$p.value, 4),
    round(t_movie$p.value, 4),
    round(t_music$p.value, 4)
  ),
  Mean_Diff = c(
    round(mean(social_high) - mean(social_low), 3),
    round(mean(movie_high) - mean(movie_low), 3),
    round(mean(music_high) - mean(music_low), 3)
  ),
  Cohen_d = c(
    round(d_social$Cohens_d, 3),
    round(d_movie$Cohens_d, 3),
    round(d_music$Cohens_d, 3)
  ),
  Significant = c(
    ifelse(t_social$p.value < 0.05, "YES", "NO"),
    ifelse(t_movie$p.value < 0.05, "YES", "NO"),
    ifelse(t_music$p.value < 0.05, "YES", "NO")
  )
)

cat("\nT-test Summary Across Datasets:\n")
print(ttest_summary)       
#Dataset           t_value     df  p_value Mean_Diff Cohen_d Significant
# Social Media   0.646      355.4   0.5189     0.022   0.045          NO
# Movies         1.807      1497.9  0.0710     0.047   0.093          NO
# Music          8.249      1288.5  0.0000     0.194   0.421         YES


# LOGISTIC REGRESSION ANALYSIS ACROSS DATASETS
#--------------------------------------------------
glm_social <- glm(satisfaction_binary ~ emotional_intensity, data = social_combined, family = binomial)
summary_social <- summary(glm_social)

glm_movie <- glm(satisfaction_binary ~ emotional_intensity, data = movies_combined, family = binomial)
summary_movie <- summary(glm_movie)

glm_music <- glm(satisfaction_binary ~ emotional_intensity, data = music_combined, family = binomial)
summary_music <- summary(glm_music)

regression_summary <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  Model_Type = c("Logistic", "Logistic", "Logistic"),
  Coefficient = c(
    round(coef(glm_social)[2], 3),
    round(coef(glm_movie)[2], 3),
    round(coef(glm_music)[2], 3)
  ),
  Std_Error = c(
    round(summary_social$coefficients[2, 2], 3),
    round(summary_movie$coefficients[2, 2], 3),
    round(summary_music$coefficients[2, 2], 3)
  ),
  Test_Statistic = c(
    round(summary_social$coefficients[2, 3], 3),
    round(summary_movie$coefficients[2, 3], 3),
    round(summary_music$coefficients[2, 3], 3)
  ),
  P_value = c(
    round(summary_social$coefficients[2, 4], 4),
    round(summary_movie$coefficients[2, 4], 4),
    round(summary_music$coefficients[2, 4], 4)
  ),
  Significant = c(
    ifelse(summary_social$coefficients[2, 4] < 0.05, "YES", "NO"),
    ifelse(summary_movie$coefficients[2, 4] < 0.05, "YES", "NO"),
    ifelse(summary_music$coefficients[2, 4] < 0.05, "YES", "NO")
  )
)

cat("\nRegression Summary Across Datasets:\n")
print(regression_summary)
# Dataset        Model_Type Coefficient Std_Error Test_Statistic P_value Significant
# Social Media   Logistic       0.042     0.052          0.812  0.4171          NO
# Movies         Logistic       0.109     0.052          2.095  0.0362         YES
# Music          Logistic       0.549     0.058          9.418  0.0000         YES


# Calculate odds ratios and CIs
or_social <- exp(coef(glm_social)[2])
or_movie <- exp(coef(glm_movie)[2])
or_music <- exp(coef(glm_music)[2])

ci_social <- exp(confint(glm_social)[2, ])
ci_movie <- exp(confint(glm_movie)[2, ])
ci_music <- exp(confint(glm_music)[2, ])


# META-ANALYSIS: COMBINED EFFECT SIZES
#-----------------------------------------------
# Effect Size Comparison
effect_sizes <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  Metric = c("Odds Ratio", "Odds Ratio", "Odds Ratio"),
  OR = c(
    round(or_social, 3),
    round(or_movie, 3),
    round(or_music, 3)
  ),
  CI = c(
    paste0("[", round(ci_social[1], 3), ", ", round(ci_social[2], 3), "]"),
    paste0("[", round(ci_movie[1], 3), ", ", round(ci_movie[2], 3), "]"),
    paste0("[", round(ci_music[1], 3), ", ", round(ci_music[2], 3), "]")
  ),
  P_value = c(
    round(summary_social$coefficients[2, 4], 4),
    round(summary_movie$coefficients[2, 4], 4),
    round(summary_music$coefficients[2, 4], 4)
  ),
  Interpretation = c(
    ifelse(summary_social$coefficients[2, 4] > 0.05, "Not significant",
           ifelse(or_social < 1.5, "Small effect",
                  ifelse(or_social < 3.5, "Moderate effect", "Large effect"))),
    ifelse(summary_movie$coefficients[2, 4] > 0.05, "Not significant",
           ifelse(or_movie < 1.5, "Small effect",
                  ifelse(or_movie < 3.5, "Moderate effect", "Large effect"))),
    ifelse(summary_music$coefficients[2, 4] > 0.05, "Not significant",
           ifelse(or_music < 1.5, "Small effect",
                  ifelse(or_music < 3.5, "Moderate effect", "Large effect")))
  )
)

cat("\n=== EFFECT SIZES ACROSS DATASETS (ODDS RATIOS) ===\n")
print(effect_sizes)
# Dataset        Metric    OR         CI       P_value  Interpretation
# Social Media Odds Ratio 1.043 [0.942, 1.154]  0.4171    Not significant
# Movies       Odds Ratio 1.115 [1.007, 1.235]  0.0362    Small effect
# Music        Odds Ratio 1.731 [1.546, 1.942]  0.0000    Moderate effect



# WEIGHTED AVERAGE (ON LOG SCALE)
# Get log odds ratios and standard errors
logOR_social <- coef(glm_social)[2]
logOR_movie <- coef(glm_movie)[2]
logOR_music <- coef(glm_music)[2]

se_social <- summary_social$coefficients[2, 2]
se_movie <- summary_movie$coefficients[2, 2]
se_music <- summary_music$coefficients[2, 2]

# Inverse variance weights
weights <- c(1/se_social^2, 1/se_movie^2, 1/se_music^2)

# Weighted average log OR
weighted_logOR <- weighted.mean(
  x = c(logOR_social, logOR_movie, logOR_music),
  w = weights
)

# Convert back to OR
weighted_OR <- exp(weighted_logOR)

# Calculate SE of weighted average
weighted_se <- sqrt(1/sum(weights))

# Confidence interval
weighted_ci_lower <- exp(weighted_logOR - 1.96 * weighted_se)
weighted_ci_upper <- exp(weighted_logOR + 1.96 * weighted_se)

cat("\n=== WEIGHTED AVERAGE (Meta-analytic) ===\n")
cat("Weighted average Odds Ratio =", round(weighted_OR, 3), "\n")                          #1.233 
cat("95% CI = [", round(weighted_ci_lower, 3), ", ", round(weighted_ci_upper, 3), "]\n")   #[ 1.16 ,  1.31 ]

if(weighted_ci_lower > 1) {
  cat("✓ Overall significant positive effect\n")
} else if(weighted_ci_upper < 1) {
  cat("✗ Overall significant negative effect\n")
} else {
  cat("⚠ Overall effect not significant (CI includes 1)\n")
}                                                                     #✓ Overall significant positive effect

# COMBINED VISUALIZATIONS
#----------------------------------------
or_plot_data <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  OR = c(or_social, or_movie, or_music),
  CI_Lower = c(ci_social[1], ci_movie[1], ci_music[1]),
  CI_Upper = c(ci_social[2], ci_movie[2], ci_music[2]),
  Significant = c(
    summary_social$coefficients[2, 4] < 0.05,
    summary_movie$coefficients[2, 4] < 0.05,
    summary_music$coefficients[2, 4] < 0.05
  )
)

p2 <- ggplot(or_plot_data, aes(x = Dataset, y = OR, color = Significant)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  scale_color_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336")) +
  labs(title = "Odds Ratios: Emotional Intensity → Satisfaction",
       subtitle = "Error bars show 95% CI. Red line = OR=1 (no effect)",
       y = "Odds Ratio") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 2.5, 0.25))

ggsave("odds_ratio_comparison.png", p2, width = 8, height = 5)


effect_plot_data <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  Effect_Size = c(or_social, or_movie, or_music),
  OR_Value = c(or_social, or_movie, or_music),
  Significant = c(
    summary_social$coefficients[2, 4] < 0.05,
    summary_movie$coefficients[2, 4] < 0.05,
    summary_music$coefficients[2, 4] < 0.05
  )
)

p3 <- ggplot(effect_plot_data, aes(x = Dataset, y = Effect_Size, fill = Significant)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(OR_Value, 2)), vjust = -0.5, size = 4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336")) +
  labs(title = "Effect Sizes Across Datasets (Odds Ratios)",
       subtitle = "Values > 1 indicate positive effect. Red line = OR=1 (no effect)",
       y = "Odds Ratio") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, max(effect_plot_data$Effect_Size) * 1.2)

ggsave("effect_size_comparison.png", p3, width = 8, height = 5)


# Satisfaction Rates by Dataset and Emotional Group
sat_rate_data <- bind_rows(
  social_combined %>%
    group_by(emotional_group) %>%
    summarise(
      dataset = "Social Media",
      satisfaction_rate = mean(satisfaction_binary) * 100,
      se = sd(satisfaction_binary) / sqrt(n()),
      .groups = 'drop'
    ),
  movies_combined %>%
    group_by(emotional_group) %>%
    summarise(
      dataset = "Movies",
      satisfaction_rate = mean(satisfaction_binary) * 100,
      se = sd(satisfaction_binary) / sqrt(n()),
      .groups = 'drop'
    ),
  music_combined %>%
    group_by(emotional_group) %>%
    summarise(
      dataset = "Music",
      satisfaction_rate = mean(satisfaction_binary) * 100,
      se = sd(satisfaction_binary) / sqrt(n()),
      .groups = 'drop'
    )
)


p4 <- ggplot(sat_rate_data, aes(x = dataset, y = satisfaction_rate, fill = emotional_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = satisfaction_rate - 1.96*se,
                    ymax = satisfaction_rate + 1.96*se),
                position = position_dodge(width = 0.9), width = 0.2) +
  scale_fill_manual(values = c("High" = "#4CAF50", "Low" = "#F44336")) +
  labs(title = "Satisfaction Rates by Dataset and Emotional Group",
       subtitle = "Error bars show 95% CI",
       x = "Dataset", y = "Satisfaction Rate (%)", fill = "Emotional Group") +
  theme_minimal() +
  ylim(0, 100)
ggsave("satisfaction_rates_by_group.png", p4, width = 10, height = 6)


# Scatterplot Comparison (Emotional Intensity vs Satisfaction)
scatter_data <- bind_rows(
  social_combined %>% mutate(dataset = "Social Media", 
                             satisfaction_value = satisfaction_binary),
  movies_combined %>% mutate(dataset = "Movies", 
                             satisfaction_value = satisfaction_binary),
  music_combined %>% mutate(dataset = "Music", 
                            satisfaction_value = satisfaction_binary)
)

p5 <- ggplot(scatter_data, aes(x = emotional_intensity, y = satisfaction_value)) +
  geom_point(alpha = 0.2, color = "darkgray", size = 0.8) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              color = "blue", se = TRUE) +
  labs(title = "Emotional Intensity vs Satisfaction Probability",
       subtitle = "Logistic regression curves with 95% CI",
       x = "Emotional Intensity (standardized)", 
       y = "Probability of Satisfaction") +
  theme_minimal() +
  facet_wrap(~dataset) +
  ylim(0, 1)

ggsave("logistic_curves_comparison.png", p5, width = 12, height = 8)


# SUMMARY TABLE
summary_table <- data.frame(
  Metric = c(
    "Sample Size",
    "Emotional Intensity (Mean)",
    "Emotional Intensity (SD)",
    "Satisfaction Rate (%)",
    "Odds Ratio (OR)",
    "OR 95% CI Lower",
    "OR 95% CI Upper",
    "OR p-value",
    "T-test p-value",
    "Cohen's d",
    "Regression p-value",
    "Supports Hypothesis?"
  ),
  Social_Media = c(
    nrow(social_combined),
    round(mean(social_combined$emotional_intensity), 2),
    round(sd(social_combined$emotional_intensity), 2),
    paste0(round(mean(social_combined$satisfaction_binary)*100, 1), "%"),
    round(or_social, 3),
    round(ci_social[1], 3),
    round(ci_social[2], 3),
    round(summary_social$coefficients[2, 4], 4),
    round(t_social$p.value, 4),
    round(d_social$Cohens_d, 3),
    round(summary_social$coefficients[2, 4], 4),
    ifelse(summary_social$coefficients[2, 4] < 0.05, "YES", "NO")
  ),
  Movies = c(
    nrow(movies_combined),
    round(mean(movies_combined$emotional_intensity), 2),
    round(sd(movies_combined$emotional_intensity), 2),
    paste0(round(mean(movies_combined$satisfaction_binary)*100, 1), "%"),
    round(or_movie, 3),
    round(ci_movie[1], 3),
    round(ci_movie[2], 3),
    round(summary_movie$coefficients[2, 4], 4),
    round(t_movie$p.value, 4),
    round(d_movie$Cohens_d, 3),
    round(summary_movie$coefficients[2, 4], 4),
    ifelse(summary_movie$coefficients[2, 4] < 0.05, "YES", "NO")
  ),
  Music = c(
    nrow(music_combined),
    round(mean(music_combined$emotional_intensity), 2),
    round(sd(music_combined$emotional_intensity), 2),
    paste0(round(mean(music_combined$satisfaction_binary)*100, 1), "%"),
    round(or_music, 3),
    round(ci_music[1], 3),
    round(ci_music[2], 3),
    round(summary_music$coefficients[2, 4], 4),
    round(t_music$p.value, 4),
    round(d_music$Cohens_d, 3),
    round(summary_music$coefficients[2, 4], 4),
    ifelse(summary_music$coefficients[2, 4] < 0.05, "YES", "NO")
  )
)

cat("\n=== COMPREHENSIVE SUMMARY TABLE ===\n")
print(summary_table)
write.csv(summary_table, "comprehensive_summary_logistic.csv", row.names = FALSE)


# Count how many datasets support the hypothesis
support_count <- sum(
  summary_social$coefficients[2, 4] < 0.05,
  summary_movie$coefficients[2, 4] < 0.05,
  summary_music$coefficients[2, 4] < 0.05
)


cat("\nFINAL CONCLUSION - LOGISTIC REGRESSION RESULTS\n")
cat("\nSupport across datasets:\n")
cat(" • Social Media:", ifelse(summary_social$coefficients[2, 4] < 0.05, "✓ SUPPORTS", "✗ DOES NOT SUPPORT"), 
    "(OR =", round(or_social, 3), ", p =", round(summary_social$coefficients[2, 4], 4), ")\n")                    #Social Media: ✗ DOES NOT SUPPORT (OR = 1.043 , p = 0.4171 )
cat(" • Movies:", ifelse(summary_movie$coefficients[2, 4] < 0.05, "✓ SUPPORTS", "✗ DOES NOT SUPPORT"),
    "(OR =", round(or_movie, 3), ", p =", round(summary_movie$coefficients[2, 4], 4), ")\n")                      #Movies: ✓ SUPPORTS (OR = 1.115 , p = 0.0362 )
cat(" • Music:", ifelse(summary_music$coefficients[2, 4] < 0.05, "✓ SUPPORTS", "✗ DOES NOT SUPPORT"),
    "(OR =", round(or_music, 3), ", p =", round(summary_music$coefficients[2, 4], 4), ")\n\n")                    # Music: ✓ SUPPORTS (OR = 1.731 , p = 0 )

cat("Overall pattern: ", support_count, "out of 3 datasets support the hypothesis\n\n")    #2 out of 3 datasets support the hypothesis

if(support_count == 3) {
  cat("✓✓✓ STRONG EVIDENCE: All three datasets support the hypothesis\n")
  cat("Emotional connection consistently predicts higher satisfaction across\n")
  cat("social media, movies, and music contexts.\n")
  cat("Odds ratios range from", round(min(or_social, or_movie, or_music), 2), 
      "to", round(max(or_social, or_movie, or_music), 2), "\n")
} else if(support_count == 2) {
  cat("✓ MODERATE EVIDENCE: Two out of three datasets support the hypothesis\n")
  cat("The relationship holds in most contexts but may be domain-specific.\n")
} else if(support_count == 1) {
  cat("⚠ WEAK EVIDENCE: Only one dataset supports the hypothesis\n")
  cat("Music shows strong effect (OR =", round(or_music, 3), 
      ", ", round((or_music-1)*100, 1), "% higher odds)\n")
  cat("But effect not found in Movies or Social Media.\n")
} else {
  cat("✗ NO EVIDENCE: None of the datasets support the hypothesis\n")
  cat("Strong evidence that emotional connection does NOT predict satisfaction\n")
  cat("across these contexts.\n")
}                                                                                                     #✓ MODERATE EVIDENCE: Two out of three datasets support the hypothesis. The relationship holds in most contexts but may be domain-specific.

cat("\nKey observations:\n")
cat(" • Strongest effect: Music dataset (OR =", round(or_music, 3), 
    ", ", round((or_music-1)*100, 1), "% higher odds)\n")
cat(" • Weakest effect: Social Media dataset (OR =", round(or_social, 3), ")\n")
cat(" • Movies dataset shows", 
    ifelse(summary_movie$coefficients[2, 4] < 0.05, "significant", "non-significant"),
    "effect (OR =", round(or_movie, 3), ")\n\n")

#Strongest effect: Music dataset (OR = 1.731 ,  73.1 % higher odds)
#Weakest effect: Social Media dataset (OR = 1.043 )
#Movies dataset shows significant effect (OR = 1.115 )


# ============================================================================
# PART 3: META-ANALYSIS USING PEARSON CORRELATIONS
# ============================================================================

# ----------------------------------------------------------------------------
# 3.1 Extract correlation coefficients and sample sizes from each dataset
# ----------------------------------------------------------------------------

meta_data <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  
  # Correlation coefficients (r)
  r = c(
    cor_social$estimate,    # 0.021
    cor_movie$estimate,      # 0.054
    cor_music$estimate       # 0.249
  ),
  
  # Sample sizes (n)
  n = c(
    length(social_combined$emotional_intensity),   # 1500
    length(movies_combined$emotional_intensity),   # 1500
    length(music_combined$emotional_intensity)     # 1500
  ),
  
  # P-values (for reference)
  p = c(
    cor_social$p.value,      # 0.4173
    cor_movie$p.value,        # 0.0359
    cor_music$p.value         # 0.0000
  )
)

cat("\nCorrelations to be combined:\n")
print(meta_data)
# Dataset          r       n            p
# Social Media 0.02096009 1500 4.172548e-01
# Movies       0.05416424 1500 3.594375e-02
# Music        0.24859862 1500 1.455987e-22


# ----------------------------------------------------------------------------
# 3.2 Transform correlations to Fisher's z (for normal distribution)
# ----------------------------------------------------------------------------

# Fisher's z transformation: z = 0.5 * ln((1+r)/(1-r))
meta_data$z <- 0.5 * log((1 + meta_data$r) / (1 - meta_data$r))

# Standard error of z: se = 1/sqrt(n-3)
meta_data$se_z <- 1 / sqrt(meta_data$n - 3)

# Variance of z
meta_data$var_z <- meta_data$se_z^2

# 95% CI for z
meta_data$z_lower <- meta_data$z - 1.96 * meta_data$se_z
meta_data$z_upper <- meta_data$z + 1.96 * meta_data$se_z

# Transform back to r for interpretation
meta_data$r_lower <- (exp(2 * meta_data$z_lower) - 1) / (exp(2 * meta_data$z_lower) + 1)
meta_data$r_upper <- (exp(2 * meta_data$z_upper) - 1) / (exp(2 * meta_data$z_upper) + 1)

cat("\nCorrelations with 95% Confidence Intervals:\n")
for(i in 1:nrow(meta_data)) {
  cat(sprintf("  %-12s: r = %.3f [%.3f, %.3f]\n", 
              meta_data$Dataset[i],
              meta_data$r[i],
              meta_data$r_lower[i],
              meta_data$r_upper[i]))
}

# Social Media: r = 0.021 [-0.030, 0.071]
# Movies      : r = 0.054 [0.004, 0.104]
# Music       : r = 0.249 [0.201, 0.295]


# ----------------------------------------------------------------------------
# 3.3 Fixed-effect meta-analysis (assumes common true effect)
# ----------------------------------------------------------------------------

# Calculate weights (inverse variance)
meta_data$weight_fixed <- 1 / meta_data$var_z

# Weighted average of z (fixed effect)
fixed_z <- sum(meta_data$z * meta_data$weight_fixed) / sum(meta_data$weight_fixed)
fixed_se <- sqrt(1 / sum(meta_data$weight_fixed))

# 95% CI for fixed effect z
fixed_z_lower <- fixed_z - 1.96 * fixed_se
fixed_z_upper <- fixed_z + 1.96 * fixed_se

# Convert back to r
fixed_r <- (exp(2 * fixed_z) - 1) / (exp(2 * fixed_z) + 1)
fixed_r_lower <- (exp(2 * fixed_z_lower) - 1) / (exp(2 * fixed_z_lower) + 1)
fixed_r_upper <- (exp(2 * fixed_z_upper) - 1) / (exp(2 * fixed_z_upper) + 1)

# Test for significance
fixed_z_p <- 2 * (1 - pnorm(abs(fixed_z / fixed_se)))


cat(sprintf("Combined r = %.3f [%.3f, %.3f]\n", fixed_r, fixed_r_lower, fixed_r_upper))  #r = 0.109 [0.080, 0.138]
cat(sprintf("z = %.3f, se = %.4f, p = %.4f\n", fixed_z, fixed_se, fixed_z_p))             #z = 0.110, se = 0.0149, p = 0.0000

if(fixed_z_p < 0.05) {
  cat("✓ Significant overall effect (p < 0.05)\n")
} else {
  cat("✗ Non-significant overall effect (p >= 0.05)\n")
}                                                              #✓ Significant overall effect (p < 0.05)

# ----------------------------------------------------------------------------
# 3.4 Random-effects meta-analysis (assumes true effect varies)
# ----------------------------------------------------------------------------

# Calculate Q statistic for heterogeneity
fixed_effects <- meta_data$z
fixed_weights <- meta_data$weight_fixed
fixed_mean <- fixed_z

Q <- sum(fixed_weights * (fixed_effects - fixed_mean)^2)
df_Q <- nrow(meta_data) - 1
Q_p <- 1 - pchisq(Q, df_Q)

# Calculate I² (percentage of variation due to heterogeneity)
I2 <- max(0, (Q - df_Q) / Q * 100)

# Calculate between-studies variance (tau²)
# Using DerSimonian-Laird estimator
C <- sum(fixed_weights) - sum(fixed_weights^2) / sum(fixed_weights)
tau2 <- max(0, (Q - df_Q) / C)

# Random effects weights
meta_data$weight_random <- 1 / (meta_data$var_z + tau2)

# Weighted average of z (random effects)
random_z <- sum(meta_data$z * meta_data$weight_random) / sum(meta_data$weight_random)
random_se <- sqrt(1 / sum(meta_data$weight_random))

# 95% CI for random effect z
random_z_lower <- random_z - 1.96 * random_se
random_z_upper <- random_z + 1.96 * random_se

# Convert back to r
random_r <- (exp(2 * random_z) - 1) / (exp(2 * random_z) + 1)
random_r_lower <- (exp(2 * random_z_lower) - 1) / (exp(2 * random_z_lower) + 1)
random_r_upper <- (exp(2 * random_z_upper) - 1) / (exp(2 * random_z_upper) + 1)

# Test for significance
random_z_p <- 2 * (1 - pnorm(abs(random_z / random_se)))

cat("\n", rep("-", 40), "\n")
cat("RANDOM-EFFECTS META-ANALYSIS")
cat("\n", rep("-", 40), "\n")
cat(sprintf("Combined r = %.3f [%.3f, %.3f]\n", random_r, random_r_lower, random_r_upper))    #Combined r = 0.109 [-0.033, 0.247]
cat(sprintf("z = %.3f, se = %.4f, p = %.4f\n", random_z, random_se, random_z_p))              #z = 0.110, se = 0.0727, p = 0.1316

if(random_z_p < 0.05) {
  cat("✓ Significant overall effect (p < 0.05)\n")
} else {
  cat("✗ Non-significant overall effect (p >= 0.05)\n")
}                                                                #✗ Non-significant overall effect (p >= 0.05)

# ----------------------------------------------------------------------------
# 3.5 Heterogeneity assessment
# ----------------------------------------------------------------------------

cat(sprintf("Q-statistic = %.2f, df = %d, p = %.4f\n", Q, df_Q, Q_p))   #Q-statistic = 47.53, df = 2, p = 0.0000
cat(sprintf("I² = %.1f%%\n", I2))         #I² = 95.8%
cat(sprintf("τ² = %.4f\n", tau2))         #τ² = 0.0152

# Interpret I²
if(I2 < 25) {
  cat("Interpretation: Low heterogeneity\n")
} else if(I2 < 50) {
  cat("Interpretation: Moderate heterogeneity\n")
} else if(I2 < 75) {
  cat("Interpretation: Substantial heterogeneity\n")
} else {
  cat("Interpretation: Considerable heterogeneity\n")
}                                                              #Interpretation: Considerable heterogeneity

# Test if heterogeneity is significant
if(Q_p < 0.05) {
  cat("✓ Significant heterogeneity detected (p < 0.05)\n")
  cat(" Random-effects model is more appropriate\n")
} else {
  cat("✗ No significant heterogeneity (p >= 0.05)\n")
  cat(" Fixed-effect model may be appropriate\n")
}                                                               #✓ Significant heterogeneity detected (p < 0.05). Random-effects model is more appropriate

# ----------------------------------------------------------------------------
# 3.6 Create forest plot
# ----------------------------------------------------------------------------

# Prepare data for forest plot
forest_data <- meta_data
forest_data$r_display <- forest_data$r
forest_data$ci_lower <- forest_data$r_lower
forest_data$ci_upper <- forest_data$r_upper

# Add combined effects
combined_fixed <- data.frame(
  Dataset = "FE Combined",
  r_display = fixed_r,
  ci_lower = fixed_r_lower,
  ci_upper = fixed_r_upper,
  stringsAsFactors = FALSE
)

combined_random <- data.frame(
  Dataset = "RE Combined",
  r_display = random_r,
  ci_lower = random_r_lower,
  ci_upper = random_r_upper,
  stringsAsFactors = FALSE
)

forest_data_all <- rbind(forest_data[, c("Dataset", "r_display", "ci_lower", "ci_upper")],
                         combined_fixed, combined_random)

# Order for plot
forest_data_all$Dataset <- factor(forest_data_all$Dataset, 
                                  levels = c("Social Media", "Movies", "Music", 
                                             "FE Combined", "RE Combined"))

# Create forest plot
p_forest <- ggplot(forest_data_all, aes(x = r_display, y = Dataset)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4, aes(color = Dataset %in% c("FE Combined", "RE Combined"))) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"), guide = FALSE) +
  labs(title = "Meta-Analysis Forest Plot: Emotional Intensity → Satisfaction",
       subtitle = paste0("I² = ", round(I2, 1), "%, p(heterogeneity) = ", round(Q_p, 4)),
       x = "Pearson Correlation (r)", 
       y = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11),
        plot.title = element_text(face = "bold")) +
  scale_x_continuous(breaks = seq(-0.1, 0.4, 0.1), limits = c(-0.1, 0.4))

# Add correlation values as text
p_forest <- p_forest + 
  geom_text(aes(label = sprintf("%.3f [%.3f, %.3f]", r_display, ci_lower, ci_upper)),
            x = max(forest_data_all$ci_upper) + 0.05, hjust = 0, size = 3.5)

ggsave("meta_analysis_forest_plot.png", p_forest, width = 12, height = 6)

# ----------------------------------------------------------------------------
# 3.7 Publication bias assessment (funnel plot)
# ----------------------------------------------------------------------------

# Create funnel plot data
funnel_data <- meta_data
funnel_data$effect <- funnel_data$r
funnel_data$se <- funnel_data$se_z  # Using SE of z for funnel plot

# Create funnel plot
p_funnel <- ggplot(funnel_data, aes(x = effect, y = 1/se)) +
  geom_point(size = 4, color = "blue") +
  geom_vline(xintercept = random_r, linetype = "dashed", color = "red") +
  
  # Add pseudo-confidence intervals
  geom_function(fun = function(x) {1 / (abs(x - random_r) / 1.96)}, 
                color = "gray70", linetype = "dotted") +
  geom_function(fun = function(x) {1 / (abs(x - random_r) / 2.58)}, 
                color = "gray70", linetype = "dotted") +
  
  labs(title = "Funnel Plot for Publication Bias Assessment",
       subtitle = "Symmetric distribution suggests no publication bias",
       x = "Correlation (r)", 
       y = "Precision (1/SE)") +
  theme_minimal() +
  geom_text(aes(label = Dataset), hjust = -0.3, vjust = 0, size = 3.5) +
  ylim(0, max(1/funnel_data$se) * 1.1)

ggsave("meta_analysis_funnel_plot.png", p_funnel, width = 8, height = 6)

# ----------------------------------------------------------------------------
# 3.8 Summary table
# ----------------------------------------------------------------------------

meta_summary <- data.frame(
  Metric = c("Number of studies", "Total sample size", 
             "Fixed-effect r [95% CI]", "Fixed-effect p-value",
             "Random-effects r [95% CI]", "Random-effects p-value",
             "Heterogeneity Q (p-value)", "I² (%)", "τ²"),
  Value = c(
    nrow(meta_data),
    sum(meta_data$n),
    sprintf("%.3f [%.3f, %.3f]", fixed_r, fixed_r_lower, fixed_r_upper),
    sprintf("%.4f", fixed_z_p),
    sprintf("%.3f [%.3f, %.3f]", random_r, random_r_lower, random_r_upper),
    sprintf("%.4f", random_z_p),
    sprintf("%.2f (%.4f)", Q, Q_p),
    sprintf("%.1f", I2),
    sprintf("%.4f", tau2)
  )
)


cat("META-ANALYSIS SUMMARY TABLE")
print(meta_summary, row.names = FALSE)
# Metric                 Value
# Number of studies                     3
# Total sample size                  4500
# Fixed-effect r [95% CI]  0.109 [0.080, 0.138]
# Fixed-effect p-value                0.0000
# Random-effects r [95% CI] 0.109 [-0.033, 0.247]
# Random-effects p-value                0.1316
# Heterogeneity Q (p-value)        47.53 (0.0000)
# I² (%)                  95.8
# τ²                0.0152



# ----------------------------------------------------------------------------
# 3.9 Final conclusion
# ----------------------------------------------------------------------------
cat("META-ANALYSIS CONCLUSION")


# Choose appropriate model based on heterogeneity
if(Q_p < 0.05) {
  cat("\nBased on significant heterogeneity (p =", round(Q_p, 4), "):\n")
  cat("✓ Using RANDOM-EFFECTS model for interpretation\n")
  final_r <- random_r
  final_ci_lower <- random_r_lower
  final_ci_upper <- random_r_upper
  final_p <- random_z_p
} else {
  cat("\nBased on non-significant heterogeneity (p =", round(Q_p, 4), "):\n")
  cat("✓ Using FIXED-EFFECT model for interpretation\n")
  final_r <- fixed_r
  final_ci_lower <- fixed_r_lower
  final_ci_upper <- fixed_r_upper
  final_p <- fixed_z_p
}                                                                       #Based on significant heterogeneity (p = 0 ):
                                                                         #✓ Using RANDOM-EFFECTS model for interpretation

cat("\nCOMBINED EFFECT ACROSS ALL DATASETS:\n")
cat(sprintf("r = %.3f, 95%% CI [%.3f, %.3f], p = %.4f\n", 
            final_r, final_ci_lower, final_ci_upper, final_p))

# Interpret effect size (Cohen's guidelines for r)
if(abs(final_r) < 0.1) {
  effect_size_desc <- "negligible"
} else if(abs(final_r) < 0.3) {
  effect_size_desc <- "small"
} else if(abs(final_r) < 0.5) {
  effect_size_desc <- "medium"
} else {
  effect_size_desc <- "large"
}

cat(sprintf("\nEffect size interpretation: %s (%s)\n", 
            ifelse(final_r > 0, "Positive", "Negative"), effect_size_desc)) #r = 0.109, 95% CI [-0.033, 0.247], p = 0.1316

# Test significance
if(final_p < 0.05) {
  cat("\n✓✓✓ META-ANALYSIS SUPPORTS THE HYPOTHESIS\n")
  cat("   Emotional connection significantly predicts satisfaction\n")
  cat("   across all three domains combined\n")
  
  # Variance explained
  cat(sprintf("   Emotional intensity explains %.1f%% of the variance in satisfaction\n", 
              final_r^2 * 100))
} else {
  cat("\n✗✗✗ META-ANALYSIS DOES NOT SUPPORT THE HYPOTHESIS\n")
  cat("   Combined evidence shows no significant relationship\n")
}                                                                                      #✗✗✗ META-ANALYSIS DOES NOT SUPPORT THE HYPOTHESIS
                                                                                      #Combined evidence shows no significant relationship

# Individual study contributions
cat("\nIndividual study contributions:\n")
for(i in 1:nrow(meta_data)) {
  contribution <- meta_data$weight_random[i] / sum(meta_data$weight_random) * 100
  cat(sprintf("  %-12s: %.1f%% weight in random-effects model\n", 
              meta_data$Dataset[i], contribution))
} 
# Social Media: 33.3% weight in random-effects model
# Movies      : 33.3% weight in random-effects model
# Music       : 33.3% weight in random-effects model

# Save meta-analysis results
meta_results <- list(
  individual_studies = meta_data,
  fixed_effect = list(r = fixed_r, ci_lower = fixed_r_lower, 
                      ci_upper = fixed_r_upper, p = fixed_z_p),
  random_effects = list(r = random_r, ci_lower = random_r_lower,
                        ci_upper = random_r_upper, p = random_z_p),
  heterogeneity = list(Q = Q, df = df_Q, p = Q_p, I2 = I2, tau2 = tau2),
  summary_table = meta_summary
)

saveRDS(meta_results, "meta_analysis_correlations_results.rds")
write.csv(meta_summary, "meta_analysis_summary.csv", row.names = FALSE)



# ============================================================================
# PART 4: DATA SPLITTING (Training vs Testing)
# ============================================================================

# Use 70% for training, 30% for testing
set.seed(123)
train_index <- createDataPartition(all_data_combined$satisfaction_binary, p = 0.7, list = FALSE)
train_data <- all_data_combined[train_index, ]
test_data <- all_data_combined[-train_index, ]

cat("\nTraining set: n =", nrow(train_data), "(", round(nrow(train_data)/nrow(all_data_combined)*100, 1), "%)\n")   # n = 3150 ( 70 %)
cat("Testing set: n =", nrow(test_data), "(", round(nrow(test_data)/nrow(all_data_combined)*100, 1), "%)\n")        #n = 1350 ( 30 %)

# Check distribution by activity in train/test
cat("\nTraining set activity distribution:\n")
print(table(train_data$activity))
# Movies        Music Social Media 
# 1024         1077         1049 

cat("\nTesting set activity distribution:\n")
print(table(test_data$activity))
# Movies        Music Social Media 
# 476          423          451 


# ============================================================================
# PART 5: MODEL 1 - LOGISTIC REGRESSION (Binary Satisfaction)
# Tests your statement: "Emotional connection predicts satisfaction"
# ============================================================================

#---------------------------------------------------------------------------
# 5.1 Model 1a: Emotional intensity only
#---------------------------------------------------------------------------

model_log1 <- glm(satisfaction_binary ~ emotional_intensity, 
                  data = train_data, 
                  family = binomial)
summary_log1 <- summary(model_log1)

cat("\n--- MODEL 1a: Emotional Intensity Only ---\n")
cat("AIC =", round(AIC(model_log1), 2), "\n")                    #AIC = 4287.8
cat("Null Deviance:", round(model_log1$null.deviance, 2), "\n")  #Null Deviance: 4315.38   
cat("Residual Deviance:", round(model_log1$deviance, 2), "\n")   #Residual Deviance: 4283.8

# Emotional intensity coefficient and odds ratio
emo_coef <- coef(model_log1)[2]
emo_p <- summary_log1$coefficients[2, 4]
emo_or <- exp(emo_coef)
emo_ci <- exp(confint(model_log1)[2, ])

cat("\nEmotional intensity coefficient =", round(emo_coef, 4), "\n")             #0.2067
cat("Odds Ratio =", round(emo_or, 3), "\n")
cat("95% CI for OR = [", round(emo_ci[1], 3), ", ", round(emo_ci[2], 3), "]\n")  #95% CI for OR = [ 1.144 ,  1.323 ]
cat("p-value =", round(emo_p, 4), "\n")                                          #p-value = 0 

if(emo_p < 0.05) {
  cat("\n✓ SUPPORTS hypothesis: Emotional connection significantly predicts satisfaction\n")
  if(emo_or > 1) {
    cat("  Positive relationship: Higher emotional intensity INCREASES odds of satisfaction\n")
  } else {
    cat("  Negative relationship: Higher emotional intensity DECREASES odds of satisfaction\n")
  }
} else {
  cat("\n✗ DOES NOT support hypothesis\n")
}
# ✓ SUPPORTS hypothesis: Emotional connection significantly predicts satisfaction
# Positive relationship: Higher emotional intensity INCREASES odds of satisfaction


#---------------------------------------------------------------------------
# 5.2 Model 1b: Add activity type
#---------------------------------------------------------------------------
# Create dummy variables for activity if not already in data
train_data <- train_data %>%
  mutate(
    activity_Music = ifelse(activity == "Music", 1, 0),
    activity_Social = ifelse(activity == "Social Media", 1, 0)
    # activity_Movies is reference
  )

test_data <- test_data %>%
  mutate(
    activity_Music = ifelse(activity == "Music", 1, 0),
    activity_Social = ifelse(activity == "Social Media", 1, 0)
  )

model_log2 <- glm(satisfaction_binary ~ emotional_intensity + activity_Music + activity_Social, 
                  data = train_data, 
                  family = binomial)
summary_log2 <- summary(model_log2)

cat("\n--- MODEL 1b: Add Activity Type ---\n")
cat("AIC =", round(AIC(model_log2), 2), "\n")                        #AIC = 4207.18 
cat("AIC change =", round(AIC(model_log1) - AIC(model_log2), 2), "\n") #AIC change = 80.63 

# Test if activity improves model
anova_log12 <- anova(model_log1, model_log2, test = "Chisq")
if(anova_log12$`Pr(>Chi)`[2] < 0.05) {
  cat("✓ Activity type significantly improves prediction\n")
  
  # Activity effects
  cat("\nActivity effects (compared to Movies):\n")
  cat("  Music: OR =", round(exp(coef(model_log2)[3]), 3), 
      ", p =", round(summary_log2$coefficients[3, 4], 4), "\n")
  cat("  Social Media: OR =", round(exp(coef(model_log2)[4]), 3), 
      ", p =", round(summary_log2$coefficients[4, 4], 4), "\n")
} else {
  cat("✗ Activity type does NOT improve prediction\n")
}

# ✓ Activity type significantly improves prediction
# 
# Activity effects (compared to Movies):
#   Music: OR = 2.057 , p = 0 
# Social Media: OR = 1.013 , p = 0.8816 




#---------------------------------------------------------------------------
# 5.3 Model 1c: Add demographics
#---------------------------------------------------------------------------
# Create demographic dummies if not already
train_data <- train_data %>%
  mutate(
    gender_Male = ifelse(gender == "Male", 1, 0),
    gender_Other = ifelse(gender == "Other/Not Specified", 1, 0),
    # gender_Female is reference
    
    age_numeric = as.numeric(age_group),
    age_std = as.numeric(scale(age_numeric))
  )

test_data <- test_data %>%
  mutate(
    gender_Male = ifelse(gender == "Male", 1, 0),
    gender_Other = ifelse(gender == "Other/Not Specified", 1, 0),
    age_numeric = as.numeric(age_group),
    age_std = as.numeric(scale(age_numeric))
  )

model_log3 <- glm(satisfaction_binary ~ emotional_intensity + activity_Music + activity_Social +
                    age_std + gender_Male + gender_Other, 
                  data = train_data, 
                  family = binomial)
summary_log3 <- summary(model_log3)

cat("\n--- MODEL 1c: Add Demographics ---\n")
cat("AIC =", round(AIC(model_log3), 2), "\n")                            #AIC = 4199.58
cat("AIC change =", round(AIC(model_log2) - AIC(model_log3), 2), "\n")   #AIC change = 7.6

# Test if demographics improve model
anova_log23 <- anova(model_log2, model_log3, test = "Chisq")
if(anova_log23$`Pr(>Chi)`[2] < 0.05) {
  cat("✓ Demographics significantly improve prediction\n")
} else {
  cat("✗ Demographics do NOT improve prediction\n")
}                                                              #✓ Demographics significantly improve prediction

#---------------------------------------------------------------------------
# 5.4 Model 1d: Add interactions (activity moderates emotion effect)
#---------------------------------------------------------------------------

model_log4 <- glm(satisfaction_binary ~ emotional_intensity * activity_Music + 
                    emotional_intensity * activity_Social + age_std + gender_Male + gender_Other,
                  data = train_data, 
                  family = binomial)
summary_log4 <- summary(model_log4)

cat("\n--- MODEL 1d: Add Interactions ---\n")
cat("AIC =", round(AIC(model_log4), 2), "\n")                           #AIC = 4172.28 
cat("AIC change =", round(AIC(model_log3) - AIC(model_log4), 2), "\n")  #AIC change = 27.3

# Test if interactions improve model
anova_log34 <- anova(model_log3, model_log4, test = "Chisq")
if(anova_log34$`Pr(>Chi)`[2] < 0.05) {
  cat("\n✓✓✓ CRITICAL FINDING: Activity MODERATES the emotion-satisfaction relationship\n")
  cat("   The effect of emotional connection DIFFERS across activities\n")
  
  # Extract interaction effects
  int_music <- coef(model_log4)["emotional_intensity:activity_Music"]
  int_social <- coef(model_log4)["emotional_intensity:activity_Social"]
  cat("\n   Music interaction coefficient =", round(int_music, 4), 
      "(OR =", round(exp(int_music), 3), ")\n")
  cat("   Social interaction coefficient =", round(int_social, 4), 
      "(OR =", round(exp(int_social), 3), ")\n")
  
  # Calculate simple slopes for each activity
  cat("\n   Effect of emotional intensity by activity:\n")
  cat("   • Movies (reference): OR =", round(exp(coef(model_log4)[2]), 3), "\n")
  cat("   • Music: OR =", round(exp(coef(model_log4)[2] + int_music), 3), "\n")
  cat("   • Social Media: OR =", round(exp(coef(model_log4)[2] + int_social), 3), "\n")
} else {
  cat("\n✗ No significant moderation - effect consistent across activities\n")
}

# ✓✓✓ CRITICAL FINDING: Activity MODERATES the emotion-satisfaction relationship
# The effect of emotional connection DIFFERS across activities
# 
# Music interaction coefficient = 0.4354 (OR = 1.546 )
# Social interaction coefficient = -0.0339 (OR = 0.967 )
# 
# Effect of emotional intensity by activity:
# • Movies (reference): OR = 1.087 
# • Music: OR = 1.681 
# • Social Media: OR = 1.051 


#---------------------------------------------------------------------------
# 5.5 Test best model on test data
#---------------------------------------------------------------------------

# Select best model (lowest AIC)
models_aic <- c(AIC(model_log1), AIC(model_log2), AIC(model_log3), AIC(model_log4))
best_model_idx <- which.min(models_aic)
best_model <- list(model_log1, model_log2, model_log3, model_log4)[[best_model_idx]]
best_model_name <- c("Emotion only", "+ Activity", "+ Demographics", "+ Interactions")[best_model_idx]

cat("\n--- BEST MODEL: ", best_model_name, " ---\n", sep="") #BEST MODEL: + Interaction
cat("AIC =", round(min(models_aic), 2), "\n")  AIC = 4172.28 

# Predict on test data
pred_prob <- predict(best_model, newdata = test_data, type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

# Confusion matrix
conf_mat <- table(Predicted = pred_class, Actual = test_data$satisfaction_binary)
cat("\n--- TEST SET PERFORMANCE ---\n")
print(conf_mat)
#            Actual
# Predicted   0   1
#         0 279 217
#         1 361 493


# Calculate metrics
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
precision <- conf_mat[2,2] / sum(conf_mat[2,])
recall <- conf_mat[2,2] / sum(conf_mat[,2])
f1 <- ifelse(is.na(precision + recall), 0, 2 * (precision * recall) / (precision + recall))

cat("\nAccuracy =", round(accuracy, 4), "\n") #0.5719
cat("Precision =", round(precision, 4), "\n") #0.5773
cat("Recall =", round(recall, 4), "\n")       #0.6944
cat("F1 Score =", round(f1, 4), "\n")         #0.6304

# ROC Curve and AUC
roc_curve <- roc(test_data$satisfaction_binary, pred_prob)
auc_value <- auc(roc_curve)
cat("AUC =", round(auc_value, 4), "\n")    #AUC = 0.6174

# Plot ROC curve
png("roc_curve.png", width = 8, height = 6, units = "in", res = 300)
plot(roc_curve, main = paste("ROC Curve -", best_model_name),
     col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), 
       col = "blue", lwd = 2)
dev.off()


# ============================================================================
# PART 6: MODEL COMPARISON
# ============================================================================

model_comparison <- data.frame(
  Model = c("Emotion Only", "+ Activity", "+ Demographics", "+ Interactions"),
  AIC = round(c(AIC(model_log1), AIC(model_log2), AIC(model_log3), AIC(model_log4)), 2),
  AUC = c(NA, NA, NA, round(auc_value, 3)),
  Test_Accuracy = c(NA, NA, NA, round(accuracy, 3))
)

print(model_comparison)
# Model            AIC      AUC   Test_Accuracy
# Emotion Only    4287.80    NA            NA
# + Activity      4207.18    NA            NA
# + Demographics  4199.58    NA            NA
# + Interactions  4172.28   0.617         0.572

# Identify best model
best_model_name <- model_comparison$Model[which.min(model_comparison$AIC)]
cat("\n✓ Best model based on AIC:", best_model_name, "\n")


# ============================================================================
# PART 7: VISUALIZE RESULTS
# ============================================================================

# Predicted probability plot
p_prob <- ggplot(test_data, aes(x = emotional_intensity, y = pred_prob, color = activity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Predicted Probability of Satisfaction",
       subtitle = paste("AUC =", round(auc_value, 3)),
       x = "Emotional Intensity (standardized)",
       y = "Predicted Probability") +
  theme_minimal()

ggsave("predicted_probabilities.png", p_prob, width = 10, height = 6)


# ============================================================================
# PART 8: FINAL CONCLUSION
# ============================================================================

cat('\nStatement: "Audiences who feel emotionally connected report higher satisfaction"\n\n')

# Test 1: Does emotional intensity significantly predict satisfaction?
if(emo_p < 0.05) {
  cat("✓ TEST 1 PASSED: Emotional intensity significantly predicts satisfaction\n")
  cat("   (p =", round(emo_p, 4), ", OR =", round(emo_or, 3), ")\n")
} else {
  cat("✗ TEST 1 FAILED: Emotional intensity does NOT significantly predict satisfaction\n")
  cat("   (p =", round(emo_p, 4), ")\n")
}

# Test 2: Does the relationship hold across activities?
if(exists("anova_log34") && anova_log34$`Pr(>Chi)`[2] < 0.05) {
  cat("✓ TEST 2 PASSED: The relationship is MODERATED by activity type\n")
  cat("   Effect differs across Movies, Music, and Social Media\n")
  
  # Show activity-specific effects
  cat("\n   Activity-specific effects:\n")
  for(i in 1:nrow(activity_results)) {
    cat("   • ", activity_results$Activity[i], ": OR = ", 
        activity_results$Odds_Ratio[i], 
        ifelse(activity_results$Significant[i] == "YES", " (significant)", " (not significant)"),
        "\n", sep = "")
  }
} else {
  cat("✗ TEST 2 FAILED: The relationship is consistent across activities\n")
}

# Test 3: Can we predict satisfaction with reasonable accuracy?
if(auc_value > 0.6) {
  cat("✓ TEST 3 PASSED: Models predict satisfaction with AUC =", round(auc_value, 3), "\n")
  if(auc_value > 0.8) {
    cat("   Excellent discrimination\n")
  } else if(auc_value > 0.7) {
    cat("   Good discrimination\n")
  } else {
    cat("   Fair discrimination\n")
  }
} else {
  cat("✗ TEST 3 FAILED: Models have poor predictive power (AUC =", round(auc_value, 3), ")\n")
}

# Overall conclusion
cat("\n", rep("=", 60), "\n")
cat("OVERALL CONCLUSION")
cat("\n", rep("=", 60), "\n")

if(emo_p < 0.05 & auc_value > 0.6) {
  cat("\n✓✓✓ STATEMENT STRONGLY SUPPORTED!\n")
  cat("   Emotional connection significantly predicts satisfaction\n")
  cat("   across Movies, Music, and Social Media.\n")
  cat("   Best model AUC =", round(auc_value, 3), "\n")
  cat("   Emotional intensity increases odds of satisfaction by", 
      round((emo_or - 1) * 100, 1), "% per SD increase\n")
} else if(emo_p < 0.05) {
  cat("\n⚠ STATEMENT PARTIALLY SUPPORTED\n")
  cat("   Emotional connection predicts satisfaction, but predictive power is weak.\n")
  cat("   AUC =", round(auc_value, 3), "\n")
} else {
  cat("\n✗✗✗ STATEMENT NOT SUPPORTED\n")
  cat("   Emotional connection does NOT predict satisfaction\n")
  cat("   in this combined analysis.\n")
}

# Add meta-analysis conclusion to final output
cat("\n", rep("=", 60), "\n")
cat("META-ANALYSIS CONCLUSION (COMBINED ACROSS DATASETS)")
cat("\n", rep("=", 60), "\n")
cat(sprintf("Combined correlation: r = %.3f, 95%% CI [%.3f, %.3f], p = %.4f\n", 
            final_r, final_ci_lower, final_ci_upper, final_p))
cat(sprintf("Heterogeneity: I² = %.1f%%, p = %.4f\n", I2, Q_p))
cat(sprintf("Variance explained: %.1f%%\n", final_r^2 * 100))

# Save all results
saveRDS(list(
  models = list(
    log1 = model_log1,
    log2 = model_log2,
    log3 = model_log3,
    log4 = model_log4
  ),
  comparison = model_comparison,
  best_model = best_model_name,
  best_metrics = c(AUC = auc_value, Accuracy = accuracy, F1 = f1),
  activity_specific = activity_results,
  meta_analysis = meta_results
), file = "complete_predictive_analysis_results.rds")



