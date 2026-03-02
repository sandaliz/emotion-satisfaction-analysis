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

# Set seed for reproducibility
set.seed(123)


# ============================================================================
# PART 1: LOAD AND COMBINE DATASETS 
# ============================================================================

cat("LOADING AND COMBINING DATASETS\n")
setwd("G:\\TPSM_Assignment_2026\\data\\processed\\musicEntertainmentType\\sample")

# Load your three datasets (adjust paths as needed)
movies_data <- read.csv("Sample_movie_dataset.csv")        
social_data <- read.csv("sample_social_media_dataset.csv")        
music_data <- read.csv("SRS_sampled_1500_emotifyData.csv")   

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
    # Emotional intensity already 1-5
    emotional_intensity = as.numeric(scale(emotional_intensity)),
    # Satisfaction already 1-5
    satisfaction = satisfaction,
    # Demographics
    gender = gender,
    age_group = age_group,
    # Binary satisfaction for classification
    satisfaction_binary = ifelse(satisfaction > median(satisfaction, na.rm = TRUE), 1, 0),
    emotional_group = ifelse(emotional_intensity > median(emotional_intensity), "High", "Low")
  ) %>%
  select(activity, emotional_intensity, satisfaction, satisfaction_binary, gender, age_group,emotional_group)

cat("\nMovies - Variable ranges:\n")
cat(" Emotional Intensity:", round(range(movies_combined$emotional_intensity), 2), "\n")     #-2.88  2.23 
cat(" Satisfaction:", round(range(movies_combined$satisfaction), 2), "\n")                   #1.33   5


# Music dataset preparation
#-------------------------------------------------------------------------
music_combined <- music_data %>%
  mutate(
    activity = "Music",
    # Emotional intensity already 1-5
    emotional_intensity = as.numeric(scale(emotional_intensity)),
    # Music satisfaction is binary (0/1) - convert to 1-5 scale for consistency
    # 0 → 1, 1 → 5
    satisfaction = ifelse(satisfaction == 1, 5, 1),
    # Demographics
    gender = gender,
    age_group = age_group,
    # Binary satisfaction (already binary)
    satisfaction_binary = satisfaction_original,  # Use original 0/1
    emotional_group = ifelse(emotional_intensity > median(emotional_intensity), "High", "Low")
  ) %>%
  select(activity, emotional_intensity, satisfaction, satisfaction_binary, gender, age_group,emotional_group)


cat("\nMusic - Variable ranges:\n")
cat(" Emotional Intensity:", round(range(music_combined$emotional_intensity), 2), "\n")
cat(" Satisfaction (binary):", round(mean(music_combined$satisfaction_binary), 2), "%\n")

print(names(music_data))

# Social media dataset preparation
#---------------------------------------------------------------------
social_combined <- social_data %>%
  mutate(
    activity = "Social Media",
    # Emotional intensity already 1-5
    emotional_intensity = as.numeric(scale(emotional_intensity)),
    # Satisfaction already 1-5
    satisfaction = satisfaction,
    # Demographics
    gender = gender,
    age_group = audience_age_distribution,
    # Binary satisfaction
    satisfaction_binary = ifelse(satisfaction > median(satisfaction, na.rm = TRUE), 1, 0),
    emotional_group = ifelse(emotional_intensity > median(emotional_intensity), "High", "Low")
  ) %>%
  select(activity, emotional_intensity, satisfaction, satisfaction_binary, gender, age_group,emotional_group)

cat("\nSocial Media - Variable ranges:\n")
cat(" Emotional Intensity:", round(range(social_combined$emotional_intensity), 2), "\n")
cat(" Satisfaction:", round(range(social_combined$satisfaction), 2), "\n")

#---------------------------------------------------------------------------
# 1.2 Combine all datasets
#---------------------------------------------------------------------------

combined_data <- bind_rows(
  movies_combined,
  music_combined,
  social_combined
) %>%
  
# Remove any rows with missing values
filter(!is.na(emotional_intensity), !is.na(satisfaction), !is.na(age_group))

cat("\nCombined dataset size: n =", nrow(combined_data), "\n")
cat("\nDistribution by activity:\n")
print(table(combined_data$activity))

cat("\nSatisfaction range:", range(combined_data$satisfaction), "\n")
cat("Emotional intensity range:", range(combined_data$emotional_intensity), "\n")

#---------------------------------------------------------------------------
# 1.3 Create dummy variables for modeling
#---------------------------------------------------------------------------

combined_data <- combined_data %>%
  mutate(
    # Activity dummies (reference = Movies)
    activity_Music = ifelse(activity == "Music", 1, 0),
    activity_Social = ifelse(activity == "Social Media", 1, 0),
    
    # Gender dummies (reference = Female)
    gender_Male = ifelse(gender == "Male", 1, 0),
    gender_Other = ifelse(gender == "Other/Not Specified", 1, 0),
    
    # Age group dummies (reference = 19-25)
    age_0_12 = ifelse(age_group == "5-12" | age_group == "0-12", 1, 0),
    age_13_18 = ifelse(age_group == "13-18", 1, 0),
    age_26_35 = ifelse(age_group == "26-35", 1, 0),
    age_36_50 = ifelse(age_group == "36-50", 1, 0),
    age_50plus = ifelse(age_group == "50+", 1, 0),
    
    # Standardized versions
    emotional_intensity_std = as.numeric(scale(emotional_intensity)),
    satisfaction_std = as.numeric(scale(satisfaction)),
    age_std = as.numeric(scale(age))
  )

# Save combined dataset
saveRDS(combined_data, "Predictive_Output/combined_predictive_data.rds")
write.csv(combined_data, "Predictive_Output/combined_predictive_data.csv", row.names = FALSE)

cat("\n✓ Combined dataset saved with", ncol(combined_data), "variables\n")

# ============================================================================
# PART 2: DESCRIPTIVE OVERVIEW OF COMBINED DATA
# ============================================================================
# Summary by activity
activity_summary <- combined_data %>%
  group_by(activity) %>%
  summarise(
    n = n(),
    mean_sat = round(mean(satisfaction), 2),
    sd_sat = round(sd(satisfaction), 2),
    mean_emo = round(mean(emotional_intensity), 2),
    sd_emo = round(sd(emotional_intensity), 2),
    correlation = round(cor(emotional_intensity, satisfaction), 3)
  )

cat("\nSummary by Activity:\n")
print(activity_summary)


#Emotional Intensity by Dataset
emo_by_dataset <- combined_data %>%
  group_by(dataset) %>%
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

#Satisfaction by Dataset
#For continuous satisfaction (Social Media and Movies)
sat_continuous <- combined_data %>%
  filter(dataset != "Music") %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    mean_sat = round(mean(satisfaction, na.rm = TRUE), 2),
    sd_sat = round(sd(satisfaction, na.rm = TRUE), 2),
    min_sat = round(min(satisfaction, na.rm = TRUE), 2),
    max_sat = round(max(satisfaction, na.rm = TRUE), 2),
    .groups = 'drop'
  )

cat("\nContinuous Satisfaction by Dataset:\n")
print(sat_continuous)


#For binary satisfaction (all datasets)
sat_binary <- combined_data %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    satisfaction_rate = round(mean(satisfaction_binary, na.rm = TRUE) * 100, 1),
    .groups = 'drop'
  )

cat("\nBinary Satisfaction Rate by Dataset:\n")
print(sat_binary)


#Correalation Analysis
#-----------------------------------------------------
#Social Media Correlation
cor_social <- cor.test(social_clean$emotional_intensity,
                       social_clean$satisfaction,
                       method = "pearson")

#Movies Correlation
cor_movie <- cor.test(movie_clean$emotional_intensity,
                      movie_clean$satisfaction,
                      method = "pearson")

#Music Correlation (point-biserial, since satisfaction is binary)
cor_music <- cor.test(music_clean$emotional_intensity,
                      music_clean$satisfaction_binary,
                      method = "pearson")

#Summary Table
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

# Overall correlation
cor_overall <- cor.test(combined_data$emotional_intensity, combined_data$satisfaction)
cat("\nOverall correlation: r =", round(cor_overall$estimate, 3), 
    ", p =", round(cor_overall$p.value, 4), "\n")


# Visualization: Boxplot by activity
p1 <- ggplot(combined_data, aes(x = activity, y = satisfaction, fill = activity)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Satisfaction by Activity (1-5 scale)",
       x = "Activity", y = "Satisfaction") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Predictive_Output/satisfaction_by_activity.png", p1, width = 8, height = 5)



#T-TEST COMPARISON (High vs Low Emotional Groups)
social_high <- social_clean %>% filter(emotional_group == "High") %>% pull(satisfaction)
social_low <- social_clean %>% filter(emotional_group == "Low") %>% pull(satisfaction)
t_social <- t.test(social_high, social_low)
d_social <- cohens_d(satisfaction ~ emotional_group, data = social_clean)

movie_high <- movie_clean %>% filter(emotional_group == "High") %>% pull(satisfaction)
movie_low <- movie_clean %>% filter(emotional_group == "Low") %>% pull(satisfaction)
t_movie <- t.test(movie_high, movie_low)
d_movie <- cohens_d(satisfaction ~ emotional_group, data = movie_clean)

music_high <- music_clean %>% filter(emotional_group == "High") %>% pull(satisfaction_binary)
music_low <- music_clean %>% filter(emotional_group == "Low") %>% pull(satisfaction_binary)
t_music <- t.test(music_high, music_low)
d_music <- cohens_d(satisfaction_binary ~ emotional_group, data = music_clean)


group_stats_combined <- bind_rows(
  social_clean %>% group_by(emotional_group) %>% summarise(
    dataset = "Social Media",
    n = n(),
    mean_sat = mean(satisfaction, na.rm = TRUE),
    sd_sat = sd(satisfaction, na.rm = TRUE),
    .groups = 'drop'
  ),
  movie_clean %>% group_by(emotional_group) %>% summarise(
    dataset = "Movies",
    n = n(),
    mean_sat = mean(satisfaction, na.rm = TRUE),
    sd_sat = sd(satisfaction, na.rm = TRUE),
    .groups = 'drop'
  ),
  music_clean %>% group_by(emotional_group) %>% summarise(
    dataset = "Music",
    n = n(),
    mean_sat = mean(satisfaction_binary, na.rm = TRUE),
    sd_sat = sd(satisfaction_binary, na.rm = TRUE),
    .groups = 'drop'
  )
)

cat("\nGroup Statistics by Dataset:\n")
print(group_stats_combined)

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


#REGRESSION ANALYSIS ACROSS DATASETS
#--------------------------------------------------
lm_social <- lm(satisfaction_std ~ emotional_intensity_std, data = social_clean)
summary_social <- summary(lm_social)

lm_movie <- lm(satisfaction_std ~ emotional_intensity_std, data = movie_clean)
summary_movie <- summary(lm_movie)

glm_music <- glm(satisfaction_binary ~ emotional_intensity_std,
                 data = music_clean,
                 family = binomial)
summary_music <- summary(glm_music)

regression_summary <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  Model_Type = c("Linear", "Linear", "Logistic"),
  Coefficient = c(
    round(coef(lm_social)[2], 3),
    round(coef(lm_movie)[2], 3),
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
  R_squared = c(
    round(summary_social$r.squared, 3),
    round(summary_movie$r.squared, 3),
    NA # Not applicable for logistic
  ),
  Significant = c(
    ifelse(summary_social$coefficients[2, 4] < 0.05, "YES", "NO"),
    ifelse(summary_movie$coefficients[2, 4] < 0.05, "YES", "NO"),
    ifelse(summary_music$coefficients[2, 4] < 0.05, "YES", "NO")
  )
)

cat("\nRegression Summary Across Datasets:\n")
print(regression_summary)

#Music Logistic Regression - Odds Ratio
odds_ratio <- exp(coef(glm_music)[2])
ci_odds <- exp(confint(glm_music)[2, ])

cat("\nMusic Dataset - Logistic Regression Details:\n")
cat(" Odds Ratio =", round(odds_ratio, 3), "\n")
cat(" 95% CI for OR = [", round(ci_odds[1], 3), ", ", round(ci_odds[2], 3), "]\n")
cat(" Interpretation: 1 SD increase in emotional intensity multiplies odds of satisfaction by",
    round(odds_ratio, 2), "\n")


#META-ANALYSIS: COMBINED EFFECT SIZES
#-----------------------------------------------
# Effect Size Comparison
effect_sizes <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  Metric = c("r²", "r²", "Odds Ratio"),
  Effect = c(
    round(cor_social$estimate^2, 3),
    round(cor_movie$estimate^2, 3),
    round(odds_ratio, 3)
  ),
  Interpretation = c(
    ifelse(cor_social$estimate^2 < 0.01, "Very weak",
           ifelse(cor_social$estimate^2 < 0.09, "Weak",
                  ifelse(cor_social$estimate^2 < 0.25, "Moderate", "Strong"))),
    ifelse(cor_movie$estimate^2 < 0.01, "Very weak",
           ifelse(cor_movie$estimate^2 < 0.09, "Weak",
                  ifelse(cor_movie$estimate^2 < 0.25, "Moderate", "Strong"))),
    ifelse(odds_ratio < 1.5, "Small",
           ifelse(odds_ratio < 3.5, "Moderate", "Large"))
  )
)

cat("\nEffect Sizes Across Datasets:\n")
print(effect_sizes)

#Using Fisher's z transformation for correlations
weighted_r <- weighted.mean(
x = c(cor_social$estimate, cor_movie$estimate, cor_music$estimate),
w = c(nrow(social_clean), nrow(movie_clean), nrow(music_clean))
)

cat("\nWeighted average correlation across all datasets: r =",
round(weighted_r, 3), "\n")
cat("Weighted r² =", round(weighted_r^2, 3), "\n")

#COMBINED VISUALIZATIONS
#----------------------------------------
#Correlation Comparison Plot
cor_plot_data <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  Correlation = c(cor_social$estimate, cor_movie$estimate, cor_music$estimate),
  CI_Lower = c(cor_social$conf.int[1], cor_movie$conf.int[1], cor_music$conf.int[1]),
  CI_Upper = c(cor_social$conf.int[2], cor_movie$conf.int[2], cor_music$conf.int[2]),
  Significant = c(cor_social$p.value < 0.05, cor_movie$p.value < 0.05, cor_music$p.value < 0.05)
)

p1 <- ggplot(cor_plot_data, aes(x = Dataset, y = Correlation, fill = Significant)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#F44336")) +
  labs(title = "Correlation: Emotional Intensity → Satisfaction",
       subtitle = "Error bars show 95% CI",
       y = "Pearson's r") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("Combined_Analysis_Output/correlation_comparison.png", p1, width = 8, height = 5)

#Effect Size Comparison Plot
effect_plot_data <- data.frame(
  Dataset = c("Social Media", "Movies", "Music"),
  Effect_Size = c(cor_social$estimate^2, cor_movie$estimate^2, odds_ratio/10), # Scale for visualization
  Effect_Type = c("R²", "R²", "OR/10"),
  Raw_Value = c(cor_social$estimate^2, cor_movie$estimate^2, odds_ratio)
)

p2 <- ggplot(effect_plot_data, aes(x = Dataset, y = Effect_Size, fill = Dataset)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(Raw_Value, 3)), vjust = -0.5) +
  labs(title = "Effect Sizes Across Datasets",
       subtitle = "Note: Music effect shown as Odds Ratio/10 for scale",
       y = "Effect Size (scaled for comparison)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Combined_Analysis_Output/effect_size_comparison.png", p2, width = 8, height = 5)

#Satisfaction Rates by Dataset and Emotional Group
sat_rate_data <- bind_rows(
  social_clean %>%
    group_by(emotional_group) %>%
    summarise(
      dataset = "Social Media",
      satisfaction_rate = mean(satisfaction_binary) * 100,
      se = sd(satisfaction_binary) / sqrt(n()),
      .groups = 'drop'
    ),
  movie_clean %>%
    group_by(emotional_group) %>%
    summarise(
      dataset = "Movies",
      satisfaction_rate = mean(satisfaction_binary) * 100,
      se = sd(satisfaction_binary) / sqrt(n()),
      .groups = 'drop'
    ),
  music_clean %>%
    group_by(emotional_group) %>%
    summarise(
      dataset = "Music",
      satisfaction_rate = mean(satisfaction_binary) * 100,
      se = sd(satisfaction_binary) / sqrt(n()),
      .groups = 'drop'
    )
)

p3 <- ggplot(sat_rate_data, aes(x = dataset, y = satisfaction_rate, fill = emotional_group)) +
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
ggsave("Combined_Analysis_Output/satisfaction_rates_by_group.png", p3, width = 10, height = 6)


#Combined Boxplot (for datasets with continuous satisfaction)
continuous_data <- bind_rows(
  social_clean %>% select(dataset, emotional_group, satisfaction),
  movie_clean %>% select(dataset, emotional_group, satisfaction)
) %>% filter(!is.na(satisfaction))

p4 <- ggplot(continuous_data, aes(x = dataset, y = satisfaction, fill = emotional_group)) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("High" = "#4CAF50", "Low" = "#F44336")) +
  labs(title = "Satisfaction Distribution by Dataset and Emotional Group",
       subtitle = "Social Media (0-10 scale), Movies (1-5 scale)",
       x = "Dataset", y = "Satisfaction Score", fill = "Emotional Group") +
  theme_minimal() +
  facet_wrap(~dataset, scales = "free_y")
ggsave("Combined_Analysis_Output/satisfaction_boxplot_comparison.png", p4, width = 10, height = 6)

# Scatterplot Comparison (Emotional Intensity vs Satisfaction)

scatter_data <- bind_rows(
  social_clean %>%
    select(dataset, emotional_intensity, satisfaction) %>%
    mutate(satisfaction_type = "Continuous") %>%
    filter(!is.na(satisfaction)),
  movie_clean %>%
    select(dataset, emotional_intensity, satisfaction) %>%
    mutate(satisfaction_type = "Continuous") %>%
    filter(!is.na(satisfaction)),
  music_clean %>%
    select(dataset, emotional_intensity, satisfaction_binary) %>%
    rename(satisfaction = satisfaction_binary) %>%
    mutate(satisfaction_type = "Binary")
)

p5 <- ggplot(scatter_data, aes(x = emotional_intensity, y = satisfaction)) +
  geom_point(alpha = 0.3, color = "darkgray") +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(title = "Emotional Intensity vs Satisfaction Across Datasets",
       x = "Emotional Intensity", y = "Satisfaction") +
  theme_minimal() +
  facet_wrap(~dataset, scales = "free")

ggsave("Combined_Analysis_Output/scatterplot_comparison.png", p5, width = 12, height = 8)


summary_table <- data.frame(
  Metric = c(
    "Sample Size",
    "Emotional Intensity (Mean)",
    "Emotional Intensity (SD)",
    "Satisfaction Rate (%)",
    "Correlation (r)",
    "Correlation p-value",
    "Effect Size (r² / OR)",
    "T-test p-value",
    "Cohen's d",
    "Regression p-value",
    "Supports Hypothesis?"
  ),
  Social Media = c(
    nrow(social_clean),
    round(mean(social_clean$emotional_intensity), 2),
    round(sd(social_clean$emotional_intensity), 2),
    paste0(round(mean(social_clean$satisfaction_binary)*100, 1), "%"),
    round(cor_social$estimate, 3),
    round(cor_social$p.value, 4),
    round(cor_social$estimate^2, 3),
    round(t_social$p.value, 4),
    round(d_social$Cohens_d, 3),
    round(summary_social$coefficients[2, 4], 4),
    ifelse(summary_social$coefficients[2, 4] < 0.05, "YES", "NO")
  ),
  Movies = c(
    nrow(movie_clean),
    round(mean(movie_clean$emotional_intensity), 2),
    round(sd(movie_clean$emotional_intensity), 2),
    paste0(round(mean(movie_clean$satisfaction_binary)*100, 1), "%"),
    round(cor_movie$estimate, 3),
    round(cor_movie$p.value, 4),
    round(cor_movie$estimate^2, 3),
    round(t_movie$p.value, 4),
    round(d_movie$Cohens_d, 3),
    round(summary_movie$coefficients[2, 4], 4),
    ifelse(summary_movie$coefficients[2, 4] < 0.05, "YES", "NO")
  ),
  Music = c(
    nrow(music_clean),
    round(mean(music_clean$emotional_intensity), 2),
    round(sd(music_clean$emotional_intensity), 2),
    paste0(round(mean(music_clean$satisfaction_binary)*100, 1), "%"),
    round(cor_music$estimate, 3),
    round(cor_music$p.value, 4),
    round(odds_ratio, 3),
    round(t_music$p.value, 4),
    round(d_music$Cohens_d, 3),
    round(summary_music$coefficients[2, 4], 4),
    ifelse(summary_music$coefficients[2, 4] < 0.05, "YES", "NO")
  )
)

print(summary_table)
write.csv(summary_table, "Combined_Analysis_Output/comprehensive_summary.csv", row.names = FALSE)

#Count how many datasets support the hypothesis
support_count <- sum(
  summary_social$coefficients[2, 4] < 0.05,
  summary_movie$coefficients[2, 4] < 0.05,
  summary_music$coefficients[2, 4] < 0.05
)

cat("Support across datasets:\n")
cat(" • Social Media:", ifelse(summary_social$coefficients[2, 4] < 0.05, "✓ SUPPORTS", "✗ DOES NOT SUPPORT"),
    "(p =", round(summary_social$coefficients[2, 4], 4), ")\n")
cat(" • Movies:", ifelse(summary_movie$coefficients[2, 4] < 0.05, "✓ SUPPORTS", "✗ DOES NOT SUPPORT"),
    "(p =", round(summary_movie$coefficients[2, 4], 4), ")\n")
cat(" • Music:", ifelse(summary_music$coefficients[2, 4] < 0.05, "✓ SUPPORTS", "✗ DOES NOT SUPPORT"),
    "(p =", round(summary_music$coefficients[2, 4], 4), ")\n\n")

cat("Overall pattern: ", support_count, "out of 3 datasets support the hypothesis\n\n")

if(support_count == 3) {
  cat("✓✓✓ STRONG EVIDENCE: All three datasets support the hypothesis\n")
  cat("Emotional connection consistently predicts higher satisfaction across\n")
  cat("social media, movies, and music contexts.\n")
} else if(support_count == 2) {
  cat("✓ MODERATE EVIDENCE: Two out of three datasets support the hypothesis\n")
  cat("The relationship holds in most contexts but may be domain-specific.\n")
} else if(support_count == 1) {
  cat("⚠ WEAK EVIDENCE: Only one dataset supports the hypothesis\n")
  cat("The relationship may be context-dependent or influenced by measurement differences.\n")
} else {
  cat("✗ NO EVIDENCE: None of the datasets support the hypothesis\n")
  cat("Strong evidence that emotional connection does NOT predict satisfaction\n")
  cat("across these contexts.\n")
}

cat("\nKey observations:\n")
cat(" • Strongest effect: Music dataset (r = 0.215, p < 0.001)\n")
cat(" • Weakest effect: Social Media dataset (r = -0.01, p = 0.690)\n")
cat(" • Movies dataset shows small but significant effect (r = 0.056, p = 0.031)\n\n")

cat("Possible explanations for differences:\n")
cat(" • Measurement differences: Satisfaction measured differently across datasets\n")
cat(" • Context effects: Music may evoke stronger emotional responses than social media\n")
cat(" • Sample characteristics: Different user populations across platforms\n")




# ============================================================================
# PART 3: DATA SPLITTING (Training vs Testing)
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("PART 3: DATA SPLITTING\n")
cat(rep("=", 80), "\n")

# Use 70% for training, 30% for testing
set.seed(123)
train_index <- createDataPartition(combined_data$satisfaction_binary, p = 0.7, list = FALSE)
train_data <- combined_data[train_index, ]
test_data <- combined_data[-train_index, ]

cat("\nTraining set: n =", nrow(train_data), "(", round(nrow(train_data)/nrow(combined_data)*100, 1), "%)\n")
cat("Testing set: n =", nrow(test_data), "(", round(nrow(test_data)/nrow(combined_data)*100, 1), "%)\n")

# Check distribution by activity in train/test
cat("\nTraining set activity distribution:\n")
print(table(train_data$activity))
cat("\nTesting set activity distribution:\n")
print(table(test_data$activity))

# ============================================================================
# PART 4: MODEL 1 - LINEAR REGRESSION (Continuous Satisfaction)
# Tests your statement: "Emotional connection predicts satisfaction"
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("PART 4: MODEL 1 - LINEAR REGRESSION\n")
cat(rep("=", 80), "\n")

#---------------------------------------------------------------------------
# 4.1 Model 1a: Emotional intensity only
#---------------------------------------------------------------------------

model_lm1 <- lm(satisfaction ~ emotional_intensity, data = train_data)
summary_lm1 <- summary(model_lm1)

cat("\n--- MODEL 1a: Emotional Intensity Only ---\n")
cat("R² =", round(summary_lm1$r.squared, 4), "\n")
cat("Adjusted R² =", round(summary_lm1$adj.r.squared, 4), "\n")
cat("F =", round(summary_lm1$fstatistic[1], 2),
    ", p =", round(pf(summary_lm1$fstatistic[1],
                      summary_lm1$fstatistic[2],
                      summary_lm1$fstatistic[3],
                      lower.tail = FALSE), 4), "\n")

# Emotional intensity coefficient
emo_coef <- coef(model_lm1)[2]
emo_p <- summary_lm1$coefficients[2, 4]
emo_ci <- confint(model_lm1)[2, ]

cat("\nEmotional intensity coefficient =", round(emo_coef, 4), "\n")
cat("95% CI = [", round(emo_ci[1], 4), ", ", round(emo_ci[2], 4), "]\n")
cat("p-value =", round(emo_p, 4), "\n")

if(emo_p < 0.05) {
  cat("\n✓ SUPPORTS hypothesis: Emotional connection significantly predicts satisfaction\n")
} else {
  cat("\n✗ DOES NOT support hypothesis\n")
}

#---------------------------------------------------------------------------
# 4.2 Model 1b: Add activity type
#---------------------------------------------------------------------------

model_lm2 <- lm(satisfaction ~ emotional_intensity + activity_Music + activity_Social, 
                data = train_data)
summary_lm2 <- summary(model_lm2)

cat("\n--- MODEL 1b: Add Activity Type ---\n")
cat("R² =", round(summary_lm2$r.squared, 4), "\n")
cat("R² change =", round(summary_lm2$r.squared - summary_lm1$r.squared, 4), "\n")

# Test if activity improves model
anova_lm12 <- anova(model_lm1, model_lm2)
if(anova_lm12$`Pr(>F)`[2] < 0.05) {
  cat("✓ Activity type significantly improves prediction\n")
} else {
  cat("✗ Activity type does NOT improve prediction\n")
}

#---------------------------------------------------------------------------
# 4.3 Model 1c: Add demographics
#---------------------------------------------------------------------------

model_lm3 <- lm(satisfaction ~ emotional_intensity + activity_Music + activity_Social +
                  age_std + gender_Male + gender_Other, 
                data = train_data)
summary_lm3 <- summary(model_lm3)

cat("\n--- MODEL 1c: Add Demographics ---\n")
cat("R² =", round(summary_lm3$r.squared, 4), "\n")
cat("R² change =", round(summary_lm3$r.squared - summary_lm2$r.squared, 4), "\n")

#---------------------------------------------------------------------------
# 4.4 Model 1d: Add interactions (activity moderates emotion effect)
#---------------------------------------------------------------------------

model_lm4 <- lm(satisfaction ~ emotional_intensity * activity_Music + 
                  emotional_intensity * activity_Social + age_std + gender_Male + gender_Other,
                data = train_data)
summary_lm4 <- summary(model_lm4)

cat("\n--- MODEL 1d: Add Interactions ---\n")
cat("R² =", round(summary_lm4$r.squared, 4), "\n")
cat("R² change =", round(summary_lm4$r.squared - summary_lm3$r.squared, 4), "\n")

# Test if interactions improve model
anova_lm34 <- anova(model_lm3, model_lm4)
if(anova_lm34$`Pr(>F)`[2] < 0.05) {
  cat("\n✓✓✓ CRITICAL FINDING: Activity MODERATES the emotion-satisfaction relationship\n")
  cat("   The effect of emotional connection DIFFERS across activities\n")
  
  # Extract interaction coefficients
  int_music <- coef(model_lm4)["emotional_intensity:activity_Music"]
  int_social <- coef(model_lm4)["emotional_intensity:activity_Social"]
  cat("   Music interaction:", round(int_music, 4), "\n")
  cat("   Social interaction:", round(int_social, 4), "\n")
} else {
  cat("\n✗ No significant moderation - effect consistent across activities\n")
}

#---------------------------------------------------------------------------
# 4.5 Test best model on test data
#---------------------------------------------------------------------------

# Use Model 4 (full model with interactions) for prediction
predictions_lm <- predict(model_lm4, newdata = test_data)
lm_rmse <- sqrt(mean((test_data$satisfaction - predictions_lm)^2))
lm_r2 <- cor(test_data$satisfaction, predictions_lm)^2

cat("\n--- TEST SET PERFORMANCE (Linear Model) ---\n")
cat("RMSE =", round(lm_rmse, 4), "\n")
cat("R² =", round(lm_r2, 4), "\n")


# ============================================================================
# PART 10: TESTING YOUR STATEMENT - FINAL CONCLUSION
# ============================================================================

cat("\n", rep("=", 80), "\n")
cat("PART 10: TESTING YOUR STATEMENT - FINAL CONCLUSION\n")
cat(rep("=", 80), "\n")
cat('\nStatement: "Audiences who feel emotionally connected report higher satisfaction"\n\n')

# Test 1: Does emotional intensity significantly predict satisfaction?
if(emo_p < 0.05) {
  cat("✓ TEST 1 PASSED: Emotional intensity significantly predicts satisfaction\n")
  cat("   (p =", round(emo_p, 4), ")\n")
} else {
  cat("✗ TEST 1 FAILED: Emotional intensity does NOT significantly predict satisfaction\n")
  cat("   (p =", round(emo_p, 4), ")\n")
}

# Test 2: Does the relationship hold across activities?
if(anova_lm34$`Pr(>F)`[2] < 0.05) {
  cat("✓ TEST 2 PASSED: The relationship is MODERATED by activity type\n")
  cat("   Effect differs across Movies, Music, and Social Media\n")
} else {
  cat("✗ TEST 2 FAILED: The relationship is consistent across activities\n")
}

# Test 3: Can we predict satisfaction with reasonable accuracy?
if(best_r2 > 0.1) {
  cat("✓ TEST 3 PASSED: Models can predict satisfaction with reasonable accuracy\n")
  cat("   Best model R² =", round(best_r2, 3), "\n")
} else {
  cat("✗ TEST 3 FAILED: Models have poor predictive power\n")
  cat("   Best model R² =", round(best_r2, 3), "\n")
}

# Overall conclusion
cat("\n", rep("=", 60), "\n")
cat("OVERALL CONCLUSION\n")
cat(rep("=", 60), "\n")

if(emo_p < 0.05 & best_r2 > 0.1) {
  cat("\n✓✓✓ STATEMENT STRONGLY SUPPORTED!\n")
  cat("   Emotional connection significantly predicts satisfaction\n")
  cat("   across Movies, Music, and Social Media.\n")
  cat("   Best model explains", round(best_r2*100, 1), "% of variance.\n")
  
} else if(emo_p < 0.05 & best_r2 <= 0.1) {
  cat("\n⚠ STATEMENT PARTIALLY SUPPORTED\n")
  cat("   Emotional connection predicts satisfaction, but effect is weak.\n")
  cat("   Only", round(best_r2*100, 1), "% of variance explained.\n")
  
} else {
  cat("\n✗✗✗ STATEMENT NOT SUPPORTED\n")
  cat("   Emotional connection does NOT predict satisfaction\n")
  cat("   in this combined analysis.\n")
}

# Activity-specific insights
cat("\nActivity-Specific Effects:\n")
for(i in 1:nrow(activity_results)) {
  sig <- ifelse(activity_results$emo_p[i] < 0.05, "✓", "✗")
  cat("  ", sig, " ", activity_results$Activity[i], ": ",
      "r² = ", round(activity_results$R2[i], 3), 
      " (p = ", activity_results$emo_p[i], ")\n", sep = "")
}

# Save results
saveRDS(list(
  models = list(
    linear = model_lm4,
    logistic = model_log2,
    rf = rf_model,
    xgb = xgb_model,
    activity_specific = activity_results
  ),
  comparison = model_comparison,
  best_model = best_model,
  best_metrics = c(R2 = best_r2, RMSE = best_rmse)
), "Predictive_Output/predictive_analysis_results.rds")

cat("\n", rep("=", 80), "\n")
cat("✓ Predictive analysis complete! Results saved in 'Predictive_Output' folder\n")
cat(rep("=", 80), "\n")