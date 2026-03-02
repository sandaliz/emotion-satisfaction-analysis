# ============================================================================
# MOVIE DATASET ANALYSIS
# TPSM Assignment 2026
# ============================================================================
# Dataset: Movie Data
# Files: movies.csv, recommendation_logs.csv, reviews.csv, 
#        search_logs.csv, users.csv, watch_history.csv
# ============================================================================

# ============================================================================
# 1. LIBRARY LOADING
# ============================================================================

library(tidyverse)
library(psych)
library(lm.beta)
library(corrplot)
library(ggplot2)
library(dplyr)
library(ResourceSelection)

# ============================================================================
# 2. DATASET LOADING
# ============================================================================

setwd("G:\\TPSM_Assignment_2026\\data\\raw\\MovieData")
set.seed(123)

# Import all files
movies <- read.csv("movies.csv", stringsAsFactors = FALSE)
recommendations <- read.csv("recommendation_logs.csv", stringsAsFactors = FALSE)
reviews <- read.csv("reviews.csv", stringsAsFactors = FALSE)
search_log <- read.csv("search_logs.csv", stringsAsFactors = FALSE)
users <- read.csv("users.csv", stringsAsFactors = FALSE)
watch_history <- read.csv("watch_history.csv", stringsAsFactors = FALSE)

# ============================================================================
# 3. DATA CLEANING AND PROCESSING
# ============================================================================

cat("\n==================================================\n")
cat("DATA CLEANING AND PROCESSING\n")
cat("==================================================\n")

# 3.1 Process users file demographics
users_clean <- users %>%
  select(user_id, age, gender, country) %>%
  mutate(
    # Clean age
    age_clean = gsub("[^0-9.-]", "", as.character(age)),
    age_numeric = as.numeric(age_clean),
    age = ifelse(age_numeric < 5 | age_numeric > 100 | is.na(age_numeric), NA, age_numeric),
    
    # Age Groups
    age_group = case_when(
      age >= 5  & age <= 12 ~ "5-12",
      age >= 13 & age <= 18 ~ "13-18",
      age >= 19 & age <= 25 ~ "19-25",
      age >= 26 & age <= 35 ~ "26-35",
      age >= 36 & age <= 50 ~ "36-50",
      age > 50               ~ "50+",
      TRUE                   ~ "Unknown"
    ),
    
    # Clean Gender
    gender = case_when(
      tolower(gender) %in% c("m", "male", "man") ~ "Male",
      tolower(gender) %in% c("f", "female", "woman") ~ "Female",
      TRUE ~ "Other/Not Specified"
    ),
    
    # Clean Country
    country = trimws(as.character(country))
  )

# 3.2 Process REVIEWS file for Emotional Connection
reviews_clean <- reviews %>%
  select(user_id, movie_id, rating, helpful_votes, sentiment_score, review_date) %>%
  mutate(
    rating = as.numeric(rating), # 1-5 scale
    helpful_votes = as.numeric(helpful_votes),
    sentiment_score = as.numeric(sentiment_score),# 0-1 scale
    
    # Formula: (sentiment_score * 4) + 1  converts 0-1 to 1-5
    emotional_intensity = (sentiment_score * 4) + 1,
    
    # Cap between 1-5
    emotional_intensity = pmax(1, pmin(5, emotional_intensity)),
    
    review_year = lubridate::year(as.Date(review_date))
  ) %>%
  filter(!is.na(emotional_intensity))

cat("\nEmotional Intensity after rescaling (should be 1-5):\n")
cat("Range:", range(reviews_clean$emotional_intensity), "\n")     # min=1 max=5
cat("Mean:", round(mean(reviews_clean$emotional_intensity), 2), "\n")   # Mean: 3.54

# 3.3 Aggregate by user - emotional intensity
user_emotional <- reviews_clean %>%
  group_by(user_id) %>%
  summarise(
    emotional_intensity = mean(emotional_intensity, na.rm = TRUE),
    emotional_sd = sd(emotional_intensity, na.rm = TRUE),
    emotional_min = min(emotional_intensity, na.rm = TRUE),
    emotional_max = max(emotional_intensity, na.rm = TRUE),
    num_reviews = n(),
    avg_rating_given = mean(rating, na.rm = TRUE),
    avg_sentiment = mean(sentiment_score, na.rm = TRUE),
    total_helpful_votes = sum(helpful_votes, na.rm = TRUE),
    first_review_year = min(review_year, na.rm = TRUE),
    last_review_year = max(review_year, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(num_reviews >= 3, emotional_sd > 0 | is.na(emotional_sd))

cat("Users with emotional intensity scores:", nrow(user_emotional), "\n")  # 1713

# 3.4 Process WATCH_HISTORY file for Satisfaction
watch_clean <- watch_history %>%
  select(user_id, movie_id, watch_date, user_rating, watch_duration_minutes, 
         progress_percentage, device_type, location_country) %>%
  mutate(
    user_rating = as.numeric(user_rating),
    watch_duration_minutes = as.numeric(watch_duration_minutes),
    progress_percentage = as.numeric(progress_percentage),
    satisfaction = user_rating,
    
    engagement_level = case_when(
      progress_percentage >= 90 ~ "High Completion",
      progress_percentage >= 50 ~ "Medium Completion",
      progress_percentage >= 25 ~ "Low Completion",
      TRUE ~ "Barely Watched"
    ),
    
    watch_year = lubridate::year(as.Date(watch_date)),
    watch_month = lubridate::month(as.Date(watch_date))
  ) %>%
  filter(!is.na(satisfaction))

# 3.5 Aggregate by user - satisfaction
user_satisfaction <- watch_clean %>%
  group_by(user_id) %>%
  summarise(
    satisfaction = mean(satisfaction, na.rm = TRUE),
    satisfaction_sd = sd(satisfaction, na.rm = TRUE),
    satisfaction_min = min(satisfaction, na.rm = TRUE),
    satisfaction_max = max(satisfaction, na.rm = TRUE),
    num_ratings = n(),
    num_movies_watched = n_distinct(movie_id),
    avg_watch_duration = mean(watch_duration_minutes, na.rm = TRUE),
    avg_completion = mean(progress_percentage, na.rm = TRUE),
    pct_high_completion = mean(progress_percentage >= 90, na.rm = TRUE) * 100,
    first_watch_year = min(watch_year, na.rm = TRUE),
    last_watch_year = max(watch_year, na.rm = TRUE),
    mobile_ratio = mean(device_type == "Mobile", na.rm = TRUE),
    desktop_ratio = mean(device_type == "Desktop", na.rm = TRUE),
    tablet_ratio = mean(device_type == "Tablet", na.rm = TRUE),
    tv_ratio = mean(device_type == "Smart TV", na.rm = TRUE)
  ) %>%
  filter(num_ratings >= 3, satisfaction_sd > 0 | is.na(satisfaction_sd))

# 3.6 Combine all files into master dataset
master_data <- users_clean %>%
  left_join(user_emotional, by = "user_id") %>%
  left_join(user_satisfaction, by = "user_id")

# 3.7 Process user_genres data (clean duplicates)
cat("\n--- Cleaning user_genres data ---\n")

# Check original sizes
cat("Original watch_history rows:", nrow(watch_history), "\n")  # 105000 
cat("Original movies rows:", nrow(movies), "\n")  # 1040

# Clean watch_history - remove duplicates
watch_history_clean <- watch_history %>%
  distinct(user_id, movie_id, .keep_all = TRUE)

cat("watch_history after deduplication:", nrow(watch_history_clean), "rows (", 
    nrow(watch_history) - nrow(watch_history_clean), "duplicates removed)\n")  # 99470 rows

# Clean movies - ensure unique movie_ids
movies_clean <- movies %>%
  distinct(movie_id, .keep_all = TRUE) %>%
  select(movie_id, genre_primary)

cat("movies after deduplication:", nrow(movies_clean), "rows (", 
    nrow(movies) - nrow(movies_clean), "duplicates removed)\n")  # 1000 rows

# Check for missing genres
movies_with_genre <- movies_clean %>%
  filter(!is.na(genre_primary) & genre_primary != "")

cat("movies with valid genre:", nrow(movies_with_genre), "rows (", 
    nrow(movies_clean) - nrow(movies_with_genre), "without genre)\n")  # 1000 rows

# Perform join
user_genres_raw <- watch_history_clean %>%
  left_join(movies_with_genre, by = "movie_id", relationship = "many-to-many")

cat("After join:", nrow(user_genres_raw), "rows\n") # 99470 rows

# Remove movies without genre
user_genres_with_genre <- user_genres_raw %>%
  filter(!is.na(genre_primary))

cat("After removing missing genres:", nrow(user_genres_with_genre), "rows (", 
    nrow(user_genres_raw) - nrow(user_genres_with_genre), "removed)\n")  # 99470 rows

# Get preferred genre per user
user_genres <- user_genres_with_genre %>%
  group_by(user_id, genre_primary) %>%
  summarise(genre_count = n(), .groups = 'drop') %>%
  group_by(user_id) %>%
  slice_max(order_by = genre_count, n = 1, with_ties = FALSE) %>%
  select(user_id, preferred_genre = genre_primary)

cat("Final user_genres created with", nrow(user_genres), "users\n") # 10000 users

# Show top genres
cat("\nTop 10 preferred genres:\n")
user_genres %>%
  count(preferred_genre) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  print()

# Join with explicit relationship handling
master_data <- master_data %>%
  left_join(user_genres, by = "user_id", relationship = "many-to-many") %>%
  distinct(user_id, .keep_all = TRUE)  # Keep one row per user

# 3.8 Final filtering
master_data_final <- master_data %>%
  filter(
    !is.na(emotional_intensity),
    !is.na(satisfaction),
    !is.na(age),
    !is.na(gender),
    num_reviews >= 3,
    num_ratings >= 3
  )

write.csv(master_data_final, "Cleaned_movie_dataset.csv", row.names = FALSE)

cat("\nFinal dataset size:", nrow(master_data_final), "users\n")  # 513 users

# ============================================================================
# 4. CREATE BOOTSTRAP SAMPLE
# ============================================================================

set.seed(123)

# Create bootstrap sample (sampling WITH replacement)
movie_1500 <- master_data_final %>%
  sample_n(size = 1500, replace = TRUE)
write.csv(movie_1500, "Sample_movie_dataset.csv", row.names = FALSE)

# Check if sample is representative
cat("\nPopulation vs Sample comparison:\n")
cat("Population mean emotional intensity:", round(mean(master_data_final$emotional_intensity), 2), "\n")    # 3.55
cat("Sample mean emotional intensity:", round(mean(movie_1500$emotional_intensity), 2), "\n")               # 3.55
cat("Population mean satisfaction:", round(mean(master_data_final$satisfaction), 2), "\n")                  # 3.36
cat("Sample mean satisfaction:", round(mean(movie_1500$satisfaction), 2), "\n")                             # 3.38

# ============================================================================
# 5. CREATE ADDITIONAL VARIABLES FOR ANALYSIS
# ============================================================================

movie_1500 <- movie_1500 %>%
  mutate(
    # Standardized variables
    emotional_intensity_std = as.numeric(scale(emotional_intensity)),
    satisfaction_std = as.numeric(scale(satisfaction)),
    age_std = as.numeric(scale(age)),
    
    # Binary satisfaction for additional analyses (median split)
    satisfaction_binary = ifelse(satisfaction > median(satisfaction, na.rm = TRUE), 1, 0),
    
    # Emotional intensity groups for t-test (median split)
    emotional_median = median(emotional_intensity, na.rm = TRUE),
    
    # Dummy variables for regression
    gender_male = ifelse(gender == "Male", 1, 0),
    gender_other = ifelse(gender == "Other/Not Specified", 1, 0),
    
    # Age group dummies (reference: 5-12)
    age_13_18 = ifelse(age_group == "13-18", 1, 0),
    age_19_25 = ifelse(age_group == "19-25", 1, 0),
    age_26_35 = ifelse(age_group == "26-35", 1, 0),
    age_36_50 = ifelse(age_group == "36-50", 1, 0),
    age_50plus = ifelse(age_group == "50+", 1, 0)
  )

# Verify scales
cat("\n=== SCALE VERIFICATION (Should be 1-5) ===\n")
cat("Emotional Intensity - Range:", range(movie_1500$emotional_intensity), "\n")  # Range: 1.912 4.82 
cat("Satisfaction - Range:", range(movie_1500$satisfaction), "\n")               # Range: 1.333333 5

# ============================================================================
# 6. DESCRIPTIVE ANALYSIS
# ============================================================================

cat("\n==================================================\n")
cat("DESCRIPTIVE ANALYSIS\n")
cat("==================================================\n")

setwd("G:\\TPSM_Assignment_2026\\outputs\\descriptive\\Movie_Descriptive")

# 6.1 DEMOGRAPHIC DESCRIPTION
# ----------------------------------------------------------------------------

# Age statistics
age_stats <- movie_1500 %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE)
  )
cat("\nAge Statistics:\n")
print(age_stats)
# mean_age  sd_age min_age max_age median_age
# 35.61467 12.2182       5      96         36

# Age groups distribution
age_group_dist <- movie_1500 %>%
  count(age_group) %>%
  mutate(percentage = n / sum(n) * 100)
cat("\nAge Group Distribution:\n")
print(age_group_dist)
# age_group   n    percentage
# 5-12       59     3.933333
# 13-18      62     4.133333
# 19-25     164    10.933333
# 26-35     450    30.000000
# 36-50     587    39.133333
# 50+       178    11.866667

# Gender distribution
gender_dist <- movie_1500 %>%
  count(gender) %>%
  mutate(percentage = n / sum(n) * 100)
cat("\nGender Distribution:\n")
print(gender_dist)
# gender                  n   percentage
# Female                526    35.06667
# Male                  624    41.60000
# Other/Not Specified   350    23.33333

# Country distribution (top 10)
country_dist <- movie_1500 %>%
  count(country) %>%
  arrange(desc(n)) %>%
  head(10) %>%
  mutate(percentage = n / sum(n) * 100)
cat("\nTop 10 Countries:\n")
print(country_dist)
# country    n   percentage
# USA      1063    70.86667
# Canada    437    29.13333

# Visualize demographics
# Age histogram
p1 <- ggplot(movie_1500, aes(x = age)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Age Distribution of Users",
       x = "Age", y = "Count") +
  theme_minimal()
ggsave("age_distribution_hist.png", p1, width = 8, height = 5)

# Gender bar chart
p2 <- ggplot(movie_1500, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue", 
                               "Other/Not Specified" = "gray"))
ggsave("gender_distribution.png", p2, width = 8, height = 5)

# 6.2 EMOTIONAL INTENSITY DESCRIPTION
# ----------------------------------------------------------------------------

# Basic statistics
emotional_stats <- movie_1500 %>%
  summarise(
    mean_emo = round(mean(emotional_intensity, na.rm = TRUE), 2),
    sd_emo = round(sd(emotional_intensity, na.rm = TRUE), 2),
    min_emo = round(min(emotional_intensity, na.rm = TRUE), 2),
    max_emo = round(max(emotional_intensity, na.rm = TRUE), 2),
    median_emo = round(median(emotional_intensity, na.rm = TRUE), 2),
    skewness = round(psych::skew(emotional_intensity), 2),
    kurtosis = round(psych::kurtosi(emotional_intensity), 2)
  )
cat("\nEmotional Intensity Statistics:\n")
print(emotional_stats)
# mean_emo sd_emo min_emo max_emo median_emo skewness kurtosis
# 3.55     0.57    1.91    4.82       3.58    -0.28    -0.28

# Distribution visualization - Histogram
p3 <- ggplot(movie_1500, aes(x = emotional_intensity)) +
  geom_histogram(bins = 20, fill = "darkgreen", color = "black", alpha = 0.7) +
  geom_density(aes(y = after_stat(count)), color = "red", size = 1) +
  labs(title = "Distribution of Emotional Intensity",
       x = "Emotional Intensity", y = "Count") +
  theme_minimal() +
  geom_vline(xintercept = mean(movie_1500$emotional_intensity), 
             color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = mean(movie_1500$emotional_intensity) + 0.5, 
           y = 50, label = "Mean", color = "blue")
ggsave("emotional_intensity_hist.png", p3, width = 8, height = 5)

# Boxplot
p4 <- ggplot(movie_1500, aes(y = emotional_intensity)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Boxplot of Emotional Intensity",
       y = "Emotional Intensity") +
  theme_minimal()
ggsave("emotional_intensity_boxplot.png", p4, width = 8, height = 5)

# Emotional connection by gender
emo_by_gender <- movie_1500 %>%
  group_by(gender) %>%
  summarise(
    mean_emo = mean(emotional_intensity, na.rm = TRUE),
    sd_emo = sd(emotional_intensity, na.rm = TRUE),
    n = n()
  )
cat("\nEmotional Intensity by Gender:\n")
print(emo_by_gender)
# gender              mean_emo sd_emo   n
# Female                 3.60  0.558  526
# Male                   3.52  0.588  624
# Other/Not Specified    3.53  0.545  350

# By age group
emo_by_age <- movie_1500 %>%
  group_by(age_group) %>%
  summarise(
    mean_emo = mean(emotional_intensity, na.rm = TRUE),
    sd_emo = sd(emotional_intensity, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(age_group)
cat("\nEmotional Intensity by Age Group:\n")
print(emo_by_age)
# age_group mean_emo sd_emo   n
# 5-12         3.58  0.456  59
# 13-18        3.74  0.513  62
# 19-25        3.70  0.544 164
# 26-35        3.52  0.597 450
# 36-50        3.55  0.520 587
# 50+          3.43  0.616 178

# Visualize emotional connection by groups
# By gender
p5 <- ggplot(movie_1500, aes(x = gender, y = emotional_intensity, fill = gender)) +
  geom_boxplot() +
  labs(title = "Emotional Intensity by Gender",
       x = "Gender", y = "Emotional Intensity") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue"))
ggsave("emotional_intensity_gender.png", p5, width = 8, height = 5)

# By age group
p6 <- ggplot(movie_1500, aes(x = age_group, y = emotional_intensity, fill = age_group)) +
  geom_boxplot() +
  labs(title = "Emotional Intensity by Age Group",
       x = "Age Group", y = "Emotional Intensity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("emotional_intensity_age.png", p6, width = 8, height = 5)

# 6.3 SATISFACTION DESCRIPTION
# ----------------------------------------------------------------------------

# Basic statistics
satisfaction_stats <- movie_1500 %>%
  summarise(
    mean_sat = mean(satisfaction, na.rm = TRUE),
    sd_sat = sd(satisfaction, na.rm = TRUE),
    min_sat = min(satisfaction, na.rm = TRUE),
    max_sat = max(satisfaction, na.rm = TRUE),
    median_sat = median(satisfaction, na.rm = TRUE),
    skewness = psych::skew(satisfaction),
    kurtosis = psych::kurtosi(satisfaction)
  )
cat("\nSatisfaction Statistics:\n")
print(satisfaction_stats)
# mean_sat   sd_sat  min_sat max_sat median_sat skewness kurtosis
# 3.37768 0.6781707 1.333333      5  3.333333 -0.31743 -0.13332

# Distribution visualization - Histogram
p7 <- ggplot(movie_1500, aes(x = satisfaction)) +
  geom_histogram(bins = 30, fill = "purple", color = "black", alpha = 0.7) +
  geom_density(aes(y = after_stat(count)), color = "red", size = 1) +
  labs(title = "Distribution of Satisfaction Scores",
       x = "Satisfaction (1-5 scale)", y = "Count") +
  theme_minimal() +
  geom_vline(xintercept = mean(movie_1500$satisfaction), 
             color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = mean(movie_1500$satisfaction) + 0.5, 
           y = 50, label = paste("Mean =", round(mean(movie_1500$satisfaction), 2)), 
           color = "blue")
ggsave("satisfaction_distribution.png", p7, width = 8, height = 5)

# Boxplot
p8 <- ggplot(movie_1500, aes(x = "All Users", y = satisfaction)) +
  geom_boxplot(fill = "purple", alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
  annotate("text", x = 1.4, y = mean(movie_1500$satisfaction), 
           label = paste("Mean =", round(mean(movie_1500$satisfaction), 2)), 
           color = "red") +
  labs(title = "Boxplot of Satisfaction",
       x = "", y = "Satisfaction Score") +
  theme_minimal()
ggsave("satisfaction_boxplot.png", p8, width = 8, height = 5)

# 6.4 SATISFACTION BY DEMOGRAPHICS
# ----------------------------------------------------------------------------

# Boxplot by gender
p9 <- ggplot(movie_1500, aes(x = gender, y = satisfaction, fill = gender)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  labs(title = "Satisfaction by Gender",
       x = "Gender", y = "Satisfaction Score") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue", 
                               "Other/Not Specified" = "gray")) +
  theme(legend.position = "none")
ggsave("satisfaction_gender_box.png", p9, width = 8, height = 5)

# Bar chart with means and error bars for gender
sat_by_gender_summary <- movie_1500 %>%
  group_by(gender) %>%
  summarise(
    mean_sat = mean(satisfaction, na.rm = TRUE),
    sd_sat = sd(satisfaction, na.rm = TRUE),
    n = n(),
    se = sd_sat / sqrt(n),
    ci_lower = mean_sat - 1.96 * se,
    ci_upper = mean_sat + 1.96 * se
  )

p10 <- ggplot(sat_by_gender_summary, aes(x = gender, y = mean_sat, fill = gender)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Mean Satisfaction by Gender (with 95% CI)",
       x = "Gender", y = "Mean Satisfaction Score") +
  theme_minimal() +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue", 
                               "Other/Not Specified" = "gray")) +
  theme(legend.position = "none") +
  geom_text(aes(label = paste0("n=", n)), y = 1, color = "black")
ggsave("mean_satisfaction_gender.png", p10, width = 8, height = 5)

# Boxplot by age group
p11 <- ggplot(movie_1500, aes(x = age_group, y = satisfaction, fill = age_group)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  labs(title = "Satisfaction by Age Group",
       x = "Age Group", y = "Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Blues")
ggsave("satisfaction_age_box.png", p11, width = 8, height = 5)

# Bar chart with means and error bars for age group
sat_by_age_summary <- movie_1500 %>%
  group_by(age_group) %>%
  summarise(
    mean_sat = mean(satisfaction, na.rm = TRUE),
    sd_sat = sd(satisfaction, na.rm = TRUE),
    n = n(),
    se = sd_sat / sqrt(n),
    ci_lower = mean_sat - 1.96 * se,
    ci_upper = mean_sat + 1.96 * se
  ) %>%
  arrange(age_group)

p12 <- ggplot(sat_by_age_summary, aes(x = age_group, y = mean_sat, fill = age_group)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Mean Satisfaction by Age Group (with 95% CI)",
       x = "Age Group", y = "Mean Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = paste0("n=", n)), y = 1, color = "black")
ggsave("mean_satisfaction_age.png", p12, width = 8, height = 5)

# Satisfaction by Country (Top countries only)
top_countries <- movie_1500 %>%
  count(country) %>%
  arrange(desc(n)) %>%
  head(8) %>%
  pull(country)

# Filter for top countries
movie_top_countries <- movie_1500 %>%
  filter(country %in% top_countries)

# Boxplot by country
p13 <- ggplot(movie_top_countries, aes(x = reorder(country, satisfaction, FUN = median), 
                                       y = satisfaction, fill = country)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Satisfaction by Country (Top 8 Countries)",
       x = "Country", y = "Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  coord_flip()
ggsave("satisfaction_country_box.png", p13, width = 8, height = 5)

# Combined visualization - Gender and age
p14 <- ggplot(movie_1500, aes(x = age_group, y = satisfaction, fill = gender)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  labs(title = "Satisfaction by Age Group and Gender",
       x = "Age Group", y = "Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Female" = "pink", "Male" = "lightblue", 
                               "Other/Not Specified" = "gray"))
ggsave("satisfaction_gender_age.png", p14, width = 8, height = 5)

# Cross-tabulation table
satisfaction_demographics_table <- movie_1500 %>%
  group_by(age_group, gender) %>%
  summarise(
    mean_satisfaction = round(mean(satisfaction, na.rm = TRUE), 2),
    sd_satisfaction = round(sd(satisfaction, na.rm = TRUE), 2),
    n = n(),
    .groups = 'drop'
  ) %>%
  pivot_wider(
    names_from = gender,
    values_from = c(mean_satisfaction, sd_satisfaction, n),
    values_fill = 0
  )

cat("\nSatisfaction by Age Group and Gender (Cross-tabulation):\n")
print(satisfaction_demographics_table)

# 6.5 EMOTION-SATISFACTION RELATIONSHIP
# ----------------------------------------------------------------------------

# Overall correlation
cor_test <- cor.test(movie_1500$emotional_intensity, 
                     movie_1500$satisfaction, 
                     method = "pearson")

cat("\nCorrelation between Emotional Connection and Satisfaction:\n")
cat("Pearson r =", round(cor_test$estimate, 3), "\n")        # r = 0.056
cat("p-value =", round(cor_test$p.value, 4), "\n")           # p-value = 0.0307
cat("95% CI: [", round(cor_test$conf.int[1], 3), ",", 
    round(cor_test$conf.int[2], 3), "]\n")                   # [ 0.005 , 0.106 ]

# Scatter plot with trend line
p15 <- ggplot(movie_1500, aes(x = emotional_intensity, y = satisfaction)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE, fill = "pink") +
  labs(title = "Relationship Between Emotional Intensity and Satisfaction",
       x = "Emotional Intensity Score", 
       y = "Satisfaction Score") +
  theme_minimal() +
  annotate("text", 
           x = min(movie_1500$emotional_intensity) + 1, 
           y = max(movie_1500$satisfaction) - 0.5,
           label = paste("r =", round(cor_test$estimate, 3), 
                         "\np =", round(cor_test$p.value, 4)),
           hjust = 0)
ggsave("emotional_intensity_satisfaction_scatter.png", p15, width = 8, height = 5)

# Create emotional connection groups (quartiles)
movie_1500 <- movie_1500 %>%
  mutate(
    emotional_group = case_when(
      emotional_intensity < quantile(emotional_intensity, 0.25) ~ "Low Emotion",
      emotional_intensity < quantile(emotional_intensity, 0.50) ~ "Medium-Low",
      emotional_intensity < quantile(emotional_intensity, 0.75) ~ "Medium-High",
      TRUE ~ "High Emotion"
    )
  )

# Satisfaction by emotional group
sat_by_emotion_group <- movie_1500 %>%
  group_by(emotional_group) %>%
  summarise(
    mean_satisfaction = mean(satisfaction, na.rm = TRUE),
    sd_satisfaction = sd(satisfaction, na.rm = TRUE),
    n = n(),
    se = sd_satisfaction / sqrt(n),
    ci_lower = mean_satisfaction - 1.96 * se,
    ci_upper = mean_satisfaction + 1.96 * se
  ) %>%
  arrange(emotional_group)

cat("\nSatisfaction by Emotional Connection Group:\n")
print(sat_by_emotion_group)
# emotional_group mean_satisfaction sd_satisfaction   n     se ci_lower ci_upper
# High Emotion               3.46           0.675 375 0.0349     3.39     3.52
# Low Emotion                3.35           0.674 374 0.0349     3.28     3.42
# Medium-High                3.31           0.716 377 0.0369     3.24     3.38
# Medium-Low                 3.40           0.639 374 0.0330     3.33     3.46

# Bar chart of satisfaction by emotional group
p16 <- ggplot(sat_by_emotion_group, aes(x = emotional_group, y = mean_satisfaction, 
                                        fill = emotional_group)) +
  geom_col() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Satisfaction by Emotional Connection Level",
       x = "Emotional Connection Group", 
       y = "Mean Satisfaction Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Low Emotion" = "red", 
                               "Medium-Low" = "orange",
                               "Medium-High" = "lightgreen", 
                               "High Emotion" = "darkgreen"))
ggsave("satisfaction_emotion_group.png", p16, width = 8, height = 5)

# Correlation by gender
cor_by_gender <- movie_1500 %>%
  group_by(gender) %>%
  summarise(
    correlation = cor(emotional_intensity, satisfaction, method = "pearson"),
    p_value = cor.test(emotional_intensity, satisfaction)$p.value,
    n = n()
  )
cat("\nCorrelation by Gender:\n")
print(cor_by_gender)
# gender              correlation p_value   n
# Female                 0.0182 0.677    526
# Male                   0.00860 0.830    624
# Other/Not Specified    0.224  0.0000244 350

# Correlation by age group
cor_by_age <- movie_1500 %>%
  group_by(age_group) %>%
  filter(n() >= 10) %>%
  summarise(
    correlation = cor(emotional_intensity, satisfaction, method = "pearson"),
    p_value = cor.test(emotional_intensity, satisfaction)$p.value,
    n = n()
  )
cat("\nCorrelation by Age Group:\n")
print(cor_by_age)
# age_group correlation p_value   n
# 13-18         0.111  0.391    62
# 19-25         0.127  0.105   164
# 26-35         0.0708 0.134   450
# 36-50         0.0514 0.213   587
# 5-12          0.190  0.149    59
# 50+          -0.0405 0.591   178

# ============================================================================
# 7. INFERENTIAL ANALYSIS - MAIN HYPOTHESIS TESTING
# ============================================================================
# Hypothesis: "Audiences who feel emotionally connected report higher satisfaction"
# ============================================================================

cat("\n==================================================\n")
cat("INFERENTIAL ANALYSIS - MAIN HYPOTHESIS TESTING\n")
cat("==================================================\n")

# 7.1 Address Scale Mismatch First
# ----------------------------------------------------------------------------

cat("\n--- STEP 1: Addressing Scale Mismatch ---\n")
cat("\nVariable Scales:\n")
cat("  Emotional Connection: Range", range(movie_1500$emotional_intensity)[1], "-", 
    range(movie_1500$emotional_intensity)[2], 
    "(Mean =", round(mean(movie_1500$emotional_intensity), 2), ")\n")
# Range 1.912 - 4.82 (Mean = 3.55)

cat("  Satisfaction: Range", range(movie_1500$satisfaction)[1], "-", 
    range(movie_1500$satisfaction)[2], 
    "(Mean =", round(mean(movie_1500$satisfaction), 2), ")\n")
# Range 1.333333 - 5 (Mean = 3.38)

cat("\n✓ Created standardized variables (mean ≈ 0, SD ≈ 1)\n")

# 7.2 Simple Regression (Emotional Connection ONLY)
# ----------------------------------------------------------------------------

cat("\n--- STEP 2: Simple Regression (Without Demographics) ---\n")

# Raw model
model_simple_raw <- lm(satisfaction ~ emotional_intensity, data = movie_1500)
cat("\n▶ Raw Model (original scales):\n")
cat("   Coefficient =", round(coef(model_simple_raw)[2], 4), "\n")        # 0.0665
cat("   p-value =", round(summary(model_simple_raw)$coefficients[2, 4], 4), "\n")  # 0.0307
cat("   R² =", round(summary(model_simple_raw)$r.squared, 4), "\n")       # R² = 0.0031

# Standardized model
model_simple_std <- lm(satisfaction_std ~ emotional_intensity_std, data = movie_1500)
cat("\n▶ Standardized Model (scale-free):\n")
cat("   Standardized β =", round(coef(model_simple_std)[2], 4), "\n")     # β = 0.0558
cat("   p-value =", round(summary(model_simple_std)$coefficients[2, 4], 4), "\n")  # p-value = 0.0307
cat("   R² =", round(summary(model_simple_std)$r.squared, 4), "\n")       # R² = 0.0031

# Interpretation
if(summary(model_simple_raw)$coefficients[2, 4] < 0.05) {
  cat("\n✓ SIMPLE MODEL: Emotional connection SIGNIFICANTLY predicts satisfaction\n")
} else {
  cat("\n✗ SIMPLE MODEL: Emotional connection does NOT significantly predict satisfaction\n")
}
# ✓ SIMPLE MODEL: Emotional connection SIGNIFICANTLY predicts satisfaction

# 7.3 Multiple Regression (WITH Demographics as Controls)
# ----------------------------------------------------------------------------

cat("\n--- STEP 3: Multiple Regression (With Demographics) ---\n")

# RAW MODEL (original scales)
model_full_raw <- lm(satisfaction ~ emotional_intensity + age + 
                       gender_male + gender_other + 
                       age_13_18 + age_19_25 + age_26_35 + age_36_50 + age_50plus,
                     data = movie_1500)

cat("\n▶ RAW MODEL RESULTS (original scales):\n")
print(summary(model_full_raw))

# Extract emotional connection coefficient
emo_coef_raw <- coef(model_full_raw)["emotional_intensity"]
emo_p_raw <- summary(model_full_raw)$coefficients["emotional_intensity", 4]

cat("\n▶ Emotional Connection (with demographics controlled):\n")
cat("   Raw coefficient =", round(emo_coef_raw, 4), "\n")        # 0.0761
cat("   p-value =", round(emo_p_raw, 4), "\n")                   # 0.0142

# STANDARDIZED MODEL (for fair comparison)
model_full_std <- lm(satisfaction_std ~ emotional_intensity_std + age_std + 
                       gender_male + gender_other + 
                       age_13_18 + age_26_35 + age_36_50 + age_50plus,
                     data = movie_1500)

cat("\n▶ STANDARDIZED MODEL RESULTS (scale-free):\n")
summary_std <- summary(model_full_std)
print(summary_std)

# Extract standardized coefficient
emo_coef_std <- coef(model_full_std)["emotional_intensity_std"]
emo_p_std <- summary_std$coefficients["emotional_intensity_std", 4]

cat("\n▶ Emotional Connection (standardized):\n")
cat("   Standardized β =", round(emo_coef_std, 4), "\n")        # β = 0.0628
cat("   p-value =", round(emo_p_std, 4), "\n")                  # 0.0158

# 7.4 Model Comparison
# ----------------------------------------------------------------------------

cat("\n--- STEP 4: Model Comparison ---\n")

# Compare simple vs full model
model_comparison <- anova(model_simple_raw, model_full_raw)
print(model_comparison)

if(model_comparison$`Pr(>F)`[2] < 0.05) {
  cat("\n✓ Adding demographics SIGNIFICANTLY improves the model\n")
} else {
  cat("\n✗ Adding demographics does NOT significantly improve the model\n")
}
# ✓ Adding demographics SIGNIFICANTLY improves the model

# 7.5 Main Hypothesis Conclusion
# ----------------------------------------------------------------------------

cat("\n--- STEP 5: MAIN HYPOTHESIS CONCLUSION ---\n")

cat("\nComparison of Emotional Connection Effect:\n")
cat("   Without demographics: p =", round(summary(model_simple_raw)$coefficients[2, 4], 4), "\n")  # p = 0.0307
cat("   With demographics:    p =", round(emo_p_raw, 4), "\n")                                      # p = 0.0142

if(emo_p_raw < 0.05) {
  cat("\n✓✓✓ MAIN HYPOTHESIS SUPPORTED!\n")
  cat("   Emotional connection significantly predicts satisfaction")
  cat("   even after controlling for age and gender.\n")
  
  if(emo_coef_raw > 0) {
    cat("   The relationship is POSITIVE (as emotional connection increases,")
    cat(" satisfaction increases).\n")
  } else {
    cat("   However, the relationship is NEGATIVE (opposite of hypothesis).\n")
  }
} else {
  cat("\n✗✗✗ MAIN HYPOTHESIS NOT SUPPORTED\n")
  cat("   Emotional connection does NOT significantly predict satisfaction")
  cat(" after controlling for age and gender.\n")
  cat("   p-value =", round(emo_p_raw, 4), "> 0.05\n")
}
# ✓✓✓ MAIN HYPOTHESIS SUPPORTED!
# Emotional connection significantly predicts satisfaction even after controlling for age and gender.
# The relationship is POSITIVE (as emotional connection increases, satisfaction increases).

# 7.6 Confidence Intervals
# ----------------------------------------------------------------------------

cat("\n--- STEP 6: Confidence Intervals ---\n")
cat("\n95% Confidence Interval for Emotional Connection:\n")
emo_ci <- confint(model_full_raw)["emotional_intensity", ]
cat("   [", round(emo_ci[1], 4), ", ", round(emo_ci[2], 4), "]\n")  # [ 0.0153 , 0.1369 ]

if(emo_ci[1] < 0 & emo_ci[2] < 0) {
  cat("   Entire interval is NEGATIVE - significant negative relationship\n")
} else if(emo_ci[1] > 0 & emo_ci[2] > 0) {
  cat("   Entire interval is POSITIVE - significant positive relationship\n")
} else if(emo_ci[1] < 0 & emo_ci[2] > 0) {
  cat("   Interval contains ZERO - relationship NOT significant\n")
}
# Entire interval is POSITIVE - significant positive relationship

# 7.7 Summary Table for Main Hypothesis
# ----------------------------------------------------------------------------

cat("\n--- STEP 7: Summary Table ---\n")

hypothesis_summary <- data.frame(
  Model = c("Simple (no controls)", "Full (with demographics)"),
  Predictor = c("Emotional Connection", "Emotional Connection"),
  Coefficient = c(round(coef(model_simple_raw)[2], 4), round(emo_coef_raw, 4)),
  Std_Coefficient = c(round(coef(model_simple_std)[2], 4), round(emo_coef_std, 4)),
  P_value = c(round(summary(model_simple_raw)$coefficients[2, 4], 4), round(emo_p_raw, 4)),
  Significant = c(
    ifelse(summary(model_simple_raw)$coefficients[2, 4] < 0.05, "YES", "NO"),
    ifelse(emo_p_raw < 0.05, "YES", "NO")
  )
)

print(hypothesis_summary)

# ============================================================================
# 8. ADDITIONAL HYPOTHESIS TESTING METHODS
# ============================================================================

cat("\n==================================================\n")
cat("ADDITIONAL HYPOTHESIS TESTING METHODS\n")
cat("==================================================\n")

# 8.1 Create High/Low Emotional Connection Groups
# ----------------------------------------------------------------------------

# Using median split
emo_median <- median(movie_1500$emotional_intensity)

movie_1500 <- movie_1500 %>%
  mutate(
    emotion_level = case_when(
      emotional_intensity >= emo_median ~ "High Emotion",
      TRUE ~ "Low Emotion"
    )
  )

cat("\n--- Emotional Connection Groups (Median Split) ---\n")
table(movie_1500$emotion_level)
# High Emotion  Low Emotion 
# 752           748

# 8.2 METHOD 1 - Compare MEANS (t-test)
# ----------------------------------------------------------------------------

cat("\n", rep("-", 60), "\n")
cat("METHOD 1: COMPARING MEANS (Independent t-test)\n")
cat(rep("-", 60), "\n")

# Extract satisfaction by group
high_emo_sat <- movie_1500 %>% filter(emotion_level == "High Emotion") %>% pull(satisfaction)
low_emo_sat <- movie_1500 %>% filter(emotion_level == "Low Emotion") %>% pull(satisfaction)

# Descriptive stats
cat("\nGroup Statistics:\n")
cat("  High Emotion Group: n =", length(high_emo_sat), 
    ", Mean =", round(mean(high_emo_sat), 3),
    ", SD =", round(sd(high_emo_sat), 3), "\n")        # High Emotion Group: n = 752, Mean = 3.382, SD = 0.699
cat("  Low Emotion Group:  n =", length(low_emo_sat), 
    ", Mean =", round(mean(low_emo_sat), 3),
    ", SD =", round(sd(low_emo_sat), 3), "\n")         # Low Emotion Group: n = 748, Mean = 3.373, SD = 0.657
cat("  Mean Difference:", round(mean(high_emo_sat) - mean(low_emo_sat), 3), "\n")  # Mean Difference: 0.009

# Independent t-test
t_test_means <- t.test(high_emo_sat, low_emo_sat, var.equal = TRUE, 
                       alternative = "greater")

cat("\n▶ Independent t-test Results:\n")
cat("  t-statistic =", round(t_test_means$statistic, 3), "\n")        # t-statistic = 0.253
cat("  df =", round(t_test_means$parameter, 0), "\n")                 # df = 1498
cat("  p-value =", round(t_test_means$p.value, 4), "\n")              # p-value = 0.4

if(t_test_means$p.value < 0.05) {
  cat("\n✓ MEANS TEST: High Emotion group has significantly HIGHER satisfaction\n")
} else {
  cat("\n✗ MEANS TEST: No significant difference in satisfaction means\n")
}
# ✗ MEANS TEST: No significant difference in satisfaction means

# Effect size (Cohen's d)
pooled_sd <- sqrt(((length(high_emo_sat)-1)*var(high_emo_sat) + 
                     (length(low_emo_sat)-1)*var(low_emo_sat)) / 
                    (length(high_emo_sat) + length(low_emo_sat) - 2))
cohens_d <- (mean(high_emo_sat) - mean(low_emo_sat)) / pooled_sd

cat("\nEffect size (Cohen's d) =", round(abs(cohens_d), 3))           # Effect size = 0.013
cat(" - Very small effect\n")

# 8.3 METHOD 2 - Compare VARIANCES (F-test)
# ----------------------------------------------------------------------------

cat("\n", rep("-", 60), "\n")
cat("METHOD 2: COMPARING VARIANCES (F-test)\n")
cat(rep("-", 60), "\n")

# F-test for variances
var_test <- var.test(high_emo_sat, low_emo_sat)

cat("\nGroup Variances:\n")
cat("  High Emotion Group variance =", round(var(high_emo_sat), 4), "\n")     # 0.4888
cat("  Low Emotion Group variance  =", round(var(low_emo_sat), 4), "\n")      # 0.4314
cat("  Ratio (High/Low) =", round(var(high_emo_sat)/var(low_emo_sat), 3), "\n") # 1.133

cat("\n▶ F-test Results:\n")
cat("  F-statistic =", round(var_test$statistic, 3), "\n")        # F-statistic = 1.133
cat("  numerator df =", var_test$parameter[1], "\n")             # numerator df = 751
cat("  denominator df =", var_test$parameter[2], "\n")           # denominator df = 747
cat("  p-value =", round(var_test$p.value, 4), "\n")              # p-value = 0.0877

if(var_test$p.value < 0.05) {
  cat("\n✓ VARIANCES TEST: Satisfaction variance differs significantly between groups\n")
} else {
  cat("\n✗ VARIANCES TEST: No significant difference in variances\n")
}
# ✗ VARIANCES TEST: No significant difference in variances

# 8.4 METHOD 3 - Compare PROPORTIONS (z-test)
# ----------------------------------------------------------------------------

cat("\n", rep("-", 60), "\n")
cat("METHOD 3: COMPARING PROPORTIONS (z-test)\n")
cat(rep("-", 60), "\n")

# Create binary satisfaction
sat_median <- median(movie_1500$satisfaction)
movie_1500 <- movie_1500 %>%
  mutate(satisfied = ifelse(satisfaction > sat_median, 1, 0))

# Create contingency table
prop_table <- table(movie_1500$emotion_level, movie_1500$satisfied)
colnames(prop_table) <- c("Not Satisfied", "Satisfied")
#                     Not Satisfied Satisfied
# High Emotion           365       387
# Low Emotion            394       354

# Calculate proportions
props <- movie_1500 %>%
  group_by(emotion_level) %>%
  summarise(
    n = n(),
    satisfied_count = sum(satisfied),
    proportion = satisfied_count / n
  )

cat("\nProportions Satisfied by Emotion Group:\n")
print(props)
# emotion_level   n  satisfied_count proportion
# High Emotion  752             387      0.515
# Low Emotion   748             354      0.473

# Two-proportions z-test
prop_test <- prop.test(prop_table, alternative = "greater")

cat("\n▶ Two-Proportions Test Results:\n")
cat("  X-squared =", round(prop_test$statistic, 3), "\n")        # X-squared = 2.404
cat("  df =", prop_test$parameter, "\n")                         # df = 1
cat("  p-value =", round(prop_test$p.value, 4), "\n")            # p-value = 0.9395

prop_diff <- props$proportion[props$emotion_level == "High Emotion"] - 
  props$proportion[props$emotion_level == "Low Emotion"]
cat("  Difference in proportions =", round(prop_diff, 4), "\n")   # Difference = 0.0414

if(prop_test$p.value < 0.05) {
  cat("\n✓ PROPORTIONS TEST: High Emotion group has significantly HIGHER satisfaction rate\n")
} else {
  cat("\n✗ PROPORTIONS TEST: No significant difference in satisfaction rates\n")
}
# ✗ PROPORTIONS TEST: No significant difference in satisfaction rates

# 8.5 SUMMARY OF ALL METHODS
# ----------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n")
cat("SUMMARY: TESTING MAIN HYPOTHESIS - ALL METHODS\n")
cat(rep("=", 80), "\n")

summary_multiple <- data.frame(
  Method = c("Regression", "Means (t-test)", "Variances (F-test)", "Proportions (z-test)"),
  What_It_Tests = c("Linear relationship", "Difference in averages", 
                    "Difference in consistency", "Difference in satisfaction rate"),
  P_value = c(round(emo_p_raw, 4), round(t_test_means$p.value, 4), 
              round(var_test$p.value, 4), round(prop_test$p.value, 4)),
  Supports_Hypothesis = c(
    ifelse(emo_p_raw < 0.05, "YES", "NO"),
    ifelse(t_test_means$p.value < 0.05, "YES", "NO"),
    ifelse(var_test$p.value < 0.05, "YES", "NO"),
    ifelse(prop_test$p.value < 0.05, "YES", "NO")
  )
)

print(summary_multiple)
# Method              What_It_Tests P_value Supports_Hypothesis
# Regression          Linear relationship 0.0142                 YES
# Means (t-test)      Difference in averages 0.4000                  NO
# Variances (F-test)  Difference in consistency 0.0877                  NO
# Proportions (z-test) Difference in satisfaction rate 0.9395                  NO

cat("\n", rep("-", 60), "\n")
cat("OVERALL CONCLUSION:\n")

sig_count <- sum(summary_multiple$Supports_Hypothesis == "YES")

if(sig_count == 0) {
  cat("  ALL methods show NO significant relationship.\n")
  cat("  STRONG evidence that emotional connection does NOT predict satisfaction.\n")
} else if(sig_count == 1) {
  cat("  Only ONE method shows significance.\n")
  cat("  Weak/Inconsistent evidence - likely no real relationship.\n")
} else if(sig_count >= 2) {
  cat("  Multiple methods show significance.\n")
  cat("  Evidence suggests emotional connection MAY affect satisfaction.\n")
}
# Only ONE method shows significance - Weak/Inconsistent evidence

# Boxplot showing the variance difference
p17 <- ggplot(movie_1500, aes(x = emotion_level, y = satisfaction, fill = emotion_level)) +
  geom_boxplot() +
  labs(title = "Satisfaction Distribution by Emotional Connection Level",
       subtitle = paste("Variance difference p =", round(var_test$p.value, 4)),
       x = "Emotional Connection Group", 
       y = "Satisfaction") +
  theme_minimal()
ggsave("satisfaction_by_emotion_variance.png", p17, width = 8, height = 5)

# ============================================================================
# 9. PROBABILITY DISTRIBUTIONS FOR MAIN HYPOTHESIS
# ============================================================================

cat("\n==================================================\n")
cat("PROBABILITY DISTRIBUTIONS FOR MAIN HYPOTHESIS\n")
cat("==================================================\n")

# 9.1 Test NORMAL Distribution Assumption
# ----------------------------------------------------------------------------

cat("\n--- 1. NORMAL Distribution Assumption ---\n")

model_normal <- lm(satisfaction ~ emotional_intensity + age + gender_male + gender_other, 
                   data = movie_1500)

# Check if residuals are Normal
residuals_normal <- residuals(model_normal)
shapiro_test_normal <- shapiro.test(residuals_normal)

cat("\nNormality of Residuals (should be Normal if assumption holds):\n")
cat("  Shapiro-Wilk W =", round(shapiro_test_normal$statistic, 4), "\n")
cat("  p-value =", round(shapiro_test_normal$p.value, 4), "\n")

if(shapiro_test_normal$p.value > 0.05) {
  cat("✓ Residuals are Normal - Normal distribution assumption is valid\n")
} else {
  cat("✗ Residuals are NOT Normal - Normal assumption may be violated\n")
}

# AIC for Normal model
aic_normal <- AIC(model_normal)
cat("\nNormal Model AIC:", round(aic_normal, 2), "\n")

# 9.2 Test GAMMA Distribution (for positive, skewed data)
# ----------------------------------------------------------------------------

cat("\n--- 2. GAMMA Distribution Assumption ---\n")

# Gamma GLM (log link ensures positive predictions)
model_gamma <- glm(satisfaction ~ emotional_intensity + age + gender_male + gender_other, 
                   family = Gamma(link = "log"), data = movie_1500)

# Check deviance residuals
residuals_gamma <- residuals(model_gamma, type = "deviance")
shapiro_test_gamma <- shapiro.test(residuals_gamma)

cat("\nDeviance Residuals Normality (should be Normal if Gamma fits):\n")
cat("  Shapiro-Wilk W =", round(shapiro_test_gamma$statistic, 4), "\n")
cat("  p-value =", round(shapiro_test_gamma$p.value, 4), "\n")

# AIC for Gamma model
aic_gamma <- AIC(model_gamma)
cat("\nGamma Model AIC:", round(aic_gamma, 2), "\n")

# 9.3 Test BINOMIAL Distribution (for binary satisfaction)
# ----------------------------------------------------------------------------

cat("\n--- 3. BINOMIAL Distribution Assumption ---\n")

# Binomial GLM (logistic regression)
model_binomial <- glm(satisfied ~ emotional_intensity + age + gender_male + gender_other, 
                      family = binomial, data = movie_1500)

# Hosmer-Lemeshow goodness of fit
hl_test <- hoslem.test(model_binomial$y, fitted(model_binomial), g = 10)

cat("\nHosmer-Lemeshow Goodness-of-Fit:\n")
cat("  X-squared =", round(hl_test$statistic, 4), "\n")
cat("  p-value =", round(hl_test$p.value, 4), "\n")

if(hl_test$p.value > 0.05) {
  cat("✓ Binomial model fits well\n")
} else {
  cat("✗ Binomial model may not fit well\n")
}

# AIC for Binomial model
aic_binomial <- AIC(model_binomial)
cat("\nBinomial Model AIC:", round(aic_binomial, 2), "\n")

# 9.4 Compare All Distributions
# ----------------------------------------------------------------------------

cat("\n", rep("-", 60), "\n")
cat("DISTRIBUTION COMPARISON FOR MAIN HYPOTHESIS\n")
cat(rep("-", 60), "\n")

comparison <- data.frame(
  Distribution = c("Normal", "Gamma", "Binomial"),
  AIC = c(aic_normal, aic_gamma, aic_binomial),
  Assumption_Check = c(
    ifelse(shapiro_test_normal$p.value > 0.05, "Pass", "Fail"),
    ifelse(shapiro_test_gamma$p.value > 0.05, "Pass", "Fail"),
    ifelse(hl_test$p.value > 0.05, "Pass", "Fail")
  )
)

print(comparison)

cat("\nBest fitting distribution (lowest AIC):", 
    comparison$Distribution[which.min(comparison$AIC)], "\n")

# 9.5 Main Hypothesis Under Different Distributions
# ----------------------------------------------------------------------------

cat("\n", rep("-", 60), "\n")
cat("MAIN HYPOTHESIS UNDER DIFFERENT DISTRIBUTIONS\n")
cat(rep("-", 60), "\n")

# Extract emotional_intensity coefficient and p-value from each model
results_dist <- data.frame(
  Distribution = c("Normal", "Gamma", "Binomial"),
  emotional_intensity_Effect = c(
    paste0(round(coef(model_normal)["emotional_intensity"], 4), 
           ifelse(summary(model_normal)$coefficients["emotional_intensity", 4] < 0.05, "*", "")),
    paste0(round(coef(model_gamma)["emotional_intensity"], 4), 
           ifelse(summary(model_gamma)$coefficients["emotional_intensity", 4] < 0.05, "*", "")),
    paste0(round(coef(model_binomial)["emotional_intensity"], 4), 
           ifelse(summary(model_binomial)$coefficients["emotional_intensity", 4] < 0.05, "*", ""))
  ),
  P_value = c(
    round(summary(model_normal)$coefficients["emotional_intensity", 4], 4),
    round(summary(model_gamma)$coefficients["emotional_intensity", 4], 4),
    round(summary(model_binomial)$coefficients["emotional_intensity", 4], 4)
  ),
  Significant = c(
    summary(model_normal)$coefficients["emotional_intensity", 4] < 0.05,
    summary(model_gamma)$coefficients["emotional_intensity", 4] < 0.05,
    summary(model_binomial)$coefficients["emotional_intensity", 4] < 0.05
  )
)

print(results_dist)

cat("\nCONCLUSION:\n")
if(all(!results_dist$Significant)) {
  cat("  Under ALL distribution assumptions, emotional connection is NOT significant.\n")
  cat("  This STRENGTHENS your finding - it's not just due to distribution choice.\n")
} else if(any(results_dist$Significant)) {
  cat("  Emotional connection is significant ONLY under", 
      results_dist$Distribution[results_dist$Significant], "distribution.\n")
  cat("  This suggests the relationship depends on distributional assumptions.\n")
}

# ============================================================================
# 10. FINAL SUMMARY
# ============================================================================

cat("\n==================================================\n")
cat("FINAL ANALYSIS SUMMARY\n")
cat("==================================================\n")

cat("\nKey Findings:\n")
cat("1. Emotional Intensity mean =", round(mean(movie_1500$emotional_intensity), 2), 
    "(SD =", round(sd(movie_1500$emotional_intensity), 2), ")\n")
cat("2. Satisfaction mean =", round(mean(movie_1500$satisfaction), 2), 
    "(SD =", round(sd(movie_1500$satisfaction), 2), ")\n")
cat("3. Correlation between Emotional Intensity and Satisfaction: r =", 
    round(cor_test$estimate, 3), "(p =", round(cor_test$p.value, 4), ")\n")
cat("4. Regression (with controls): β =", round(emo_coef_std, 3), 
    "(p =", round(emo_p_std, 4), ")\n")
cat("5. Best fitting distribution:", comparison$Distribution[which.min(comparison$AIC)], "\n")

cat("\n==================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("==================================================\n")