
#Below are all the libraries that are needed to run this code. If you do not have these libraries, please
# install the packages before running the code.

library(tidyverse)
library(scales)
library(knitr)
library(kableExtra)
library(infer)
library(car)
library(gtsummary)
library(DescTools)
library(yardstick)
library(pROC)
library(ranger)


# **************************************************************
# PART 1. CREATING DATASET AND CLEANING
# **************************************************************

#First, load in the cleaned songs and artists datasets.
#I have named these datasets as "cleaned" because I have altered these
# by deleting columns and/or editing columns.

songs_cleaned <- read_csv("songs_cleaned.csv", col_names = TRUE)
songs_cleaned

artists_cleaned <- read_csv("artists_cleaned.csv", col_names = TRUE)
artists_cleaned

#Everything looks great with these 2 data frames. 
# Now left join them together.

songs_artists_combined <- songs_cleaned %>%
  left_join(artists_cleaned, by = "artist_id")
songs_artists_combined

#Now, time to load in the cleaned acoustic features and left
#join it to songs_artists_combined.

acoustic_features_cleaned <- read_csv("acoustic_features_cleaned.csv", col_names = TRUE)
acoustic_features_cleaned

songs_artists_acoustic_combined <- songs_artists_combined %>%
  left_join(acoustic_features_cleaned, by = "song_id")
songs_artists_acoustic_combined

#Now, time to load in the cleaned song pop.

song_pop_cleaned <- read_csv("song_pop_cleaned.csv", col_names = TRUE)
song_pop_cleaned

# Now, left join song_pop_cleaned to songs_artists_acoustic_combined.

songs_artists_acoustic_pop_combined <- songs_artists_acoustic_combined %>%
  left_join(song_pop_cleaned, by = "song_id")
songs_artists_acoustic_pop_combined

# Now, I'm going to filter the dataset using the filter() function, so only songs from the years 2000-2018 are in the
# dataset. I'm going to call the new dataset songs_artists_acoustic_pop_combined_final.

songs_artists_acoustic_pop_combined_final <- songs_artists_acoustic_pop_combined %>%
  filter(year >= 2000)
songs_artists_acoustic_pop_combined_final

# Now joining files is completed. There is now a dataset for the project, which contains 5,791 observations and 23 variables.
# Time to check that all of the variables have been correctly
# labelled by R using the glimpse() function.

glimpse(songs_artists_acoustic_pop_combined_final)

# R has correctly labelled all of the continuous numerical variables as dbl, the
# explicit and is_pop variables as logical and all of the categorical variables
# as character. Now, to check that there are no duplicate 
# song ids using the duplicated() and sum() functions.

sum(duplicated(songs_artists_acoustic_pop_combined_final$song_id))

# As you can see, there are no duplicate song ids. 
# Now, to check to see if there are any missing values for each variable in the dataset
# using the sum(is.na()) function.

sum(is.na(songs_artists_acoustic_pop_combined_final$song_id))
sum(is.na(songs_artists_acoustic_pop_combined_final$song_name))
sum(is.na(songs_artists_acoustic_pop_combined_final$artist))
sum(is.na(songs_artists_acoustic_pop_combined_final$artist_updated))
sum(is.na(songs_artists_acoustic_pop_combined_final$artist_id))
sum(is.na(songs_artists_acoustic_pop_combined_final$artist_followers))
sum(is.na(songs_artists_acoustic_pop_combined_final$explicit))
sum(is.na(songs_artists_acoustic_pop_combined_final$song_type))
sum(is.na(songs_artists_acoustic_pop_combined_final$artist_type))
sum(is.na(songs_artists_acoustic_pop_combined_final$main_genre))
sum(is.na(songs_artists_acoustic_pop_combined_final$duration_ms))
sum(is.na(songs_artists_acoustic_pop_combined_final$time_signature))
sum(is.na(songs_artists_acoustic_pop_combined_final$acousticness))
sum(is.na(songs_artists_acoustic_pop_combined_final$danceability))
sum(is.na(songs_artists_acoustic_pop_combined_final$energy))
sum(is.na(songs_artists_acoustic_pop_combined_final$instrumentalness))
sum(is.na(songs_artists_acoustic_pop_combined_final$liveness))
sum(is.na(songs_artists_acoustic_pop_combined_final$loudness))
sum(is.na(songs_artists_acoustic_pop_combined_final$speechiness))
sum(is.na(songs_artists_acoustic_pop_combined_final$valence))
sum(is.na(songs_artists_acoustic_pop_combined_final$tempo))
sum(is.na(songs_artists_acoustic_pop_combined_final$year))
sum(is.na(songs_artists_acoustic_pop_combined_final$is_pop))

# Of importance to this particular project, there are 670 missing values for artist_type. These will be dealt with later.
# Now, I'm going to rename the final main dataset to music_dataset.

music_dataset <- songs_artists_acoustic_pop_combined_final

# Now, to export music_dataset to a table
# using write.table() function and the file will be called "music_dataset.csv".

write.table(music_dataset, "music_dataset.csv", sep=",", row.names=FALSE)

# **************************************************************
# PART 2. EXPLORATORY DATA ANALYSIS
# **************************************************************

# **************************************************************
# EDA FOR RESEARCH QUESTION 1
# **************************************************************

# Now, I'm going to create box plots for the acoustic features to see their relationship with the is_pop variable. 
# Now, I'm creating a dataset for the acoustic features from the music_dataset and I'm calling it acoustic_features_dataset.

acoustic_features_dataset <- music_dataset 

# Define a shared theme for all plots to ensure consistency
shared_theme <- theme_minimal(base_size = 18) + 
  theme(plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
  axis.title = element_text(face = "bold"),
  axis.text = element_text(color = "black", face = "bold"),
  panel.grid.minor = element_blank()
  )

# 1. Danceability Box plot
Danceability_graph <- ggplot(acoustic_features_dataset, 
  aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
  y = danceability, 
  fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Relationship Between Song Popularity \nand Danceability", x = "Song Popularity", y = "Danceability") +
  scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
  shared_theme +
  theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Danceability_graph.pdf", plot = Danceability_graph, width = 10, height = 6, dpi = 300)


# 2. Energy Box plot
Energy_graph <- ggplot(acoustic_features_dataset, 
  aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
  y = energy, 
  fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Relationship Between Song Popularity \nand Energy", x = "Song Popularity", y = "Energy") +
  scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
  shared_theme +
  theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Energy_graph.pdf", plot = Energy_graph, width = 10, height = 6, dpi = 300)

# 3. Loudness Box plot
Loudness_graph <- ggplot(acoustic_features_dataset, 
  aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
  y = loudness, 
  fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Relationship Between Song Popularity \nand Loudness", x = "Song Popularity", y = "Loudness (Decibels)") +
  scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
  shared_theme +
  theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Loudness_graph.pdf", plot = Loudness_graph, width = 10, height = 6, dpi = 300)

# 4. Valence Box plot
Valence_graph <- ggplot(acoustic_features_dataset, 
  aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
  y = valence, 
  fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Relationship Between Song Popularity \nand Valence", x = "Song Popularity", y = "Valence") +
  scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
  shared_theme +
  theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Valence_graph.pdf", plot = Valence_graph, width = 10, height = 6, dpi = 300)

# 5. Instrumentalness Box plot
Instrumentalness_graph <- ggplot(acoustic_features_dataset, 
  aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
  y = instrumentalness, 
  fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Relationship Between Song Popularity \nand Instrumentalness", x = "Song Popularity", y = "Instrumentalness") +
  scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
  shared_theme +
  theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Instrumentalness_graph.pdf", plot = Instrumentalness_graph, width = 10, height = 6, dpi = 300)

# 6. Acousticness Box plot
Acousticness_graph <- ggplot(acoustic_features_dataset, 
  aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
  y = acousticness, 
  fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Relationship Between Song Popularity \nand Acousticness", x = "Song Popularity", y = "Acousticness") +
  scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
  shared_theme +
  theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Acousticness_graph.pdf", plot = Acousticness_graph, width = 10, height = 6, dpi = 300)

# 7. Speechiness Box Plot
Speechiness_graph <- ggplot(acoustic_features_dataset, 
  aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
  y = speechiness, 
  fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Relationship Between Song Popularity \nand Speechiness", x = "Song Popularity", y = "Speechiness") +
  scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
  shared_theme +
  theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Speechiness_graph.pdf", plot = Speechiness_graph, width = 10, height = 6, dpi = 300)

# 8. Liveness Box plot
Liveness_graph <- ggplot(acoustic_features_dataset, 
  aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
  y = liveness, 
  fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Relationship Between Song Popularity \nand Liveness", x = "Song Popularity", y = "Liveness") +
  scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
  shared_theme +
  theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Liveness_graph.pdf", plot = Liveness_graph, width = 10, height = 6, dpi = 300)

# 9. Duration Box plot
Duration_graph <- ggplot(acoustic_features_dataset, 
  aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
  y = duration_ms, 
  fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Relationship Between Song Popularity \nand Duration", x = "Song Popularity", y = "Duration (Milliseconds)") +
  scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
  shared_theme +
  theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Duration_graph.pdf", plot = Duration_graph, width = 10, height = 6, dpi = 300)

# 10. Tempo Box plot
Tempo_graph <- ggplot(acoustic_features_dataset, 
  aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
  y = tempo, 
  fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
  geom_boxplot(color = "black") + 
  labs(title = "Relationship Between Song Popularity \nand Tempo", x = "Song Popularity", y = "Tempo (Beats Per Minute)") +
  scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
  shared_theme +
  theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Tempo_graph.pdf", plot = Tempo_graph, width = 10, height = 6, dpi = 300)

# We can see from the boxplots that instrumentalness and speechiness are extremely right skewed.
# acousticness and liveness are highly right skewed. Duration is moderately right skewed. We are going to transform acousticness, liveness, and speechiness using the log(1 + x) or 
# log1p() function. For duration, we will transform it using log(x). For instrumentalness, we can see that due to the fact that most of the values are 0, 
# treating it as a binary categorical variable would be more suitable. Therefore,
# I will change instrumentalness to a binary character variable, where instrumentalness = 0 equals "vocals only" and instrumentalness > 0 equals "has instrumental and vocals".
# I will add a column to the acoustic_features_dataset for each of the transformations using the mutate()
# function. I will also use the ifelse() function to set up the different levels for instrumentalness. 
# I will call the new dataset acoustic_features_transformed_dataset. Then, I will redo the box plots using these transformations. 
# For instrumentalness, I will now create a segmented bar chart. For loudness and energy, they are moderately left skewed, but they are too complicated to transform, so
# I am going to leave them as is. I will leave the remaining acoustic features as is, as they are approximately symmetrical.

acoustic_features_transformed_dataset <- acoustic_features_dataset %>%
  mutate(instrumentalness_transformed = ifelse(instrumentalness > 0, "has instrumental and vocals", "vocals only"), speechiness_transformed = log1p(speechiness), 
  duration_transformed = log(duration_ms), liveness_transformed = log1p(liveness), acousticness_transformed = log1p(acousticness))
acoustic_features_transformed_dataset

view(acoustic_features_transformed_dataset)

# Define a shared theme for all plots to ensure consistency
shared_theme <- theme_minimal(base_size = 18) + 
   theme(
   plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
   axis.title = element_text(face = "bold"),
   axis.text = element_text(color = "black", face = "bold"),
   panel.grid.minor = element_blank(),
   legend.title = element_text(face = "bold")
   )

# 1. Segmented Bar Chart for Instrumentalness
Instrumentalness_transformed_bar_chart <- ggplot(data = acoustic_features_transformed_dataset, 
    aes(x = instrumentalness_transformed, fill = factor(is_pop))) + 
    geom_bar(position = "fill") + 
    geom_text(aes(label = scales::percent(after_stat(count) / ave(after_stat(count), after_stat(x), FUN = sum), accuracy = 1)), # Adding percentage labels to each segment.
    stat = "count",
    position = position_fill(vjust = 0.5), # Centers the text in the middle of each segment
    color = "white", # White color for readability
    size = 5, fontface = "bold") +
    labs(title = "Proportion of Popular/Not Popular Songs \nby Instrumentalness", x = "Instrumentalness", y = "Proportion of Songs", fill = "Song Popularity") + 
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) + # Starts at 0, ends at 100%, increments by 10%. Also, Format Y-axis ticks as percentages
    scale_fill_manual(name = "Song Popularity", values = c("FALSE" = "#D55E00", "TRUE" = "#0072B2"), labels = c("FALSE" = "Not Popular", "TRUE" = "Popular")) +
    shared_theme +
    theme(panel.grid.major = element_blank())

# Saving in highest resolution.
ggsave("Instrumentalness_Transformed.pdf", plot = Instrumentalness_transformed_bar_chart, width = 10, height = 6, dpi = 300)

# 2. Transformed Speechiness Box plot 
Speechiness_transformed <- ggplot(acoustic_features_transformed_dataset, 
    aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), 
    y = speechiness_transformed, 
    fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
    geom_boxplot(color = "black") + 
    labs(title = "Relationship Between Song Popularity \nand log(1 + x) Speechiness", x = "Song Popularity", y = "Speechiness [log(1 + x)]") +
    scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) + 
    shared_theme +
    theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Speechiness_Transformed.pdf", plot = Speechiness_transformed, width = 10, height = 6, dpi = 300)

# 3. Transformed Liveness Box plot 
Liveness_transformed <- ggplot(acoustic_features_transformed_dataset, aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), y = liveness_transformed,
    fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
    geom_boxplot(color = "black") +
    labs(title = "Relationship Between Song Popularity \nand log(1 + x) Liveness", x = "Song Popularity", y = "Liveness [log(1 + x)]") +
    scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) +
    shared_theme +
    theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Liveness_Transformed.pdf", plot = Liveness_transformed, width = 10, height = 6, dpi = 300)

# 4. Transformed Duration Boxplot 
Duration_transformed <- ggplot(acoustic_features_transformed_dataset, aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), y = duration_transformed,
    fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
    geom_boxplot(color = "black") +
    labs(title = "Relationship Between Song Popularity \nand log(x) Duration", x = "Song Popularity", y = "Duration [log(x)]") +
    scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) +
    shared_theme +
    theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Duration_Transformed.pdf", plot = Duration_transformed, width = 10, height = 6, dpi = 300)

# 5. Transformed Acousticness Boxplot 
Acousticness_transformed <- ggplot(acoustic_features_transformed_dataset, aes(x = factor(is_pop, labels = c("Not Popular", "Popular")), y = acousticness_transformed,
    fill = factor(is_pop, labels = c("Not Popular", "Popular")))) + 
    geom_boxplot(color = "black") +
    labs(title = "Relationship Between Song Popularity \nand log(1 + x) Acousticness", x = "Song Popularity", y = "Acousticness [log(1 + x)]") +
    scale_fill_manual(values = c("Not Popular" = "#D55E00", "Popular" = "#0072B2")) +
    shared_theme +
    theme(legend.position = "none")

# Saving in highest resolution.
ggsave("Acousticness_Transformed.pdf", plot = Acousticness_transformed, width = 10, height = 6, dpi = 300)

# The transformations have either significantly improved the distributions or marginally improved them. From the boxplots made above (9 variables), we can observe that for all of the graphs, the median and interquartile ranges are roughly the same for both
# popular and unpopular songs. This overlap and lack of separability between popular and unpopular songs suggests that the above acoustic features are weak individual predictors of is_pop.
# For instrumentalness, we can see on the segmented bar chart that there is no major difference in the proportion of popular songs or not popular songs for "vocals only" compared to the proportion of popular songs or not popular songs for "has instrumental and vocals".
# Therefore, this suggests a lack of separability between the two categories, which suggests that instrumentalness is also a poor individual predictor of is_pop.

# Now, I am going to perform a chi square test of independence, to see if there is a statistically significant relationship between song popularity and instrumentalness.
# I will use the chisq_test() function to calculate this. You need to install the "infer" package in R in order to 
# use the chisq_test() function. Also, I am going to create contigency tables to show the
# number of popular and unpopular songs by instrumentalness. I will use the table() function to achieve this.

acoustic_features_transformed_dataset %>%
  chisq_test(is_pop ~ instrumentalness_transformed)

table(acoustic_features_transformed_dataset$instrumentalness_transformed, acoustic_features_transformed_dataset$is_pop)

# We observe from the chi square test of independence for instrumentalness, that the p-value is .0618, which is greater than alpha = .05. Therefore,
# there is not enough evidence to prove that there is a statistically significant relationship between instrumentalness and song popularity. From the contingency table, we observe
# that songs that have vocals only tend to have the highest number of popular songs. The results from the chi square test support what we found in the 
# segmented bar chart above. Instrumentalness is not a significant individual predictor of song popularity. 

# Also, we should check for collinearity. The best way to check for this is using the variance inflation factor, but it can only be calculated 
# after building the model. First, pairwise correlations will be calculated using a correlation matrix. Below, I use the select() function on the acoustic_features_transformed_dataset to only show the acoustic features
# and I have named the new dataset acoustic_features_for_correlation_matrix. Then, the correlation matrix will be calculated using the cor() function.
# Then, I'm going to create a correlation matrix table using "knitr" and "kableExtra" packages.

acoustic_features_for_correlation_matrix <- acoustic_features_transformed_dataset %>%
  select(danceability, energy, loudness, valence, tempo, duration_transformed, acousticness_transformed, liveness_transformed, speechiness_transformed)
acoustic_features_for_correlation_matrix

acoustic_correlation_matrix <- cor(acoustic_features_for_correlation_matrix)

print("Correlation Matrix for Acoustic Features")
print(round(acoustic_correlation_matrix, 3))

# Convert the matrix to a data frame
cor_df <- as.data.frame(round(acoustic_correlation_matrix, 3))

# Create the table
cor_df %>%
  kable(caption = "Pearson Correlation Matrix for Acoustic Features") %>%
  kable_classic(full_width = F, html_font = "Times New Roman")

# We observe from the correlation matrix that most of the variables have pairwise correlations of less than 0.7 apart from energy and loudness which have a pairwise correlation of 0.72. 
# Due to the high collinearity between loudness and energy, we need to exclude one of them from the stepwise logistic regression. I am going to choose to exclude loudness. 

# Now, I am going to run a stepwise logistic regression in both directions, to see which variables should go into my logistic regression model.
# First, I am going to make a new column in acoustic_features_transformed_dataset called Binary_Is_Pop and it converts the TRUE/FALSE values for 
# is_pop into 0/1 values.

acoustic_features_transformed_dataset$Binary_Is_Pop <- as.numeric(as.logical(acoustic_features_transformed_dataset$is_pop)) # Ensures TRUE becomes 1 and FALSE becomes 0 regardless of current format.


view(acoustic_features_transformed_dataset)

# Now, I am going to create my stepwise logistic regression. Family = binomial because it is a logistic regression and direction = both because I want both forward and backward
# stepwise logistic regression performed on the acoustic features.

null_model_acoustic <- glm(Binary_Is_Pop ~ 1, data = acoustic_features_transformed_dataset, family = "binomial")

full_model_acoustic <- glm(Binary_Is_Pop ~ danceability + energy + valence + tempo + duration_transformed + acousticness_transformed + liveness_transformed +
                    speechiness_transformed + instrumentalness_transformed, data = acoustic_features_transformed_dataset, family = "binomial")

step_model_acoustic <- step(null_model_acoustic, scope = list(lower = null_model_acoustic, upper = full_model_acoustic), direction = "both")

# From the stepwise logistic regression above, we will build our logistic regression classification model and our random forest classification model using valence, duration_transformed, danceability, acousticness_transformed,
# speechiness_transformed, instrumentalness_transformed, and energy. The stepwise logistic regression eliminated liveness_transformed and tempo. We didn't add in loudness because of the high collinearity with energy.

# **************************************************************
# EDA FOR RESEARCH QUESTION 2
# **************************************************************

#Before performing an exploratory analysis, I am going to use the filter() and is.na() functions
# to get rid of the missing values in the artist_type variable. As mentioned previously, there are 670 missing values for artist_type.
# I will call the new dataset metadata_features_dataset.

metadata_features_dataset <- music_dataset %>%
  filter(!is.na(artist_type))
metadata_features_dataset

# You can see that 670 missing values were removed.
# Now, I'm going to create a segmented bar chart to see the relationship between is_pop and artist_type. 

# Define a shared theme for all plots to ensure consistency
shared_theme <- theme_minimal(base_size = 18) + 
    theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black", face = "bold"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold"))

artist_type_bar_chart <- ggplot(data = metadata_features_dataset, 
    aes(x = artist_type, fill = factor(is_pop))) + 
    geom_bar(position = "fill") + #colour by is_pop
    geom_text(aes(label = scales::percent(after_stat(count) / ave(after_stat(count), after_stat(x), FUN = sum), accuracy = 1)), # Adding percentage labels to each segment.
    stat = "count",
    position = position_fill(vjust = 0.5), # Centers the text in the middle of each segment
    color = "white", # White color for readability
    size = 5, fontface = "bold") +
    labs(title = "Proportion of Popular/Not Popular Songs by Artist Type",
    x = "Artist Type", y = "Proportion of Songs", fill = "Song Popularity") + 
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) + # Starts at 0, ends at 100%, increments by 10%. Also, Format Y-axis ticks as percentages
    scale_fill_manual(name = "Song Popularity", values = c("FALSE" = "#D55E00", "TRUE" = "#0072B2"), labels = c("FALSE" = "Not Popular", "TRUE" = "Popular")) + 
    shared_theme

# Saving in highest resolution.
ggsave("Artist_Type.pdf", plot = artist_type_bar_chart, width = 10, height = 6, dpi = 300)
    
# As we observe from the bar chart, the proportion of popular songs for bands, DJs, duos, singers and rappers is roughly the same give or take +- 5%. This suggests that artist type is a weak predictor of
# song popularity. 

# Now, I am going to perform a chi square test of independence, to show if there is a statistically significant relationship between
# artist type and song popularity. I will use the chisq_test() function to calculate this. You need to install the "infer" package in R in order to 
# use the chisq_test() function. Also, I am going to create a contingency table to show the
#number of popular and unpopular songs by artist type. I will use the table() function to achieve this.

metadata_features_dataset %>%
  chisq_test(is_pop ~ artist_type)

table(metadata_features_dataset$artist_type, metadata_features_dataset$is_pop)

# We observe from the chi square test of independence, that the p-value is .000130, which is less than alpha = .05. Therefore,
# artist type has a statistically significant relationship with song popularity. However, even though the chi square test suggests that there is a statistically significant relationship
# between artist type and song popularity, the information gathered from the bar chart above suggests that artist type is still a weak predictor of song popularity. 
# From the contingency table, we observe that singers tend to have the highest number of popular songs, followed by bands.

# Now, I'm going to create a segmented bar chart to see the relationship between is_pop and song_type.

song_type_bar_chart <- ggplot(data = metadata_features_dataset, 
  aes(x = song_type, fill = factor(is_pop))) + 
  geom_bar(position = "fill") + #colour by is_pop
  geom_text(aes(label = scales::percent(after_stat(count) / ave(after_stat(count), after_stat(x), FUN = sum), accuracy = 1)), # Adding percentage labels to each segment.
  stat = "count",
  position = position_fill(vjust = 0.5), # Centers the text in the middle of each segment
  color = "white", # White color for readability
  size = 5, fontface = "bold") +
  labs(title = "Proportion of Popular/ Not Popular Songs by Song Type", x = "Song Type", y = "Proportion of Songs", fill = "Song Popularity") + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) + # Starts at 0, ends at 100%, increments by 10%. Also, Format Y-axis ticks as percentages
  scale_fill_manual(name = "Song Popularity", values = c("FALSE" = "#D55E00", "TRUE" = "#0072B2"), labels = c("FALSE" = "Not Popular", "TRUE" = "Popular")) + 
  shared_theme

# Saving in highest resolution.
ggsave("song_type.pdf", plot = song_type_bar_chart, width = 10, height = 6, dpi = 300)

# As we observe from the bar chart, the proportion of popular and not popular songs for both collaboration and solo songs is roughly the same +/ 5%. 
# The lack of separability between unpopular and popular songs between the different song types suggests that song type is also a weak predictor of song popularity. 

# Now, I am going to perform a chi square test of independence, to show if there is statistically significant relationship between
# song type and song popularity. I will use the chisq_test() function to calculate this. Also, I am going to create a contingency table to show the
# number of popular and unpopular songs by song type. I will use the table() function to achieve this.

metadata_features_dataset %>%
  chisq_test(is_pop ~ song_type)

table(metadata_features_dataset$song_type, metadata_features_dataset$is_pop)

# We observe from the chi square test of independence, that the p-value is .0387, which is less than alpha = .05. Therefore,
# song type has a statistically significant relationship with song popularity. However, even though the chi square test suggests that there is a statistically significant relationship
# between song type and song popularity, the information gathered from the bar chart above suggests that song type is still a weak predictor of song popularity. 
# From the contingency table, we observe that solo songs tend to have the highest number of popular songs, followed by collaborations.

# Finally, I am going to do a segmented bar chart for the explicit variable to see the relationship between 
# song popularity and song explicitness.

explicitness_bar_chart <- ggplot(data = metadata_features_dataset, aes(x = explicit, fill = factor(is_pop))) + 
  geom_bar(position = "fill") + #colour by is_pop
  geom_text(aes(label = scales::percent(after_stat(count) / ave(after_stat(count), after_stat(x), FUN = sum), accuracy = 1)), # Adding percentage labels to each segment.
  stat = "count",
  position = position_fill(vjust = 0.5), # Centers the text in the middle of each segment
  color = "white", # White color for readability
  size = 5, fontface = "bold") +
  labs(title = "Proportion of Popular/ Not Popular Songs by Explicitness", x = "Explicitness", y = "Proportion of Songs", fill = "Song Popularity") + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) + # Starts at 0, ends at 100%, increments by 10%. Also, Format Y-axis ticks as percentages
  scale_x_discrete(labels = c("FALSE" = "Clean", "TRUE" = "Explicit")) +
  scale_fill_manual(name = "Song Popularity", values = c("FALSE" = "#D55E00", "TRUE" = "#0072B2"), labels = c("FALSE" = "Not Popular", "TRUE" = "Popular")) + 
  shared_theme

# Saving in highest resolution.
ggsave("explicitness.pdf", plot = explicitness_bar_chart, width = 10, height = 6, dpi = 300)

# As we observe from the bar chart, the proportion of popular and not popular songs for clean and explicit songs is roughly the same +- 5%. Once again,
# this suggests song explicitness is another weak predictor of song popularity.

# Now, I am going to perform a chi square test of independence, to show if there is statistically significant relationship between
# song explicitness and song popularity. I will use the chisq_test() function to calculate this. Also, I am going to create a contingency table to show the
# number of popular and unpopular songs by explicitness. I will use the table() function to achieve this.

metadata_features_dataset %>%
  chisq_test(is_pop ~ explicit)

table(metadata_features_dataset$explicit, metadata_features_dataset$is_pop)

# We observe from the chi square test of independence, that the p-value is .0234, which is less than alpha = .05. Therefore,
# explicitness has a statistically significant relationship with song popularity. However, even though the chi square test suggests that there is a statistically significant relationship
# between explicitness and song popularity, the information gathered from the bar chart above suggests that explicitness is still a weak predictor of song popularity.
# From the contingency table, we observe that explicit songs tend to be the most popular songs.

# Now, I am going to run a stepwise logistic regression in both directions, to see which variables should go into my logistic regression model.
# First, I am going to make a new column in metadata_features_dataset called Binary_Is_Pop and it converts the TRUE/FALSE values for 
# is_pop into 0/1 values.

metadata_features_dataset$Binary_Is_Pop <- as.numeric(as.logical(metadata_features_dataset$is_pop)) # Ensures TRUE becomes 1 and FALSE becomes 0 regardless of current format.

view(metadata_features_dataset)

# Now, I am going to create my stepwise logistic regression. Family = binomial because it is a logistic regression and direction = both because I want both forward and backward
# stepwise logistic regression performed on the extrinsic features.

null_model_metadata <- glm(Binary_Is_Pop ~ 1, data = metadata_features_dataset, family = "binomial")

full_model_metadata <- glm(Binary_Is_Pop ~ explicit + artist_type + song_type, data = metadata_features_dataset, family = "binomial")

step_model_metadata <- step(null_model_metadata, scope = list(lower = null_model_metadata, upper = full_model_metadata), direction = "both")

# From the stepwise logistic regression above, we will build our logistic regression model and our random forest model using just song_type and artist_type, as 
# the stepwise logistic regression model excluded explicit.

# **************************************************************
# PART 3. MODEL PRODUCTION
# **************************************************************

# **************************************************************
# MULTIPLE REGRESSION FOR RESEARCH QUESTION 1
# **************************************************************

# I am now going to conduct a logistic regression classification model on the 7 acoustic features identified in the stepwise logistic regression above. 
# I will be using the acoustic_features_transformed_dataset. First, I am going to set the seed for reproducibility.
# Also, I am shuffling the rows in the dataset in random order.

set.seed(123)

acoustic_transformed <- acoustic_features_transformed_dataset[sample(1:nrow(acoustic_features_transformed_dataset)),]

# Now, I am going to create my train and test set. I will be putting 70% of the data into the train dataset. The remaining 30% of 
# acoustic_features_transformed_dataset will go into my test dataset.

train_size = 0.7 
acoustic_transformed_train <- acoustic_transformed[1:(train_size*nrow(acoustic_transformed)),]
acoustic_transformed_test <- acoustic_transformed[(nrow(acoustic_transformed_train)+1):nrow(acoustic_transformed),]

# Before we run the logistic regression classification model on the train set, I want to change the reference level
# for the transformed instrumentalness variable in my train dataset. I will first change it to a factor 
# using factor() function. Then, I will use the relevel() function to achieve this. The new reference category for the transformed instrumentalness variable will be "has instrumental and vocals". 

acoustic_transformed_train$instrumentalness_transformed <- factor(acoustic_transformed_train$instrumentalness_transformed)
acoustic_transformed_train$instrumentalness_transformed <- relevel(acoustic_transformed_train$instrumentalness_transformed, ref = "has instrumental and vocals")

# Now, I will build the logistic regression model. I will use the glm() function to do this. family = "binomial" for a logistic regression.
# I will call the logistic regression model acoustic_regression_model.

acoustic_regression_model <- glm(Binary_Is_Pop ~ danceability + valence + duration_transformed + energy + acousticness_transformed + instrumentalness_transformed + speechiness_transformed, data = acoustic_transformed_train, family = "binomial")

# I'm going to first check the variance inflation factor to make sure there is no problematic collinearity. First, install the car package to do this.
# Then, use the vif() function to calculate the variance inflation factor.

vif_acoustic <- vif(acoustic_regression_model)

print(vif_acoustic)

# We can see from above that the VIFs for the acoustic features are all < 5, which means no problematic collinearity. 

# Running the summary() function below on the logistic regression will give the coefficients as log odds. We want the odds ratios,
# as they are easier to interpret. To get the odds ratios, install the "gtsummary" package and then use tbl_regression() function to get the odds ratios.
# exponentiate = true because we want the odds ratios.

summary(acoustic_regression_model)

acoustic_regression_model %>%
  tbl_regression(exponentiate = TRUE)

# According to the odds ratios, danceability, valence, and duration had positive associations with song popularity. Compared to songs with instrumental and vocal parts, songs that had vocals had 17% increased odds of being popular.
# acousticness and speechiness had negative associations with song popularity. Energy had no significant statistical relationship with song popularity.
# For a more detailed explanation of the odds ratios, please see my report. Now, I will calculate the Nagelkerke R2 for the model. 
# First, install DescTools package and then use PseudoR2() function.

PseudoR2(acoustic_regression_model, which = "Nagelkerke")

# the overall model fit was low (Nagelkerke R2 = 2.7%). This indicates that while features like danceability, valence, duration, instrumentalness, acousticness, and speechiness had statistically significant relationships with song popularity and they are the strongest audio predictive features when it comes
# to song popularity, they account for only a small fraction of the total variation in song popularity (approx. 3%), suggesting that external factors play a dominant role in predicting song popularity. The fact that variables outside our model are more important predictors of song popularity matches
# what was shown in the exploratory data analysis section.

# Now, I am going to evaluate how well the model predicts song popularity on new data (test dataset). First, I want to change the reference level 
# for the transformed instrumentalness variable in my test dataset. I will first change it to a factor 
# using factor() function. Then, I will use the relevel() function to achieve this. The new reference category for the transformed instrumentalness variable will be "has instrumental and vocals".  
# To generate predictions, I am using the predict() function. type = "response" because we want to get the predictions. 

acoustic_transformed_test$instrumentalness_transformed <- factor(acoustic_transformed_test$instrumentalness_transformed)
acoustic_transformed_test$instrumentalness_transformed <- relevel(acoustic_transformed_test$instrumentalness_transformed, ref = "has instrumental and vocals")

acoustic_regression_model_predict <- predict(acoustic_regression_model, acoustic_transformed_test, type = "response")

#Now, I am going to create a dataframe, so that I can create a confusion matrix. 

acoustic_regression_df <- data.frame(
  truth = factor(acoustic_transformed_test$Binary_Is_Pop, levels = c(0, 1), labels = c("FALSE", "TRUE")), # Map actual test values (0/1) to factor levels (FALSE/TRUE) for yardstick compatibility.
  estimate = factor(ifelse(acoustic_regression_model_predict > 0.5, "TRUE", "FALSE"), levels = c("FALSE", "TRUE")), #Apply 0.5 threshold to convert probabilities into binary class predictions.
  prob = acoustic_regression_model_predict # Store raw probabilities for ROC curve and AUC calculation.
)

# Now, I am going to make a confusion matrix and calculate performance metrics. To do this, first install "yardstick" package.
# Create the confusion matrix using conf_mat() function. To calculate performance metrics, use summary() function and select your confusion matrix and
# event_level = "second" because the outcome we are interested in is when is_pop is "TRUE".

confusion_regression_acoustic <- conf_mat(acoustic_regression_df, truth = truth, estimate = estimate)

confusion_regression_acoustic

summary(confusion_regression_acoustic, event_level = "second")

# Accuracy was 65%. However, this is not a good performance metric to go by, as our predictions are imbalanced. Our model on the testing data accurately predicted most of the unpopular songs (1119/1130),
# but it missed most of the popular songs. Our model only predicted (14/608) as being popular. Also, our test dataset had 1130 not popular songs versus 608 popular songs, which suggests the testing dataset is also imbalanced.
# Therefore, we need to calculate AUC, as this is a more accurate performance metric to go by for an imbalanced dataset and imbalanced predictions. Finally, we are going to calculate AUC. First, install "pROC" package.
# Then, use roc() and auc() functions to calculate AUC. 

acoustic_regression_roc <- roc(response = acoustic_regression_df$truth, predictor = acoustic_regression_df$prob)

auc_value_acoustic_regression <- auc(acoustic_regression_roc)

print(auc_value_acoustic_regression)

# Here, the AUC is approx. .58, which means that our predictive model is performing only 8% better than random chance. Therefore, this reinforces the findings made previously on our train dataset,
# that these acoustic features are weak predictors of song popularity.

# **************************************************************
# RANDOM FOREST FOR RESEARCH QUESTION 1
# **************************************************************

# First, we are changing our is_pop variable to a factor, as we need the dependent variable to be a factor for a classification random forest.
# we use as.factor() to do this. Then, download the ranger package and we will use ranger() function to create a classification random forest on our train dataset.
# probability = TRUE because we need probabilities to calculate OOB AUC. respect.unordered.factors = "order" because we want our random forest to order the intstrumentalness_transformed
# variable levels by their effectiveness at predicting song popularity. importance = "permutation" because we want to calculate the importance scores for each acoustic feature.
# Note, we are only including valence, duration_transformed, danceability, acousticness_transformed,
# speechiness_transformed, instrumentalness_transformed, and energy in our random forest classification model.

set.seed(123)

acoustic_transformed_train$is_pop <- as.factor(acoustic_transformed_train$is_pop)

random_forest_acoustic <- ranger(is_pop ~ danceability + valence + duration_transformed + energy + acousticness_transformed + instrumentalness_transformed + speechiness_transformed, data = acoustic_transformed_train, 
                          respect.unordered.factors = "order", probability = TRUE, importance = "permutation")

# After the random forest is created on our training set, I'm going to graph the importance scores for the acoustic features.

# I'm extracting the importance scores first from the random forest. variable.importance is the slot in the ranger() package where the importance scores are kept.
feature_importance <- random_forest_acoustic$variable.importance

# Converts the raw importance values into a data frame
acoustic_importance_df <- data.frame(Feature = names(feature_importance), # Extracts the names of the features (e.g., "energy", "valence") as the first column.
  Importance = as.numeric(feature_importance)) %>% # Converts the importance numbers into a numeric column for plotting.
  mutate(ColorGroup = ifelse(Importance >= 0, "Positive", "Negative")) # Adds a new column to tag each score as "Positive" or "Negative" for color-coding.

# Create the bar chart
ggplot(acoustic_importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = ColorGroup)) + # reorder() ensures the bars are sorted from lowest to highest importance on the graph, rather than just being in alphabetical order.
  geom_bar(stat = "identity") + # Specifically tells R to use the actual numbers in the Importance column for the height of the bars, rather than trying to count how many times a feature appears.
  scale_fill_manual(values = c("Positive" = "#0072B2", "Negative" = "#D55E00")) +
  geom_text(aes(label = format(round(Importance, 4), scientific = FALSE)), # Adds the numerical scores as text labels on the bars
  hjust = ifelse(acoustic_importance_df$Importance < 0, 1.3, -0.3), # If the value is negative, it nudges the text left on the graph; if positive, it nudges it right on the graph.
  size = 5, fontface = "bold", color = "black") + # Sets text size, color and makes it bold for readability
  coord_flip() + # Flips the axes to make the bars horizontal (easier to read long feature names).
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.3))) + # Adds 30% extra space to the left and right edges so the labels aren't cut off
  labs(title = "Premutation Feature Importance Scores of \nAcoustic Features for Song Popularity", x = "Acoustic Feature", y = "Premutation Importance Score (Mean Decrease in Accuracy)") +
  theme_minimal(base_size = 18) + # Applies a clean white background and sets the general font size to 18 for high visibility
  theme(
  plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
  axis.title = element_text(face = "bold", hjust = 0.5),
  axis.text = element_text(color = "black", face = "bold", hjust = 0.5),
  legend.position = "none" # Removes the legend
  )

# Based on the importance score graph above, energy is the strongest predictor of song popularity in this model, followed by valence, danceability, and then duration_transformed.
# However, their scores fall into the range of .001 to .01 suggesting they have low effect on song popularity in general. 
# Overall, this confirms the results that we have seen in the logistic regression and in the exploratory data analysis.

# Now, we are going to calculate the Brier Score.

brier_score_acoustic <- random_forest_acoustic$prediction.error

brier_score_acoustic

# The brier score for the random forest model on our train dataset is approx. .23. This is only slightly better than random chance (brier score .25)

# Now, we are going to calculate our OOB AUC using auc() and roc() functions. 

oob_auc_acoustic <- auc(roc(acoustic_transformed_train$is_pop, random_forest_acoustic$predictions[, 2])) # Compares actual values of is_pop to the OOB predicted probabilities (column 2 = 'TRUE').

oob_auc_acoustic

# Our OOB AUC is .55. This means that our random forest model on our train dataset is only slightly better than random chance (OOB AUC of .5). 

# Now, we are going to see how well our model performs on unseen data (test dataset). First, we are changing our is_pop variable to a factor, as we need our dependent variable to be a factor for a classification random forest.
# we use as.factor() to do this. Then, we are calculating our predictions using predict() function.

acoustic_transformed_test$is_pop <- as.factor(acoustic_transformed_test$is_pop)

predictive_acoustic_random_forest <- predict(random_forest_acoustic, data = acoustic_transformed_test)$predictions

# Here, we are creating a dataframe, so that we can create a confusion matrix.

acoustic_random_forest_df <- data.frame(truth = acoustic_transformed_test$is_pop, prob  = predictive_acoustic_random_forest[, 2], #The 'truth' column contains the actual popularity status from the test set. For prob = , we are extracting the probability of the 'TRUE' class (column 2) for AUC calculation.
           estimate = factor(ifelse(predictive_acoustic_random_forest[, 2] > 0.5, TRUE, FALSE), levels = c("FALSE", "TRUE"))) #Convert probabilities into binary labels (TRUE/FALSE) using a 0.5 threshold for the Confusion Matrix, ensuring factor levels match the 'truth' column.

# Then, we make our confusion matrix

confusion_acoustic_random_forest <- conf_mat(acoustic_random_forest_df, truth = truth, estimate = estimate)

print(confusion_acoustic_random_forest)

# Then, we run our performance metrics. event_level = "second" because we are interested in when is_pop is "TRUE".

summary(confusion_acoustic_random_forest, event_level = "second")

# # Accuracy was 63%. However, this is not a good performance metric to go by, as our predictions are imbalanced. Our model on the testing data accurately predicted most of the unpopular songs (1025/1130),
# but it missed most of the popular songs. Our model only accurately predicted (64/608) as being popular. Also, our test dataset had 1130 not popular songs versus 608 popular songs, which suggests the testing dataset is also imbalanced.
# Therefore, we need to calculate AUC, as this is a more accurate performance metric to go by for an imbalanced dataset and imbalanced predictions. 

# Now, we will calculate the AUC on our test dataset using roc_auc() function. 

roc_auc(acoustic_random_forest_df, truth = truth, prob, event_level = "second")

# Our AUC was .56. Our logistic regression performed slightly better at 0.58 AUC. Our random forest model on the test dataset performed only slightly better at predicting song popularity than random chance (AUC = .5).
# Again, this confirms what we have seen in the logistic regression model and the exploratory data analysis. Acoustic features are a weak predictor of
# song popularity and this also confirms that external factors play a dominant role in predicting song popularity.

# **************************************************************
# MULTIPLE REGRESSION FOR RESEARCH QUESTION 2
# **************************************************************

# Now, I am now going to conduct a multiple logistic regression on song_type and artist_type.
# First, I will use set.seed() for reproducibility. Then, I am shuffling the rows in the dataset in random order.

set.seed(123)

metadata_transformed <- metadata_features_dataset[sample(1:nrow(metadata_features_dataset)),]

# Now, I am going to create my train and test set. I will be putting 70% of the data (and it is being randomly selected) into the train dataset. The remaining 30% of 
# metadata_features_dataset will go into my test dataset. 

train_size = 0.7 
metadata_features_train <- metadata_transformed[1:(train_size*nrow(metadata_transformed)),]
metadata_features_test <- metadata_transformed[(nrow(metadata_features_train)+1):nrow(metadata_transformed),]

# Before we run the multiple logistic regression model on the train set, I want to change the reference level
# for the song_type and artist_type variables. I will first change them to factors 
# using factor() function. Then, I will use the relevel() function to achieve this. The new reference category for song_type will be "Solo". 
# the new reference category for artist_type will be "band". 

metadata_features_train$song_type <- factor(metadata_features_train$song_type)
metadata_features_train$song_type <- relevel(metadata_features_train$song_type, ref = "Solo")

metadata_features_train$artist_type <- factor(metadata_features_train$artist_type)
metadata_features_train$artist_type <- relevel(metadata_features_train$artist_type, ref = "band")

# Now, I will build the multiple logistic regression model. I will use the glm() function to do this. family = "binomial" for a logistic regression.
# I will call the logistic regression model metadata_regression_model.

metadata_regression_model <- glm(Binary_Is_Pop ~ song_type + artist_type, data = metadata_features_train, family = "binomial")

# I'm going to first check the variance inflation factor to make sure there is no problematic collinearity. Use the vif() function to calculate the variance inflation factor.

vif_metadata <- vif(metadata_regression_model)

print(vif_metadata)

# We can see from above that the VIFs for the extrinsic features are all <5, which means no problematic collinearity. 

# Running the summary() function below on the multiple logistic regression will give the coefficients as log odds. We want the odds ratios,
# as they are easier to interpret. To get the odds ratios, use tbl_regression() function to get the odds ratios.
# exponentiate = true because we want the odds ratios.

summary(metadata_regression_model)

metadata_regression_model %>%
  tbl_regression(exponentiate = TRUE)

# According to the odds ratios, compared to bands, singers had a 31% increased odds of making popular songs.
# Compared to bands, duos had a 19% decreased odds of making popular songs, although this was not significant. Compared to bands, rappers had a 13% decreased odds of their songs being popular,
# although this was also not significant. Finally, compared to bands, a DJ had a 1% increased odd of making popular songs, although this was also not significant. Compared to solo songs, collaborations had a 25% increased odds of becoming popular.
# For a more detailed explanation of the odds ratios, please see my report. Now, I will calculate the Nagelkerke R2 for the model. 
# Use PseudoR2() function to do this.

PseudoR2(metadata_regression_model, which = "Nagelkerke")

# the overall model fit was low (Nagelkerke R2 = .9%). This indicates that while features like song type and artist type had statistically significant associations with song popularity, 
# they account for only a small fraction of the total variation in song popularity (approx. 1%), suggesting that external factors play a dominant role in predicting song popularity. The fact that variables outside our model are more important predictors of song popularity matches
# what was shown in the exploratory data analysis section.

# Now, I am going to evaluate how well the model predicts song popularity on new data (test dataset). First, I want to change the reference level
# for the song_type and artist_type variables in the test dataset. I will first change them to factors using factor() function. 
# Then, I will use the relevel() function to achieve this. The new reference category for song_type will be "Solo". 
# the new reference category for artist_type will be "band". Then, to generate predictions, I am using the predict()
# function. type = "response" because we want to get the predictions. 

metadata_features_test$song_type <- factor(metadata_features_test$song_type)
metadata_features_test$song_type <- relevel(metadata_features_test$song_type, ref = "Solo")

metadata_features_test$artist_type <- factor(metadata_features_test$artist_type)
metadata_features_test$artist_type <- relevel(metadata_features_test$artist_type, ref = "band")

metadata_regression_model_predict <- predict(metadata_regression_model, metadata_features_test, type = "response")

#Now, I am going to create a dataframe, so that I can create a confusion matrix. 

metadata_regression_df <- data.frame(
  truth = factor(metadata_features_test$Binary_Is_Pop, levels = c(0, 1), labels = c("FALSE", "TRUE")), # Map actual test values (0/1) to factor levels (FALSE/TRUE) for yardstick compatibility.
  estimate = factor(ifelse(metadata_regression_model_predict > 0.5, "TRUE", "FALSE"), levels = c("FALSE", "TRUE")), #Apply 0.5 threshold to convert probabilities into binary class predictions.
  prob = metadata_regression_model_predict # Store raw probabilities for ROC curve and AUC calculation.
  )

# Now, I am going to make a confusion matrix and calculate performance metrics. 
# Create the confusion matrix using conf_mat() function. To calculate performance metrics, use summary() function and select your confusion matrix and
# event_level = "second" because the outcome we are interested in is when is_pop is "TRUE".

confusion_regression_metadata <- conf_mat(metadata_regression_df, truth = truth, estimate = estimate)

confusion_regression_metadata

summary(confusion_regression_metadata, event_level = "second")

# Accuracy was 63%. However, this is not a good performance metric to go by, as our predictions are extremely imbalanced. Our model on the testing data accurately predicted all of the unpopular songs (972/972),
# but it missed ALL of the popular songs (0/565). Also, our test dataset had 972 not popular songs versus 565 popular songs, which suggests the testing dataset is also imbalanced.
# Therefore, we need to calculate AUC, as this is a more accurate performance metric to go by for an imbalanced dataset and imbalanced predictions. 
# Use roc() and auc() functions to calculate AUC. 

metadata_regression_roc <- roc(response = metadata_regression_df$truth, predictor = metadata_regression_df$prob)

auc_value_metadata_regression <- auc(metadata_regression_roc)

print(auc_value_metadata_regression)

# Here, the AUC is approx. .53, which means that our predictive model is almost no better at predicting song popularity than random chance. Therefore, this reinforces the findings made previously on our train dataset,
# that these metadata features are very weak predictors of song popularity.

# **************************************************************
# RANDOM FOREST FOR RESEARCH QUESTION 2
# **************************************************************

# First, we are changing our is_pop variable to a factor, as we need the dependent variable to be a factor for a classification random forest.
# we use as.factor() to do this. Then, we will use ranger() function to create a classification random forest on our train dataset.
# probability = TRUE because we need probabilities to calculate OOB AUC. respect.unordered.factors = "order" because we want our random forest to order the categorical variables
# by their effectiveness at predicting song popularity. importance = "permutation" because we want to calculate the importance scores for each metadata feature.
# We are including artist_type and song_type in our random forest model.

set.seed(123)

metadata_features_train$is_pop <- as.factor(metadata_features_train$is_pop)

random_forest_metadata <- ranger(is_pop ~ song_type + artist_type, data = metadata_features_train, respect.unordered.factors = "order", probability = TRUE, importance = "permutation")

# After the random forest is created on our training set, I'm going to graph the importance scores for the metadata features.

# I'm extracting the importance scores first from the random forest. variable.importance is the slot in the ranger() package where the importance scores are kept.
feature_importance_ <- random_forest_metadata$variable.importance

# Converts the raw importance values into a data frame
metadata_importance_df <- data.frame(Feature = names(feature_importance_), # Extracts the names of the features (e.g., "artist_type", "song_type") as the first column.
                          Importance = as.numeric(feature_importance_)) %>% # Converts the importance numbers into a numeric column for plotting.
                          mutate(ColorGroup = ifelse(Importance >= 0, "Positive", "Negative")) # Adds a new column to tag each score as "Positive" or "Negative" for color-coding.

# Create the bar chart
ggplot(metadata_importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = ColorGroup)) + # reorder() ensures the bars are sorted from lowest to highest importance on the graph, rather than just being in alphabetical order.
  geom_bar(stat = "identity") + # Specifically tells R to use the actual numbers in the Importance column for the height of the bars, rather than trying to count how many times a feature appears.
  scale_fill_manual(values = c("Positive" = "#0072B2", "Negative" = "#D55E00")) +
  geom_text(aes(label = format(round(Importance, 4), scientific = FALSE)), # Adds the numerical scores as text labels on the bars
  hjust = ifelse(metadata_importance_df$Importance < 0, 1.3, -0.3), # If the value is negative, it nudges the text left on the graph; if positive, it nudges it right on the graph.
  size = 5, fontface = "bold", color = "black") + # Sets text size, color and makes it bold for readability
  coord_flip() + # Flips the axes to make the bars horizontal (easier to read long feature names).
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.3))) + # Adds 30% extra space to the left and right edges so the labels aren't cut off
  labs(title = "Premutation Feature Importance Scores of \nMetadata Features for Song Popularity", x = "Metadata Feature", y = "Premutation Importance Score (Mean Decrease in Accuracy)") +
  theme_minimal(base_size = 18) + # Applies a clean white background and sets the general font size to 18 for high visibility
  theme(
  plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
  axis.title = element_text(face = "bold", hjust = 0.5),
  axis.text = element_text(color = "black", face = "bold"),
  legend.position = "none" # Removes the legend
  )

# Based on the importance score graph above, artist type is the strongest predictor of song popularity in this model.
# However, the score for artist type falls into the range of .001 to .01 suggesting artist type has a low effect on song popularity in general. Song type has almost no
# effect on song popularity.
# Overall, this confirms the results that we have seen in the logistic regression and in the exploratory data analysis.


# Now, we are going to calculate the Brier Score.

brier_score_metadata <- random_forest_metadata$prediction.error

brier_score_metadata

# The brier score for the random forest model on our train dataset is approx. .23. This is only slightly better than random chance (brier score .25)
# This matches our results found in our logistic regression.

# Now, we are going to calculate our OOB AUC using auc() and roc() functions. 

oob_auc_metadata <- auc(roc(metadata_features_train$is_pop, random_forest_metadata$predictions[, 2])) # Compares actual values of is_pop to the OOB predicted probabilities (column 2 = 'TRUE').

oob_auc_metadata

# Our OOB AUC is .50. This means that our random forest model on our train dataset performs the same as random chance (OOB AUC of .5). This confirms what
# we have found in our logistic regression model.

# Now, we are going to see how well our model performs on unseen data (test dataset). First, we are changing our is_pop variable to a factor, as we need our dependent variable to be a factor for a classification random forest.
# we use as.factor() to do this. Then, we are calculating our predictions using predict() function.

metadata_features_test$is_pop <- as.factor(metadata_features_test$is_pop)

predictive_metadata_random_forest <- predict(random_forest_metadata, data = metadata_features_test)$predictions

# Here, we are creating a dataframe, so that we can create a confusion matrix.

metadata_random_forest_df <- data.frame(truth = metadata_features_test$is_pop, prob  = predictive_metadata_random_forest[, 2], #The 'truth' column contains the actual popularity status from the test set. For prob = , we are extracting the probability of the 'TRUE' class (column 2) for AUC calculation.
                                        estimate = factor(ifelse(predictive_metadata_random_forest[, 2] > 0.5, TRUE, FALSE), levels = c("FALSE", "TRUE"))) #Convert probabilities into binary labels (TRUE/FALSE) using a 0.5 threshold for the Confusion Matrix, ensuring factor levels match the 'truth' column.

# Then, we make our confusion matrix

confusion_metadata_random_forest <- conf_mat(metadata_random_forest_df, truth = truth, estimate = estimate)

print(confusion_metadata_random_forest)

# Then, we run our performance metrics. event_level = "second" because we are interested in when is_pop is "TRUE".

summary(confusion_metadata_random_forest, event_level = "second")

# Accuracy was 63%. However, this is not a good performance metric to go by, as our predictions are extremely imbalanced. Our model on the testing data accurately predicted all of the unpopular songs (972/972),
# but it missed ALL of the popular songs (0/565). Also, our test dataset had 972 not popular songs versus 565 popular songs, which suggests the testing dataset is also imbalanced.
# Therefore, we need to calculate AUC, as this is a more accurate performance metric to go by for an imbalanced dataset and imbalanced predictions. 
# Use roc_auc() function to calculate AUC. 

# Now, we will calculate the AUC on our test dataset using roc_auc() function. 

roc_auc(metadata_random_forest_df, truth = truth, prob, event_level = "second")

# Our AUC was .53. Our model on the test dataset performed only slightly better at predicting song popularity than random chance (AUC = .50).
# Again, this confirms what we have seen in the logistic regression model and the exploratory data analysis. metadata features are very weak predictors of
# song popularity and this also confirms that external factors play a dominant role in predicting song popularity.

