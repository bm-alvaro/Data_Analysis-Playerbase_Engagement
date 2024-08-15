library(dplyr)
library(readxl)
library(ggplot2)

##### Data preprocessing ####
# Rounds data
raw_data_dir <- "raw_data"
round_files <- list.files(path = raw_data_dir, pattern = "Round *", full.names = TRUE)
round_data <- lapply(round_files, read_excel)

rename_data <- function(df_list) {
names(df_list) <- basename(round_files)
names(df_list) <- sub("\\.xlsx$", "", names(df_list))
return(df_list)
}

round_data <- rename_data(round_data)

round_data <- lapply(names(round_data), function(df_name) {
  df <- round_data[[df_name]]
  df$Source <- df_name
  return(df)
})

round_data <- rename_data(round_data)

round_data_merged <- do.call(rbind, round_data)

colnames(round_data_merged)[colnames(round_data_merged) == "Event ID"] <- "Event_ID"
colnames(round_data_merged)[colnames(round_data_merged) == "Created at"] <- "Created_at"
colnames(round_data_merged)[colnames(round_data_merged) == "Changed at"] <- "Changed_at"

rownames(round_data_merged) <- NULL

summary(round_data_merged)
colSums(is.na(round_data_merged))

duplicates <- round_data_merged[duplicated(round_data_merged), ]

str(round_data_merged)

hist(round_data_merged$Points)

# Players data
players_data <- read_excel(path = paste0(raw_data_dir, "/List_Of_Players.xlsx"))

summary(players_data)
colSums(is.na(players_data))

duplicates <- players_data[duplicated(players_data), ]

str(players_data)

colnames(players_data)[colnames(players_data) == "Category of Player"] <- "Category_of_Player"

####################

##### Descriptive Analysis ####
merged_data <- left_join(round_data_merged, players_data, by = c("Player" = "PlayerID"))
merged_data$Source <- factor(merged_data$Source, levels = c("Round 1","Round 2", "Round 3", "Round 4", "Round 5", "Round 6", "Round 7", "Round 8", "Round 9", "Round 10", "Round 11"))

# Number of players per category
unique_players <- merged_data %>%
  group_by(Player) %>%
  slice(1) %>%
  ungroup()

category_of_player <- unique_players %>%
  count(Category_of_Player) %>%
  arrange(n)

category_of_player$Category_of_Player <- factor(category_of_player$Category_of_Player, 
                                                  levels = category_of_player$Category_of_Player)

ggplot(category_of_player, aes(x = Category_of_Player, y = n)) +
  geom_bar(stat = "identity", fill = "#006d72", color = "#006d72", width = 0.7) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_text(color = "black", size = 15, margin = margin(t = 20)), 
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "none" 
  ) +
  geom_text(data = subset(category_of_player, Category_of_Player != "Casino"), 
            aes(label = n), vjust = -0.5, size = 5, color = "#5b696f") +
  scale_y_continuous(expand = c(0, 0)) +  
  labs(y = "Number of Players")
ggsave("plots/num_players_by_cat.png", dpi = 300)

type_of_player <- unique_players %>%
  count(Player_Type) %>%
  arrange(n)

type_of_player$Player_Type <- factor(type_of_player$Player_Type, 
                                                  levels = type_of_player$Player_Type)

ggplot(type_of_player, aes(x = Player_Type, y = n)) +
  geom_bar(stat = "identity", fill = "#1f354e", color = "#1f354e", width = 0.7) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(color = "lightgrey"), 
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_text(color = "black", size = 15, margin = margin(t = 20)),  
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "none" 
  ) +
  geom_text(data = subset(type_of_player, Player_Type != "Regular"), 
            aes(label = n), vjust = -0.5, size = 5, color = "#5b696f") +
  scale_y_continuous(expand = c(0, 0)) +  
  labs(y = "Number of Players")
ggsave("plots/num_players_by_type.png", dpi = 300)

# How are points awarded
ggplot(merged_data, aes(x=Source, y=Points)) + 
  geom_point()

merged_data <- merged_data %>%
  mutate(Coded_Points = case_when(
    Source %in% c("Round 1", "Round 2", "Round 3", "Round 4", "Round 5", "Round 6") & Points == 0 ~ 0,
    Source %in% c("Round 1", "Round 2", "Round 3", "Round 4", "Round 5", "Round 6") & Points == 4 ~ 1,
    Source %in% c("Round 1", "Round 2", "Round 3", "Round 4", "Round 5", "Round 6") & Points == 10 ~ 2,
    
    Source %in% c("Round 7", "Round 8", "Round 9") & Points == 0 ~ 0,
    Source %in% c("Round 7", "Round 8", "Round 9") & Points == 6 ~ 1,
    Source %in% c("Round 7", "Round 8", "Round 9") & Points == 15 ~ 2,
    
    Source == "Round 10" & Points == 0 ~ 0,
    Source == "Round 10" & Points == 9 ~ 1,
    Source == "Round 10" & Points == 20 ~ 2,
    
    Source == "Round 11" & Points == 0 ~ 0,
    Source == "Round 11" & Points == 13 ~ 1,
    Source == "Round 11" & Points == 30 ~ 2,
  ))

# Number of predictions
nrow(merged_data)

# Average points awarded
mean(merged_data$Points)

avg_per_round <- aggregate(Points ~ Source, data = merged_data, mean)
print(avg_per_round)
avg_per_round$Round_Num <- as.numeric(gsub("Round ", "", avg_per_round$Source))

ggplot(avg_per_round, aes(x = Round_Num, y = Points)) +
  geom_line(color = "#006d72", linewidth = 1) +
  geom_point(color = "#006d72", size = 3) + 
  labs(title = "User Count Over Rounds", x = "Rounds", y = "Points") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(color = "black", size = 15, margin = margin(t = 20)),
    axis.title.y = element_text(color = "black", size = 15, margin = margin(t = 20)),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  scale_x_continuous(breaks = avg_per_round$Round_Num) +
  scale_y_continuous(breaks = seq(0, max(avg_per_round$Points), by = 1))
ggsave("plots/avg_points_per_round.png", dpi = 300)

# How often did users predict correctly? 
# Overall accuracy for each player
player_accuracy <- merged_data %>%
  group_by(Player) %>%
  summarise(
    Total_Predictions = n(),
    Correct_Predictions = sum(Coded_Points > 0),
    Accuracy = Correct_Predictions / Total_Predictions
  )

player_accuracy_only_winner <- merged_data %>%
  group_by(Player) %>%
  summarise(
    Total_Predictions = n(),
    Correct_Predictions = sum(Coded_Points == 1),
    Accuracy = Correct_Predictions / Total_Predictions
  )

player_accuracy_score <- merged_data %>%
  group_by(Player) %>%
  summarise(
    Total_Predictions = n(),
    Correct_Predictions = sum(Coded_Points == 2),
    Accuracy = Correct_Predictions / Total_Predictions
  )

mean_player_acc <- mean(player_accuracy$Accuracy)
mean_player_acc_winner <- mean(player_accuracy_only_winner$Accuracy)
mean_player_acc_score <- mean(player_accuracy_score$Accuracy)

merged_data <- left_join(merged_data, player_accuracy, by = "Player")

# Accuracy by player type
accuracy_by_type <- merged_data %>%
  group_by(Player_Type) %>%
  summarise(
    Total_Predictions = n(),
    Correct_Predictions = sum(Coded_Points > 0),
    Accuracy = Correct_Predictions / Total_Predictions
  )

ggplot(accuracy_by_type, aes(x = Player_Type, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "#1f354e", color = "#1f354e", width = 0.7) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(color = "lightgrey"),  
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "none"
  ) +
  geom_text(aes(label = scales::percent(Accuracy, accuracy = 0.1)), vjust = -0.5, size = 5, color = "#5b696f") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(0, 1))  

ggplot(merged_data, aes(x = Player_Type, y = Accuracy)) +
  geom_boxplot(fill = "#1f354e", color = "black", width = 0.7) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(color = "lightgrey"), 
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(color = "black", size = 15, margin = margin(t = 20)),  
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "none" 
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01), limits = c(0, 1))
ggsave("plots/boxplot_type.png", dpi = 300)

# Perform one-way ANOVA
player_accuracy <- left_join(player_accuracy, players_data, by = c("Player"="PlayerID"))

anova_result_by_type <- aov(Accuracy ~ Player_Type, data = player_accuracy)
summary(anova_result_by_type)

tukey_by_type <- TukeyHSD(anova_result_by_type)

# Check that assumptions are correct
residuals <- residuals(anova_result_by_type)
qqnorm(residuals)
qqline(residuals, col = "red")
# shapiro.test(residuals) --> We discarded shapiro.test because the sample size was too big
ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
leveneTest(Accuracy ~ Player_Type, data = player_accuracy)

player_accuracy$Log_Accuracy <- log(player_accuracy$Accuracy + 1) # Doesn't help improve the normality of the data
player_accuracy$Sqrt_Accuracy <- sqrt(player_accuracy$Accuracy) # Doesn't help improve the normality of the data
player_accuracy$CubeRoot_Accuracy <- sign(player_accuracy$Accuracy) * abs(player_accuracy$Accuracy)^(1/3) # Doesn't help improve the normality of the data
player_accuracy$Inverse_Accuracy <- 1 / (player_accuracy$Accuracy + 1) # Doesn't help improve the normality of the data

# As we don't fulfill the normality assumption, and having tried different transformations without good results, ANOVA can not be used. We will try to perform a non-parametric analysis as a work-around
kruskal.test(Accuracy ~ Player_Type, data = player_accuracy)

# Since the p-value '0.03' is less than the alpha level 0f 0.05, we can reject the null hipotesis. This suggests that there are statistically significant differences in the median accuracy among different categories of players
# The dunn's test is a post-hoc test that can help specify which groups differ
dunn_test <- dunn.test(player_accuracy$Accuracy, player_accuracy$Player_Type, kw = TRUE, label = TRUE)

adj.P_dunn <- dunn_test$P.adjusted
comparisons <- dunn_test$comparisons

alpha <- 0.05

significant_indices <- which(adj.P_dunn <= alpha)
significant_comparisons <- comparisons[significant_indices]
significant_p_values <- adj.P_dunn[significant_indices]

results_dunn_test <- data.frame(
  Comparison = significant_comparisons,
  Adj.P_value = significant_p_values
)

# In conclusion, this analysis reveals that *it is possible* that different groups of players reach different levels of accuracy
# However, the huge number of outliers present in the dataset and the similarity between medians and quartile distribution can make this conclusion difficult to accept
# Further research would be needed before assessing the difference in accuracy means is being caused by Player_Type categories

# Accuracy by player category
accuracy_by_category <- merged_data %>%
  group_by(Category_of_Player) %>%
  summarise(
    Total_Predictions = n(),
    Correct_Predictions = sum(Coded_Points > 0),
    Accuracy = Correct_Predictions / Total_Predictions
  )

ggplot(accuracy_by_category, aes(x = Category_of_Player, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "#720500", color = "#720500", width = 0.7) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.major.y = element_line(color = "lightgrey"), 
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "none" 
  ) +
  geom_text(aes(label = scales::percent(Accuracy, accuracy = 0.1)), vjust = -0.5, size = 5, color = "#5b696f") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(0, 1)) 

ggplot(merged_data, aes(x = Category_of_Player, y = Accuracy)) +
  geom_boxplot(fill = "#006d72", color = "black", width = 0.7) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major.x = element_blank(),  
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor = element_blank(),  
    axis.title.x = element_blank(), 
    axis.title.y = element_text(color = "black", size = 15, margin = margin(t = 20)), 
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "none" 
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01), limits = c(0, 1))  # Set y-axis to 100%
ggsave("plots/boxplot_cat.png", dpi = 300)

# Perform one-way ANOVA
anova_result_by_category <- aov(Accuracy ~ Category_of_Player, data = player_accuracy)
summary(anova_result_by_category)

tukey_by_category <- TukeyHSD(anova_result_by_category)

# Check that assumptions are correct
residuals <- residuals(anova_result_by_category)
qqnorm(residuals)
qqline(residuals, col = "red")
# shapiro.test(residuals) --> We discarded shapiro.test because the sample size was too big
ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
leveneTest(Accuracy ~ Category_of_Player, data = player_accuracy)

# As we don't fulfill the normality assumption, and having tried different transformations without good results, ANOVA can not be used. We will try to perform a non-parametric analysis as a work-around
kruskal.test(Accuracy ~ Category_of_Player, data = player_accuracy)

# Since the p-value '0.002848' is less than the alpha level 0f 0.05, we can reject the null hipotesis. This suggests that there are statistically significant differences in the median accuracy among different categories of players
# The dunn's test is a post-hoc test that can help specify which groups differ
dunn_test <- dunn.test(player_accuracy$Accuracy, player_accuracy$Category_of_Player, kw = TRUE, label = TRUE)

adj.P_dunn <- dunn_test$P.adjusted
comparisons <- dunn_test$comparisons

alpha <- 0.05

significant_indices <- which(adj.P_dunn <= alpha)
significant_comparisons <- comparisons[significant_indices]
significant_p_values <- adj.P_dunn[significant_indices]

results_dunn_test <- data.frame(
  Comparison = significant_comparisons,
  Adj.P_value = significant_p_values
)

# In conclusion, this analysis reveals that *it is possible* that different groups of players reach different levels of accuracy
# However, the huge number of outliers present in the dataset and the similarity between medians and quartile distribution can make this conclusion difficult to accept
# Further research would be needed before assessing the difference in accuracy means is being caused by Category_of_Player categories

# Accuracy by round
accuracy_by_round <- merged_data %>%
  group_by(Source) %>%
  summarise(
    Total_Predictions = n(),
    Correct_Predictions = sum(Coded_Points > 0),
    Accuracy = Correct_Predictions / Total_Predictions
  )

ggplot(accuracy_by_round, aes(x = Source, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "#006d72", color = "#006d72", width = 0.7) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(color = "lightgrey"), 
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "none" 
  ) +
  geom_text(aes(label = scales::percent(Accuracy, accuracy = 0.1)), hjust = -0.5, size = 5, color = "#5b696f") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(0, 1)) +
  coord_flip()

ggplot(merged_data, aes(x = Source, y = Accuracy)) +
  geom_boxplot(fill = "#adc7ea", color = "#1f354e", width = 0.7) +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"), 
    panel.grid.minor = element_blank(), 
    axis.title.x = element_text(color = "black", size = 15, margin = margin(t = 20)), 
    axis.title.y = element_blank(), 
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "none" 
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.001, 0.015), limits = c(0, 1)) +  
  coord_flip()
ggsave("plots/boxplot_round.png", dpi = 300)

# Sort players by accuracy
top_performers <- player_accuracy %>%
  arrange(desc(Accuracy))

###### User behavior analysis
round_summary <- merged_data %>%
  group_by(Source) %>%
  summarise(
    User_Count = n_distinct(Player)
  )

ggplot(round_summary, aes(x = Source, y = User_Count, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "User Participation Over Rounds", x = "Round", y = "Number of Participants")

round_summary$Change <- c(NA, diff(round_summary$User_Count))
round_summary$Change_Percent <- round((round_summary$Change / lag(round_summary$User_Count, n = 1) * 100), 2)

round_summary$Round_Num <- as.numeric(gsub("Round ", "", round_summary$Source))

ggplot(round_summary, aes(x = Round_Num, y = User_Count)) +
  geom_line(color = "#1f354e", size = 1) +  
  geom_point(color = "#1f354e", size = 3) +  
  labs(title = "User Count Over Rounds", x = "Rounds", y = "Number of Participants") +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(color = "black", size = 15, margin = margin(t = 20)),
    axis.title.y = element_text(color = "black", size = 15, margin = margin(t = 20)),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  scale_x_continuous(breaks = round_summary$Round_Num)
ggsave("plots/user_count_over_rounds.png", dpi = 300)

ggplot(round_summary, aes(x = Round_Num, y = Change_Percent)) +
  geom_bar(stat = "identity", fill = "#720500", alpha = 0.7) + 
  labs(title = "Percentage Change Over Rounds", x = "Rounds", y = "Percentage Change") +
  theme_minimal(base_size = 15) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor = element_blank(), 
    axis.title.x = element_text(color = "black", size = 15, margin = margin(t = 20)), 
    axis.title.y = element_text(color = "black", size = 15, margin = margin(t = 20)), 
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black") 
  ) +
  scale_x_continuous(breaks = round_summary$Round_Num) 
ggsave("plots/user_percent.png", dpi = 300)

# User engagement by category of player
round_summary_by_category <- merged_data %>%
  group_by(Category_of_Player, Source) %>%
  summarise(
    User_Count = n_distinct(Player) 
  ) %>%
  arrange(Category_of_Player, Source)

round_summary_by_category <- round_summary_by_category %>%
  group_by(Category_of_Player) %>%
  mutate(
    Change = c(NA, diff(User_Count)), 
    Change_Percent = round((Change / lag(User_Count, n = 1) * 100), 2)  
  )

round_summary_by_category$Round_Num <- as.numeric(gsub("Round ", "", round_summary_by_category$Source))

ggplot(round_summary_by_category, aes(x = Round_Num, y = User_Count, color = Category_of_Player, group = Category_of_Player)) +
  geom_line(size = 1) +  
  geom_point(size = 3) +  
  labs(
    title = "User Count Over Rounds by Category", 
    x = "Rounds", 
    y = "Number of Participants", 
    color = "Player Category"  
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(color = "black", size = 15, margin = margin(t = 20)),
    axis.title.y = element_text(color = "black", size = 15, margin = margin(t = 20)),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.title = element_text(size = 15),  
    legend.text = element_text(size = 13) 
  ) +
  scale_x_continuous(breaks = unique(round_summary_by_category$Round_Num)) + 
  scale_color_manual(values = c("#1f354e", "#720500", "#006d72", "#e0a800"))  
ggsave("plots/user_count_over_rounds_by_category.png", dpi = 300)

# User engagement by type of player
round_summary_by_type <- merged_data %>%
  group_by(Player_Type, Source) %>%
  summarise(
    User_Count = n_distinct(Player) 
  ) %>%
  arrange(Player_Type, Source)

round_summary_by_type <- round_summary_by_type %>%
  group_by(Player_Type) %>% 
  mutate(
    Change = c(NA, diff(User_Count)), 
    Change_Percent = round((Change / lag(User_Count, n = 1) * 100), 2) 
  )

round_summary_by_type$Round_Num <- as.numeric(gsub("Round ", "", round_summary_by_type$Source))

ggplot(round_summary_by_type, aes(x = Round_Num, y = User_Count, color = Player_Type, group = Player_Type)) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  labs(
    title = "User Count Over Rounds by Type", 
    x = "Rounds", 
    y = "Number of Participants", 
    color = "Player Type"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(color = "black", size = 15, margin = margin(t = 20)),
    axis.title.y = element_text(color = "black", size = 15, margin = margin(t = 20)),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.title = element_text(size = 15), 
    legend.text = element_text(size = 13)  
  ) +
  scale_x_continuous(breaks = unique(round_summary_by_type$Round_Num)) +  
  scale_color_manual(values = c("#1f354e", "#720500", "#006d72", "#e0a800", "#3c5775", "#a33112", "#00989d", "#f2c441"))
ggsave("plots/user_count_over_rounds_by_type.png", dpi = 300)

