# Load package
library(tidyverse)
library(dplyr)
library(tidyr)
# -----------------------------
# 1. Import and clean data
# -----------------------------

data <- read.csv(file.choose(), stringsAsFactors = FALSE)
#View(data)
data[data == ""] <- NA

# Remove missing rows
data_clean <- data %>%
drop_na()
# View(data_clean)

# Data Clean comparison
# data[data == ""] <- NA
# data_clean_super <- data %>%
#   drop_na()
# View(data_clean_super)

# Add unique id for each row
data_clean <- data_clean %>%
  mutate(user_id = row_number())
#View(data)
# View structure
head(data_clean)
str(data_clean)
summary(data_clean)

# -----------------------------
# 2. SELECT
# Choose important columns
# -----------------------------
selected_data <- data_clean %>%
  select(user_id, 
         age, 
         daily_screen_time, 
         social_media_hours,
         study_hours,
         sleep_hours)

head(selected_data)

# -----------------------------
# 3. FILTER
# Keep users with high screen time
# -----------------------------
high_screen <-
  data_clean %>%
  filter(
    daily_screen_time > 6
    )

head(high_screen)

# -----------------------------
# 4. MUTATE
# Create new columns
# -----------------------------
data_clean <- data_clean %>%
  mutate(
    screen_time_category = 
      ifelse(daily_screen_time > 6, 
             "High", "Low"
             ),
    sleep_category = 
      ifelse(sleep_hours >= 7, 
             "Good Sleep", "Less Sleep"
             )
  )
View(data_clean)
head(data_clean)

# -----------------------------
# 5. ARRANGE
# Sort by productivity score
# -----------------------------
sorted_data <- data_clean %>%
  arrange(desc(productivity_score))

#print(sorted_data)
head(sorted_data)

# -----------------------------
# 6. GROUP_BY + SUMMARISE
# Summary by addiction level
# -----------------------------
addiction_summary <- data_clean %>%
  group_by(addiction_level) %>%
  summarise(
    avg_screen_time = mean(daily_screen_time, na.rm = TRUE),
    avg_social_media = mean(social_media_hours, na.rm = TRUE),
    avg_focus = mean(focus_score, na.rm = TRUE),
    avg_productivity = mean(productivity_score, na.rm = TRUE),
    count = n()
  )

print(addiction_summary)

# -----------------------------
# 7. RENAME
# Rename columns for readability
# -----------------------------
renamed_data <- data_clean %>%
  rename(
    screen_time = daily_screen_time,
    social_time = social_media_hours,
    study_time = study_hours,
    sleep_time = sleep_hours
  )
#print(renamed_data)
head(renamed_data)

# -----------------------------
# 8. DISTINCT
# Show unique addiction levels
# -----------------------------
unique_addiction <- data_clean %>%
  distinct(addiction_level)

print(unique_addiction)

# -----------------------------
# 9. COUNT
# Count users in each addiction level
# -----------------------------
addiction_count <- data_clean %>%
  count(addiction_level)

print(addiction_count)

# -----------------------------
# 10. SLICE
# Take first 10 rows
# -----------------------------
first_ten <- data_clean %>%
  slice(1:10)

print(first_ten)

# -----------------------------
# 11. ROWWISE
# Create row-level average activity score
# -----------------------------
rowwise_data <- data_clean %>%
  rowwise() %>%
  mutate(
    avg_daily_activity = mean(c(daily_screen_time, social_media_hours, study_hours, sleep_hours), na.rm = TRUE)
  ) %>%
  ungroup()

head(rowwise_data)

# -----------------------------
# 12. Additional group_by + summarise
# Summary by screen time category
# -----------------------------
screen_summary <- data_clean %>%
  group_by(screen_time_category) %>%
  summarise(
    avg_sleep = mean(sleep_hours, na.rm = TRUE),
    avg_focus = mean(focus_score, na.rm = TRUE),
    avg_productivity = mean(productivity_score, na.rm = TRUE),
    total_users = n()
  )

print(screen_summary)

# -----------------------------
# 13. TIDYR - pivot_longer
# Convert wide to long
# -----------------------------
long_data_clean <- data_clean %>%
  select(user_id, age, daily_screen_time, social_media_hours, study_hours, sleep_hours) %>%
  pivot_longer(
    cols = c(daily_screen_time, social_media_hours, study_hours, sleep_hours),
    names_to = "activity_type",
    values_to = "hours"
  )

View(long_data_clean)

# -----------------------------
# 14. TIDYR - pivot_wider
# Convert long back to wide
# -----------------------------
wide_data_clean <- long_data_clean %>%
  pivot_wider(
    id_cols = c(user_id, age),
    names_from = activity_type,
    values_from = hours
  )

View(wide_data_clean)

#Question
data_clean %>% 
filter(sleep_category == "Good Sleep") %>% 
summarize(avg_study = mean(study_hours)) 

# -----------------------------
# 15. Visualizations
# -----------------------------

# Bar chart: addiction level vs average productivity

ggplot(addiction_summary, aes(x = reorder(addiction_level, -avg_productivity), 
                              y = avg_productivity, fill = addiction_level)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_text(aes(label = round(avg_productivity, 1)), 
            vjust = -0.5, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Low" = "#10B981", "Medium" = "#F59E0B", "High" = "#EF4444")) +
  scale_y_continuous(limits = c(0, 80), expand = c(0, 0)) +
  labs(
    title = "More Addicted = Less Productive",
    subtitle = "Average productivity score drops sharply as addiction level rises",
    x = NULL,
    y = "Avg Productivity Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16, color = "#0F172A"),
    plot.subtitle = element_text(color = "#64748B", margin = margin(b = 12)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x  = element_text(face = "bold", size = 12),
    plot.margin  = margin(20, 20, 20, 20)
  )

# Scatter plot: screen time vs productivity
ggplot(data_clean, aes(x = daily_screen_time, y = productivity_score, 
                       color = addiction_level)) +
  geom_point(alpha = 0.35, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#0F172A", 
              linewidth = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("Low" = "#10B981", "Medium" = "#F59E0B", "High" = "#EF4444"),
                     name = "Addiction Level") +
  scale_x_continuous(breaks = seq(2, 12, by = 2)) +
  labs(
    title    = "More Screen Time, Less Done",
    subtitle = "Each dot is one student. Trend line shows the overall decline.",
    x        = "Daily Screen Time (hours)",
    y        = "Productivity Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 16, color = "#0F172A"),
    plot.subtitle   = element_text(color = "#64748B", margin = margin(b = 12)),
    legend.position = "top",
    legend.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )
  
  # Scatter plot: notifications vs focus
  ggplot(data_clean, aes(x = notifications_per_day, y = focus_score,
                         color = screen_time_category)) +
    geom_point(alpha = 0.3, size = 1.6) +
    geom_smooth(aes(group = 1), method = "loess", se = TRUE,
                color = "#7C3AED", linewidth = 1.3, linetype = "dashed") +
    scale_color_manual(values = c("High" = "#EF4444", "Low" = "#0D9488"),
                       name = "Screen Time") +
    annotate("text", x = 250, y = 52, 
             label = "High notifications\n= lower focus ceiling", 
             color = "#EF4444", fontface = "italic", size = 3.8) +
    labs(
      title    = "Constant Pings Kill Concentration",
      subtitle = "Students with more daily notifications trend toward lower focus scores",
      x        = "Notifications Per Day",
      y        = "Focus Score"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title      = element_text(face = "bold", size = 16, color = "#0F172A"),
      plot.subtitle   = element_text(color = "#64748B", margin = margin(b = 12)),
      legend.position = "top",
      legend.title    = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(20, 20, 20, 20)
    )