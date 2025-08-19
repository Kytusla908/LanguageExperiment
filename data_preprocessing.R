# Load libraries ===============================================================
library(xlsx)
library(dplyr)
library(ggplot2)





# Load data ==========================================
data <- read.xlsx("data/results_false.xlsx", sheetIndex = 1)

#Fix issue with sex categories
data$sex[which(data$sex == "female ")] <- "female"

factor_cols <- c("subject", "session", "language", "sex",
                 "test.category", "test.number", "trial")
data[factor_cols] <- lapply(data[factor_cols], as.factor)
data$ACC <- as.numeric(data$ACC)
str(data)
summary(data)

## New df with one single entry per subject =======
subject_info <- data %>%
  group_by(subject) %>%
  summarise(
    session  = first(session),
    language = first(language),
    age      = first(age),
    sex      = first(sex),
    ACC      = sum(ACC),
    .groups = "drop"
  )

# Descriptive analysis ==============================
table(is.na(data))

## Age distribution ==============
ggplot(subject_info, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "skyblue",
                 color = "white", linewidth = 0.2) +
  labs(title = "Age Distribution of Participants",
       x = "Age", y = "Count")
ggsave("plots/DescAnalysis_age_distribution.png", device = "png",
       width = 16, height = 12, units = "cm", dpi = 500, create.dir = T)

## Sex distribution =============
sex_counts <- subject_info %>%
  count(sex) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(sex_counts, aes(x = "", y = percentage, fill = sex)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Sex Proportions", fill = "Sex",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  geom_text(aes(label = paste0(round(percentage, 2), "%")),
            position = position_stack(vjust = 0.5)) # +
  # scale_fill_manual(values = cols)
ggsave("plots/DescAnalysis_sex_proportions.png", device = "png",
       width = 16, height = 12, units = "cm", dpi = 500, create.dir = T)


## Accuracy by Language ============
subject_info %>%
  group_by(language, subject) %>%
  summarise(acc = sum(ACC, na.rm = TRUE)) %>%
  ggplot(aes(x = language, y = acc, fill = language)) +
  geom_boxplot() +
  labs(title = "Accuracy by Language",
       x = "Language",
       y = "Accuracy")
ggsave("plots/DescAnalysis_boxplot_ACCbyLanguage.png", device = "png",
       width = 16, height = 12, units = "cm", dpi = 500, create.dir = T)

## RT by language ============0
ggplot(data, aes(x = language, y = RT, fill = language)) +
  geom_boxplot() +
  labs(title = "Reaction Time by Language",
       x = "Language",
       y = "RT (ms)")
ggsave("plots/DescAnalysis_boxplot_RTbyLang.png", device = "png",
       width = 16, height = 12, units = "cm", dpi = 500, create.dir = T)


## ACC by sex and language ==========
subject_info %>%
  group_by(language, sex) %>%
  summarise(acc = mean(ACC, na.rm = TRUE)) %>%
  ggplot(aes(x = language, y = acc, fill = sex)) +
  geom_col(position = "dodge") +
  labs(title = "Mean Accuracy by Language and Sex",
       x = "Language",
       y = "Accuracy")

## RT distribution ==========
ggplot(data, aes(x = RT)) +
  geom_histogram(binwidth = 100, fill = "skyblue",
                 color = "white", linewidth = 0.2) +
  facet_wrap(~ language) +
  labs(title = "Distribution of Reaction Times by Language",
       x = "Reaction Time (ms)",
       y = "Count")
ggsave("plots/DescAnalysis_hist_RTdistribution.png", device = "png",
       width = 24, height = 12, units = "cm", dpi = 500, create.dir = T)

## Accuracy over Test ==========
data %>%
  mutate(test.number = factor(test.number,
                              levels = c("test1", "test2", "test3", "exam"))) %>%
  group_by(test.number, language) %>%
  summarise(acc = sum(ACC, na.rm = TRUE)) %>%
  ggplot(aes(x = test.number, y = acc,
             group = language, color = language)) +
    stat_summary(fun = mean, geom = "line") +
    labs(title = "Accuracy over Test by Language",
         x = "Test",
         y = "Accuracy")
ggsave("plots/DescAnalysis_hist_ACCvsTESTbyLANG.png", device = "png",
       width = 24, height = 12, units = "cm", dpi = 500, create.dir = T)

















