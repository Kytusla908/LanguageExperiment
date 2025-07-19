# Load libraries ===============================================================
library(xlsx)
library(dplyr)
library(ggplot2)





# Load data ==========================================
data <- read.xlsx("data/results.xlsx", sheetIndex = 1)
factor_cols <- c("subject", "session", "language", "sex",
                 "test.category", "test.number", "trial")
data[factor_cols] <- lapply(data[factor_cols], as.factor)
data$ACC <- as.numeric(data$ACC)
str(data)
summary(data)

# Descriptive analysis ==============================
table(is.na(data))

# Accuracy by Language
data %>%
  group_by(language) %>%
  summarise(acc = sum(ACC, na.rm = TRUE)) %>%
  ggplot(aes(x = language, y = acc, fill = language)) +
  geom_boxplot() +
  labs(title = "Accuracy by Language",
       x = "Language",
       y = "Accuracy")

# RT by language
ggplot(data, aes(x = language, y = RT, fill = language)) +
  geom_boxplot() +
  labs(title = "Reaction Time by Language",
       x = "Language",
       y = "RT (ms)")
ggsave("plots/DescAnalysis_boxplot_RTbyLang.png", device = "png",
       width = 12, height = 12, units = "cm", dpi = 500, create.dir = T)


# ACC by sex and language
data %>%
  group_by(language, sex) %>%
  summarise(acc = sum(ACC, na.rm = TRUE)) %>%
  ggplot(aes(x = language, y = mean_acc, fill = sex)) +
  geom_col(position = "dodge") +
  labs(title = "Mean Accuracy by Language and Sex",
       x = "Language",
       y = "Accuracy")

# RT distribution
ggplot(data, aes(x = RT)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "white", linewidth = 0.2) +
  facet_wrap(~ language) +
  labs(title = "Distribution of Reaction Times by Language",
       x = "Reaction Time (ms)",
       y = "Count")
ggsave("plots/DescAnalysis_hist_RTdistribution.png", device = "png",
       width = 24, height = 12, units = "cm", dpi = 500, create.dir = T)

# Accuracy over Test 
data %>%
  group_by(test.number, language) %>%
  summarise(acc = sum(ACC, na.rm = TRUE)) %>%
  ggplot(data, aes(x = test.number, y = ACC, group = language, color = language)) +
    stat_summary(fun = mean, geom = "line") +
    labs(title = "Accuracy over Test by Language",
         x = "Test",
         y = "Mean Accuracy")

















