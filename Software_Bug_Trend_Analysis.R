install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
library(dplyr)
library(ggplot2)
library(lubridate)
bug_data <- read.csv("C:/Users/ROHITHMANI/Downloads/bugs_dataset.csv")
bug_data$created_date <- ymd(bug_data$created_date)
bug_data$month <- month(bug_data$created_date, label = TRUE)
bug_data$year <- year(bug_data$created_date)
bug_data <- bug_data %>%
  filter(!is.na(module),
         !is.na(severity),
         !is.na(status))
module_bugs <- bug_data %>%
  group_by(module) %>%
  summarise(total_bugs = n())
severity_count <- bug_data %>%
  group_by(severity) %>%
  summarise(count = n())
status_count <- bug_data %>%
  group_by(status) %>%
  summarise(count = n())
monthly_bugs <- bug_data %>%
  group_by(year, month) %>%
  summarise(total = n())
print(module_bugs)
print(severity_count)
print(status_count)
print(monthly_bugs)
ggplot(module_bugs, aes(x = module, y = total_bugs)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_minimal() +
  labs(title="Bug Count by Module",
       x="Module",
       y="Number of Bugs")
ggplot(severity_count, aes(x = severity, y = count)) +
  geom_bar(stat="identity", fill="red") +
  theme_minimal() +
  labs(title="Bug Severity Distribution",
       x="Severity",
       y="Bug Count")
ggplot(status_count, aes(x = status, y = count)) +
  geom_bar(stat="identity", fill="green") +
  theme_minimal() +
  labs(title="Bug Status Distribution",
       x="Status",
       y="Bug Count")
ggplot(monthly_bugs, aes(x = month, y = total, group = year)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title="Monthly Bug Trend",
       x="Month",
       y="Number of Bugs")