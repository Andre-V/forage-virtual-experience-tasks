# INSTALL AND LOAD PACKAGES
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse)
# LOAD DATA ####
(data <- import("data/ANZDataSet.xlsx") %>% as_tibble())

## Volume of transaction amounts ####
data %>% ggplot(aes(x = amount, fill = status)) +
  geom_histogram()

## Volume of transaction amounts scaled on log10 scale ####
log_seq <- vector()
for (x in 0:4) {
  log_seq <- c(log_seq, seq(1*10^x, 10*10^x, 1*10^x))
}
summary(data$amount)
(transc_amnt_dist <- data %>% ggplot(aes(x = amount)) +
  geom_histogram() +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                minor_breaks = log_seq) +
    labs(x = "Transaction Amount ($AUD)", y = "count"))
transc_amnt_dist + aes(x = amount, fill = txn_description) +
  facet_grid(txn_description ~ .) +
  labs(x = "Transaction Amount ($AUD)", fill = "Transaction Type")

## Transaction volume over time ####

data %>% ggplot(aes(x = date)) +
  geom_histogram()

data %>% 
  filter(!(is.na(merchant_state))) %>%
  ggplot(aes(x = date, fill = merchant_state)) +
  facet_grid(merchant_state ~ .) +
  geom_histogram()

data_vol <- data %>% 
  mutate(days = factor(
    weekdays(date), 
    levels = c("Monday", "Tuesday", "Wednesday", 
               "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  group_by(days) 

# average transaction amount over a week
data_vol %>%
  summarise(m_amount = mean(amount)) %>%
  ggplot(aes(x = days, y = m_amount)) +
  geom_col()

data_vol %>%
  summarise(m_amount = median(amount)) %>%
  ggplot(aes(x = days, y = m_amount)) +
  geom_col() +
  labs(x = "Days of the Week", y = "Median Amount ($AUD)") +
  coord_cartesian(ylim=c(20,40))

# transaction volume over a week
data_vol %>% 
  ggplot(aes(x = days, fill = txn_description)) +
  geom_bar() +
  labs(x = "Days of the Week", y = "count", fill = "Transaction Type")

# distribution over a week
data_vol %>% 
  ggplot(aes(x = days, y = amount)) +
  geom_boxplot() +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)),
                minor_breaks = log_seq)



