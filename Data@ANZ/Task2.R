# INSTALL AND LOAD PACKAGES ####
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, lubridate, corrplot, magrittr)

# LOAD DATA ####
(data <- import("data/ANZDataSet.xlsx") %>% as_tibble())

# ORGANIZE BY CUSTOMER ####
## Annual salary + other variables ####
data_per_cust <- data %>%
  filter(txn_description == "PAY/SALARY") %>% # Only salaries should be considered
  group_by(customer_id) %>% # Ensures some functions applied per group
  arrange(date, .by_group = TRUE) %>% # Sort for processing difference
  slice_head(n = 2) %>% # Assume first two entires are representive of salary
  mutate(pay_period = floor(difftime(lead(date), date, units = "weeks"))) %>%
  # Determine period between salaries according to transactions
  slice_head(n = 1) %>%
  filter(!(pay_period == 0)) %>% # Outlier
  mutate(annual_salary = (amount / as.numeric(pay_period)) * 52.1429) %>%
  mutate(gender = factor(gender)) %>%
  select(customer_id, first_name, age, gender, long_lat, pay_period, annual_salary)

## Monthly spending ####
data_spending <- data %>%
  filter(txn_description == "POS" | txn_description == "SALES-POS") %>%
  group_by(customer_id) %>%
  summarise(spent_per_mnth = sum(amount)/3)

data_per_cust <- inner_join(data_per_cust, data_spending, by = "customer_id")
# CORRELATIONS ####

data %>%
  ungroup() %>%
  select(age, amount, balance) %>%
  cor() %>%
  corrplot(
    type   = "upper",     # Matrix: full, upper, or lower
    diag   = F,           # Remove diagonal
    order  = "original",  # Order for labels
    tl.col = "black",     # Font color
    tl.srt = 45           # Label angle
  )
## Annual salary vs. age ####
fit1 <- data_per_cust %>%
  ungroup() %>%
  select(annual_salary, age) %>%
  lm()
cor1 <- data_per_cust %>%
  ungroup() %>%
  select(annual_salary, age) %>%
  cor()
fit1
fit1 %>% summary()
cor1
cor1 %>% summary()
data_per_cust %$% cor.test(age, annual_salary)


data_per_cust %>%
  ggplot(aes(x = age, y = annual_salary)) +
  geom_jitter() +
  geom_smooth(method = lm) +
  labs( y = "Annual salary ($AUD)", x = "Age", 
        title = "Annual Salary by Age") +
  scale_x_continuous(breaks = seq(0, 90, by = 5)) +
  scale_y_continuous(breaks = seq(0, 200000, by = 10000))

## Annual salary vs. total monthly spending ####
fit2 <- data_per_cust %>%
  ungroup() %>%
  select(annual_salary, spent_per_mnth) %>%
  lm()
fit2
fit2 %>% summary()
data_per_cust %$% cor.test(spent_per_mnth, annual_salary)

data_per_cust %>%
  ggplot(aes(y = annual_salary, x = spent_per_mnth)) +
  geom_jitter() +
  geom_smooth(method = lm) +
  labs( y = "Annual salary ($AUD)", x = "Total amount spent per month ($AUD/month)", 
        title = "Annual Salary by Monthly Spending") +
  scale_x_continuous(breaks = seq(0, 5000, by = 500)) +
  scale_y_continuous(breaks = seq(0, 200000, by = 20000))
## Annual salary vs. gender ####
data_per_cust %>% 
  group_by(gender) %>%
  summarise(m_salary = mean(annual_salary)) %>%
  ggplot(aes(x = gender, y = m_salary)) +
  geom_col()

data_per_cust %>% 
  ggplot(aes(x = gender, y = annual_salary, fill = gender)) +
  geom_boxplot()

data_per_cust %>% 
  ggplot(aes(x = annual_salary, fill = gender)) +
  facet_grid(gender ~ .) +
  geom_density()
