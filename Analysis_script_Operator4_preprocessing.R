# Script to clean, wrangle and combine datasets and calculate some descriptive stats for operator 4

# Load any relevant packages
library("ggplot2")
library("forcats")
library("sjmisc")
library("data.table")
library("tidyr")
library("dplyr")
library("yarrr")
library("lubridate")
library("cowplot")
library("readr")

#Remove all objects from work space
rm(list = ls())

# load and name all datasets ------------------------------------------------------------------------------
clients.group1.4<- read.csv("Analysis data/Operator 4/UoS_client_data_group_1.csv") 
clients.group2.4<- read.csv("Analysis data/Operator 4/UoS_client_data_group_2.csv") 
clients.group4.4<- read.csv("Analysis data/Operator 4/UoS_client_data_group_4.csv") 
clients.group6.4<- read.csv("Analysis data/Operator 4/UoS_client_data_group_6.csv") 

fulllimits.group1.4<- read.csv("Analysis data/Operator 4/UoS_full_deposit_limit_data_group_1.csv") 
fulllimits.group2.4<- read.csv("Analysis data/Operator 4/UoS_full_deposit_limit_data_group_2.csv") 
fulllimits.group4.4<- read.csv("Analysis data/Operator 4/UoS_full_deposit_limit_data_group_4.csv") 
fulllimits.group6.4<- read.csv("Analysis data/Operator 4/UoS_full_deposit_limit_data_group_6.csv") 

limits.group1.4<- read.csv("Analysis data/Operator 4/UoS_complete_deposit_limit_data_group_1.csv") 
limits.group2.4<- read.csv("Analysis data/Operator 4/UoS_complete_deposit_limit_data_group_2.csv") 
limits.group4.4<- read.csv("Analysis data/Operator 4/UoS_complete_deposit_limit_data_group_4.csv") 
limits.group6.4<- read.csv("Analysis data/Operator 4/UoS_complete_deposit_limit_data_group_6.csv") 

timeout.group1.4<- read.csv("Analysis data/Operator 4/UoS_timeout_data_group_1.csv") 
timeout.group2.4<- read.csv("Analysis data/Operator 4/UoS_timeout_data_group_2.csv") 
timeout.group4.4<- read.csv("Analysis data/Operator 4/UoS_timeout_data_group_4.csv") 
timeout.group6.4<- read.csv("Analysis data/Operator 4/UoS_timeout_data_group_6.csv") 
selfexclusion1.4<- read.csv("Analysis data/Operator 4/UoS_self_exclusion_data_group_1.csv") 
selfexclusion2.4<- read.csv("Analysis data/Operator 4/UoS_self_exclusion_data_group_2.csv") 
selfexclusion4.4<- read.csv("Analysis data/Operator 4/UoS_self_exclusion_data_group_4.csv") 
selfexclusion6.4<- read.csv("Analysis data/Operator 4/UoS_self_exclusion_data_group_6.csv") 

transactions1.4<- read.csv("Analysis data/Operator 4/UoS_transaction_data_group_1.csv") 
transactions2.4<- read.csv("Analysis data/Operator 4/UoS_transaction_data_group_2.csv") 
transactions4.4<- read.csv("Analysis data/Operator 4/UoS_transaction_data_group_4.csv") 
transactions6.4<- read.csv("Analysis data/Operator 4/UoS_transaction_data_group_6.csv") 

wagers1.4<- read.csv("Analysis data/Operator 4/UoS_wager_data_group_1.csv") 
wagers2.4<- read.csv("Analysis data/Operator 4/UoS_wager_data_group_2.csv") 
wagers4.4<- read.csv("Analysis data/Operator 4/UoS_wager_data_group_4.csv") 
wagers6.4<- read.csv("Analysis data/Operator 4/UoS_wager_data_group_6.csv") 

# ------------------------------------------------------------------------------
# Beginning of cleaning/wrangling
# Join each group's datasets for each variable together and make them usable: 

# Let's start with the clients datasets------------------------------------------------------------------------------
# Some of the columns are different variable types (e.g. factor vs. dbl.) between datasets & the condition column has two different names. 
# Let's make them consistent:
clients.group1.4 <- clients.group1.4 %>% 
  as_tibble() %>%
  mutate(postcode = as.double(post_code)) %>%
  select(-post_code) %>%
  print()
nrow(clients.group1.4)

clients.group2.4 <- clients.group2.4 %>% 
  as_tibble() %>%
  rename(postcode = post_code) %>%
  print()
nrow(clients.group2.4)

clients.group4.4 <- clients.group4.4 %>% 
  as_tibble() %>%
  mutate(postcode = as.double(post_code)) %>%
  rename(condition = messaging_group, customerid = account_id) %>%
  select(-post_code) %>%
  print()
nrow(clients.group4.4)

clients.group6.4 <- clients.group6.4 %>% 
  as_tibble() %>%
  rename(postcode = post_code, 
         condition = messaging_group, 
         customerid = account_id) %>%
  print()
nrow(clients.group6.4)

# Now that all of the datasets are consistent, join them together:
clients4<- clients.group1.4 %>% 
  bind_rows(clients.group2.4) %>% 
  bind_rows(clients.group4.4) %>%
  bind_rows(clients.group6.4) %>%
  separate(account_open_date, into = c("open.date", "time_account_opened"), sep ="T") %>% # Make a usable date column
  mutate(date_account_opened = as.Date(open.date, format = "%Y-%m-%d")) %>%
  select(-open.date) %>%
  rename(date_account_closed = account_closed_dt) %>%
  print()

clients4 %>% 
  count(condition) # Check the total number of participants per condition is consistent with the original data
n_distinct(clients4$customerid) # Now check when using unique customer IDs

clients4 %>% 
  count(gender) # Check the gender distribution and see whether any factors require recoding:

clients4 %>% 
  # distinct(customerid, .keep_all = TRUE) %>% 
  summarise(
    agemean = mean(age),
    agemedian = median(age),
    agemin = min(age),
    agemax = max(age),
    n = n()
  )  # summary figures for age (total sample, duplicates removed)

demographic.summary.2<- clients4 %>% 
  # distinct(customerid, .keep_all = TRUE) %>% 
  group_by(condition, gender) %>% 
  summarise(
    agemean = mean(age),
    agemedian = median(age),
    agemin = min(age),
    agemax = max(age),
    n = n()
  ) %>%
  print() # Demographic breakdown by condition and gender

clients4 %>% 
  group_by(condition,
           gender) %>% 
  summarise(
    total = n()
  ) %>% 
  group_by(condition) %>% 
  mutate(percentage = total/sum(total)*100) # Percentage of males and females in each condition

clients4 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  group_by(condition) %>% 
  summarise(
    agemean = mean(age),
    agemedian = median(age),
    agemin = min(age),
    agemax = max(age),
    n = n()
  ) # Demographic breakdown by condition only

# ------------------------------------------------------------------------------
# Now let's take a look at the deposit limit data (data for study period only)
limits.group1.4 %>% 
  as_tibble()
limits.group2.4 %>% 
  as_tibble()

# Variables seem consistent so let's join the datasets together:
templimits1.4 <- limits.group1.4 %>% 
  bind_rows(limits.group1.4) %>% 
  bind_rows(limits.group4.4) %>%
  bind_rows(limits.group6.4) %>%
  as_tibble() %>%
  separate(limit_date, into = c("limit.date", "limit.waste"), sep ="T") %>% # Make a usable date column
  mutate(limit_date = as.Date(limit.date, format = "%Y-%m-%d")) %>%
  select(-limit.waste, 
         - limit.date) %>%
  rename(customerid = account_id) %>%
  arrange(limit_date) %>%
  print(n = 50)

# Now let's take a look at the deposit limit data (ever):
fulllimits.group1.4 %>% 
  as_tibble()
fulllimits.group2.4 %>% 
  as_tibble()

# Again, variables seem consistent so let's join the datasets together:
# First: for some reason, the data is split into previous and new limits and we need both.
# So we'll join them first and then split by columns into two separate datasets (old and new limits) then row bind these.
tempoldlimits.4 <- fulllimits.group1.4 %>% 
  bind_rows(fulllimits.group1.4) %>% 
  bind_rows(fulllimits.group4.4) %>%
  bind_rows(fulllimits.group6.4) %>%
  as_tibble() %>%
  rename(customerid = account_id) %>% # Make columns names consistent with other operator
  select(customerid, # Select all old limit data columns
         limit_date,
         limit_time,
         limit_type,
         limit_amount,
         limit_window)%>%
  print(n = 50)

templimits2.4 <- fulllimits.group1.4 %>% 
  bind_rows(fulllimits.group1.4) %>% 
  bind_rows(fulllimits.group4.4) %>%
  bind_rows(fulllimits.group6.4) %>%
  as_tibble() %>%
  rename(customerid = account_id) %>%
  select(customerid, # Select all new limit data columns
         new_limit_date,
         new_limit_time,
         new_limit_type,
         new_limit,
         new_limit_window) %>%
  rename(limit_date = new_limit_date,# Make columns names consistent with other operator and dataset
         limit_time = new_limit_time,
         limit_type = new_limit_type,
         limit_amount = new_limit,
         limit_window = new_limit_window) %>%
  bind_rows(tempoldlimits.4) %>%
  mutate(limit.date = as.Date(limit_date, format = "%d/%m/%y")) %>% # Make a usable date column
  select(-limit_date) %>%
  rename(limit_date = limit.date) %>%
  print(n = 50)

# Summaries of deposit limit use  ------------------------------------------------------------------------------
# First let's join the clients and limits datasets:
clients.limits4<- clients4 %>%
  full_join(templimits2.4) %>%
  print()

clients.limits4 %>% 
  group_by(condition, limit_window) %>%
  summarise(
    Individuals = n_distinct(customerid),
    Total.n = n()
  ) %>% 
  print (n = 42) # Calculate total number of deposit limits used across conditions during the full study period (July-Jan; ~190 days)

limit.summary<- clients.limits4 %>% count(limit_date) %>% 
  arrange(as.Date(limit_date, "%d/%m/%y")) %>%
  print (n = 450) # The total number of deposit limits set each day during the full study period (July-Jan; ~190 days)

# Recode deposit limit durations into a factor with clear names for ease and consistency with other operators
clients.limits1.4<- clients.limits4 %>% 
  transform(limit_window = factor(limit_window,
                                 levels = c(1, 7, 30, 365), 
                                 labels = c("DepositLimitDay",
                                            "DepositLimitWeek",
                                            "DepositLimitMonth",
                                            "DepositLimitYear"))) %>%
  as_tibble() %>%
  print()
# Check the values have recoded properly:
table(clients.limits4$limit_window)
# Output:
# 1  7 30 
# 11 24  3 
table(clients.limits1.4$limit_window)
# Output:
# DepositLimitDay  DepositLimitWeek DepositLimitMonth 
# 11                24                 3 

# ------------------------------------------------------------------------------
# Isolate and table those who set a limit within 5 days of viewing *either* message (also add demographic breakdown):
response.to.messages.4<- clients.limits1.4 %>% 
  filter(limit_date >= as.Date("2019-10-14") & 
           limit_date <= as.Date("2019-10-23")) %>%
  filter(limit_window == "DepositLimitDay"|
           limit_window == "DepositLimitWeek"|
           limit_window == "DepositLimitMonth"|
           limit_window == "DepositLimitYear") %>%
  group_by(condition, gender) %>%
  summarise(
    unique.participants = n_distinct(customerid),
    Total.participants = n(),
    m.age = mean(age),
    median.age = median(age),
    min.age = min(age),
    max.age = max(age),
  ) %>%
  print ()

sum(response.to.messages.4$Total.participants)# Total Limit setters with duplicates
sum(response.to.messages.4$unique.participants) # Total Limit setters without duplicates

# Isolate And table those who set a limit within 5 days of viewing the *first* message:
clients.limits1.4 %>% 
  filter(limit_date >= as.Date("2019-10-14") &
           limit_date <= as.Date("2019-10-18")) %>%
  filter(limit_window == "DepositLimitDay"| 
           limit_window == "DepositLimitWeek"| 
           limit_window == "DepositLimitMonth"|
           limit_window == "DepositLimitYear") %>%
  group_by(condition) %>%
  summarise(
    unique.participants = n_distinct(customerid),
    Total.participants = n()
  ) %>% 
  print ()

# Isolate and table those who set a limit within 5 days of viewing the *Second* message:
clients.limits1.4 %>% 
  filter(limit_date >= as.Date("2019-10-19") &
           limit_date <= as.Date("2019-10-23")) %>%
  filter(limit_window == "DepositLimitDay"| 
           limit_window == "DepositLimitWeek"| 
           limit_window == "DepositLimitMonth"|
           limit_window == "DepositLimitYear") %>%
  group_by(condition) %>%
  summarise(
    unique.participants = n_distinct(customerid),
    Total.participants = n()
  ) %>% 
  print ()

# ------------------------------------------------------------------------------
# End of summary statistics for operator 4
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Prepare datasets for confirmatory and exploratory analyses
# ------------------------------------------------------------------------------

# Responsible gambling tool datasets
# Start by adding separate columns that display RG tool use at the different time periods
# To avoid any errors in coding at this stage, all of the new columns include duplicates.

# LIMITS***
# Create a new column that easily splits the dataset into limit setters (i.e. Within the window of interest) and non-limit setters***
LimitSetter.window.column.4<- clients.limits1.4 %>% 
  as_tibble() %>%
  filter(limit_window == "DepositLimitDay"| 
           limit_window == "DepositLimitWeek"| 
           limit_window == "DepositLimitMonth"|
           limit_window == "DepositLimitYear") %>%
  filter(limit_date >= as.Date("2019-10-14") & 
           limit_date <= as.Date("2019-10-23")) %>%
  mutate(window_limit_setter = fct_recode(limit_window,
                                          "LimitSetter" = "DepositLimitDay",
                                          "LimitSetter" = "DepositLimitWeek",
                                          "LimitSetter" = "DepositLimitMonth",
                                          "limitSetter" = "DepositLimitYear")) %>%
  select(customerid,
         window_limit_setter,
         limit_date,
         limit_window,
         -condition,
         limit_amount) %>%
  rename(window_limit_date = limit_date,
         limit_window_setters = limit_window,
         window_limit_amount = limit_amount) %>%
  as_tibble() %>%
  arrange(window_limit_date) %>%
  print()

# Create a new column that Identifies anyone who set a limit prior to messages:  
LimitSetter.pre.column.4<- clients.limits1.4 %>%
  filter(limit_date < as.Date("2019-10-14") &
           limit_window == "DepositLimitDay" | 
           limit_window == "DepositLimitWeek" |
           limit_window == "DepositLimitMonth"|
           limit_window == "DepositLimitYear") %>%
  mutate(limit_setter_pre = fct_recode(limit_window,
                                       "PreLimitSetter" = "DepositLimitDay",
                                       "PreLimitSetter" = "DepositLimitWeek",
                                       "PreLimitSetter" = "DepositLimitMonth",
                                       "PrelimitSetter" = "DepositLimitYear")) %>%
  select(customerid, 
         limit_setter_pre, 
         limit_date,
         limit_window,
         -condition,
         limit_amount) %>%
  rename(limit_date_pre = limit_date,
         limit_amount_pre = limit_amount,
         limit_window_pre = limit_window) %>%
  as_tibble() %>%
  arrange(limit_date_pre) %>%
  print()

# Create a new column that Identifies *anyone* who set a limit post message one
# (allows for later excluding non-Window.limit.setters who set limits later on to be excluded from exploratory analyses of limit effectiveness)
LimitSetter.post.all.column.4<- clients.limits1.4 %>%
  filter(limit_window == "DepositLimitDay"| 
           limit_window == "DepositLimitWeek"| 
           limit_window == "DepositLimitMonth"|
           limit_window == "DepositLimitYear") %>%
  filter(limit_date > as.Date("2019-10-23")) %>%
  mutate(limit_setter_post = fct_recode(limit_window,
                                            "PostLimitSetter" = "DepositLimitDay",
                                            "PostLimitSetter" = "DepositLimitWeek",
                                            "PostLimitSetter" = "DepositLimitMonth",
                                        "PostlimitSetter" = "DepositLimitYear")) %>%
  select(customerid, 
         limit_setter_post,
         limit_date,
         limit_window,
         limit_amount,
         -condition) %>%
  rename(limit_date_post = limit_date,
         limit_window_post = limit_window,
         limit_amount_post = limit_amount) %>%
  as_tibble() %>%
  arrange(limit_date_post) %>%
  print()

# Now remove Window.limit.setters from this column to easily exclude non-Window.limit.setters who set limits later on:
anti_join(LimitSetter.post.all.column.4, 
          LimitSetter.window.column.4) %>%
  print()
# No window limit setters are included in this group and therefore no need to remove them

# TIMEOUTS***
# Now let's take a look at the timeout data 
timeout.group1.4 %>% 
  as_tibble() # No one in this group used the timeout feature
View(timeout.group1.4) # Double checked and this appears to be correct
timeout.group2.4 %>% 
  as_tibble()
timeout.group4.4 %>% 
  as_tibble()
timeout.group6.4 %>% 
  as_tibble()

# Variables seem consistent so let's join the datasets together:
timeout4 <- timeout.group2.4 %>% #Remember, no one in group 1 used this feature
  bind_rows(timeout.group4.4) %>%
  bind_rows(timeout.group6.4) %>%
  as_tibble() %>% # Make the timeout data workable
  separate(start_dt, into = c("start.date", "timeout.waste"), sep ="T") %>% # Make a usable date column for start
  separate(end_dt, into = c("end.date", "timeout.waste2"), sep ="T") %>% # Make a usable date column for end
  mutate(timeout_start_date = as.Date(start.date, format = "%Y-%m-%d")) %>%
  mutate(timeout_end_date = as.Date(end.date, format = "%Y-%m-%d")) %>%
  select(-timeout.waste,
         - start.date, 
         - end.date, 
         -timeout.waste2,
         - gamcare_id) %>%
  rename(customerid = account_id,
         timeout_start_time = start_time,
         timeout_end_time = end_time) %>%
  # mutate(timeout.duration =  timeout_start_date %--% timeout_end_date,
  #        timeout_duration = as.period(timeout.duration)) %>% #Another option
  mutate(timeout.duration = as.Date(timeout_end_date, format = "%Y-%m-%d") - 
           as.Date(timeout_start_date, format = "%Y-%m-%d")) %>% # Creating duration using start and end dates
  separate(timeout.duration, into = c("timeout_duration", "timeout.waste2"), sep =" ") %>% 
  select(-timeout.waste2,
         -limit_type) %>%
  arrange(timeout_start_date) %>%
  print()

# Cleaning note: Use this new data to check if anyone who was on timeout when messages were sent:
timeout4 %>%
  filter(timeout_start_date <= as.Date("2019-10-13")) %>%
  print()
# **No one was on timeout**

# # Create a new column to identify those who have used time outs prior to messages:
TimeOut.pre<- as.factor(rep(x = "TimeOut", times = 28)) # Create a character vector to identify this group
TimeOut.pre.column.4<- timeout4 %>%
  filter(timeout_start_date <= as.Date("2019-10-13")) %>%
  bind_cols(list(TimeOut.pre)) %>% # Add character vector
  rename(timeout_pre = ...8,
         timeout_start_date_pre = timeout_start_date,
         timeout_duration_pre =  timeout_duration,
         timeout_finish_date_pre = timeout_end_date) %>%
  select(-timeout_start_time,
         -timeout_end_time) %>% 
  print(n = 40)

# # Create a new column to identify those who used time outs following the first message
TimeOut.post<- as.factor(rep(x = "TimeOut", times = 4)) # Create a character vector to identify this group
TimeOut.post.column.4<- timeout4 %>%
  filter(timeout_start_date >= as.Date("2019-10-14")) %>%
  bind_cols(list(TimeOut.post)) %>% # Add character vector
  rename(timeout_post = ...8,
         timeout_start_date_post = timeout_start_date,
         timeout_duration_post =  timeout_duration,
         timeout_finish_date_post = timeout_end_date) %>% 
  select(-timeout_start_time,
         -timeout_end_time) %>%
  as_tibble() %>% 
  print(n = 40)

# SELF-EXCLUSION***
selfexclusion1.4 %>%
  as_tibble()
selfexclusion2.4 %>%
  as_tibble()
selfexclusion4.4 %>%
  as_tibble()
selfexclusion6.4 %>%
  as_tibble()

# Make the self exclusion data workable and...
# Create a new column to identify those who used self exclusion following the first message (which is all of those in the data)
Exclusion <- as.factor(rep(x = "SelfExcluder", times = 8)) # Create a character vector to identify this group
SelfExclusion.post.column.4<- selfexclusion1.4 %>% 
  bind_rows(selfexclusion2.4) %>%
  bind_rows(selfexclusion4.4) %>%
  bind_rows(selfexclusion6.4) %>%
  as_tibble() %>% # Make the data workable
  bind_cols(list(Exclusion)) %>%
  separate(start_date, into = c("self_exclusion_start_date", "Exclusion.waste"), sep ="T") %>% # Make a usable date column for start
  separate(end_date, into = c("self_exclusion_finish_date", "Exclusion.waste2"), sep ="T") %>% # Make a usable date column for start
  rename(self_exclusion = ...7,
         customerid = account_id,
         self_exclusion_start_time = start_time) %>%
   select(-Exclusion.waste,
          -Exclusion.waste2,
         -scenario,
         -end_time) %>%
  print()

# Add all new columns to dataset:
client.condition.limit4.final.withnas<- clients4 %>%
  full_join(LimitSetter.window.column.4, by = "customerid")  %>%
  full_join(LimitSetter.pre.column.4, by = "customerid")  %>%
  full_join(LimitSetter.post.all.column.4, by = "customerid") %>%
  full_join(TimeOut.pre.column.4, by = "customerid")  %>%
  full_join(TimeOut.post.column.4, by = "customerid")  %>%
  full_join(SelfExclusion.post.column.4, by = "customerid") %>%
  distinct(customerid, .keep_all = TRUE) %>% 
  as_tibble() %>%
  # rename(limit_window = limit_window.y) %>%
  print()

client.condition.limit4.final<- client.condition.limit4.final.withnas %>%
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') # Remove all the "NA" values in the factor columns just added

client.condition.limit4.final %>% 
  select(14:29) %>%
  print(n = 50) # Check new dataset has all merged correctly and "NA" values have been replaced

#
# TRANSACTIONS DATA -----
#
transactions1.4 %>% 
  distinct(account_id, .keep_all = TRUE) %>%
  as_tibble()
transactions2.4 %>% 
  distinct(account_id, .keep_all = TRUE) %>%
  as_tibble()
transactions4.4 %>% 
  distinct(account_id, .keep_all = TRUE) %>%
  as_tibble()
transactions6.4 %>%
  distinct(account_id, .keep_all = TRUE) %>%
  as_tibble()

names(transactions6.4)
# Merge, convert to tibble, convert date column to date format:
transactions4<- bind_rows(transactions1.4, 
                                         transactions2.4,
                                         transactions4.4,
                                         transactions6.4) %>% # Merge transactions datasets
  separate(cal_date_time, into = c("transaction.date", "transaction_time"), sep ="T") %>% # Make a usable date column   
  mutate(transaction_date = as.Date(transaction.date, format = "%Y-%m-%d")) %>%
  select(-transaction.date,
         -requestor,
         # -acc_bal_after_transaction,
         -dep_limit_after_trans, last_4_digits,
         -scenario,
         # -acc_bal_prior_transaction,
         -deposit_limit_type,
         -deposit_limit_set,
         -dep_limit_prior_trans,
         -method,
         -last_4_digits) %>% # Remove all the irrelevant columns
rename(customerid = account_id) %>%
  as_tibble() %>%
  print() 

transactions4 %>% distinct(customerid,
                           .keep_all = TRUE) %>% 
  nrow() # There are only transactions for around half the sample (1082)

transactions4 %>% summarise(
  range(as.Date(transaction_date)))

# What different types of transactions are there?
table(transactions4$trans_type)
# Bet Resettled        Credit         Debit       Deposit    Withdrawal
transactions4 %>%
  filter(trans_type == "Debit") 
# Debits appear to increase the persons account balance and there are only 19 of them
transactions4 %>%
  filter(trans_type == "Credit") 
# Credit appear to decrease the persons account balance and there are only 423 of them
# Remove that bet resettled, credit, debit transactions in next pipeline

# Let's check whether transaction IDs are unique to every transaction or whether there are any duplicated.
nrow(transactions4)
# Output: 11902

transactions4 %>% 
  distinct(account_trans_id, 
           customerid,
           transaction_date,
           transaction_time, .keep_all = TRUE) %>% 
  nrow() # Output: 11900
# Discrepancy = 2
# Spoke with a operator and this is most likely to be caused by the account transaction ID being cut-short during the data curation process

# Convert negative values to positives for withdrawals (e.g., convert "-50" to "50")
transaction.amount.converted<- abs(transactions4$amount) %>% 
  as_tibble() %>%
  rename(transaction_amount_converted = value) %>%
  print() #Check conversions are correct

# And new transaction amount column and create final transactions dataset for use:
transactions.combined.withlimits.4<- bind_cols(transactions4,
                                               transaction.amount.converted) %>%
  select(- amount) %>%
  filter(trans_type != "Debit" &
           trans_type != "Credit" &
           trans_type != "Bet Resettled") %>% # Remove all transaction types other than deposits and withdrawals 
  rename(transaction_amount = transaction_amount_converted,
         transaction_type = trans_type) %>%
  full_join(LimitSetter.window.column.4, 
            by = "customerid") %>% # Add column that identifies limit setters (i.e., within the window of interest) so that the data can be divided accordingly
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') %>%
  print()

# Create usable summary figures re transactions pre and post messages *****

# Cleaning note: There's a need to label all NA values returned 
# (i.e., no transactions as == "0" to allow for within group comparisons pre—post 
# when some individuals don't have any transactions for one of the time periods)
# do this after creating each set of summary values as it will be easier to isolate and replace the target NA values

# NON-limit setters----
# Summary transaction figures for non-limit setters *Pre* Message 1
transaction.summaries.pre.nonsetters.4<- transactions.combined.withlimits.4 %>% 
  filter(transaction_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  group_by(customerid, 
           transaction_type) %>%
  summarise(
    mean_transactionamount = mean(transaction_amount),
    median_transactionamount = median(transaction_amount),
    min_transactionamount = min(transaction_amount),
    max_transactionamount =  max(transaction_amount),
    total_transaction_amount = sum(transaction_amount),
    total_number_transactions = n()
  ) %>% pivot_wider(names_from = transaction_type, 
                    values_from = c(total_transaction_amount, 
                                    mean_transactionamount, 
                                    total_number_transactions, 
                                    median_transactionamount,
                                    min_transactionamount,
                                    max_transactionamount)) %>% 
  rename(total_deposit_amount_pre = total_transaction_amount_Deposit, 
         total_withdrawal_amount_pre = total_transaction_amount_Withdrawal, 
         mean_deposit_pre = mean_transactionamount_Deposit, 
         mean_withdrawal_pre = mean_transactionamount_Withdrawal, 
         number_of_deposits_pre = total_number_transactions_Deposit, 
         number_of_withdrawals_pre = total_number_transactions_Withdrawal,
         median_deposit_pre = median_transactionamount_Deposit,
         median_withdrawal_pre = median_transactionamount_Withdrawal,
         min_deposit_pre = min_transactionamount_Deposit,
         min_withdrawal_pre = min_transactionamount_Withdrawal,
         max_deposit_pre = max_transactionamount_Deposit,
         max_withdrawal_pre = max_transactionamount_Withdrawal
  ) %>% 
  mutate_if(is.numeric, list(~replace_na(.,0))) %>%
  print(n = 40)

names(transaction.summaries.pre.nonsetters.4) # Check all new names are correct

#Check accuracy of output against original transactions datasets using random customer ids:
transactions4 %>% 
  filter(transaction_date < as.Date("2019-10-14") &
           customerid == "****") %>% 
  print() # Select one of these customers for accuracy check

# Summary transaction figures for non-limit setters *Post* Message 1:
transaction.summaries.post.nonsetters.4<-  transactions.combined.withlimits.4 %>% 
  filter(transaction_date >= as.Date("2019-10-14") &
           transaction_date <= as.Date("2020-01-12") &
           window_limit_setter == "None") %>%
  group_by(customerid, 
           transaction_type) %>%
  summarise(
    mean_transaction_amount = mean(transaction_amount),
    median_transaction_amount = median(transaction_amount),
    min_transaction_amount = min(transaction_amount),
    max_transaction_amount =  max(transaction_amount),
    total_transaction_amount = sum(transaction_amount),
    total_number_transactions = n()
  ) %>% pivot_wider(names_from = transaction_type, 
                    values_from = c(total_transaction_amount, 
                                    mean_transaction_amount, 
                                    total_number_transactions, 
                                    median_transaction_amount,
                                    min_transaction_amount,
                                    max_transaction_amount)) %>% 
  rename(total_deposit_amount_post = total_transaction_amount_Deposit, 
         total_withdrawal_amount_post = total_transaction_amount_Withdrawal, 
         mean_deposit_post = mean_transaction_amount_Deposit, 
         mean_withdrawal_post = mean_transaction_amount_Withdrawal, 
         number_of_deposits_post = total_number_transactions_Deposit, 
         number_of_withdrawals_post = total_number_transactions_Withdrawal,
         median_deposit_post = median_transaction_amount_Deposit,
         median_withdrawal_post = median_transaction_amount_Withdrawal,
         min_deposit_post = min_transaction_amount_Deposit,
         min_withdrawal_post = min_transaction_amount_Withdrawal,
         max_deposit_post = max_transaction_amount_Deposit,
         max_withdrawal_post = max_transaction_amount_Withdrawal
  ) %>% 
  mutate_if(is.numeric,list(~replace_na(.,0))) %>% # Replace All NAs with 
  print(n = 40)

#Check accuracy of output against original transactions datasets using random customer ids:
transactions4 %>%  
  filter(transaction_date >= as.Date("2019-10-14") &
           customerid == "****") %>% # This person may be a bot – their median withdrawal is $55,000
  # Cleaning note: Need to check for bots in final data
  print() # Select one of these customers for accuracy check

# # Add all new datasets to temporary non-setter main dataset
client.condition.limits.transactionsnonsettersonly.4<- client.condition.limit4.final %>%
  filter(window_limit_setter == "None") %>%
  full_join(transaction.summaries.pre.nonsetters.4, by = "customerid") %>%
  full_join(transaction.summaries.post.nonsetters.4, by = "customerid") %>%
  print() # Merge with existing dataset

# # Identify new column range to selectively replace NA values:
ncol(client.condition.limits.transactionsnonsettersonly.4)
client.condition.limits.transactionsnonsettersonly.4 %>% 
  select(30:53) %>%
  print(n = 20)

# Replace NAs with 0 to reflect that no transactions were made, wherever relevant.
# Although we already remove any values from the original calculations, any participant without transactions for either period will have NA values once the datasets are merged
client.condition.limits.transactionsnonsettersonly.4[, c("total_deposit_amount_pre", 
                                                         "total_withdrawal_amount_pre", 
                                                         "mean_deposit_pre", 
                                                         "mean_withdrawal_pre", 
                                                         "number_of_deposits_pre", 
                                                         "number_of_withdrawals_pre",
                                                         "median_deposit_pre",
                                                         "median_withdrawal_pre",
                                                         "min_deposit_pre",
                                                         "min_withdrawal_pre",
                                                         "max_deposit_pre",
                                                         "max_withdrawal_pre", 
                                                         "total_deposit_amount_post", 
                                                         "total_withdrawal_amount_post", 
                                                         "mean_deposit_post", 
                                                         "mean_withdrawal_post", 
                                                         "number_of_deposits_post", 
                                                         "number_of_withdrawals_post",
                                                         "median_deposit_post",
                                                         "median_withdrawal_post",
                                                         "min_deposit_post",
                                                         "min_withdrawal_post",
                                                         "max_deposit_post",
                                                         "max_withdrawal_post") ][is.na(client.condition.limits.transactionsnonsettersonly.4[, c("total_deposit_amount_pre", 
                                                                                                                                                 "total_withdrawal_amount_pre", 
                                                                                                                                                 "mean_deposit_pre", 
                                                                                                                                                 "mean_withdrawal_pre", 
                                                                                                                                                 "number_of_deposits_pre", 
                                                                                                                                                 "number_of_withdrawals_pre",
                                                                                                                                                 "median_deposit_pre",
                                                                                                                                                 "median_withdrawal_pre",
                                                                                                                                                 "min_deposit_pre",
                                                                                                                                                 "min_withdrawal_pre",
                                                                                                                                                 "max_deposit_pre",
                                                                                                                                                 "max_withdrawal_pre", 
                                                                                                                                                 "total_deposit_amount_post", 
                                                                                                                                                 "total_withdrawal_amount_post", 
                                                                                                                                                 "mean_deposit_post", 
                                                                                                                                                 "mean_withdrawal_post", 
                                                                                                                                                 "number_of_deposits_post", 
                                                                                                                                                 "number_of_withdrawals_post",
                                                                                                                                                 "median_deposit_post",
                                                                                                                                                 "median_withdrawal_post",
                                                                                                                                                 "min_deposit_post",
                                                                                                                                                 "min_withdrawal_post",
                                                                                                                                                 "max_deposit_post",
                                                                                                                                                 "max_withdrawal_post") ])] <- 0


# Check accuracy of merge & NA replacement:
client.condition.limits.transactionsnonsettersonly.4 %>% 
  select(condition, 
         customerid,
         window_limit_setter,
         40:50) %>% 
  print(n = 40)

# LIMIT SETTERS----
# Calculate transaction summaries for Limit Setters *Pre* messages:
transaction.summaries.pre.setters.4<- transactions.combined.withlimits.4 %>% 
  filter(window_limit_setter == "LimitSetter") %>% 
  mutate(pre.limit.date = window_limit_date - days(90)) %>% 
  filter(transaction_date < as.Date(window_limit_date) & 
           transaction_date >= as.Date(pre.limit.date)) %>% 
  group_by(customerid,
           transaction_type) %>%
  summarise(
    mean_transaction_amount = mean(transaction_amount),
    median_transaction_amount = median(transaction_amount),
    min_transaction_amount = min(transaction_amount),
    max_transaction_amount =  max(transaction_amount),
    total_transaction_amount = sum(transaction_amount),
    total_number_transactions = n()
  ) %>% pivot_wider(names_from = transaction_type, 
                    values_from = c(total_transaction_amount, 
                                    mean_transaction_amount, 
                                    total_number_transactions, 
                                    median_transaction_amount,
                                    min_transaction_amount,
                                    max_transaction_amount)) %>% 
  rename(total_deposit_amount_pre = total_transaction_amount_Deposit, 
         total_withdrawal_amount_pre = total_transaction_amount_Withdrawal, 
         mean_deposit_pre = mean_transaction_amount_Deposit, 
         mean_withdrawal_pre = mean_transaction_amount_Withdrawal, 
         number_of_deposits_pre = total_number_transactions_Deposit, 
         number_of_withdrawals_pre = total_number_transactions_Withdrawal,
         median_deposit_pre = median_transaction_amount_Deposit,
         median_withdrawal_pre = median_transaction_amount_Withdrawal,
         min_deposit_pre = min_transaction_amount_Deposit,
         min_withdrawal_pre = min_transaction_amount_Withdrawal,
         max_deposit_pre = max_transaction_amount_Deposit,
         max_withdrawal_pre = max_transaction_amount_Withdrawal
  ) %>% 
  print(n = 40)

#Check accuracy of output against original transactions datasets using random customer ids:
transactions.combined.withlimits.4 %>% 
  mutate(pre.limit.date = window_limit_date - days(90)) %>%
  filter(transaction_date < as.Date(window_limit_date) & 
           transaction_date >= as.Date(pre.limit.date) &
           customerid == "****") %>% 
  print()

# Summary transaction figures for Limit Setters *Post* Message 1:
transaction.summaries.post.setters.4<- transactions.combined.withlimits.4 %>% 
  filter(window_limit_setter == "LimitSetter") %>% 
  mutate(pre.limit.date = window_limit_date + days(90)) %>% 
  filter(transaction_date > as.Date(window_limit_date) &
           transaction_date <= as.Date(pre.limit.date)) %>% 
  group_by(customerid,
           transaction_type) %>%
  summarise(
    mean_transaction_amount = mean(transaction_amount),
    median_transaction_amount = median(transaction_amount),
    min_transaction_amount = min(transaction_amount),
    max_transaction_amount =  max(transaction_amount),
    total_transaction_amount = sum(transaction_amount),
    total_number_transactions = n()
  ) %>% pivot_wider(names_from = transaction_type, 
                    values_from = c(total_transaction_amount, 
                                    mean_transaction_amount, 
                                    total_number_transactions, 
                                    median_transaction_amount,
                                    min_transaction_amount,
                                    max_transaction_amount)) %>% 
  rename(total_deposit_amount_post = total_transaction_amount_Deposit, 
         total_withdrawal_amount_post = total_transaction_amount_Withdrawal, 
         mean_deposit_post = mean_transaction_amount_Deposit, 
         mean_withdrawal_post = mean_transaction_amount_Withdrawal, 
         number_of_deposits_post = total_number_transactions_Deposit, 
         number_of_withdrawals_post = total_number_transactions_Withdrawal,
         median_deposit_post = median_transaction_amount_Deposit,
         median_withdrawal_post = median_transaction_amount_Withdrawal,
         min_deposit_post = min_transaction_amount_Deposit,
         min_withdrawal_post = min_transaction_amount_Withdrawal,
         max_deposit_post = max_transaction_amount_Deposit,
         max_withdrawal_post = max_transaction_amount_Withdrawal
  ) %>% 
  print(n = 40)

# Check accuracy of output against original transactions datasets using random customer ids:
transactions.combined.withlimits.4 %>% 
  mutate(post.limit.date = window_limit_date + days(90)) %>%
  filter(transaction_date > as.Date(window_limit_date) & 
           transaction_date <= as.Date(post.limit.date) &
           customerid == "****") %>% 
  print(n=44)

# Add all new datasets to temporary setter main dataset
client.condition.limits.transactionssettersonly.4<- client.condition.limit4.final %>%
  filter(window_limit_setter == "LimitSetter") %>%
  full_join(transaction.summaries.pre.setters.4, by = "customerid") %>%
  full_join(transaction.summaries.post.setters.4, by = "customerid") %>%
  print() # Merge with existing dataset


# # Identify new column range to selectively replace NA values
ncol(client.condition.limits.transactionssettersonly.4)
client.condition.limits.transactionssettersonly.4 %>% 
  select(30:53) %>%
  print(n = 20)

# Replace NAs with 0 to reflect that no transactions were made, wherever relevant.
client.condition.limits.transactionssettersonly.4[, c("total_deposit_amount_pre", 
                                                         "total_withdrawal_amount_pre", 
                                                         "mean_deposit_pre", 
                                                         "mean_withdrawal_pre", 
                                                         "number_of_deposits_pre", 
                                                         "number_of_withdrawals_pre",
                                                         "median_deposit_pre",
                                                         "median_withdrawal_pre",
                                                         "min_deposit_pre",
                                                         "min_withdrawal_pre",
                                                         "max_deposit_pre",
                                                         "max_withdrawal_pre", 
                                                         "total_deposit_amount_post", 
                                                         "total_withdrawal_amount_post", 
                                                         "mean_deposit_post", 
                                                         "mean_withdrawal_post", 
                                                         "number_of_deposits_post", 
                                                         "number_of_withdrawals_post",
                                                         "median_deposit_post",
                                                         "median_withdrawal_post",
                                                         "min_deposit_post",
                                                         "min_withdrawal_post",
                                                         "max_deposit_post",
                                                         "max_withdrawal_post") ][is.na(client.condition.limits.transactionssettersonly.4[, c("total_deposit_amount_pre", 
                                                                                                                                                 "total_withdrawal_amount_pre", 
                                                                                                                                                 "mean_deposit_pre", 
                                                                                                                                                 "mean_withdrawal_pre", 
                                                                                                                                                 "number_of_deposits_pre", 
                                                                                                                                                 "number_of_withdrawals_pre",
                                                                                                                                                 "median_deposit_pre",
                                                                                                                                                 "median_withdrawal_pre",
                                                                                                                                                 "min_deposit_pre",
                                                                                                                                                 "min_withdrawal_pre",
                                                                                                                                                 "max_deposit_pre",
                                                                                                                                                 "max_withdrawal_pre", 
                                                                                                                                                 "total_deposit_amount_post", 
                                                                                                                                                 "total_withdrawal_amount_post", 
                                                                                                                                                 "mean_deposit_post", 
                                                                                                                                                 "mean_withdrawal_post", 
                                                                                                                                                 "number_of_deposits_post", 
                                                                                                                                                 "number_of_withdrawals_post",
                                                                                                                                                 "median_deposit_post",
                                                                                                                                                 "median_withdrawal_post",
                                                                                                                                                 "min_deposit_post",
                                                                                                                                                 "min_withdrawal_post",
                                                                                                                                                 "max_deposit_post",
                                                                                                                                                 "max_withdrawal_post") ])] <- 0


# # Check accuracy of merge & NA replacement:
client.condition.limits.transactionssettersonly.4 %>% 
  select(condition, 
         customerid,
         window_limit_setter,
         40:50) %>% 
  print()

# 
# WAGERING DATA -----
#
wagers1.4 %>% 
  distinct(account_id, .keep_all = TRUE) %>%
  as_tibble()
wagers2.4 %>% 
  distinct(account_id, .keep_all = TRUE) %>%
  as_tibble()
wagers4.4 %>% 
  distinct(account_id, .keep_all = TRUE) %>%
  as_tibble()
wagers6.4 %>%
  distinct(account_id, .keep_all = TRUE) %>%
  as_tibble()

names(wagers6.4)
# Merge, convert to tibble, convert date column to date format:
wagers4<- bind_rows(wagers1.4, 
                    wagers2.4,
                    wagers4.4,
                    wagers6.4) %>% # Merge wager datasets
  mutate(wager_date = as.Date(placed_date, format = "%Y-%m-%d")) %>% # Make a usable date column
  rename(customerid = account_id) %>%
  select(-placed_date) %>%
  full_join(LimitSetter.window.column.4, by = "customerid") %>% # Enable differentiation between limit setters and non-setters
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') %>%
  mutate(wager_time = as.character(placed_time)) %>%
  as_tibble() %>%
  print() 

wagers4 %>% distinct(customerid, .keep_all = TRUE) %>% 
  nrow() # There are only transactions for around half the sample (1332)

# Let's also check whether wager IDs are unique to every wager or whether there are any duplicated.
nrow(wagers4)
# Output: 420,669

wagers4 %>%
  distinct(ref_id,
           customerid,
           wager_date,
           wager_time,
           .keep_all = TRUE) %>% 
  nrow() # Output: 379584
# This is a difference of 41,085 wagers — ~10% and therefore a reasonable proportion.

# Let's do some further digging 
# Check whether the wager types have anything to do with it (e.g.. "errors")
table(wagers4$win_lose) # 636 of wagers are errors — let's see how many of these have duplicated wager IDs

wagers4 %>%
  # filter(win_lose == "ERR") %>%
  distinct(ref_id, .keep_all = TRUE) %>% 
  group_by(win_lose) %>%
  count(win_lose) # There are only 90 unique wager IDs for the errors in the sample, suggesting many are duplicates.

unique(wagers4$ref_id[duplicated(wagers4$ref_id)]) # Isolate all duplicated wager IDs so that we can explore these

# Explore some random duplicated wagers to see what's going on:
wagers4 %>%
  filter(ref_id == "185085540684") # Wager duplicated three times (I.e. four instances in total) – all wager details are exactly the same 

wagers4 %>%
  filter(ref_id == "176809000000") # Wager not found
  
wagers4 %>%
  filter(ref_id == "175492826284") # Wager Duplicated once and there are differences in the win_lose column (one is an error)

wagers4 %>%
  filter(ref_id == "184054000000") # Wager not found

wagers4 %>%
  filter(ref_id == "183267009415") # Wager duplicated once and there are differences in the win_lose column (one is an error)

wagers4 %>%
  filter(ref_id == "173349623214") # Wager duplicated once and there are differences in the win_lose column (one is an error)

# Based on the above, all errors appear to be duplicates. Let's see if it works the other way around: 
# Filter for errors and then see if these have a matching ref_id:
wagers4 %>%
  filter(win_lose == "ERR") %>%
  select(1:10)

# Pick a random ID to test this:
wagers4 %>%
  filter(ref_id == "182872476454") # Yes this appears to work
# Once more:
wagers4 %>%
  filter(ref_id == "172239640718") # Yes worked again

# Emailed the operator to discuss this issue and the confirmed that only "ERR"s are exact duplicates. See below correspondence:
# *************
# "Where a market has been settled incorrectly, the originally entry is preserved with the ‘ERR’ code.
# The ‘ERR’ bet shows the original settlement’s effect on account balance at the time of settlement.  These bets can be deleted to get a better indication of aggregate account metrics.
# As for the other duplicates, it looks as though I’ve truncated the ref_id when saving the information in Excel’s standard format for groups 4 and 6.
# While these bets look similar, I believe they are all separate bets and should not be deleted.
# I’ve rerun the query for group 4 with the red_id preserved and found that there are no duplicates (aside from ‘ERR’)"
# *************

# Based on this finding let's remove all errors ("ERR"s) from the datasets going forward
wagers.workable4<- wagers4 %>%
  filter(win_lose != "ERR") %>%
  as_tibble() %>%
  print() 

# Before moving forward, let's figure out what equals a win (for the consumer) and a loss based on this data:
wagers.workable4 %>%
  filter(net_result >=0)  # Plus value net_results (e.g., 55) indicate the person won.
wagers.workable4 %>%
  filter(net_result <0) # Minus value net_results (e.g., -33) indicate the person lost 

# WAGERS NON-LIMIT SETTERS----
# Calculate wager summaries for non-limit setters *pre* Message 1:
wager.summaries.pre.nonsetters.4<- wagers.workable4 %>% 
  filter(wager_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  group_by(customerid) %>%
  summarise(
    total_number_of_bets_pre = n(),
    betting_days_frequency_pre = n_distinct(wager_date),
    total_amount_wagered_pre = sum(stake),
    total_amount_won_pre = sum(payout)
  ) %>%
  mutate(average_daily_wager_pre = total_amount_wagered_pre/ betting_days_frequency_pre) %>%
  mutate(betting_intensity_pre = total_number_of_bets_pre/ betting_days_frequency_pre) %>%
  mutate(net_loss_pre = total_amount_wagered_pre - total_amount_won_pre) %>%
  # select(4:3) %>% # View customer IDs for the check below
  print()

wager.summaries.pre.nonsetters.4 %>%
  filter(betting_days_frequency_pre < 5)

# Check outputs seem correct against a random customer ID:
wagers.workable4 %>% 
  filter(wager_date < as.Date("2019-10-14") &
           customerid == "****") %>%
  # summarise(n_distinct(wager_date)) %>%
  # (adding the "summarise" function to the above checks the total no. betting days is correct)
  print(n = 57)
# Note: Negative net loss outcomes indicate the person won money overall in the specified time period

# And new column for the total number of wagers in the 30 days prior to message 1 (eligibility criteria)----
wager.days.last.30.nonsetters.4<- wagers.workable4 %>% 
  filter(wager_date > as.Date("2019-09-13") &
           wager_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  group_by(customerid) %>%
  summarise(
    last30_betting_days_frequency = n_distinct(wager_date)) %>% 
  print(n = 100)

wager.days.last.30.nonsetters.4 %>%
  filter(last30_betting_days_frequency < 5)

# Add new column for final wager in the day before being sent message 1:
last.wager.column.nonsetters.4<- wagers.workable4 %>% 
  filter(wager_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  arrange(desc(wager_date,
               wager_time,
               customerid)) %>% 
  distinct(customerid, .keep_all = TRUE) %>%
  mutate(lost_last_bet = payout == 0) %>% 
  select(customerid, 
         lost_last_bet) %>%
  print(n = 400) # Checked for accuracy against last bets (i.e., without columns selected)

# Check to see if the distribution of the losses versus wins appears reasonable (yes):
table(last.wager.column.nonsetters.4$lost_last_bet)

# Add column for SD of daily wager:
daily.wager.SD.pre.nonsetters.4<- wagers.workable4 %>% 
  filter(wager_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  group_by(customerid,
           wager_date) %>%
  summarise(
    dailywager = sum(stake)
  ) %>%
  group_by(customerid) %>% 
  summarise(
    daily_wager_SD_pre = sd(dailywager)
  ) %>%
  print()

# Calculate wager summaries for non-limit setters *post* Message 1:
wager.summaries.post.nonsetters.4<- wagers.workable4 %>% 
  filter(wager_date > as.Date("2019-10-14") &
           wager_date <= as.Date("2020-01-12") &
           window_limit_setter == "None") %>%
  group_by(customerid) %>%
  summarise(
    total_number_of_bets_post = n(),
    betting_days_frequency_post = n_distinct(wager_date),
    total_amount_wagered_post = sum(stake),
    total_amount_won_post = sum(payout)
  ) %>%
  mutate(average_daily_wager_post = total_amount_wagered_post/ betting_days_frequency_post) %>%
  mutate(betting_intensity_post = total_number_of_bets_post/ betting_days_frequency_post) %>%
  mutate(net_loss_post = total_amount_wagered_post - total_amount_won_post) %>%
  # select(4:3) %>% # View customer IDs for the check below
  print()

# Add column for SD of daily wager:
daily.wager.SD.post.nonsetters.4<- wagers.workable4 %>% 
  filter(wager_date > as.Date("2019-10-14") &
           wager_date <= as.Date("2020-01-12") &
           window_limit_setter == "None") %>%
  group_by(customerid,
           wager_date) %>%
  summarise(
    dailywager = sum(stake)
  ) %>%
  group_by(customerid) %>% 
  summarise(
    daily_wager_SD_post = sd(dailywager)
  ) %>%
  print()

# Add all new datasets (pre/post message wagers for non-limit setters) to main temp non-setter dataset:
client.condition.limits.transactions.wagers.nonsettersonly.4<- client.condition.limits.transactionsnonsettersonly.4 %>% 
  full_join(wager.summaries.pre.nonsetters.4, by = "customerid") %>%
  full_join(wager.days.last.30.nonsetters.4, by = "customerid") %>%
  full_join(last.wager.column.nonsetters.4, by = "customerid") %>% 
  full_join(daily.wager.SD.pre.nonsetters.4, by = "customerid") %>% 
  full_join(wager.summaries.post.nonsetters.4, by = "customerid") %>% 
  full_join(daily.wager.SD.post.nonsetters.4, by = "customerid") %>% 
  select(-operator.y,
         -operator.x) %>%
  print() # Merge with existing dataset

# Check this new dataset:
client.condition.limits.transactions.wagers.nonsettersonly.4 %>% 
  filter(is.na(total_number_of_bets_post)) %>%
  group_by(customerid, 
           date_account_closed, 
           self_exclusion) %>%
  summarise(
    n(),
  ) %>%
  print(n = 200)# There are 1013 unique clients with no wagers post message which seems odd — more so than for the other operators
# Self excluders and those who closed the account will be removed from the data wherever appropriate....
# during the cleaning of the final, combined (all operators) dataset

# Identify new column range to selectively replace NA wager values to allow proper pre and post comparisons 
# NA values would exclude clients from within subjects comparisons, 
# even if they simply did not wager during this time and didn't close their account/self-exclude)
ncol(client.condition.limits.transactions.wagers.nonsettersonly.4)
client.condition.limits.transactions.wagers.nonsettersonly.4 %>% 
  select(total_number_of_bets_pre,
         betting_days_frequency_pre,
         total_amount_wagered_pre,
         total_amount_won_pre,
         average_daily_wager_pre,
         betting_intensity_pre,
         net_loss_pre,
         daily_wager_SD_pre,
         lost_last_bet,
         total_number_of_bets_post,
         betting_days_frequency_post,
         total_amount_wagered_post,
         total_amount_won_post,
         average_daily_wager_post,
         betting_intensity_post,
         net_loss_post,
         daily_wager_SD_post)

# Replace NAs with 0 0 to reflect that no wagers were made, wherever relevant:
client.condition.limits.transactions.wagers.nonsettersonly.4[, c("total_number_of_bets_pre",
                                                                 "betting_days_frequency_pre",
                                                                 "total_amount_wagered_pre",
                                                                 "total_amount_won_pre",
                                                                 "average_daily_wager_pre",
                                                                 "betting_intensity_pre",
                                                                 "net_loss_pre",
                                                                 "daily_wager_SD_pre",
                                                                 "lost_last_bet",
                                                                 "total_number_of_bets_post",
                                                                 "betting_days_frequency_post",
                                                                 "total_amount_wagered_post",
                                                                 "total_amount_won_post",
                                                                 "average_daily_wager_post",
                                                                 "betting_intensity_post",
                                                                 "net_loss_post",
                                                                 "last30_betting_days_frequency",
                                                                 "daily_wager_SD_post") ][is.na(
                                                                   client.condition.limits.transactions.wagers.nonsettersonly.4[, c("total_number_of_bets_pre",
                                                                                                                                    "betting_days_frequency_pre",
                                                                                                                                    "total_amount_wagered_pre",
                                                                                                                                    "total_amount_won_pre",
                                                                                                                                    "average_daily_wager_pre",
                                                                                                                                    "betting_intensity_pre",
                                                                                                                                    "net_loss_pre",
                                                                                                                                    "daily_wager_SD_pre",
                                                                                                                                    "lost_last_bet",
                                                                                                                                    "total_number_of_bets_post",
                                                                                                                                    "betting_days_frequency_post",
                                                                                                                                    "total_amount_wagered_post",
                                                                                                                                    "total_amount_won_post",
                                                                                                                                    "average_daily_wager_post",
                                                                                                                                    "betting_intensity_post",
                                                                                                                                    "net_loss_post",
                                                                                                                                    "last30_betting_days_frequency",
                                                                                                                                    "daily_wager_SD_post") ])] <- 0

client.condition.limits.transactions.wagers.nonsettersonly.4 %>% 
  select(55:68) # Check NAs were removed

# WAGERS LIMIT SETTERS----
# Calculate wager summaries for Limit Setters *Pre* messages:
wager.summaries.pre.setters.4<- wagers.workable4 %>% 
  filter(window_limit_setter == "LimitSetter") %>% 
  mutate(pre.limit.date = window_limit_date - days(90)) %>% 
  filter(wager_date < as.Date(window_limit_date) & 
           wager_date >= as.Date(pre.limit.date)) %>% 
  group_by(customerid) %>%
  summarise(
    total_number_of_bets_pre = n(),
    betting_days_frequency_pre = n_distinct(wager_date),
    total_amount_wagered_pre = sum(stake),
    total_amount_won_pre = sum(payout)
  ) %>%
  mutate(average_daily_wager_pre = total_amount_wagered_pre/ betting_days_frequency_pre) %>%
  mutate(betting_intensity_pre = total_number_of_bets_pre/ betting_days_frequency_pre) %>%
  mutate(net_loss_pre = total_amount_wagered_pre - total_amount_won_pre) %>%
  # select(4:3) %>% # View customer IDs for the check below
  print()

wager.summaries.pre.setters.4 %>%
  filter(betting_days_frequency_pre < 5)

# Check accuracy of output against random customer ids:
wagers.workable4 %>%
  mutate(pre.limit.date = window_limit_date - days(90)) %>% 
  filter(wager_date < as.Date(window_limit_date) & 
           wager_date >= as.Date(pre.limit.date) & 
           customerid == "****")  # %>% summarise(n_distinct(wager_date)) 

# And new column for the total number of wagers in the 30 days prior to message 1 (eligibility criteria)----
wager.days.last.30.setters.4<- wagers.workable4 %>% 
  filter(wager_date > as.Date("2019-09-13") &
           wager_date < as.Date("2019-10-14") &
           window_limit_setter == "LimitSetter") %>%
  group_by(customerid) %>%
  summarise(
    last30_betting_days_frequency = n_distinct(wager_date)) %>% 
  print(n = 100)

# Add new column for final wager in the day before setting their limit:
last.wager.column.setters.4<- wagers.workable4 %>% 
  filter(wager_date < as.Date(window_limit_date) & 
           window_limit_setter == "LimitSetter") %>%
  arrange(desc (wager_date,
                wager_time,
                customerid)) %>% 
  distinct(customerid, .keep_all = TRUE) %>%
  mutate(lost_last_bet = payout <= 0) %>% 
  select(customerid,
         lost_last_bet) %>%
  print() # Checked for accuracy against last bets (i.e., without columns selected)

# Add column for SD of daily wager:
daily.wager.SD.pre.setters.4<- wagers.workable4 %>% 
  filter(wager_date < as.Date("2019-10-14") &
           window_limit_setter == "LimitSetter") %>%
  group_by(customerid, 
           wager_date) %>%
  summarise(
    dailywager = sum(stake)
  ) %>%
  group_by(customerid) %>% 
  summarise(
    daily_wager_SD_pre = sd(dailywager)
  ) %>%
  print()

# Calculate wager summaries for Limit Setters *post* messages:
wager.summaries.post.setters.4<- wagers.workable4 %>% 
  filter(window_limit_setter == "LimitSetter") %>% 
  mutate(post.limit.date = window_limit_date + days(90)) %>% 
  filter(wager_date > as.Date(window_limit_date) &
           wager_date <= as.Date(post.limit.date)) %>% 
  group_by(customerid) %>%
  summarise(
    total_number_of_bets_post = n(),
    betting_days_frequency_post = n_distinct(wager_date),
    total_amount_wagered_post = sum(stake),
    total_amount_won_post = sum(payout)
  ) %>%
  mutate(average_daily_wager_post = total_amount_wagered_post/ betting_days_frequency_post) %>%
  mutate(betting_intensity_post = total_number_of_bets_post/ betting_days_frequency_post) %>%
  mutate(net_loss_post = total_amount_wagered_post - total_amount_won_post) %>%

  print()

wagers.workable4 %>%
  mutate(post.limit.date = window_limit_date + days(90)) %>% # Check accuracy of output against random customer ids 
  filter(wager_date > as.Date(window_limit_date) & 
           wager_date <= as.Date(post.limit.date) & 
           customerid == "****") # %>% summarise(n_distinct(wager_date)) 

# Add column for SD of daily wager:
daily.wager.SD.post.setters.4<- wagers.workable4 %>% 
  filter(wager_date > as.Date("2019-10-14") &
           window_limit_setter == "LimitSetter") %>%
  group_by(customerid, wager_date) %>%
  summarise(
    dailywager = sum(stake)
  ) %>%
  group_by(customerid) %>% 
  summarise(
    daily_wager_SD_post = sd(dailywager)
  ) %>%
  print()

client.condition.limits.transactions.wagerssetterssonly.4<- client.condition.limits.transactionssettersonly.4 %>% 
  full_join(wager.summaries.pre.setters.4, by = "customerid") %>%
  full_join(wager.days.last.30.setters.4, by = "customerid") %>%
  full_join(last.wager.column.setters.4, by = "customerid") %>%
  full_join(daily.wager.SD.pre.setters.4, by = "customerid") %>%
  full_join(wager.summaries.post.setters.4, by = "customerid") %>% 
  full_join(daily.wager.SD.post.setters.4, by = "customerid") %>%
  select(-operator.y,
         -operator.x) %>%
  print() # Merge with existing dataset

client.condition.limits.transactions.wagerssetterssonly.4 %>% 
  filter(is.na(total_number_of_bets_post))# Both clients in this group wagered after messages. 
# Two self excluded and two closed their account

# Replace NAs with 0 0 to reflect that no wagers were made, wherever relevant:
client.condition.limits.transactions.wagerssetterssonly.4[, c("total_number_of_bets_pre",
                                                              "betting_days_frequency_pre",
                                                              "total_amount_wagered_pre",
                                                              "total_amount_won_pre",
                                                              "average_daily_wager_pre",
                                                              "betting_intensity_pre",
                                                              "net_loss_pre",
                                                              "daily_wager_SD_pre",
                                                              "lost_last_bet",
                                                              "total_number_of_bets_post",
                                                              "betting_days_frequency_post",
                                                              "total_amount_wagered_post",
                                                              "total_amount_won_post",
                                                              "average_daily_wager_post",
                                                              "betting_intensity_post",
                                                              "net_loss_post",
                                                              "last30_betting_days_frequency",
                                                              "daily_wager_SD_post") ][is.na(
                                                                client.condition.limits.transactions.wagerssetterssonly.4[, c("total_number_of_bets_pre",
                                                                                                                                 "betting_days_frequency_pre",
                                                                                                                                 "total_amount_wagered_pre",
                                                                                                                                 "total_amount_won_pre",
                                                                                                                                 "average_daily_wager_pre",
                                                                                                                                 "betting_intensity_pre",
                                                                                                                                 "net_loss_pre",
                                                                                                                                 "daily_wager_SD_pre",
                                                                                                                                 "lost_last_bet",
                                                                                                                                 "total_number_of_bets_post",
                                                                                                                                 "betting_days_frequency_post",
                                                                                                                                 "total_amount_wagered_post",
                                                                                                                                 "total_amount_won_post",
                                                                                                                                 "average_daily_wager_post",
                                                                                                                                 "betting_intensity_post",
                                                                                                                                 "net_loss_post",
                                                                                                                                 "last30_betting_days_frequency",
                                                                                                                                 "daily_wager_SD_post") ])] <- 0
# Check the last function worked:
client.condition.limits.transactions.wagerssetterssonly.4 %>% 
  select("total_number_of_bets_pre",
         "betting_days_frequency_pre",
         "total_amount_wagered_pre",
         "total_amount_won_pre",
         "average_daily_wager_pre",
         "betting_intensity_pre",
         "net_loss_pre",
         "daily_wager_SD_pre",
         "lost_last_bet",
         "total_number_of_bets_post",
         "betting_days_frequency_post",
         "total_amount_wagered_post",
         "total_amount_won_post",
         "average_daily_wager_post",
         "betting_intensity_post",
         "net_loss_post",
         "last30_betting_days_frequency",
         "daily_wager_SD_post")

# ------------------------------------------------------------------------------
# COMBINE TRANSACTIONS & WAGER DATA SETS FOR BOTH GROUPS
client.condition.limits.transactions.wagers.4<- bind_rows(client.condition.limits.transactions.wagerssetterssonly.4, 
                                                          client.condition.limits.transactions.wagers.nonsettersonly.4) %>% 
  print()

client.condition.limits.transactions.wagers.4  %>% 
  select("total_number_of_bets_pre",
         "betting_days_frequency_pre",
         "total_amount_wagered_pre",
         "total_amount_won_pre",
         "average_daily_wager_pre",
         "betting_intensity_pre",
         "net_loss_pre",
         "daily_wager_SD_pre",
         "lost_last_bet",
         "total_number_of_bets_post",
         "betting_days_frequency_post",
         "total_amount_wagered_post",
         "total_amount_won_post",
         "average_daily_wager_post",
         "betting_intensity_post",
         "net_loss_post",
         "daily_wager_SD_post")

# Explore net loss summaries:
client.condition.limits.transactions.wagers.4 %>% 
  group_by(window_limit_setter) %>%
  summarise(
    mean(net_loss_pre, na.rm = TRUE),
    median(net_loss_pre, na.rm = TRUE),
    max(net_loss_pre, na.rm = TRUE),
    mean(net_loss_post, na.rm = TRUE),
    median(net_loss_post, na.rm = TRUE),
    max(net_loss_post, na.rm = TRUE)
  )  

client.condition.limits.transactions.wagers.4 %>% 
  group_by(window_limit_setter) %>%
  count(lost_last_bet) # Limit setters & non-setters proportions in relation to whether their last bet pre-limit/message was a win/loss

client.condition.limits.transactions.wagers.4 %>% 
  filter(net_loss_pre >= 0) %>%
  group_by(window_limit_setter) %>%
  summarise(
    n() # Most clients in both groups lost money prior to messages
  )

client.condition.limits.transactions.wagers.4 %>%
  filter(net_loss_pre < 0) %>%
  group_by(window_limit_setter) %>%
  summarise(
    n())

# ------------------------------------------------------------------------------
# Finalise dataset for analyses

# Create operator column and label to identify client operator:
Operator<- rep(x = 4, times = 2002) %>% 
  as_tibble() %>% 
  rename(operator = value) %>%
  print()

client.condition.limits.transactions.wagers.final.4<- bind_cols(Operator, # Add operator column and label to identify client operator
                                                                client.condition.limits.transactions.wagers.4) %>% 
  print()

names4<- names(client.condition.limits.transactions.wagers.final.4)
write.csv(names4, file = "Analysis data/final_names4.csv") # Create a CSV file from the final dataset 

# Create a CSV file from the final dataset so that it can be easily accessed and merged with data sets from other operators:
write.csv(client.condition.limits.transactions.wagers.final.4, file = "Analysis data/Operator 4/Final_dataset_Operator4.csv")

# End 