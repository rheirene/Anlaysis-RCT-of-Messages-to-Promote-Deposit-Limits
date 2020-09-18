# Script to clean, wrangle and combine datasets and calculate some descriptive stats for operator 2
# Note: All customer IDs in the code have been removed to protect the anonymity of customers involved in the trial

# Load any relevant packages
library("ggplot2")
library("forcats")
library("data.table")
library("tidyr")
library("dplyr")
library("yarrr")
library("lubridate")
library("cowplot")
library("readr")

#Remove all objects from work space
rm(list = ls())

# load and name all datasets
clients2<- read.csv("Analysis data/Operator 2/clients2.csv") %>% 
  rename(customerid = ClientID) %>%
  slice_head(n = 9987) # There are 9987 rows in this dataset, but numerous "NA" rows after the data that require removing
prelimresults2<- read.csv("Analysis data/Operator 2/prelimresults2.csv") %>% 
  rename(customerid = DUMMY_CLIENTID)
limits2<- read.csv("Analysis data/Operator 2/depositlimit2.csv") %>% 
  rename(customerid = ClientID) %>%
  slice_head(n = 3117) # There are 3117 rows in this dataset, but numerous "NA" rows after the data that require removing
timeout2<- read.csv("Analysis data/Operator 2/timeout2.csv") %>% 
  rename(customerid = ClientID)
selfexclusion2<- read.csv("Analysis data/Operator 2/selfexclusion2.csv") %>% 
  rename(customerid = ClientID) %>%
  slice_head(n = 25) # There are 25 rows in this dataset, but numerous "NA" rows after the data that require removing
transactions2<- read.csv("Analysis data/Operator 2/transactions2.csv") %>% 
  rename(customerid = clientid)
wagers2<- read.csv("Analysis data/Operator 2/wagers2.csv") 

# ------------------------------------------------------------------------------
# Explore the data, check column names are present, and check there are ~10,000 customers:

# Start with preliminary results (which contains condition numbers) and clients datasets.

# Check the number of unique customers is consistent between the preliminary and final results (yep)
clients2 %>%
  distinct(customerid) %>%
  nrow()

prelimresults2 %>% 
  distinct(customerid) %>%
  nrow() 
# There are a total of 9987 participants in this data

# Let's make these two datasets consistent and workable:
head(clients2)
head(prelimresults2)
# Check if you get the correct number of customers when joining by customer ID only:
clients2 %>% 
  full_join(prelimresults2, by = "customerid") %>%
  distinct(customerid) %>%
  nrow()
# Yes, let's join these together and make workable date columns:
clients.conditions2 <- prelimresults2 %>%
  select(SEGMENT, customerid) %>% 
  full_join(clients2, by = "customerid") %>%
  as_tibble() %>%
  separate(Date_Account_Opened, into = c("Date_Account_Opened.old", "time_account_opened"), sep =" ") %>%
  mutate(date_account_opened = as.Date(Date_Account_Opened.old, "%d/%m/%Y")) %>%
  mutate(date_account_closed = as.Date(Date_account_Closed, "%d/%m/%y")) %>%
  select(-Date_Account_Opened.old, - Date_account_Closed) %>%
  rename(age = Age, gender = Gender, 
         condition = SEGMENT, 
         postcode = Postcode) %>% # Rename columns for consistency with other operators
  print()
  
# Calculate demographic characteristics for the sample ------------------------------------------------------------------------------
clients.conditions2 %>% 
  distinct() %>%
  count(condition) # total number of participants per condition
# Outputs with original condition names
# condition             n
# <fct>             <int>
# 1 Group 1 - Control  1299
# 2 Group 2             875
# 3 Group 3            2026
# 4 Group 4             966
# 5 Group 5            1972
# 6 Group 6             900
# 7 Group 7            1949

# Recode condition and gender names ease and consistency with the other operators:
clients.conditions1.2<- clients.conditions2 %>% 
  mutate(condition = fct_recode(condition, 
                                '1' = "Group 1 - Control",
                                '2' = "Group 2",
                                '3' = "Group 3",
                                '4' = "Group 4",
                                '5' = "Group 5",
                                '6' = "Group 6",
                                '7' = "Group 7")) %>% 
  mutate(gender = fct_recode(gender, 
                             U = "UNK")) %>%
  print()

clients.conditions1.2 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  count(gender) # gender distribution (total sample, duplicates removed) 

clients.conditions1.2 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  summarise(
    agemean = mean(age),
    agemedian = median(age),
    agemin = min(age),
    agemax = max(age),
    n = n()
  )  # summary figures for age (total sample, duplicates removed)

demographic.summary.2<- clients.conditions1.2 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  group_by(condition, 
           gender) %>% 
  summarise(
    agemean = mean(age),
    agemedian = median(age),
    agemin = min(age),
    agemax = max(age),
    n = n()
  ) %>%
  print() # Demographic breakdown by condition and gender

clients.conditions1.2 %>% group_by(condition, 
                                   gender) %>% 
  summarise(
    total = n()
    ) %>% 
  group_by(condition) %>% 
  mutate(percentage = total/sum(total)*100) # Percentage of males and females in each condition

clients.conditions1.2 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  group_by(condition) %>% 
  summarise(
    agemean = mean(age),
    agemedian = median(age),
    agemin = min(age),
    agemax = max(age),
    n = n()
  ) # Demographic breakdown by condition only

# Summaries of deposit limit use  ------------------------------------------------------------------------------
#Check deposit limits datasets before use
head(limits2)
head(prelimresults2)

# Check dates are in interpretable formats (previous issues with this operator)
table(prelimresults2$Previous.Limit.Start.Date)
table(prelimresults2$Current.Limit.Start.Date)

# Merge limits2 data set (which contains all deposit limits set within the study period)...
# with a dataset that contains all limits set prior to the study period (prelimresults2)
names(limits2) # First make all column names consistent
names(prelimresults2) # First make all column names consistent

limits.1.2<- limits2 %>% rename(deposit_limit_id = DepLimitID, 
                                       operator = Operator, 
                                       limit.date = Date, 
                                       limit_time = Time, 
                                       previous_limit_amount = PreviousLimit,
                                       previous_limit_window = PreviousWindow,
                                       limit_amount = NewLimit,
                                       limit_window = NewWindow) %>% # Rename all relevant columns
  as_tibble() %>%
  mutate(limit_date = as.Date(limit.date, "%d/%m/%y")) %>% # Make a workable date column
  select(-limit.date) %>%
  (print)

prelimresults2 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
                              count(Limit.Between.2019.07.16...2020.01.17)

# Merge limit datasets and then merge them with clients and conditions datasets to be able to provide summary figures
# First, see whether the "Previous.Deposit.Limit" column in the preliminary data represents the limit window or limit amount
table(prelimresults2$Previous.Deposit.Limit) # == limit amount

limits.2.2<- prelimresults2 %>% select(customerid,
                                Previous.Limit.Start.Date, 
                                limit_amount = Previous.Deposit.Limit) %>% # Rename all relevant columns
  as_tibble() %>%
  separate(Previous.Limit.Start.Date, into = c("limit.date", "limit_time"), sep =" ") %>%
  mutate(limit_date = as.Date(limit.date, "%d/%m/%Y")) %>% # Make a workable date column
  select(-limit.date) %>%
  bind_rows(limits.1.2) %>% # Merge Limit dataset with clients dataset
  as_tibble() %>%
  select(-deposit_limit_id, - operator, -previous_limit_amount, - previous_limit_window) %>%
  arrange(limit_window) %>%
  print(n = 30) 

# Check new dataset
tail(limits.2.2)
limits.2.2 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  filter(limit_amount != is.na(limit_amount)) # See total number of unique limit setters

limits.2.2 %>% 
  filter(limit_amount < 0) # There appears to be approximately 130 customers with their limit is set as a minus value...
# All of which had a previous limit. Contacted operator to enquire about this
limits.2.2 %>% 
  filter(limit_amount < 0) %>%
  summarise(
    range(limit_amount)
  ) # All minus values are "-1"
# Minus values are only present in the "limits2" dataset:
summary(limits2$PreviousLimit)
summary(prelimresults2$Previous.Deposit.Limit)

# Cleaning note: Contacted the operator and found out that all -1 values represent removal of the limit.
# Remove these from all future figures

# Merge Limit dataset with clients dataset:
clients.condition.limit2<- clients.conditions1.2 %>%
  full_join(limits.2.2, by = "customerid") %>% 
  as_tibble() %>%
  print(n = 30) 

clients.condition.limit2 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  filter(limit_amount != is.na(limit_amount)) # Check there are the same number of limits set

clients.condition.limit2 %>% 
  group_by(condition, limit_window) %>%
  filter(limit_amount >= 0) %>%
  summarise(
    Individuals = n_distinct(customerid),
    Total.n = n()
  ) %>% 
  print (n = 30) # Calculate total number of deposit limits used across conditions ever

limit.summary<- clients.condition.limit2 %>%
  filter(limit_amount >= 0) %>%
  distinct(customerid, limit_date, .keep_all = TRUE) %>% 
  count(limit_date) %>% 
  arrange(as.Date(limit_date, "%d/%m/%y")) %>%
  filter(limit_date != is.na(limit_date)) %>%
  print (n = 250) # The total number of deposit limits set each day

# Recode deposit limit durations into a factor with clear names for ease and consistency with other operators:
table(clients.condition.limit2$limit_window)
client.condition.limit2.2<- clients.condition.limit2 %>% 
  transform(limit_window = factor(limit_window,
                                 levels = c(1, 7, 30), 
                                 labels = c("DepositLimitDay",
                                            "DepositLimitWeek",
                                            "DepositLimitMonth"))) %>%
  as_tibble() %>%
  print()

table(client.condition.limit2.2$limit_window) # How many of each? (With duplicates)

# -----------------------------------------------------------------------------
# Isolate and table those who set a limit within 5 days of viewing *either* message (also add demographic breakdown)
response.to.messages<- client.condition.limit2.2 %>% 
  filter(limit_date >= as.Date("2019-10-14") &
           limit_date <= as.Date("2019-10-23") &
           limit_amount > 0) %>%
  filter(limit_window == "DepositLimitDay"|
           limit_window == "DepositLimitWeek"|
           limit_window == "DepositLimitMonth") %>%
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

sum(response.to.messages$Total.participants)# Total Limit setters with duplicates
sum(response.to.messages$unique.participants) # Total Limit setters without duplicates

# Isolate and table those who set a limit within 5 days of viewing the *first* message
response.to.message1<- client.condition.limit2.2 %>% 
  filter(limit_date >= as.Date("2019-10-14") & 
           limit_date <= as.Date("2019-10-18") &
           limit_amount > 0) %>%
  filter(limit_window == "DepositLimitDay"|
           limit_window == "DepositLimitWeek"|
           limit_window == "DepositLimitMonth") %>%
  group_by(condition) %>%
  summarise(
    unique.participants = n_distinct(customerid),
    Total.participants = n()
  ) %>% 
  print ()

sum(response.to.message1$Total.participants) # Total Limit setters with duplicates (Message 2)
sum(response.to.message1$unique.participants) # Total Limit setters without duplicates (Message 2)

# Isolate and table those who set a limit within 5 days of viewing the *Second* message
response.to.message2<- client.condition.limit2.2 %>% 
  filter(limit_date >= as.Date("2019-10-19") &
           limit_date <= as.Date("2019-10-23") &
           limit_amount > 0) %>%
  filter(limit_window == "DepositLimitDay"|
           limit_window == "DepositLimitWeek"|
           limit_window == "DepositLimitMonth") %>%
  group_by(condition) %>%
  summarise(
    unique.participants = n_distinct(customerid),
    Total.participants = n()
  ) %>% 
  print ()

sum(response.to.message2$Total.participants)# Total Limit setters with duplicates (Message 2)
sum(response.to.message2$unique.participants) # Total Limit setters without duplicates (Message 2)

# ------------------------------------------------------------------------------
# End of summary statistics for operator 2
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Prepare datasets for confirmatory and exploratory analyses
# ------------------------------------------------------------------------------

# Responsible gambling tool dataset
# Start by adding separate columns that display RG tool use at the different time periods
# To avoid any errors in coding at this stage, all of the new columns include duplicates.

#
# LIMITS -----
#

# Create a new column that easily splits the dataset into limit setters (i.e. within the window of interest) and non-limit setters***
LimitSetter.window.column.2<- client.condition.limit2.2 %>% 
  filter(limit_window == "DepositLimitDay"| 
           limit_window == "DepositLimitWeek"|
           limit_window == "DepositLimitMonth") %>%
  filter(limit_amount > 0 &
         limit_date >= as.Date("2019-10-14") & 
         limit_date <= as.Date("2019-10-23")) %>%
  mutate(window_limit_setter = fct_recode(limit_window,
                                          "LimitSetter" = "DepositLimitDay",
                                          "LimitSetter" = "DepositLimitWeek",
                                          "LimitSetter" = "DepositLimitMonth")) %>%
  select(customerid,
         window_limit_setter, 
         limit_date,
         limit_amount,
         limit_window) %>%
  rename(window_limit_date = limit_date,
         limit_window_setters = limit_window,
         window_limit_amount= limit_amount) %>%
  arrange(window_limit_date) %>%
  print(n = 128)

LimitSetter.window.column.2 %>%
  distinct(customerid)

# Create a new column that Identifies anyone who set a limit prior to messages  
LimitSetter.pre.column.2<- client.condition.limit2.2 %>%
  filter(limit_date < as.Date("2019-10-14") & 
           limit_amount > 0 &
           limit_window == "DepositLimitDay" | 
           limit_window == "DepositLimitWeek" |
           limit_window == "DepositLimitMonth") %>%
  mutate(limit_setter_pre = fct_recode(limit_window,
                                       "PreLimitSetter" = "DepositLimitDay",
                                       "PreLimitSetter" = "DepositLimitWeek",
                                       "PreLimitSetter" = "DepositLimitMonth")) %>%
  select(customerid,
         limit_setter_pre,
         limit_date,
         limit_window,
         limit_amount) %>%
  rename(limit_date_pre = limit_date,
         limit_amount_pre = limit_amount,
         limit_window_pre = limit_window) %>%
  arrange(limit_date_pre) %>%
  print(n = 100)

# Create a new column that Identifies *anyone* who set a limit post message one
# (allows for later excluding non-Window.limit.setters who set limits later on to be excluded from exploratory analyses of limit effectiveness)
LimitSetter.post.all.column.temp.2<- client.condition.limit2.2 %>%
  filter(limit_window == "DepositLimitDay"|
           limit_window == "DepositLimitWeek"|
           limit_window == "DepositLimitMonth") %>%
  filter(limit_date > as.Date("2019-10-13")) %>%
  mutate(limit_setter_post = fct_recode(limit_window,
                                            "PostLimitSetter" = "DepositLimitDay",
                                            "PostLimitSetter" = "DepositLimitWeek",
                                            "PostLimitSetter" = "DepositLimitMonth")) %>%
  select(customerid,
         limit_setter_post, 
         limit_window,
         limit_date,
         limit_amount) %>%
  rename(limit_date_post = limit_date,
         limit_window_post = limit_window,
         limit_amount_post = limit_amount) %>%
  arrange(limit_date_post) %>%
  print(LimitSetter.post.all.column, n = 50)

# Check to see if any window limit setters later removed their limit:
client.condition.limit2.2 %>%
  select(-limit_time,
         -limit_amount,
         -limit_date,
         -limit_window) %>%
  full_join(LimitSetter.window.column.2, by = "customerid")  %>%
  full_join(LimitSetter.post.all.column.temp.2, by = "customerid")  %>%
  filter(limit_setter_post == "PostLimitSetter" & 
           window_limit_setter == "LimitSetter") 
# No one removed their limit

# Now remove Window.limit.setters from this column to easily exclude non-Window.limit.setters who set limits later on:
LimitSetter.post.all.column.2<- anti_join(LimitSetter.post.all.column.temp.2,
                                          LimitSetter.window.column.2) %>%
  filter(limit_amount > 0) # Also exclude all of those who just removed their limits
  print()
LimitSetter.post.all.column.2 %>%
  filter(customerid == "****") # Check random window limit setters aren't in this dataset

# Check limit setter window column isn't affected by removing those who set limits after messages who weren't window limit setters:
anti_join(LimitSetter.window.column.2,
          LimitSetter.post.all.column.2) %>%
  print()

#
# TIMEOUTS ----
#
head(timeout2)

# Make the timeout data workable
timeout1.2<- timeout2 %>% 
  as_tibble() %>%
  mutate(timeout_start_date = as.Date(Start_date, format = "%d/%m/%y")) %>%
  mutate(timeout_end_date = as.Date(Finish_date, format = "%d/%m/%y")) %>%
  select(-Operator, 
         - Start_date, 
         - Finish_date,
         - TimeoutID) %>%
  rename(timeout_start_time = Start_time,
         timeout_end_time = Finish_time,
         timeout_duration = Duration) %>%
  arrange(timeout_start_date) %>%
  print()

# Cleaning note: Use this new data to check if anyone who was on timeout when messages were sent:
timeout1.2 %>%
  filter(timeout_start_date <= as.Date("2019-10-13")) %>%
  print(n = 104)
# **No one was on timeout**

# # Create a new column to identify those who have used time outs prior to messages
TimeOut.pre<- as.factor(rep(x = "TimeOutPre", times = 104)) # Create a character vector to identify this group
TimeOut.pre.column.2<- timeout1.2 %>%
  filter(timeout_start_date <= as.Date("2019-10-13")) %>%
  bind_cols(list(TimeOut.pre)) %>% # Add character vector
  rename(timeout_pre = ...7,
         timeout_duration_pre = timeout_duration,
         timeout_start_date_pre = timeout_start_date,
         timeout_finish_date_pre = timeout_end_date) %>% 
  select(-timeout_start_time, -timeout_end_time) %>% 
  print()

# # Create a new column to identify those who used time outs following the first message
TimeOut.post<- as.factor(rep(x = "TimeOutPost", times = 240)) # Create a character vector to identify this group
TimeOut.post.column.2<- timeout1.2 %>%
  filter(timeout_start_date > as.Date("2019-10-13")) %>%
  bind_cols(list(TimeOut.post)) %>% # Add character vector
  rename(timeout_post = ...7,
         timeout_duration_post = timeout_duration,
         timeout_start_date_post = timeout_start_date,
         timeout_finish_date_post = timeout_end_date) %>% 
  select(-timeout_start_time, -timeout_end_time) %>% 
  arrange(timeout_start_date_post) %>%
  print()

#
# SELF EXCLUSION ----
#
selfexclusion2
# Note: This operator only offers permanent self-exclusion and so there are none pre-messages

# Make the self exclusion data workable
# First, create a new column to identify those who used self exclusion following the first message
Self.Excluded<- as.factor(rep(x = "SelfExcluder", times = 25)) # Create a character vector to identify this group
SelfExclusion.post.column.2<- selfexclusion2 %>%
  bind_cols(list(Self.Excluded)) %>% # Add character vector
  mutate(self_exclusion_start_date = as.Date(Start_date, format = "%d/%m/%y")) %>%
  select(-Operator, - Start_date) %>%
  rename(self_exclusion = ...5,
    self_exclusion_start_time = Start_time) %>%
  arrange(self_exclusion_start_date) %>%
  as_tibble() %>%
  print()

# Add all new columns to dataset
client.condition.limit2.final.withnas<- client.condition.limit2.2 %>%
  select(-limit_time,
         -limit_amount,
         -limit_date,
         -limit_window) %>%
  full_join(LimitSetter.window.column.2, by = "customerid")  %>%
  full_join(LimitSetter.pre.column.2, by = "customerid")  %>%
  full_join(LimitSetter.post.all.column.temp.2, by = "customerid")  %>%
  full_join(TimeOut.pre.column.2, by = "customerid")  %>%
  full_join(TimeOut.post.column.2, by = "customerid")  %>%
  full_join(SelfExclusion.post.column.2, by = "customerid") %>%
  distinct(customerid, .keep_all = TRUE) %>% 
  # rename(limit_window = limit_window.y) %>%
  print(n = 200)

           
client.condition.limit2.final<- client.condition.limit2.final.withnas %>%
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') # Remove all the "NA" values in the factor columms just added

client.condition.limit2.final %>% select(20:28) %>%
  print(n=200) # Check new dataset has all merged correctly and "NA" values have been replaced

names(client.condition.limit2.final) # Output:
# "condition"                 "customerid"                "age"                       "gender"                    "postcode"                 
# "time_account_opened"       "date_account_opened"       "date_account_closed"       "window_limit_setter"       "window_limit_date"        
# "limit_window_setters"      "limit_setter_pre"          "limit_date_pre"            "limit_window_pre"          "limit_setter_post"        
# "limit_window_post"         "limit_date_post"           "timeout_duration_pre"      "timeout_start_date_pre"    "timeout_finish_date_pre"  
# "timeout_pre"               "timeout_duration_post"     "timeout_start_date_post"   "timeout_finish_date_post"  "timeout_post"             
# "self_exclusion_start_time" "self_exclusion"            "self_exclusion_start_date"

#
# TRANSACTIONS DATA -----
#
head(transactions2)

# Add pre and post-message transactions datasets and combine them
transactions.tib.2<- transactions2 %>% 
  as_tibble() %>% 
  rename(transaction_type = Trans_type,
         transaction_amount = Amount) %>%
  mutate(transaction_date = as.Date(Date, "%d/%m/%y")) %>%
  select(-Date,
         -Operator,
         -Requestor
         ) %>%
  print(n = 20) # Convert to tibble, convert date column to date format, and check setup of the dataset 

# Let's check whether transaction IDs are unique to every transaction or whether there are any duplicated.
nrow(transactions.tib.2)
# Output: 418145

transactions.tib.2 %>% 
  distinct(AccountTransID, 
           customerid,
           transaction_date,
           Time, .keep_all = TRUE) %>% 
  nrow() # Output: 418145
# All unique :-)

table(transactions.tib.2$transaction_type) # Check all of the different transaction types within the data

transactions.tib.2 %>%
  filter(transaction_type == "Deposit" | 
           transaction_type == "Withdrawal") %>% # isolate the relevant transaction types
  group_by(transaction_type) %>%
  summarise(
    range(transaction_amount) # See whether minus values are given for transactions
  ) # According to this, there are minus values for both deposits and withdrawals. Let's see how many are minus values in each:

transactions.tib.2 %>%
  filter(transaction_type == "Deposit" |
           transaction_type == "Withdrawal") %>% # isolate the relevant transaction types
  group_by(transaction_type) %>%
  filter(transaction_amount < 0) %>%
  summarise(
    range(transaction_amount),
    n()) # Only six withdrawals are in positive figures and only one deposit has a minus value (let's remove these from the data before moving forward)

transactions.1.2 <- transactions.tib.2 %>%
  filter(transaction_type == "Deposit" &
           transaction_amount >= 0|
           transaction_type == "Withdrawal" &
           transaction_amount < 0) %>% # isolate the relevant transaction types & remove irregular values (see above)
  print() 

# Convert negative values to positives for withdrawals (e.g., convert "-50" to "50")
transaction.amount.converted<- abs(transactions.1.2$transaction_amount) %>% 
  as_tibble() %>%
  rename(transaction_amount_converted = value) %>%
  print() # Check conversions are correct
# Check conversions marry with the original values using random customers:
transactions.1.2 %>% 
  filter(customerid == "****" &
           AccountTransID == "413393645")

# And new transaction amount column and create final transactions dataset for use:
transactions.combined.withlimits.2<- bind_cols(transactions.1.2, 
                                               transaction.amount.converted) %>%
  select(- transaction_amount) %>%
  full_join(LimitSetter.window.column.2, by = "customerid") %>% # Add column that identifies limit setters (i.e., within the window of interest) so that the data can be divided accordingly
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') %>%
  rename(transaction_amount = transaction_amount_converted) %>%
  print()

transactions.combined.withlimits.2 %>% 
  select(customerid, transaction_type, window_limit_setter) %>%
  arrange(window_limit_setter, customerid) %>%
  print(n = 20) # Check irrelevant transactions have been removed and new column has been added appropriately.

# Create usable summary figures re transactions pre and post messages *****
# First check how many customers are in the data
transactions.combined.withlimits.2 %>% distinct(customerid) %>%
  nrow() # Only 9125. No deposits and withdrawals for some customers.

# Cleaning note: There's a need to label all NA values returned 
# (i.e., no transactions as == "0" to allow for within group comparisons pre—post 
# when some individuals don't have any transactions for one of the time periods)
# do this after creating each set of summary values as it will be easier to isolate and replace the target NA values

# NON-limit setters----
# Summary transaction figures for non-limit setters *Pre* Message 1
transaction.summaries.pre.nonsetters.2<- transactions.combined.withlimits.2 %>% 
  filter(transaction_date < as.Date("2019-10-14") &
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
  mutate_if(is.numeric,list(~replace_na(.,0))) %>%
  print(n = 20)

names(transaction.summaries.pre.nonsetters.2) # Check all new names are correct

#Check accuracy of output against original transactions datasets using random customer ids:
transaction.summaries.pre.nonsetters.2 %>% 
  select(2,4:7) # Make customer IDs visible along with some key outcomes
# Accuracy check:
transactions.combined.withlimits.2 %>% 
  filter(transaction_date < as.Date("2019-10-14") &
           customerid == "****") %>% 
  print() # Select one of these customers for accuracy check

# Summary transaction figures for non-limit setters *Post* Message 1
transaction.summaries.post.nonsetters.2<-  transactions.combined.withlimits.2 %>% 
  filter(transaction_date > as.Date("2019-10-14")  &
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
  mutate_if(is.numeric,list(~replace_na(.,0))) %>% # Replace All NAs with 0
  print(n = 20)

#Check accuracy of output against original transactions datasets using random customer ids:
transaction.summaries.post.nonsetters.2 %>% 
  select(2,4:7) # Make customer IDs visible along with some key outcomes
# Accuracy check:
transactions.combined.withlimits.2 %>% 
  filter(transaction_date > as.Date("2019-10-14") &
           customerid == "****") %>% # Select one of these customers for accuracy check
  print(n = 45)

# # Add all new datasets to temporary non-setter main dataset
client.condition.limits.transactionsnonsettersonly.2<- client.condition.limit2.final %>%
  filter(window_limit_setter == "None") %>%
  full_join(transaction.summaries.pre.nonsetters.2, by = "customerid") %>%
  full_join(transaction.summaries.post.nonsetters.2, by = "customerid") %>%
  print() # Merge with existing dataset

# # Identify new column range to selectively replace NA values
ncol(client.condition.limits.transactionsnonsettersonly.2)
client.condition.limits.transactionsnonsettersonly.2 %>% 
  select(33:52) %>%
  print(n = 200)
# Replace NAs with 0 to reflect that no transactions were made, wherever relevant.
# Although we already remove any values from the original calculations, any participant without transactions for either period will have NA values once the datasets are merged
client.condition.limits.transactionsnonsettersonly.2[, c("total_deposit_amount_pre", 
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
                                                     "max_withdrawal_post") ][is.na(client.condition.limits.transactionsnonsettersonly.2[, c("total_deposit_amount_pre", 
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


# # Check accuracy of merge & NA replacement
client.condition.limits.transactionsnonsettersonly.2 %>% 
  select(condition, 
         customerid,
         window_limit_setter,
         40:50) %>% 
  print(n = 20)

# LIMIT SETTERS----
# Calculate transaction summaries for Limit Setters *Pre* messages
transaction.summaries.pre.setters.2<- transactions.combined.withlimits.2 %>% 
  filter(window_limit_setter == "LimitSetter") %>% 
  mutate(pre.limit.date = window_limit_date - days(90)) %>% 
  filter(transaction_date < as.Date(window_limit_date) & 
           transaction_date >= as.Date(pre.limit.date)) %>% 
  group_by(customerid,transaction_type) %>%
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
  mutate_if(is.numeric,list(~replace_na(.,0))) %>% # Replace All NAs with 0
  print(n = 20)

# Check accuracy of output against original transactions datasets using random customer ids:
transaction.summaries.pre.setters.2 %>% 
  select(2,3:7) # Make customer IDs visible along with some key outcomes and select one customer ID
# Accuracy check:
transactions.combined.withlimits.2 %>% 
  mutate(pre.limit.date = window_limit_date - days(90)) %>%
  filter(window_limit_setter == "LimitSetter" &
           transaction_date < as.Date(window_limit_date) & 
           transaction_date >= as.Date(pre.limit.date) & 
           customerid == "****") %>%
  select(customerid, 
         transaction_amount,
         transaction_type,
         transaction_date)  %>% # Make customer IDs visible along with some key outcomes and select one customer ID
  print(n = 100)

# Summary transaction figures for Limit Setters *Post* setting their limit
transaction.summaries.post.setters.2<- transactions.combined.withlimits.2 %>% 
  filter(window_limit_setter == "LimitSetter") %>% 
  mutate(post.limit.date = window_limit_date + days(90)) %>% 
  filter(transaction_date > as.Date(window_limit_date) & 
           transaction_date <= as.Date(post.limit.date)) %>% 
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
  mutate_if(is.numeric,list(~replace_na(.,0))) %>% # Replace All NAs with 0
  print(n = 25)

#Check accuracy of output against original transactions datasets using random customer ids:
transaction.summaries.post.setters.2 %>% 
  select(2,3:7) # Make customer IDs visible along with some key outcomes and select one customer ID
# Accuracy check:
transactions.combined.withlimits.2 %>% 
  mutate(pre.limit.date = window_limit_date + days(90)) %>% 
  filter(transaction_date > as.Date(window_limit_date) & 
           transaction_date <= as.Date(pre.limit.date)  & 
           customerid == "****") %>% 
  select(customerid,
         transaction_amount,
         transaction_type,
         transaction_date) %>% 
  print(n = 100)

# Add all new datasets to temporary setter main dataset:
client.condition.limits.transactionssettersonly.2<- client.condition.limit2.final %>%
  filter(window_limit_setter == "LimitSetter") %>%
  full_join(transaction.summaries.pre.setters.2, by = "customerid") %>%
  full_join(transaction.summaries.post.setters.2, by = "customerid") %>%
  print() # Merge with existing dataset

# Identify new column range to selectively replace NA values:
ncol(client.condition.limits.transactionssettersonly.2)
client.condition.limits.transactionssettersonly.2 %>% 
  select(33:52)
# Replace NAs with 0 to reflect that no transactions were made, wherever relevant.
client.condition.limits.transactionssettersonly.2[, c("total_deposit_amount_pre", 
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
                                                       "max_withdrawal_post") ][is.na(client.condition.limits.transactionssettersonly.2[, c("total_deposit_amount_pre", 
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

client.condition.limits.transactionssettersonly.2 %>% 
  select(condition, customerid, window_limit_setter, 40:50) %>% 
  print(n = 200) #Check accuracy of merge

#
# WAGERING DATA -----
#
wagers2

# Add column names to wager dataset 
# Use names from original wager data
colnames(wagers2) <- c("wagerid",
                       "customerid",	
                       "Operator",
                       "Date",
                       "Time",
                       "Platform",
                       "Sports",
                       "Wager_type",
                       "Acc_bal_prior_wager",
                       "Bonus_bal_prior_wager",
                       "Odds",
                       "Stake",
                       "Payout",
                       "Net_result",
                       "Bonus",
                       "Promotion",
                       "Final_net_result",
                       "Acc_bal_after_wager",
                       "Bonus_bal_after_wager") # Add new names

wagers2 %>% distinct(customerid) %>%
  nrow() # Only 9921 in this dataset - should be 9987 (i.e., the full sample) if they followed the instructions

# Check whether "final_net_result" ever differs from "net_result"
wagers2 %>% 
  mutate(discrepancy = Final_net_result - Net_result) %>%
  summarise(
    min(discrepancy),
    max(discrepancy)
  )  # No, always zero.

# Let's check whether wager IDs are unique to every wager or whether there are any duplicated.
nrow(wagers2)
# Output: 3,385,409

wagers2 %>% 
  distinct(wagerid, 
                     customerid,
                     Date,
                     Time, .keep_all = TRUE) %>% 
  nrow() # Output: 2,401,980
# Discrepancy:
3385409-2401980
# This removes 983,429 wagers – around one third of the sample! This is consistent with other data we have received from this operator....
# And suggests it's simply an error in the recording process

unique(wagers2$wagerid[duplicated(wagers2$wagerid)]) # Isolate all duplicated wager IDs so that we can explore these

# Explore some random duplicated wagers to see what's going on:
wagers2 %>%
  filter(wagerid == "314615480") # Wager duplicated once – All features of the wager seem consistent across both entries...
# other than the account balance prior to and following the wager. Sport type == Thoroughbred racing

wagers2 %>%
  filter(wagerid == "311258357") # Wager duplicated once – again, all variables consistent of them and those relating to the account balance. Sport type == Greyhounds

wagers2 %>%
  filter(wagerid == "271102144") # Duplicate wager. Same consistencies/inconsistencies. Sport type == Cricket

wagers2 %>%
  filter(wagerid == "296461323") # Random duplicate again – all variables consistent

# Check whether the wager types have anything to do with it (e.g.. "errors")
table(wagers2$Wager_type) # Nope

# There is one difference between the duplicates — The account balance before and after. 
# It seems like one of the wages include any changes to the account balance after winning, and one…
# Includes the account balance after placing the bet (but doesn't include the money one)

# Lets test this: Do any duplicate wagers remain when it is equal to 0 (I.e. the Customer lost)
# First let's calculate the total number of losses:
wagers2 %>% 
  filter(Payout == "0") %>% 
  nrow() # Output: 2227254

wagers2 %>% 
  filter(Payout == "0") %>% 
  distinct(wagerid, 
           customerid,
           Date,
           Time, .keep_all = TRUE) %>% 
  nrow() # Output: 1920847
# There are still duplicate wagers even for losses 

# Given that the amount won and wagered is consistent...
# It seems safe to remove duplicate wager IDs for this data in the below pipeline. 
# Spoke with the operator and got this response:
# "The duplicates are when multiple transactions had occurred for the same wager id e.g. for below example:
# First record is when customer used $35 to place a bet, so account balance decreased from $38 to $3
# Second record is when customer won and was paid out $65, so account balance increased from $23 to $88
# If the analysis doesn’t use the before and after account balance data, then dupes can be removed."

# Let's make the data ready for use
# Remove all the irrelevant columns
wagers.1.2<- wagers2 %>% 
  select("wagerid",
         "customerid",	
         "Operator",
         "Wager_type",
         "Date",
         "Odds",
         "Time",
         "Sports",
         "Stake",
         "Payout",
         "Net_result") %>%
  as_tibble() %>%
  mutate(wager_date = as.Date(Date, "%Y-%m-%d")) %>% # Create a useable/subsetable date column 
  select(- Date) %>%
  rename(stake = Stake,
         payout = Payout) %>%
  distinct(wagerid, 
           customerid,
           wager_date,
           Time,
           .keep_all = TRUE) %>% # Removal of duplicate wagers
  full_join(LimitSetter.window.column.2, by = "customerid") %>% # Enable differentiation between limit setters and non-setters
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') %>%
  print() # Convert to tibble and check new data setup

# Let's figure out what equals a win (for the consumer) and a loss based on this data:
wagers.1.2 %>%
  filter(Net_result >=0)  # Plus value net_results (e.g., 55) indicate the person won.
wagers.1.2 %>%
  filter(Net_result <0) # Minus value net_results (e.g., -33) indicate the person lost (there are nearly twice as many of these)
# For this operator, payout is exactly how much the operator pays out and so values of 0 indicate a loss.
# This makes calculations easier than for Operator 1

# WAGERS NON-LIMIT SETTERS----
# Calculate wager summaries for non-limit setters *pre* message 1
wager.summaries.pre.nonsetters.2<- wagers.1.2 %>% 
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
  # select(2:3) %>% # View customer IDs for the check below
  print()
# Check outputs seem correct against a random customer ID
wagers.1.2 %>% 
  filter(wager_date < as.Date("2019-10-14") &
           customerid == "****") %>%
  # summarise(n_distinct(wager_date)) %>%
  # (adding the "summarise" function to the above checks the total no. betting days is correct)
  print(n = 27)
# Note: Negative net loss outcomes indicate the person won money overall in the specified time period

# As there were customers with no wages in this data, let's see how many meet the criterion of ≥ 5 betting days:
wager.summaries.pre.nonsetters.2 %>%
  filter(betting_days_frequency_pre >= 5) # ~600 customers didn't meet the criterion of betting on at least five days prior to messages

# And new column for the total number of wagers in the 30 days prior to message 1 (eligibility criteria)----
wager.days.last.30.nonsetters.2<- wagers.1.2 %>% 
  filter(wager_date > as.Date("2019-09-13") &
           wager_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  group_by(customerid) %>%
  summarise(
    last30_betting_days_frequency = n_distinct(wager_date)) %>% 
  print(n = 100)

wager.days.last.30.nonsetters.2 %>%
  filter(last30_betting_days_frequency < 5)

# Add new column for final wager in the day before being sent message 1
last.wager.column.nonsetters.2<- wagers.1.2 %>% 
  filter(wager_date < as.Date("2019-10-14") & 
           window_limit_setter == "None") %>%
  separate(Time, into = c("wager.time", "wager.waste"), sep = "\\.") %>%
  mutate(wager_time = hms(wager.time)) %>%
  arrange(desc(wager_date, wager_time, customerid)) %>% 
  distinct(customerid, .keep_all = TRUE) %>%
  mutate(lost_last_bet = payout == 0) %>% 
  select(customerid,
         lost_last_bet) %>%
  print(n = 200) # Checked for accuracy against last bets (i.e., without columns selected)
# Check to see if the distribution of the losses versus wins appears reasonable (yes)
table(last.wager.column.nonsetters.2$lost_last_bet)

# Add column for SD of daily wager
daily.wager.SD.pre.nonsetters.2<- wagers.1.2 %>% 
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

# Calculate wager summaries for non-limit setters *post* Message 1
wager.summaries.post.nonsetters.2<- wagers.1.2 %>% 
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
  # select(2:3) %>% # View customer IDs for the check below
  print()

# Add column for SD of daily wager
daily.wager.SD.post.nonsetters.2<- wagers.1.2 %>% 
  filter(wager_date > as.Date("2019-10-14") &
           wager_date <= as.Date("2020-01-12") &
           window_limit_setter == "None") %>%
  group_by(customerid, wager_date) %>%
  summarise(
    dailywager = sum(stake)
  ) %>%
  group_by(customerid) %>% 
  summarise(
    daily_wager_SD_post = sd(dailywager)
  ) %>%
  print()

# Add all new datasets (pre/post message wagers for non-limit setters) to main temp non-setter dataset
client.condition.limits.transactions.wagers.nonsettersonly.2<- client.condition.limits.transactionsnonsettersonly.2 %>% 
  full_join(wager.summaries.pre.nonsetters.2, by = "customerid") %>%
  full_join(wager.days.last.30.nonsetters.2, by = "customerid") %>% 
  full_join(last.wager.column.nonsetters.2, by = "customerid") %>% 
  full_join(daily.wager.SD.pre.nonsetters.2, by = "customerid") %>% 
  full_join(wager.summaries.post.nonsetters.2, by = "customerid") %>% 
  full_join(daily.wager.SD.post.nonsetters.2, by = "customerid") %>% 
  print() # Merge with existing dataset

# Check this new dataset:
client.condition.limits.transactions.wagers.nonsettersonly.2 %>% 
  filter(is.na(total_number_of_bets_post)) %>%
  group_by(customerid, 
           date_account_closed, 
           self_exclusion) %>%
  summarise(
    n(),
  ) %>%
  print(n = 200) # There are 523 unique clients with no wagers post message which seems odd
# Some people appear to have close the account or self excluded during this window, but not the full cohort
# Self excluders and those who closed the account will be removed from the data wherever appropriate....
# during the cleaning of the final, combined (all operators) dataset

# Identify new column range to selectively replace NA wager values to allow proper pre and post comparisons 
# NA values would exclude clients from within subjects comparisons, 
# even if they simply did not wager during this time and didn't close their account/self-exclude)
ncol(client.condition.limits.transactions.wagers.nonsettersonly.2)
client.condition.limits.transactions.wagers.nonsettersonly.2 %>% 
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
         daily_wager_SD_post
  ) %>%
  print(n = 20)

# Replace NAs with 0 0 to reflect that no transactions were made, wherever relevant.
client.condition.limits.transactions.wagers.nonsettersonly.2[, c("total_number_of_bets_pre",
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
                                                                   client.condition.limits.transactions.wagers.nonsettersonly.2[, c("total_number_of_bets_pre",
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

client.condition.limits.transactions.wagers.nonsettersonly.2 %>% 
  select(50:69) # Check NAs were removed

# WAGERS LIMIT SETTERS----
# Calculate wager summaries for Limit Setters *Pre* messages
wager.summaries.pre.setters.2<- wagers.1.2 %>% 
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
  # select(2:3) %>% # View customer IDs for the check below
  print()

wagers.1.2 %>%
  mutate(pre.limit.date = window_limit_date - days(90)) %>% # Check accuracy of output against random customer ids 
  filter(wager_date < as.Date(window_limit_date) & 
           wager_date >= as.Date(pre.limit.date) & 
           customerid == "****")  # %>% summarise(n_distinct(wager_date)) 

# And new column for the total number of wagers in the 30 days prior to message 1 (eligibility criteria)----
wager.days.last.30.setters.2<- wagers.1.2 %>% 
  filter(wager_date > as.Date("2019-09-13") &
           wager_date < as.Date("2019-10-14") &
           window_limit_setter == "LimitSetter") %>%
  group_by(customerid) %>%
  summarise(
    last30_betting_days_frequency = n_distinct(wager_date)) %>% 
  print(n = 100)

# Add new column for final wager in the day before setting their limit
last.wager.column.setters.2<- wagers.1.2 %>% 
  filter(wager_date < as.Date(window_limit_date) & 
           window_limit_setter == "LimitSetter") %>%
  separate(Time, into = c("wager.time", "wager.waste"), sep = "\\.") %>%
  mutate(wager_time = hms(wager.time)) %>%
  arrange(desc(wager_date, wager_time, customerid)) %>% 
  distinct(customerid, .keep_all = TRUE) %>%
  mutate(lost_last_bet = payout == 0) %>% 
  select(customerid, lost_last_bet) %>%
  print() # Checked for accuracy against last bets (i.e., without columns selected)

# Check to see if the distribution of the losses versus wins appears reasonable (yes)
table(last.wager.column.setters.2$lost_last_bet)

# Add column for SD of daily wager pre limit
daily.wager.SD.pre.setters.2<- wagers.1.2 %>% 
  filter(wager_date < as.Date(window_limit_date) &
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

# Calculate wager summaries for Limit Setters Post* messages
wager.summaries.post.setters.2<- wagers.1.2 %>% 
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

wagers.1.2 %>%
  mutate(post.limit.date = window_limit_date + days(90)) %>% # Check accuracy of output against random customer ids 
  filter(wager_date > as.Date(window_limit_date) & 
           wager_date <= as.Date(post.limit.date) & 
           customerid == "****") #  %>% summarise(n_distinct(wager_date)) 

# Add column for SD of daily wager
daily.wager.SD.post.setters.2<- wagers.1.2 %>% 
  mutate(post.limit.date = window_limit_date + days(90)) %>% # Check accuracy of output against random customer ids 
  filter(wager_date > as.Date(window_limit_date) & 
           wager_date <= as.Date(post.limit.date) &
           window_limit_setter == "LimitSetter") %>%
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

client.condition.limits.transactions.wagerssetterssonly.2<- client.condition.limits.transactionssettersonly.2 %>% 
  full_join(wager.summaries.pre.setters.2, by = "customerid") %>%
  full_join(wager.days.last.30.setters.2, by = "customerid") %>% 
  full_join(last.wager.column.setters.2, by = "customerid") %>%
  full_join(daily.wager.SD.pre.setters.2, by = "customerid") %>%
  full_join(wager.summaries.post.setters.2, by = "customerid") %>% 
  full_join(daily.wager.SD.post.setters.2, by = "customerid") %>%
  print() # Merge with existing dataset

client.condition.limits.transactions.wagerssetterssonly.2 %>% 
  # filter(is.na(total_number_of_bets_post)) %>%
  group_by(customerid, 
           date_account_closed, 
           self_exclusion) %>%
  summarise(
    n(),
  ) %>%
  print() # 0 (NO) clients in this group didn't wager after messages. 
# One closed their account after the study ended

# Replace NAs with 0 to reflect that no transactions were made, wherever relevant.
client.condition.limits.transactions.wagerssetterssonly.2[, c("total_number_of_bets_pre",
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
                                                                client.condition.limits.transactions.wagerssetterssonly.2[, c("total_number_of_bets_pre",
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
client.condition.limits.transactions.wagerssetterssonly.2 %>% 
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
         "daily_wager_SD_post") %>% # View the "post" colums to see if there are NA values
  print(n = 200)

# ------------------------------------------------------------------------------
# COMBINE TRANSACTIONS & WAGER DATA SETS FOR BOTH GROUPS
client.condition.limits.transactions.wagers.2<- bind_rows(client.condition.limits.transactions.wagerssetterssonly.2, 
                                                          client.condition.limits.transactions.wagers.nonsettersonly.2) %>% 
  print()

# Explore net loss summaries
client.condition.limits.transactions.wagers.2 %>% 
  group_by(window_limit_setter) %>%
  summarise(
    mean(net_loss_pre),
    median(net_loss_pre),
    max(net_loss_pre),
    mean(net_loss_post),
    median(net_loss_post),
    max(net_loss_post)
  ) # summary values appear to have decreased for limit setters and to a lesser extent non-setters

client.condition.limits.transactions.wagers.2 %>% 
  group_by(window_limit_setter) %>%
  count(lost_last_bet) # Limit setters & non-setters proportions in relation to whether their last bet pre-limit/message was a win/loss

client.condition.limits.transactions.wagers.2 %>% 
  filter(net_loss_pre <= 0) %>%
  group_by(window_limit_setter) %>%
  summarise(
    n() # Most clients in both groups lost money prior to messages
  )

client.condition.limits.transactions.wagers.2 %>% 
  filter(net_loss_pre > 0) %>%
  group_by(window_limit_setter) %>%
  summarise(
    n()
  ) # Only a small proportion (2/206 of limit setters) had a positive net loss (indicating they won money overall)

# ------------------------------------------------------------------------------
# Finalise dataset for analyses
# Create operator column and label to identify client operator
Operator<- rep(x = 2, times = 9987) %>% 
  as_tibble() %>% 
  rename(operator = value) %>%
  print()

client.condition.limits.transactions.wagers.final.2<- bind_cols(Operator, # Add operator column and label to identify client operator
                                                                client.condition.limits.transactions.wagers.2) %>% 
   print()

names(client.condition.limits.transactions.wagers.final.2)
# [1] "operator"                      "condition"                     "customerid"                    "age"                           "gender"                       
# [6] "postcode"                      "time_account_opened"           "date_account_opened"           "date_account_closed"           "window_limit_setter"          
# [11] "window_limit_date"             "window_limit_amount"           "limit_window_setters"          "limit_setter_pre"              "limit_date_pre"               
# [16] "limit_window_pre"              "limit_amount_pre"              "limit_setter_post"             "limit_window_post"             "limit_date_post"              
# [21] "limit_amount_post"             "timeout_duration_pre"          "timeout_start_date_pre"        "timeout_finish_date_pre"       "timeout_pre"                  
# [26] "timeout_duration_post"         "timeout_start_date_post"       "timeout_finish_date_post"      "timeout_post"                  "self_exclusion_start_time"    
# [31] "self_exclusion"                "self_exclusion_start_date"     "total_deposit_amount_pre"      "total_withdrawal_amount_pre"   "mean_deposit_pre"             
# [36] "mean_withdrawal_pre"           "number_of_deposits_pre"        "number_of_withdrawals_pre"     "median_deposit_pre"            "median_withdrawal_pre"        
# [41] "min_deposit_pre"               "min_withdrawal_pre"            "max_deposit_pre"               "max_withdrawal_pre"            "total_deposit_amount_post"    
# [46] "total_withdrawal_amount_post"  "mean_deposit_post"             "mean_withdrawal_post"          "number_of_deposits_post"       "number_of_withdrawals_post"   
# [51] "median_deposit_post"           "median_withdrawal_post"        "min_deposit_post"              "min_withdrawal_post"           "max_deposit_post"             
# [56] "max_withdrawal_post"           "total_number_of_bets_pre"      "betting_days_frequency_pre"    "total_amount_wagered_pre"      "total_amount_won_pre"         
# [61] "average_daily_wager_pre"       "betting_intensity_pre"         "net_loss_pre"                  "last30_betting_days_frequency" "lost_last_bet"                
# [66] "daily_wager_SD_pre"            "total_number_of_bets_post"     "betting_days_frequency_post"   "total_amount_wagered_post"     "total_amount_won_post"        
# [71] "average_daily_wager_post"      "betting_intensity_post"        "net_loss_post"                 "daily_wager_SD_post"          

names2<- names(client.condition.limits.transactions.wagers.final.2)
write.csv(names2, file = "Analysis data/final_names2.csv")

# Create a CSV file from the final dataset so that it can be easily accessed and merged with data sets from other operators:
write.csv(client.condition.limits.transactions.wagers.final.2, file = "Analysis data/Operator 2/Final_dataset_Operator2.csv")

# End