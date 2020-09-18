# Script to clean, wrangle and combine datasets and calculate some descriptive stats for operator 1
# Note: All customer IDs in the code have been removed to protect the anonymity of customers involved in the trial

# install and load any relevant packages
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

# Remove all objects from work space
rm(list = ls())

# load and name all datasets
clients1<- read.csv("Analysis data/Operator 1/clients1.csv") 
conditions1<- read.csv("Analysis data/Operator 1/conditions1.csv")
limits1<- read.csv("Analysis data/Operator 1/depositlimits1.csv")
limitsprestudy1<- read.csv("Analysis data/Operator 1/Limitdata_prestudy1.csv")
timeout1<- read.csv("Analysis data/Operator 1/timeout1.csv") 
timeoutprestudy1 <-  read.csv("Analysis data/Operator 1/Previoustimeouts.csv") 
selfexclusion1<- read.csv("Analysis data/Operator 1/selfexclusion1.csv")
prelimresults1<- read.csv("Analysis data/Operator 1/prelimresults1.csv")
transactionspre1<- read.csv("Analysis data/Operator 1/transactionspre1.csv")
transactionspost1<- read.csv("Analysis data/Operator 1/transactionspost1.csv")
wagerspre1.1<- read.csv("Analysis data/Operator 1/wagerpre1.1.csv")
wagerspre2.1<- read.csv("Analysis data/Operator 1/wagerpre2.1.csv")
wagerspre3.1<- read.csv("Analysis data/Operator 1/wagerpre3.1.csv")
wagerspre4.1<- read.csv("Analysis data/Operator 1/wagerpre4.1.csv")
wagerspost1.1<- read.csv("Analysis data/Operator 1/wagerpost1.1.csv")
wagerspost2.1<- read.csv("Analysis data/Operator 1/wagerpost2.1.csv")
wagerspost3.1<- read.csv("Analysis data/Operator 1/wagerpost3.1.csv")
wagerspost4.1<- read.csv("Analysis data/Operator 1/wagerpost4.1.csv")

# Calculate demographic characteristics for the sample ------------------------------------------------------------------------------
# check *CONDITIONS* dataset before use (primary dataset used to divide participants)
conditions1<- as_tibble(conditions1) 
  
head(conditions1)

conditions1 %>% 
  count(condition) # total number of participants per condition
# Outputs with original condition names
# 1 1: Control group            1375
# 2 2: Informative; email        875
# 3 3: Informative; in-account  2000
# 4 4: Social; email             875
# 5 5: Social; in-account       2000
# 6 6: Personal; email           875
# 7 7: Personal; in-account     2000

# Recode Condition names ease and consistency With the Other operators
conditions1<- conditions1 %>% 
  mutate(condition = fct_recode(condition, 
                                                '1' = "1: Control group",
                                                '2' = "2: Informative; email",
                                                '3' = "3: Informative; in-account",
                                                '4' = "4: Social; email",
                                                '5' = "5: Social; in-account",
                                                '6' = "6: Personal; email",
                                                '7' = "7: Personal; in-account")) %>% 
                               print()
                                                  
                                                
# Check clients dataset before use
head(clients1) 

# Recode "Unknown" as "U" For consistency with other operators
clients1<- clients1 %>% 
  mutate(gender = fct_recode(gender, 
                               U = "Unknown")) %>%
  print()

combined.condition.clients1<- conditions1 %>% 
  full_join(clients1) %>% 
  print() # Merge condition dataset with the clients dataset

# Demographic characteristics
combined.condition.clients1 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  count(gender) # gender distribution (total sample, duplicates removed) 

combined.condition.clients1 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  summarise(
    agemean = mean(age),
    agemedian = median(age),
    agemin = min(age),
    agemax = max(age),
    n = n()
  )  # summary figures for age (total sample, duplicates removed)

demographic.summary.1<- combined.condition.clients1 %>% 
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

combined.condition.clients1 %>%
  group_by(condition,
           gender) %>% 
  summarise(
    total = n()
  ) %>% 
  group_by(condition) %>% 
  mutate(percentage = total/sum(total)*100) # Percentage of males and females in each condition

combined.condition.clients1 %>% 
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
#Check deposit limits dataset before use
head(limits1)

# Merge limits1 data set (which contains all deposit limits set within the study period)...
# with a dataset that contains all limits set prior to the study period

names(limits1) # First make all column names consistent
names(limitsprestudy1) # First make all column names consistent

limitstemp.1<- limits1 %>% 
  rename(
         limit_date = limitdate, 
         limit_time = limittime, 
         limit_amount = limitamount,
         limit_window = limitwindow) %>%
  as_tibble() %>%
  (print)

limits1.1<- limitsprestudy1 %>% 
  rename(depositlimitid = DepLimID, 
                                 customerid  = clientID, 
                                 operator = Operator, 
                                 limit_date = Date, 
                                 limit_time = Time, 
                                 limit_amount = New_limit,
                                 limit_window = New_Window) %>%
  bind_rows(limitstemp.1) %>% 
  select(- Previous_limit,
         - Previous_window,
         - previous_window,
         - previouslimitamount) %>% 
  as_tibble() %>%
  (print)

condition.limit1<- conditions1 %>%
  full_join(limits1.1, by = c("customerid")) %>% 
  as_tibble() %>%
  select(-depositlimitid, 
         - operator) %>%
  print(n = 30) # Merge Limit dataset with conditions dataset

condition.limit1 %>% 
  group_by(condition,
           limit_window) %>%
  summarise(
    Individuals = n_distinct(customerid),
    Total.n = n()
  ) %>% 
  print (n = 42) # Calculate total number of deposit limits used across conditions during the full study period (July-Jan; ~190 days)

limit.summary<- condition.limit1 %>%
  count(limit_date) %>% 
  arrange(as.Date(limit_date, "%d/%m/%y")) %>%
  filter(limit_date != "NA") %>%
  print (n = 150) # The total number of deposit limits set each day during the full study period (July-Jan; ~190 days)

# Start of major merges  ------------------------------------------------------------------------------
# Merge 1 datasets: conditions, limits,&  clients
client.condition.limit1<- condition.limit1 %>% 
  full_join(clients1, by = c("customerid")) %>% 
  as_tibble() %>%
  print () # Merge Limit dataset with conditions dataset

# Recode deposit limit durations into a factor with clear names for ease and consistency with other operators
client.condition.limit1.1<- client.condition.limit1 %>% 
  transform(limit_window = factor(limit_window,
                                  levels = c(1, 7, 28, 30, 365), 
                                  labels = c("DepositLimitDay",
                                            "DepositLimitWeek",
                                            "DepositLimitMonth",
                                            "DepositLimitMonth",
                                            "DepositLimitYear"))) %>%
  as_tibble() %>%
  print()

names(client.condition.limit1.1)

# ------------------------------------------------------------------------------
# Make date selections possible for the dataset

# Add new limit date and account open and closed date column to dataset
client.condition.limit1.2<- client.condition.limit1.1 %>% 
  mutate(limit.date = as.Date(limit_date, "%d/%m/%y"),
         date.account.opened = as.Date(date_account_opened, "%d/%m/%Y"),
         date.account.closed = as.Date(date_account_closed, "%d/%m/%Y")) %>% # Make a workable date column
  select(-date_account_opened,
  -date_account_closed,
  -limit_date) %>% # Remove old date columns
  rename(limit_date = limit.date,
          date_account_opened = date.account.opened,
          date_account_closed = date.account.closed) %>% # Rename new date column after automatic conversion
  as_tibble() %>%
  print()

# Isolate and table those who set a limit within 5 days of viewing *either* message (also add demographic breakdown)
response.to.messages<- client.condition.limit1.2 %>% 
  filter(limit_date >= as.Date("2019-10-14") &
           limit_date <= as.Date("2019-10-23")) %>%
  filter(limit_window == "DepositLimitDay"|
           limit_window == "DepositLimitWeek"|
           limit_window == "DepositLimitMonth"|
           limit_window == "DepositLimitYear") %>%
  group_by(condition,
           gender) %>%
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

# Isolate And table those who set a limit within 5 days of viewing the *first* message
response.to.message1<- client.condition.limit1.2 %>% 
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

sum(response.to.message1$Total.participants) # Total Limit setters with duplicates (Message 1)
sum(response.to.message1$unique.participants) # Total Limit setters without duplicates (Message 1)

# Isolate and table those who set a limit within 5 days of viewing the *Second* message
response.to.message2<- client.condition.limit1.2 %>% 
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

sum(response.to.message2$Total.participants)# Total Limit setters with duplicates (Message 2)
sum(response.to.message2$unique.participants) # Total Limit setters without duplicates (Message 2)
# Note: Most people set limits after seeing the first message****

# ------------------------------------------------------------------------------
# End of summary statistics for operator 1
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Prepare datasets for confirmatory and exploratory analyses
# ------------------------------------------------------------------------------

# Responsible gambling tool dataset
# Start by adding separate columns that display RG tool use at the different time periods.
# To avoid any errors in coding at this stage, all of the new columns include duplicates.

#
# LIMITS -----
#

# Create a new column that easily splits the dataset into limit setters (i.e. Within the window of interest) and non-limit setters***
LimitSetter.window.column.1<- client.condition.limit1.2 %>% 
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
                                          "LimitSetter" = "DepositLimitYear")) %>%
  select(customerid,
         window_limit_setter,
         limit_date,
         limit_window,
         limit_amount) %>%
  rename(window_limit_date = limit_date,
         limit_window_setters = limit_window,
         window_limit_amount= limit_amount) %>%
  arrange(window_limit_date) %>%
  print(n = 108)

# According to the new column above, no participants to set limits on 23/10. Check this is accurate using the original dataset:
table(client.condition.limit1.2$limit_date) # Yes, no one set limits on the date

# Create a new column that identifies anyone who set a limit prior to messages  
limit_setter_pre.column.1<- client.condition.limit1.2 %>%
  filter(limit_date < as.Date("2019-10-14") & 
           limit_window == "DepositLimitDay" | 
           limit_window == "DepositLimitWeek" |
           limit_window == "DepositLimitMonth" |
           limit_window == "DepositLimitYear") %>%
  mutate(limit_setter_pre = fct_recode(limit_window,
                                        "PreLimitSetter" = "DepositLimitDay",
                                        "PreLimitSetter" = "DepositLimitWeek",
                                        "PreLimitSetter" = "DepositLimitMonth",
                                        "PreLimitSetter" = "DepositLimitYear")) %>%
  select(customerid, 
         limit_setter_pre, 
         limit_date,
         limit_window,
         limit_amount) %>%
  rename(limit_date_pre = limit_date,
         limit_window_pre = limit_window,
         limit_amount_pre = limit_amount) %>%
  arrange(limit_date_pre) %>%
  print(n = 100)

# Create a new column that Identifies *anyone* who set a limit post message one
# (allows for later excluding non-Window.limit.setters who set limits later on to be excluded from exploratory analyses of limit effectiveness)
LimitSetter.post.all.column.temp.1<- client.condition.limit1.2 %>%
  filter(limit_window == "DepositLimitDay"|
           limit_window == "DepositLimitWeek"|
           limit_window == "DepositLimitMonth"|
           limit_window == "DepositLimitYear") %>%
  filter(limit_date > as.Date("2019-10-23")) %>%
  mutate(limit_setter_post = fct_recode(limit_window,
                                            "PostLimitSetter" = "DepositLimitDay",
                                            "PostLimitSetter" = "DepositLimitWeek",
                                            "PostLimitSetter" = "DepositLimitMonth",
                                            "PostLimitSetter" = "DepositLimitYear")) %>%
  select(customerid, 
         limit_setter_post, 
         limit_window,
         limit_date,
         limit_amount) %>%
  rename(limit_date_post = limit_date,
         limit_window_post = limit_window,
         limit_amount_post = limit_amount) %>%
  arrange(limit_date_post) %>%
  print(LimitSetter.post.all.column, n = 150)
# Now remove Window.limit.setters from this column to easily exclude non-Window.limit.setters who set limits later on:
LimitSetter.post.all.column.1<- anti_join(LimitSetter.post.all.column.temp.1, 
                                          LimitSetter.window.column.1) %>%
  print()
# Check limit setter window column isn't affected by removing those who set limits after messages who weren't window limit setters:
anti_join(LimitSetter.window.column.1, 
          LimitSetter.post.all.column.1) %>%
  print()

#
# TIMEOUTS ----
#
# Check data from study period and pre-study:
head(timeout1) %>% as_tibble
head(timeoutprestudy1) %>% as_tibble
# These datasets identical and can be merged using a row bind. We just need to ensure all variables have the same datatype:
timeoutprestudy1$timeoutid <- as.factor(timeoutprestudy1$timeoutid)

# Make the timeout data workable
timeout1.1<- timeout1 %>% 
  bind_rows(timeoutprestudy1) %>%
  as_tibble() %>%
  mutate(timeout_start_date = as.Date(start_date, format = "%d/%m/%Y")) %>%
  mutate(timeout_end_date = as.Date(finish_date, format = "%d/%m/%Y")) %>%
  select(- operator,
         - start_date,
         - finish_date,
         - timeoutid) %>%
  rename(timeout_start_time = start_time,
         timeout_end_time = finish_time,
         timeout_duration = duration) %>%
  arrange(timeout_start_date) %>%
  print()

# Cleaning note: Use this new data to check if anyone who was on timeout when messages were sent:
timeout1.1 %>%
  filter(timeout_start_date <= as.Date("2019-10-13")) %>%
  print()
# **No one was on timeout**

# # Create a new column to identify those who have used time outs prior to messages
TimeOut.pre<- as.factor(rep(x = "TimeOutPre", times = 44)) # Create a character vector to identify this group
TimeOut.pre.column.1<- timeout1.1 %>%
  filter(timeout_start_date <= as.Date("2019-10-13")) %>%
  bind_cols(list(TimeOut.pre)) %>% # Add character vector
  rename(timeout_pre = ...7,
         timeout_duration_pre = timeout_duration,
         timeout_start_date_pre = timeout_start_date,
         timeout_finish_date_pre = timeout_end_date) %>% 
  select(-timeout_start_time, -timeout_end_time) %>% 
  print()

# # Create a new column to identify those who used time outs following the first message
TimeOut.post<- as.factor(rep(x = "TimeOutPost", times = 262)) # Create a character vector to identify this group
TimeOut.post.column.1<- timeout1.1 %>%
  filter(timeout_start_date >= as.Date("2019-10-14")) %>%
  bind_cols(list(TimeOut.post)) %>% # Add character vector
  rename(timeout_post = ...7,
         timeout_duration_post = timeout_duration,
         timeout_start_date_post = timeout_start_date,
         timeout_finish_date_post = timeout_end_date) %>% 
  select(-timeout_start_time,
         -timeout_end_time) %>% 
  arrange(timeout_start_date_post) %>%
  as_tibble() %>% 
  print()

#
# SELF EXCLUSION ----
#
selfexclusion1
# Note: This operator only offers permanent self-exclusion and so there are none pre-messages

# Make the self exclusion data workable
# First, create a new column to identify those who used self exclusion following the first message
Self.Excluded<- as.factor(rep(x = "SelfExcluder", times = 69)) # Create a character vector to identify this group
SelfExclusion.post.column.1<- selfexclusion1 %>%
  bind_cols(list(Self.Excluded)) %>% # Add character vector
  mutate(self_exclusion_start_date = as.Date(start_date, format = "%d/%m/%Y")) %>%
  select(- Operator,
         - start_date,
         - self_excluded) %>%
  rename(self_exclusion = ...6,
         self_exclusion_start_time = start_time) %>%
  arrange(self_exclusion_start_date) %>%
  as_tibble() %>%
  print()


# # Add all new CPT columns to dataset
client.condition.limit1.final.withnas<- client.condition.limit1.2 %>%
  full_join(LimitSetter.window.column.1, by = "customerid")  %>%
  full_join(limit_setter_pre.column.1, by = "customerid")  %>%
  full_join(LimitSetter.post.all.column.1, by = "customerid")  %>%
  full_join(TimeOut.pre.column.1, by = "customerid")  %>%
  full_join(TimeOut.post.column.1, by = "customerid")  %>%
  full_join(SelfExclusion.post.column.1, by = "customerid") %>%
  distinct(customerid, .keep_all = TRUE) %>% 
  print(n = 100)

client.condition.limit1.final<- client.condition.limit1.final.withnas %>%
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') # Remove all the "NA" values in the factor columns just added

client.condition.limit1.final %>% select(14:30) %>%
  print(n=100) # Check new dataset has all merged correctly and "NA" values have been replaced

#
# TRANSACTIONS DATA -----
#

# Cleaning note: the "Deposit_limit_set" column in the transaction data doesn't seem to be a accurate:
# some people set a limit and then deposit money the next and it still has them as having no limit

# Add pre and post-message transactions datasets and combine them:
transactionspre.1.tib<- transactionspre1 %>% 
  as_tibble() %>% 
  rename(transaction_date.old = transactiondate) %>%
  mutate(transaction_date = as.Date(transaction_date.old, "%d/%m/%y")) %>%
  select(-transaction_date.old) %>%
  print(n = 10) # Convert to tibble, convert date column to date format, and check setup of the dataset:

transactionspost.1.tib<- transactionspost1 %>% 
  as_tibble() %>%
  rename(transaction_date.old = transactiondate) %>%
  mutate(transaction_date = as.Date(transaction_date.old, "%d/%m/%y")) %>%
  select(-transaction_date.old) %>%
  print(n = 10) # Convert to tibble, convert date column to date format, and check setup of the dataset:

transactions.combined.temp.1<- bind_rows(transactionspre.1.tib,
                                         transactionspost.1.tib) %>% 
  print() # Merge transactions datasets

# Convert negative values to positives for withdrawals (e.g., convert "-50" to "50")
transaction.amount.converted<- abs(transactions.combined.temp.1$transactionamount) %>% 
  as_tibble() %>%
  rename(transactionamount.converted = value) %>%
  print() #Check conversions are correct

# And new transaction amount column and create final transactions dataset for use:
transactions.combined.1<- bind_cols(transactions.combined.temp.1,
                                    transaction.amount.converted) %>%
  select(- transactionamount) %>%
  rename(transactionamount = transactionamount.converted) %>%
  print()

# Let's check whether transaction IDs are unique to every transaction or whether there are any duplicated.
nrow(transactions.combined.1)
# Output: 807599

transactions.combined.1 %>% 
  distinct(account_trans_id, 
           customerid,
           transaction_date,
           transactiontime, .keep_all = TRUE) %>% 
  nrow() # Output: 807599
# All unique :-)

# Check all transactions types available in the data:
table(transactions.combined.1$transactiontype)

# Remove all transaction types other than deposits and withdrawals &
# Add column that identifies limit setters (i.e., within the window of interest) so that the data can be divided accordingly:
transactions.combined.withlimits.1<- transactions.combined.1 %>%
  filter(transactiontype == "Withdrawal" |
           transactiontype == "Deposit") %>%
  full_join(LimitSetter.window.column.1, by = "customerid") %>% 
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') %>%
  rename(transaction_type = transactiontype,
         transaction_amount = transactionamount) %>% # Make consistent with other operators
  print()

transactions.combined.withlimits.1 %>% 
  select(customerid,
         transaction_type,
         window_limit_setter) %>%
  arrange(window_limit_setter, customerid) %>%
  print(n = 20) # Check irrelevant transactions have been removed and new column has been added appropriately.

# Create usable summary figures re transactions pre and post messages*****

# Cleaning note: There's a need to label all NA values returned 
# (i.e., no transactions as == "0" to allow for within group comparisons pre—post 
# when some individuals don't have any transactions for one of the time periods)
# do this after creating each set of summary values as it will be easier to isolate and replace the target NA values

# NON-limit setters----
# Summary transaction figures for non-limit setters *Pre* Message 1:
transaction.summaries.pre.nonsetters.1<- transactions.combined.withlimits.1 %>% 
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
  mutate_if(is.numeric,list(~replace_na(.,0))) %>% # Replace All NAs with 0
  print(n = 10)

names(transaction.summaries.pre.nonsetters.1) # Check all new names are correct

#Check accuracy of output against original transactions datasets using random customer ids:
transaction.summaries.pre.nonsetters.1 %>% 
  select(1,4:7) # Make customer IDs visible along with some key outcomes
transactionspre1 %>% 
  filter(customerid == "****") %>% 
  print() # Select one of these customers for accuracy check

# Summary transaction figures for non-limit setters *Post* Message 1:
transaction.summaries.post.nonsetters.1<-  transactions.combined.withlimits.1 %>% 
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
  print(n = 10)

anyNA(transaction.summaries.post.nonsetters.1)

#Check accuracy of output against original transactions datasets using random customer ids:
transaction.summaries.post.nonsetters.1 %>% 
  select(1,4:7) # Make customer IDs visible along with some key outcomes
transactionspost1 %>%  
  filter(customerid == "****") %>% 
  print() # Select one of these customers for accuracy check


# # Add all new datasets to temporary nonsetter main dataset
client.condition.limits.transactionsnonsettersonly.1<- client.condition.limit1.final %>%
  filter(window_limit_setter == "None") %>%
  full_join(transaction.summaries.pre.nonsetters.1, by = "customerid") %>%
  full_join(transaction.summaries.post.nonsetters.1, by = "customerid") %>%
  print() # Merge with existing dataset

# # Identify new column range to selectively replace NA values:
ncol(client.condition.limits.transactionsnonsettersonly.1)
client.condition.limits.transactionsnonsettersonly.1 %>% 
  select(33:54) %>%
  print(n=100)

# Replace NAs with 0 to reflect that no transactions were made, wherever relevant. 
# Although we already remove any values from the original calculations, any participant without transactions for either period will have NA values once the datasets are merged
client.condition.limits.transactionsnonsettersonly.1[, c("total_deposit_amount_pre", 
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
                                                         "max_withdrawal_post") ][is.na(client.condition.limits.transactionsnonsettersonly.1[, c("total_deposit_amount_pre", 
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
client.condition.limits.transactionsnonsettersonly.1 %>% 
  select(condition, 
         customerid,
         window_limit_setter,
         40:50) %>% 
  print(n = 20)


# LIMIT SETTERS----
# Calculate transaction summaries for Limit Setters *Pre* messages:
transaction.summaries.pre.setters.1<- transactions.combined.withlimits.1 %>% 
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

#Check accuracy of output against original transactions datasets using random customer ids:
transaction.summaries.pre.setters.1 %>% 
  select(1,3:7) # Make customer IDs visible along with some key outcomes and select one customer ID
transactions.combined.withlimits.1 %>% 
  mutate(pre.limit.date = window_limit_date - days(90)) %>%
  filter(transaction_date < as.Date(window_limit_date) &
           transaction_date >= as.Date(pre.limit.date) &
           customerid == "****") %>% 
  print()

# Summary transaction figures for Limit Setters *Post* Messasge 1:
transaction.summaries.post.setters.1<- transactions.combined.withlimits.1 %>% 
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
  print(n = 10)

#Check accuracy of output against original transactions datasets using random customer ids:
transaction.summaries.post.setters.1 %>% 
  select(1,3:7) # Make customer IDs visible along with some key outcomes and select one customer ID
transactions.combined.withlimits.1 %>% 
  mutate(post.limit.date = window_limit_date + days(90)) %>%
  filter(transaction_date > as.Date(window_limit_date) &
           transaction_date <= as.Date(post.limit.date) & 
           customerid == "****") %>% 
  select(2,8:10, transaction_date) %>%
  print(n=32)

# Add all new datasets to temporary setter main dataset:
client.condition.limits.transactionssettersonly.1<- client.condition.limit1.final %>%
  filter(window_limit_setter == "LimitSetter") %>%
  full_join(transaction.summaries.pre.setters.1, by = "customerid") %>%
  full_join(transaction.summaries.post.setters.1, by = "customerid") %>%
  print() # Merge with existing dataset

# # Identify new column range to selectively replace NA values
ncol(client.condition.limits.transactionssettersonly.1)
client.condition.limits.transactionssettersonly.1 %>% select(33:54)

# Replace NAs with 0 to reflect that no transactions were made, wherever relevant:
client.condition.limits.transactionssettersonly.1[, c("total_deposit_amount_pre", 
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
                                                      "max_withdrawal_post") ][is.na(client.condition.limits.transactionssettersonly.1[, c("total_deposit_amount_pre", 
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

client.condition.limits.transactionssettersonly.1 %>% 
  select(condition, customerid, window_limit_setter, 40:50) %>% 
  print(n = 200) #Check accuracy of merge



#
# WAGERING DATA -----
#
# Before moving forward, first clean and merge all wager datasets and then remove original files
# Failing to do this early on results in reaching memory capacity and the following error:
# "Error: Evaluation error: vector memory exhausted (limit reached?)."

# Explore possible columns to remove to decrease file size:
table(wagerspre1.1$platform) # Not interested
table(wagerspre1.1$bonus) # Y/N
table(wagerspre1.1$promotion) # Y/N
table(wagerspre1.1$bonus_bal_prior_wager) # Always blank
table(wagerspre1.1$bonus_bal_after_wager) # Always blank

# Check whether "final_net_result" ever differs from "net_result"
wagerspre1.1 %>% 
  mutate(discrepancy = final_net_result - net_result) %>%
  summarise(
    min(discrepancy),
    max(discrepancy)
  )  # No, always zero.

# Remove all the irrelevant columns
wagerspre.short.1.1<- wagerspre1.1 %>% # 1
  select(-platform,
         -bonus,
         -odds,
         -promotion,
         -bonus_bal_prior_wager,
         -bonus_bal_after_wager,
         -operator) %>%
  print()

# Quickly explore the wager details available
table(wagerspre.short.1.1$sport)
table(wagerspre.short.1.1$wager_type)

wagerspre.short.2.1<- wagerspre2.1 %>% # 2
  select(-platform,
         -bonus,
         -odds,
         -promotion,
         -bonus_bal_prior_wager,
         -bonus_bal_after_wager,
         -operator) %>%
  print()

wagerspre.short.3.1<- wagerspre3.1 %>% # 3
  select(-platform,
         -bonus,
         -odds,
         -promotion,
         -bonus_bal_prior_wager,
         -bonus_bal_after_wager,
         -operator) %>%
  print()

wagerspre.short.4.1<- wagerspre4.1 %>% # 4
  select(-platform,
         -bonus,
         -odds,
         -promotion,
         -bonus_bal_prior_wager,
         -bonus_bal_after_wager,
         -operator) %>%
  print()

wagerspost.short.1.1<- wagerspost1.1 %>% # 5
  select(-platform,
         -bonus,
         -odds,
         -promotion,
         -bonus_bal_prior_wager,
         -bonus_bal_after_wager,
         -operator) %>%
  print()

wagerspost.short.2.1<- wagerspost2.1 %>% # 6
  select(-platform,
         -bonus,
         -odds,
         -promotion,
         -bonus_bal_prior_wager,
         -bonus_bal_after_wager,
         -operator,
         -final_net_result) %>%
  print()

wagerspost.short.3.1<- wagerspost3.1 %>% # 7
  select(-platform,
         -bonus,
         -odds,
         -promotion,
         -bonus_bal_prior_wager,
         -bonus_bal_after_wager,
         -operator,
         -final_net_result) %>%
  print()

wagerspost.short.4.1<- wagerspost4.1 %>% # 8
  select(-platform,
         -bonus,
         -odds,
         -promotion,
         -bonus_bal_prior_wager,
         -bonus_bal_after_wager,
         -operator,
         -final_net_result) %>%
  print()

wagers1<- wagerspre.short.1.1 %>%
  bind_rows(wagerspre.short.2.1,
            wagerspre.short.3.1,
            wagerspre.short.4.1,
            wagerspost.short.1.1,
            wagerspost.short.2.1,
            wagerspost.short.3.1,
            wagerspost.short.4.1) %>%
  as_tibble() %>% # Convert to tibble and check new data setup
  print() 

wagers1 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  nrow() # Check all customers wagered at least once during the period of study (yes)

# Let's also check whether wager IDs are unique to every wager or whether there are any duplicated.
nrow(wagers1)
# Output: 6,981,494

wagers1 %>% 
  distinct(wagerid, 
                     customerid,
                     date,
                     time, .keep_all = TRUE) %>% 
  nrow() # Output: 6,980,663 — A small number of duplicates present In the data (i.e. 831)

unique(wagers1$wagerid[duplicated(wagers1$wagerid)]) # Isolate all duplicated wager IDs so that we can explore these

# Explore some random duplicated wagers to see what's going on:
wagers1 %>%
  filter(wagerid == "eed90f35-4ee6-4f9e-b5d0-b9c7b9637a60") # Wager duplicated once – It was a multibet so perhaps this explains the duplicates
# Account balance before and after wager are consistent across these, And the date and time are the same, indicating they are the same wager duplicated.

wagers1 %>%
  filter(wagerid == "af8f8346-fe70-4ef9-9a6a-08d37ad1fafc") # Wager duplicated once – NOT a multibet So this doesn't explain the duplicates alone. Sport type == horseracing

wagers1 %>%
  filter(wagerid == "9d697dd4-1124-43d7-9b9a-a69f7a59ed49") # Duplicate wager not a multibet. Again for horse racing

wagers1 %>%
  filter(wagerid == "794fffc7-d371-49e9-a0aa-0d7cfb75de1d") # Random duplicate again for horse racing

wagers1 %>%
  filter(wagerid == "84c181d8-2b1a-4e96-9064-c75bd72a1989") # Random duplicate again for horse racing

wagers1 %>%
  filter(wagerid == "93341fed-d4fb-42b8-863f-273981dfea7f") # Random duplicate again for horse racing

wagers1 %>%
  filter(wagerid == "ef582591-720b-48c6-bbed-33ce33baf66e") # Random duplicate again for horse racing. These wagers and the above are all exact duplicates across every variable

# Let's see if all horse racing wages are duplicated:
wagers1 %>%
  filter(sport == "Horse Racing") # No, definitely not

# But are all duplicated wages for horseracing
# First let's see how many wagers they are without horseracing:
wagers1 %>%
  filter(sport != "Horse Racing") %>% 
  distinct(wagerid, 
         customerid,
         date,
         time, .keep_all = TRUE) %>% 
  nrow() #Output: 2,781,599

# Now lets see if all of this number increases when not filtering out possible duplicates:
wagers1 %>%
  filter(sport != "Horse Racing") %>% 
  nrow() #Output: 2,781,862. 
# No, 263 duplicates remain in the non-horseracing wagers

# It seems safe to remove duplicate wager IDs for this data:
# Spoke with the operator and they seem to think that this is an error from the original data pull
# From the operator: "they’re in the results by an erroneous element of duplication in the original code"

# Lets make this wager data workable:
wagers1.workable<- wagers1 %>% 
  mutate(wager_date = as.Date(date, "%d/%m/%y")) %>% # Create a usable/subset-able date column 
  select(-date) %>%
  full_join(LimitSetter.window.column.1, by = "customerid") %>% # Enable differentiation between limit setters and non-setters
  distinct(wagerid, 
           customerid,
           wager_date,
           time, .keep_all = TRUE) %>% # Removal of duplicate wagers
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') %>%
  print(n = 20)

# Before moving forward with calculations, let's figure out what equals a win (for the consumer) and a loss based on this data:
wagers1.workable %>%
  filter(net_result >=0)  # Plus value net_results (e.g., 55) indicate the person lost.
wagers1.workable %>%
  filter(net_result <0) # Minus value net_results (e.g., -33) indicate the person won (there are much fewer of these)

# Payout column is what the operator will have to pay out if the customer wins...
# this is mostly irrelevant to calculations here, but it would be nice to have "payout" as what people win. Let's change:
# First, convert negative values to positives for net_result (e.g., convert "-50" to "50")...
# so that we have a positive outcome value for calculations
winners1<- wagers1.workable %>%
  filter(net_result <0) %>% # Isolate winners with negative outcome values
  select(-payout) # Remove this column now so we can replace with one of the same name

net_result.converted<- abs(winners1$net_result) %>% 
  as_tibble() %>%
  rename(payout = value) %>%
  print() #Check conversions are correct

wagers1.winners.only<- bind_cols(winners1,
                                 net_result.converted) %>%
  print()

losers1<- wagers1.workable %>%
  filter(net_result >= 0) %>% # Isolate losers with positive outcome values
  mutate(payout = net_result - net_result) %>%
  print()

wagers.workable.1<- bind_rows(wagers1.winners.only,
                             losers1) %>% # re–join the two datasets
  print() # Check the number of rows is consistent with the original dataset (yes)

# Check new payout column in new dataset:
wagers.workable.1 %>% summarise(
  mean(payout),
  min(payout),
  max(payout)) 

# WAGERS NON-LIMIT SETTERS----
# Calculate wager summaries for non-limit setters *pre* Message 1:
wager.summaries.pre.nonsetters.1<- wagers.workable.1 %>% 
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
  # select(1:3) %>% # View customer IDs for the check below
  print()
# Check outputs seem correct against a random customer ID
wagers.workable.1 %>% 
  filter(wager_date < as.Date("2019-10-14") &
           customerid == "****") %>%
  # summarise(n_distinct(wager_date)) %>%
  # (adding the "summarise" function to the above checks the total no. betting days is correct)
  print(n = 27)
# Note: Negative net loss outcomes indicate the person won money overall in the specified time period

# And new column for the total number of wagers in the 30 days prior to message 1 (eligibility criteria)----
wager.days.last.30.nonsetters.1<- wagers.workable.1 %>% 
  filter(wager_date > as.Date("2019-09-13") &
         wager_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  group_by(customerid) %>%
  summarise(
    last30_betting_days_frequency = n_distinct(wager_date)) %>% 
  print(n = 100)
  
wager.days.last.30.nonsetters.1 %>%
  filter(last30_betting_days_frequency < 5) %>% 
  print(n = 100)

# Add new column for final wager in the day before being sent message 1:
last.wager.column.nonsetters.1<- wagers.workable.1 %>% 
  filter(wager_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  mutate(wager_time = hms(time)) %>%
  arrange(desc(wager_date, 
               wager_time,
               customerid)) %>% 
  distinct(customerid, .keep_all = TRUE) %>%
  mutate(lost_last_bet = payout == 0) %>% 
  select(customerid,
         lost_last_bet) %>%
  print(n = 100) # Checked for accuracy against last bets (i.e., without columns selected)

# Check to see if the distribution of the losses versus wins appears reasonable (yes)
table(last.wager.column.nonsetters.1$lost_last_bet)

# Add column for SD of daily wager pre:
daily.wager.SD.pre.nonsetters.1<- wagers.workable.1 %>% 
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
wager.summaries.post.nonsetters.1<- wagers.workable.1 %>% 
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

# Add column for SD of daily wager:
daily.wager.SD.post.nonsetters.1<- wagers.workable.1 %>% 
  filter(wager_date > as.Date("2019-10-14") 
         & wager_date <= as.Date("2020-01-12") &
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

# Add all new datasets (pre/post message wagers for non-limit setters) to main temp non-setter dataset:
client.condition.limits.transactions.wagers.nonsettersonly.1<- client.condition.limits.transactionsnonsettersonly.1 %>% 
  full_join(wager.summaries.pre.nonsetters.1, by = "customerid") %>%
  full_join(wager.days.last.30.nonsetters.1, by = "customerid") %>%
  full_join(last.wager.column.nonsetters.1, by = "customerid") %>% 
  full_join(daily.wager.SD.pre.nonsetters.1, by = "customerid") %>% 
  full_join(wager.summaries.post.nonsetters.1, by = "customerid") %>% 
  full_join(daily.wager.SD.post.nonsetters.1, by = "customerid") %>% 
  print() # Merge with existing dataset

# Check this new dataset:
client.condition.limits.transactions.wagers.nonsettersonly.1 %>% 
  filter(is.na(total_number_of_bets_post)) %>%
  group_by(customerid, 
           date_account_closed, 
           self_exclusion) %>%
  summarise(
    n(),
  ) %>%
  print(n = 200) # There are 604 unique clients with no wagers post message which seems odd
# Some people appear to have close the account or self excluded during this window, but not the full 604
# Self excluders and those who closed the account will be removed from the data wherever appropriate....
# during the cleaning of the final, combined (all operators) dataset

# Identify new column range to selectively replace NA wager values to allow proper pre and post comparisons 
# NA values would exclude clients from within subjects comparisons, 
# even if they simply did not wager during this time and didn't close their account/self-exclude)
ncol(client.condition.limits.transactions.wagers.nonsettersonly.1)
client.condition.limits.transactions.wagers.nonsettersonly.1 %>% 
  select(60:71) %>% # View the "post" columns to see if there are NA values
  print(n = 200)

# Replace NAs with 0 0 to reflect that no transactions were made, wherever relevant.
client.condition.limits.transactions.wagers.nonsettersonly.1[, c("total_number_of_bets_pre",
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
                                                                   client.condition.limits.transactions.wagers.nonsettersonly.1[, c("total_number_of_bets_pre",
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

client.condition.limits.transactions.wagers.nonsettersonly.1 %>% 
  select(50:69) # Check NAs were removed

# WAGERS LIMIT SETTERS----
# Calculate wager summaries for Limit Setters *Pre* messages:
wager.summaries.pre.setters.1<- wagers.workable.1 %>% 
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

wagers.workable.1 %>%
  mutate(pre.limit.date = window_limit_date - days(90)) %>% # Check accuracy of output against random customer ids 
  filter(wager_date < as.Date(window_limit_date) & 
           wager_date >= as.Date(pre.limit.date) & 
           customerid == "****")  # %>% summarise(n_distinct(wager_date)) 

# And new column for the total number of wages in the previous 30 days (eligibility criteria)----
wager.days.last.30.setters.1<- wagers.workable.1 %>% 
  filter(wager_date > as.Date("2019-09-13") &
           wager_date < as.Date("2019-10-14") &
           window_limit_setter == "LimitSetter") %>%
  group_by(customerid) %>%
  summarise(
    last30_betting_days_frequency = n_distinct(wager_date)) %>% 
  print(n = 100)

# Add new column for final wager in the day before setting their limit:
last.wager.column.setters.1<- wagers.workable.1 %>% 
  filter(wager_date < as.Date(window_limit_date) &
           window_limit_setter == "LimitSetter") %>%
  mutate(wager_time = hms(time)) %>%
  arrange(desc (wager_date, 
                wager_time,
                customerid)) %>% 
  distinct(customerid, .keep_all = TRUE) %>%
  mutate(lost_last_bet = payout <= 0) %>% 
  select(customerid,
         lost_last_bet) %>%
  print() # Checked for accuracy against last bets (i.e., without columns selected)
# Check to see if the distribution of the losses versus wins appears reasonable (yes)
table(last.wager.column.setters.1$lost_last_bet)

# Add column for SD of daily wager:
daily.wager.SD.pre.setters.1<- wagers.workable.1 %>% 
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

# Calculate wager summaries for Limit Setters Post* messages:
wager.summaries.post.setters.1<- wagers.workable.1 %>% 
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

wagers.workable.1 %>%
  mutate(post.limit.date = window_limit_date + days(90)) %>% # Check accuracy of output against random customer ids 
  filter(wager_date > as.Date(window_limit_date) & 
           wager_date <= as.Date(post.limit.date) & 
           customerid == "****") #  %>% summarise(n_distinct(wager_date)) 

# Add column for SD of daily wager:
daily.wager.SD.post.setters.1<- wagers.workable.1 %>% 
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

client.condition.limits.transactions.wagerssetterssonly.1<- client.condition.limits.transactionssettersonly.1 %>% 
  full_join(wager.summaries.pre.setters.1, by = "customerid") %>%
  full_join(wager.days.last.30.setters.1, by = "customerid") %>%
  full_join(last.wager.column.setters.1, by = "customerid") %>%
  full_join(daily.wager.SD.pre.setters.1, by = "customerid") %>%
  full_join(wager.summaries.post.setters.1, by = "customerid") %>% 
  full_join(daily.wager.SD.post.setters.1, by = "customerid") %>%
  print() # Merge with existing dataset


client.condition.limits.transactions.wagerssetterssonly.1 %>% 
  filter(is.na(total_number_of_bets_post)) %>%
  group_by(customerid, 
           date_account_closed, 
           self_exclusion) %>%
  summarise(
    n(),
  ) %>%
  print() #10 clients in this group didn't wager after messages. 
# Two self excluded and two closed their account

# Replace NAs with 0 to reflect that no transactions were made, wherever relevant:
client.condition.limits.transactions.wagerssetterssonly.1[, c("total_number_of_bets_pre",
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
                                                                client.condition.limits.transactions.wagerssetterssonly.1[, c("total_number_of_bets_pre",
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
client.condition.limits.transactions.wagerssetterssonly.1 %>% 
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
         "daily_wager_SD_post") %>% # View the "post" columns to see if there are NA values
  print(n = 200)


# ------------------------------------------------------------------------------
# COMBINE TRANSACTIONS & WAGER DATA SETS FOR BOTH GROUPS
client.condition.limits.transactions.wagers.1<- bind_rows(client.condition.limits.transactions.wagerssetterssonly.1, 
                                                          client.condition.limits.transactions.wagers.nonsettersonly.1) %>% 
  select(-limit_time,                  
         -limit_amount,
         -limit_window) %>% # Remove some irrelevant column left over (all of these have been divided into time periods)
  print()

# Explore net loss summaries for fun:
client.condition.limits.transactions.wagers.1 %>% 
  group_by(window_limit_setter) %>%
  summarise(
    mean(net_loss_pre),
    median(net_loss_pre),
    max(net_loss_pre),
    mean(net_loss_post),
    median(net_loss_post),
    max(net_loss_post)
  ) # summary values appear to have degreased for Limit setters and to a lesser extent non-setters

client.condition.limits.transactions.wagers.1 %>% 
  group_by(window_limit_setter) %>%
  count(lost_last_bet) # Limit setters & non-setters proportions in relation to whether their last bet pre-limit/message was a win/loss

client.condition.limits.transactions.wagers.1 %>% 
  filter(net_loss_pre >= 0) %>%
  group_by(window_limit_setter) %>%
  summarise(
    n() # Most clients in both groups lost money prior to messages
  )

client.condition.limits.transactions.wagers.1 %>% 
  filter(net_loss_pre < 0) %>%
  group_by(window_limit_setter) %>%
  summarise(
    n()
  ) # Only a small proportion (2/106 of limit setters) had a negative net loss (indicating they won money overall)

# ------------------------------------------------------------------------------
# Finalise dataset for analyses
# Create operator column and label to identify client operator
Operator<- rep(x = 1, times = 10000) %>% 
  as_tibble() %>% 
  rename(operator = value) %>%
  print()

client.condition.limits.transactions.wagers.final.1<- bind_cols(Operator, # Add operator column and label to identify client operator
                                                                client.condition.limits.transactions.wagers.1) %>% 
  print()

names1<- names(client.condition.limits.transactions.wagers.final.1)
write.csv(names1, file = "Analysis data/final_names1.csv")

# Create a CSV file from the final dataset so that it can be easily accessed and merged with data sets from other operators:
write.csv(client.condition.limits.transactions.wagers.final.1, file = "Analysis data/Operator 1/Final_dataset_Operator1.csv")

# End