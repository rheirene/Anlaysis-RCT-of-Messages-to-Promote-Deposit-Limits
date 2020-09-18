# Script to clean, wrangle and combine datasets and calculate some descriptive stats for operator 3
# Note: All customer IDs in the code have been removed to protect the anonymity of customers involved in the trial.

# Load any relevant packages
library("ggplot2")
library("forcats")
library("sjmisc")
library("data.table")
library("tidyr")
library("dplyr")
library("yarrr")
library("lubridate")
library(cowplot)
library(readr)

#Remove all objects from work space
rm(list = ls())

# load and name all datasets
conditions3<- read.csv("Analysis data/Operator 3/customerconditions3.csv")
accountclosuredates3<- read.csv("Analysis data/Operator 3/accountclosuredates3.csv")
limits3<- read.csv("Analysis data/Operator 3/depositlimits3.csv")
clients3<- read.csv("Analysis data/Operator 3/clients3.csv") 
transactionspre3<- read.csv("Analysis data/Operator 3/transactionspre3.csv")
transactionspost3<- read.csv("Analysis data/Operator 3/transactionspost3.csv")
wagers3<- read.csv("Analysis data/Operator 3/wager3.csv")

# Calculate demographic characteristics for the sample ------------------------------------------------------------------------------
# check *CONDITIONS* dataset before use (primary dataset used to divide participants)
head(conditions3)

conditions3 %>% 
  count(condition) # total number of participants per condition

# Check clients dataset before use
head(clients3) 

summary(clients3$age) # summary figures for age (total sample)
summary(clients3$gender) # gender distribution (total sample)

# The date account closed column doesn't work for the clients dataset and so a new dataset was provided
# Lets make this new dataset workable and join it to the old one
accountclosuredates3

# First check whether the customer IDs are consistent:
accountclosuredates3 %>%
  full_join(clients3, by = "customerid") %>% # Does joining by this ID leave the same number of participants?
  nrow() # Yes

# Make usable:
accountdates3<- accountclosuredates3 %>%
  select(-X,
         -customerid.1,
         -customersignupdatetime.1,
         -accountcloseddate.1)  %>% # Remove irrelevant columns:
  mutate(date_account_closed = as.Date(accountcloseddate, format = "%d/%m/%y")) %>% #  Make workable date columns
  mutate(date_account_opened = as.Date(customersignupdatetime, format = "%d/%m/%y")) %>% #  Make workable date columns
  select(-accountcloseddate,
         -customersignupdatetime) %>% 
  as_tibble() %>% 
  print()

# Merge condition dataset with the clients dataset and the account closure/opening dates data
combined.condition.clients3<- conditions3 %>% 
  full_join(clients3, by = "customerid") %>% 
  full_join(accountdates3, by = "customerid") %>% 
  select(-accountcloseddate,
         -customersignupdatetime) %>% 
  as_tibble() %>% 
  print()

combined.condition.clients3 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  count(gender) # gender distribution (total sample, duplicates removed) 

combined.condition.clients3 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  summarise(
    agemean = mean(age),
    agemedian = median(age),
    agemin = min(age),
    agemax = max(age),
    n = n()
  )  # summary figures for age (total sample, duplicates removed)

demographic.summary.2<- combined.condition.clients3 %>% 
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

combined.condition.clients3 %>%
  group_by(condition,
           gender) %>% 
  summarise(
    total = n()
  ) %>% 
  group_by(condition) %>% 
  mutate(percentage = total/sum(total)*100) # Percentage of males and females in each condition

combined.condition.clients3 %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  group_by(condition) %>% 
  summarise(
    agemean = mean(age),
    agemedian = median(age),
    agemin = min(age),
    agemax = max(age),
    n = n()
  ) # Demographic breakdown by condition only

# Summaries of RG tool use ------
#Check deposit limits dataset before use
head(limits3)

table(limits3$limittype)
# This operator has included all CPTs under the title of limits.
# Let's start by splitting up on the data according to each CPT

# Limits
limits.workable.3<- conditions3 %>% 
  full_join(limits3, by = c("customerid")) %>% # Merge Limit dataset with conditions dataset
  rename(limit_window = limittype) %>%
  filter(limit_window == "DepositLimitMonth" | 
           limit_window == "DepositLimitDay" | 
           limit_window == "DepositLimitWeek"| 
           limit_window == "CoolingOffDepositLimit") %>% 
  mutate(limit_date = as.Date(limitdate, format = "%d/%m/%y")) %>% # Make a usable date column
  mutate(limit_amount = as.numeric(limitvalue)) %>%
  select(-limitdate,
         -limitvalue) %>%
  as_tibble() %>%
  print(n = 100) 

# 
limits.workable.3 %>%
  arrange(customerid) %>%
  select(4:11) %>%
  print(n = 100)


limits.workable.3 %>% group_by(condition, 
                              limit_window) %>%
  summarise(
    Individuals = n_distinct(customerid),
    RGtools = n()
  ) %>% 
  print (n = 32) # calculate total number of limits EVER used across conditions
  
limits.workable.3 %>% filter(limit_window == "DepositLimitMonth" | 
                                       limit_window == "DepositLimitDay" | 
                                       limit_window == "DepositLimitWeek"| 
                                       limit_window == "CoolingOffDepositLimit") %>% 
  distinct(customerid, .keep_all = TRUE) %>% 
  group_by(limit_window) %>%
  summarise(
    RGtools = n(),
  ) 
# Including "CoolingOff" in the above formulas appears to add a unique cases to this list That are not covered by any of the other limit types. 
# This suggests "CoolingOff" refers to a different form of RG tool or the removal of limits and therefore they are omitted here

# Timeouts
timeouts3<- limits3 %>% 
  rename(timeout = limittype,
         timeout_duration = limitvalue) %>%
  filter(timeout == "TimeOut") %>% 
  mutate(timeout_start_date = as.Date(limitdate, format = "%d/%m/%y")) %>% # Make a usable Start date column
  mutate(timeout_finish_date = as.Date(todate, format = "%d/%m/%y")) %>% # Make a usable end date column
  select(-limitdate,
         -todate) %>%
  as_tibble() %>%
  print() 

# Self exclusion
selfexclusion3<- limits3 %>% 
  rename(self.exclusion = limittype, # full stop used instead of "_" as this is temporary
         self_exclusion_duration = limitvalue,
         self_exclusion_start_time = limittime) %>%
  filter(self.exclusion == "SelfExclusion") %>% 
  mutate(self_exclusion_start_date = as.Date(limitdate, format = "%d/%m/%y")) %>% # Make a usable Start date column
  mutate(self_exclusion_finish_date = as.Date(todate, format = "%d/%m/%y")) %>% # Make a usable end date column
  select(-limitdate,
         -todate) %>%
  as_tibble() %>%
  print() 

# ----
# Deposit limits Summaries----
# ----
# Start by joining the limits dataset with the clients and conditions datasets
client.condition.limit1.3 <-limits.workable.3 %>% 
  select(-condition) %>% 
  full_join(combined.condition.clients3, by = "customerid") %>% 
  as_tibble() %>% 
  print()

# Isolate and table those who set a limit within 5 days of viewing *either* message (also add demographic breakdown)
response.to.messages<- client.condition.limit1.3 %>% 
  filter(limit_date >= as.Date("2019-10-14") &
           limit_date <= as.Date("2019-10-23")) %>%
  filter(limit_window == "DepositLimitDay"|
           limit_window == "DepositLimitWeek"|
           limit_window == "DepositLimitMonth") %>%
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
  print(n = 100)

sum(response.to.messages$Total.participants)# Total Limit setters with duplicates
sum(response.to.messages$unique.participants) # Total Limit setters without duplicates

# Isolate And table those who set a limit within 5 days of viewing the *first* message
response.to.message1<- client.condition.limit1.3 %>% 
  filter(limit_date >= as.Date("2019-10-14") & 
           limit_date <= as.Date("2019-10-18")) %>%
  filter(limit_window == "DepositLimitDay"|
           limit_window == "DepositLimitWeek"|
           limit_window == "DepositLimitMonth") %>%
  group_by(condition) %>%
  summarise(
    unique.participants = n_distinct(customerid),
    Total.participants = n()
  ) %>% 
  print ()

sum(response.to.message1$Total.participants) # Total Limit setters with duplicates (Message 1)
sum(response.to.message1$unique.participants) # Total Limit setters without duplicates (Message 1)

# Isolate and table those who set a limit within 5 days of viewing the *Second* message
response.to.message2<- client.condition.limit1.3 %>% 
  filter(limit_date >= as.Date("2019-10-19") &
           
           limit_date <= as.Date("2019-10-23")) %>%
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
# Note: Most people set limits after seeing the second message****

# ------------------------------------------------------------------------------
# End of summary statistics for operator 3
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Prepare datasets for confirmatory and exploratory analyses
# ------------------------------------------------------------------------------

# Responsible gambling tool dataset
# Start by adding separate columns that display RG tool use at the different time periods
# To avoid any errors in coding at this stage, all of the new columns include duplicates.

#
# LIMITS ----
#
# Create a new column that splits the dataset into limit setters (i.e. Within the window of interest) and non-limit setters***
LimitSetter.window.column.3<- client.condition.limit1.3 %>% 
  filter(limit_date >= as.Date("2019-10-14") &
           limit_date <= as.Date("2019-10-23") &
           limit_window != "CoolingOffDepositLimit") %>%
  mutate(window_limit_setter = fct_recode(limit_window,
                                          "LimitSetter" = "DepositLimitDay",
                                          "LimitSetter" = "DepositLimitWeek",
                                          "LimitSetter" = "DepositLimitMonth")) %>%
  select(customerid,
         window_limit_setter,
         limit_date,
         limit_window,
         limit_amount) %>%
  rename(window_limit_date = limit_date,
         limit_window_setters = limit_window,
         window_limit_amount = limit_amount) %>%
  arrange(window_limit_date) %>%
  print(n = 108)

# Create a new column that identifies anyone who set a limit prior to messages
# "ChangeToDepositLimit" included in this group as well...
# as if somebody changed a deposit limit then they must have had one at some point
limit_setter_pre.column.3<- client.condition.limit1.3 %>%   
  filter(limit_date < as.Date("2019-10-14") &
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

# Create a new column that identifies *anyone* who set a limit post message one 
# (allows for later excluding non-Window.limit.setters who set limits later on to be excluded from exploratory analyses of limit effectiveness)
# "ChangeToDepositLimit" Not included in this group
LimitSetter.post.all.column.temp.3<- client.condition.limit1.3 %>%   
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
  print(LimitSetter.post.all.column, n = 150)
# Now remove Window.limit.setters from this column to easily exclude non-Window.limit.setters who set limits later on:
LimitSetter.post.all.column.3<- anti_join(LimitSetter.post.all.column.temp.3, 
                                          LimitSetter.window.column.3) %>%
  print()

LimitSetter.post.all.column.3 %>%
  filter(customerid == "****") # Check random window limit setters aren't in this dataset

# Check limit setter window column isn't affected by removing those who set limits after messages who weren't window limit setters:
anti_join(LimitSetter.window.column.3, 
          LimitSetter.post.all.column.3) %>%
  print()

#
# TIMEOUTS ----
#

# # Create a new column to identify those who have used time outs prior to messages
TimeOut.pre.column.3<- timeouts3 %>%
  filter(timeout_start_date <= as.Date("2019-10-13")) %>%
  mutate(timeout_pre = fct_recode(timeout,
                                        "TimeOutPre" = "TimeOut")) %>%  # Create a factor vector to identify this group
  rename(timeout_duration_pre = timeout_duration,
         timeout_start_date_pre = timeout_start_date,
         timeout_finish_date_pre = timeout_finish_date) %>% 
  select(-limittime, 
         -totime,
         -cdwaccountlimitid,
         -accountlimitsk,
         -timeout,
         -operatorid) %>% 
  print()

# # Create a new column to identify those who used time outs following the first message
TimeOut.post.column.3<- timeouts3 %>%
  filter(timeout_start_date > as.Date("2019-10-13")) %>%
  mutate(timeout_post = fct_recode(timeout,
                                  "TimeOutPost" = "TimeOut")) %>% # Create a factor vector to identify this group
  rename(timeout_duration_post = timeout_duration,
         timeout_start_date_post = timeout_start_date,
         timeout_finish_date_post = timeout_finish_date) %>% 
  select(-limittime, 
         -totime,
         -cdwaccountlimitid,
         -accountlimitsk,
         -timeout,
         -operatorid) %>% 
  arrange(timeout_start_date_post) %>%
  print()

#
# SELF EXCLUSION ----
#
# Create a new column to identify those who used self exclusion
# Note: This operator has a temporary self exclusion option and so more detail needs to be included (E.g. duration)
table(selfexclusion3$self_exclusion_duration) # The durations available seem to be all over the place (From zero to over 2,000,000 and various random numbers in between)

SelfExclusion.column.3<- selfexclusion3 %>%
  mutate(self_exclusion = fct_recode(self.exclusion,
                                   "SelfExcluder" = "SelfExclusion")) %>% # Create a factor vector to identify this group
  arrange(self_exclusion_start_date) %>%
  select(-totime,
         -cdwaccountlimitid,
         -accountlimitsk,
         -operatorid,
         -self.exclusion,
         -self_exclusion_start_time) %>% 
  as_tibble() %>%
  print()

# Add all new columns to dataset
client.condition.limit3.final<- combined.condition.clients3 %>%
  full_join(LimitSetter.window.column.3, by = "customerid")  %>%
  full_join(limit_setter_pre.column.3, by = "customerid")  %>%
  full_join(LimitSetter.post.all.column.3, by = "customerid")  %>%
  full_join(TimeOut.pre.column.3, by = "customerid")  %>%
  full_join(TimeOut.post.column.3, by = "customerid")  %>%
  full_join(SelfExclusion.column.3, by = "customerid")  %>%
  distinct(customerid, .keep_all = TRUE) %>% 
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') %>% # Remove all the "NA" values in the factor columns just added
  print()
 
client.condition.limit3.final %>% select(22:30) %>%
  print() # Check new dataset has all merged correctly and "NA" values have been replaced

#
# TRANSACTIONS DATA -----
#

# Add pre and post-message transactions datasets and combine them
transactionspre.3.tib<- transactionspre3 %>%
  as_tibble() %>% 
  rename(transactiondate.old = transactiondate) %>%
  mutate(transaction_date = as.Date(transactiondate.old, "%d/%m/%y")) %>%
  select(-transactiondate.old) %>%
  print(n = 30) # Convert to tibble, convert date column to date format, and check setup of the dataset 

transactionspost.3.tib<- transactionspost3 %>% 
  as_tibble() %>% 
  rename(transactiondate.old = transactiondate) %>%
  mutate(transaction_date = as.Date(transactiondate.old, "%d/%m/%y")) %>%
  select(-transactiondate.old) %>%
  print(n = 30) # Convert to tibble, convert date column to date format, and check setup of the dataset 

transactions.combined.withlimits.3<- bind_rows(transactionspre.3.tib, 
                                    transactionspost.3.tib) %>% # Merge transactions datasets
  full_join(LimitSetter.window.column.3, by = "customerid") %>% # Add Column that identifies limit setters (i.e., within the window of interest) so that the data can be divided accordingly
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') %>%
  rename(transaction_type = transactiontype,
         transaction_amount = transactionamount) %>%
  print() 

# Let's check whether transaction IDs are unique to every transaction or whether there are any duplicated.
nrow(transactions.combined.withlimits.3)
# Output: 485,174

transactions.combined.withlimits.3 %>% 
  distinct(paymentid, 
           customerid,
           transaction_date,
           transactiontime, .keep_all = TRUE) %>% 
  nrow() # Output: 480,652
# Discrepancy:
485174-480652
# This is only 4522 transactions (a small proportion)

unique(transactions.combined.withlimits.3$paymentid[duplicated(transactions.combined.withlimits.3$paymentid)]) # Isolate all duplicated transaction IDs so that we can explore these

# Explore some random duplicated Transactions to see what's going on:
transactions.combined.withlimits.3 %>%
  filter(paymentid == "159798723") # Transaction duplicated once – All the the variables are exactly the same, indicating they are the same transaction duplicated.

transactions.combined.withlimits.3 %>%
  filter(paymentid == "159881113") # Transaction duplicated once – All the the variables are exactly the same

transactions.combined.withlimits.3 %>%
  filter(paymentid == "159817294") # Transaction duplicated once – All the the variables are exactly the same

# All of the above are for Limit setters — Is this an artefact of the joining of the datasets? Let's check with the datasets pre-join:
nrow(transactionspre.3.tib)
# Output: 220760

transactionspre.3.tib %>% 
  distinct(paymentid, 
           customerid,
           transaction_date,
           transactiontime, .keep_all = TRUE) %>% 
  nrow() # Output: 220760

nrow(transactionspost.3.tib)
# Output: 259889

transactionspost.3.tib %>% 
  distinct(paymentid, 
           customerid,
           transaction_date,
           transactiontime, .keep_all = TRUE) %>% 
  nrow() # Output: 259889

# Yes, it was an artefact of joining the datasets

table(transactions.combined.withlimits.3$transaction_type) # Check all of the different transaction types within the data

# Create usable summary figures re transactions pre and post messages:

# Cleaning note: There's a need to label all NA values returned 
# (i.e., no transactions as == "0" to allow for within group comparisons pre—post 
# when some individuals don't have any transactions for one of the time periods)
# do this after creating each set of summary values as it will be easier to isolate and replace the target NA values

# NON-limit setters----
# Summary transaction figures for non-limit setters *Pre* Message 1
transaction.summaries.pre.nonsetters.3<- transactions.combined.withlimits.3 %>% 
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
  rename(total_deposit_amount_pre = total_transaction_amount_D, 
         total_withdrawal_amount_pre = total_transaction_amount_W, 
         mean_deposit_pre = mean_transactionamount_D, 
         mean_withdrawal_pre = mean_transactionamount_W, 
         number_of_deposits_pre = total_number_transactions_D, 
         number_of_withdrawals_pre = total_number_transactions_W,
         median_deposit_pre = median_transactionamount_D,
         median_withdrawal_pre = median_transactionamount_W,
         min_deposit_pre = min_transactionamount_D,
         min_withdrawal_pre = min_transactionamount_W,
         max_deposit_pre = max_transactionamount_D,
         max_withdrawal_pre = max_transactionamount_W
  ) %>% 
  as_tibble() %>% 
  mutate_if(is.numeric,list(~replace_na(.,0))) %>%
  print(n = 30)

names(transaction.summaries.pre.nonsetters.3) # Check all new names are correct

transactionspre.3.tib %>% 
  filter(customerid == "****") %>% 
  print() # Check accuracy of output against random customer ids

# Summary transaction figures for non-limit setters *Post* Message 1
transaction.summaries.post.nonsetters.3<-  transactions.combined.withlimits.3 %>% 
  filter(transaction_date > as.Date("2019-10-14")  &
           transaction_date <= as.Date("2020-01-12") & 
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
  rename(total_deposit_amount_post = total_transaction_amount_D, 
         total_withdrawal_amount_post = total_transaction_amount_W, 
         mean_deposit_post = mean_transactionamount_D, 
         mean_withdrawal_post = mean_transactionamount_W, 
         number_of_deposits_post = total_number_transactions_D, 
         number_of_withdrawals_post = total_number_transactions_W,
         median_deposit_post = median_transactionamount_D,
         median_withdrawal_post = median_transactionamount_W,
         min_deposit_post = min_transactionamount_D,
         min_withdrawal_post = min_transactionamount_W,
         max_deposit_post = max_transactionamount_D,
         max_withdrawal_post = max_transactionamount_W
  ) %>% 
  mutate_if(is.numeric,list(~replace_na(.,0))) %>% # Replace All NAs with 0
  as_tibble() %>% 
  print(n = 20)

transactionspost.3.tib %>% 
  filter(transaction_date > as.Date("2019-10-14")  &
           transaction_date <= as.Date("2020-01-12")) %>%
  filter(customerid == "****") # Check accuracy of output against random customer ids

 # Add all new datasets to temporary non-setter main dataset
client.condition.limits.transactionsnonsettersonly.3<- client.condition.limit3.final %>% 
  filter(window_limit_setter == "None") %>%
  full_join(transaction.summaries.pre.nonsetters.3, by = "customerid") %>% 
  full_join(transaction.summaries.post.nonsetters.3, by = "customerid") %>% 
  print() # Merge with existing dataset

# Identify new column range to selectively replace NA values
ncol(client.condition.limits.transactionsnonsettersonly.3)
client.condition.limits.transactionsnonsettersonly.3 %>%
  select(33:55) 

# Replace NAs with 0 to reflect that no transactions were made, wherever relevant.
# Although we already remove any values from the original calculations, any participant without transactions for either period will have NA values once the datasets are merged
client.condition.limits.transactionsnonsettersonly.3[, c("total_deposit_amount_pre", 
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
                                                         "max_withdrawal_post") ][is.na(client.condition.limits.transactionsnonsettersonly.3[, c("total_deposit_amount_pre", 
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
client.condition.limits.transactionsnonsettersonly.3 %>% 
  select(condition, 
         customerid,
         window_limit_setter,
         40:50) %>% 
  print(n = 20)

# LIMIT SETTERS----
# Calculate transaction summaries for Limit Setters *Pre* messages
transaction.summaries.pre.setters.3<- transactions.combined.withlimits.3 %>% 
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
  rename(total_deposit_amount_pre = total_transaction_amount_D, 
         total_withdrawal_amount_pre = total_transaction_amount_W, 
         mean_deposit_pre = mean_transaction_amount_D, 
         mean_withdrawal_pre = mean_transaction_amount_W, 
         number_of_deposits_pre = total_number_transactions_D, 
         number_of_withdrawals_pre = total_number_transactions_W,
         median_deposit_pre = median_transaction_amount_D,
         median_withdrawal_pre = median_transaction_amount_W,
         min_deposit_pre = min_transaction_amount_D,
         min_withdrawal_pre = min_transaction_amount_W,
         max_deposit_pre = max_transaction_amount_D,
         max_withdrawal_pre = max_transaction_amount_W
  ) %>% 
  mutate_if(is.numeric,list(~replace_na(.,0))) %>% # Replace All NAs with 0
  print(n = 30)

transactions.combined.withlimits.3 %>% 
  mutate(pre.limit.date = window_limit_date - days(90)) %>% # Check accuracy of output against random customer ids 
  filter(transaction_date < as.Date(window_limit_date) &
           transaction_date >= as.Date(pre.limit.date) &
           customerid == "****")

# Summary transaction figures for Limit Setters *Post* Message 1:
transaction.summaries.post.setters.3<- transactions.combined.withlimits.3 %>% 
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
  rename(total_deposit_amount_post = total_transaction_amount_D, 
         total_withdrawal_amount_post = total_transaction_amount_W, 
         mean_deposit_post = mean_transaction_amount_D, 
         mean_withdrawal_post = mean_transaction_amount_W, 
         number_of_deposits_post = total_number_transactions_D, 
         number_of_withdrawals_post = total_number_transactions_W,
         median_deposit_post = median_transaction_amount_D,
         median_withdrawal_post = median_transaction_amount_W,
         min_deposit_post = min_transaction_amount_D,
         min_withdrawal_post = min_transaction_amount_W,
         max_deposit_post = max_transaction_amount_D,
         max_withdrawal_post = max_transaction_amount_W
  ) %>% 
  mutate_if(is.numeric,list(~replace_na(.,0))) %>% # Replace All NAs with 0
  print(n = 30)

#Check accuracy of output against original transactions datasets using random customer ids:
transactions.combined.withlimits.3 %>% 
  mutate(post.limit.date = window_limit_date + days(90)) %>%
  filter(transaction_date > as.Date(window_limit_date) & 
           transaction_date <= as.Date(post.limit.date) & 
           customerid == "****") %>%
  print(n = 34)

# Add all new datasets to temporary setter main dataset
client.condition.limits.transactionssettersonly.3<- client.condition.limit3.final %>% 
  filter(window_limit_setter == "LimitSetter") %>%
  full_join(transaction.summaries.pre.setters.3, by = "customerid") %>% 
  full_join(transaction.summaries.post.setters.3, by = "customerid") %>% 
  print() # Merge with existing dataset

# Identify new column range to selectively replace NA values
ncol(client.condition.limits.transactionssettersonly.3)
client.condition.limits.transactionssettersonly.3 %>%
  select(33:55)

# Replace NAs with 0 to reflect that no transactions were made, wherever relevant.
client.condition.limits.transactionssettersonly.3[, c("total_deposit_amount_pre", 
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
                                                      "max_withdrawal_post") ][is.na(client.condition.limits.transactionssettersonly.3[, c("total_deposit_amount_pre", 
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

client.condition.limits.transactionssettersonly.3 %>% 
  select(condition, 
         customerid, 
         window_limit_setter, 40:50) %>% 
  print() # Check accuracy of merge

#
# WAGERING DATA -----
#
head(wagers3)
nrow(wagers3)

# Add column names (missing from original dataset):
colnames(wagers3, do.NULL = FALSE)
colnames(wagers3) <- c("wagerid",	
                       "customerid",	
                       "date.time",
                       "wager.channel",
                       "eventclassparentcategory",
                       "bet.type",
                       "fixedprice.win",
                       "fixedprice.place",
                       "is.multibet",
                       "stake",
                       "bonusbet_stake",
                       "payout",
                       "bonusbet_payout",
                       "power.play.flag")


wagers3.tib<- wagers3 %>% 
  as_tibble() %>%
  print(n = 20) # Convert to tibble and check new column names 

wagers3.tib %>% 
  distinct(wagerid,
           customerid,
           date.time, .keep_all = TRUE) %>% 
  nrow() # Check all customers wagered at least once during the period of study (yes)

# Let's also check whether wager IDs are unique to every wager or whether there are any duplicated:
nrow(wagers3.tib)
# Output:5,733,424 rows

wagers3.tib %>%
  distinct(wagerid, .keep_all = TRUE) %>% 
  nrow() # Output: 3,627,844 rows
# Are there the same number of wager IDs as wagers? 
# This is a difference of 2,105,580 wagers

View(wagers3.tib) # Take a look at the data and explore some of these duplicated wager ids and check there are no NA rows at the end
# No NA rows present

# Could this be explained by the number of multibets in the data:
table(wagers3.tib$is.multibet)
# Output:
# 0       1 
# 3104102 2,629,322 
#  The number "1"s aren't equivalent to the difference between the wager datasets with and without duplicates
# However, this could be because it includes the unique identifiers for the multibets
# If we exclude multibets are all wager IDs unique:
wagers3.tib %>%
  filter(is.multibet != "1") %>%
  distinct(wagerid, .keep_all = TRUE) %>% 
  nrow()
# Yes they are!!!!

# Double check: Does adding the unique number of multi-bets (below pipline)...
# to the unique number of non-multi-bets (i.e., 3104102) result in the same overall number of unique bets (i.e., 3,627,844):
wagers3.tib %>%
  filter(is.multibet == "1") %>%
  distinct(wagerid, .keep_all = TRUE) %>% 
  nrow() # Output: 523742
# Equation:
3104102 + 523742
# Yup!!!!

# Let's do some further digging just in case
# Check whether the wager types have anything to do with it (e.g.. "errors")
table(wagers3.tib$bet.type) # Nope

unique(wagers3.tib$wagerid[duplicated(wagers3.tib$wagerid)]) # Isolate all duplicated wager IDs so that we can explore these

# Explore some random duplicated wagers to see what's going on:
wagers3.tib %>%
  filter(wagerid == "1292447623") # Wager duplicated three times (I.e. four instances in total) – all wager details are the same other than "fixedprice.win". Bet type == Multi bet

wagers3.tib %>%
  filter(wagerid == "1292176632") # # Wager duplicated once times (I.e. two instances in total) – again, all wager details are the same other than "fixedprice.win". Bet type == Multi bet

wagers3.tib %>%
  filter(wagerid == "1292456496") # Wager duplicated 9 times but this time not all wager details are the same: Differences in sport and "fixedprice.win". Bet type == Multi bet

# It seems safe to remove duplicate wager IDs for this data as they all represent multibets and only have one stake and payout
# Confirmed with the operator that this is the case

wagers3.tib %>% 
  filter(payout == "0" &
           bonusbet_payout > 0) # Check whether bonus bet payouts can be made independent of regular payouts:
# Yes they can. Bonus bets can be made independent of regular bets and therefore...
# the column "payout" cannot be used in isolation to determine whether a person's bet was a W/L

# Lets make this wager data workable going forward:
wagers3.tib.workable<- wagers3.tib %>%
  separate(date.time, into = c("date",
                               "wager_time"), sep =" ") %>% # Create a useable/subsetable date column 
  mutate(wager_date = as.Date(date)) %>%
  select(-date) %>%
  distinct(wagerid, 
           customerid,
           wager_date,
           wager_time,
           .keep_all = TRUE) %>% # Removal of duplicate wagers
  full_join(LimitSetter.window.column.3, by = "customerid") %>% # Add column that identifies limit setters (i.e., within the window of interest) so that the data can be divided accordingly
  mutate_if(is.factor, fct_explicit_na, na_level = 'None') %>%
  print(n = 20)

# Doublecheck there are the same number of distinct wager IDs
wagers3.tib.workable %>%
  distinct(wagerid, .keep_all = TRUE) %>% 
  nrow() # Output: 3,627,844 rows (Same as original data)

# NON-limit setters----
# Calculate wager summaries for non-limit setters *pre* Message 1
wager.summaries.pre.nonsetters.3<- wagers3.tib.workable %>% 
  filter(wager_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  group_by(customerid) %>%
  summarise(
    total_number_of_bets_pre = n(),
    betting_days_frequency_pre = n_distinct(wager_date),
    total_amount_wagered_pre = sum(stake),
    bonusbet_amount_wagered_pre = sum(bonusbet_stake), # Don't include the stake for bonus bets in total stake or net loss calculations as this isn't the customers' money. Only inlcude payout, which is equal to payout - bonus bet stake.
    regular_payout_pre = sum(payout),
    bonusbet_payout_pre = sum(bonusbet_payout)
    ) %>%
  mutate(total_amount_won_pre = (bonusbet_payout_pre - bonusbet_amount_wagered_pre) + regular_payout_pre) %>%  # For bonus bets, customers only keep the amount won and not what was wagered.
  mutate(average_daily_wager_pre = total_amount_wagered_pre/ betting_days_frequency_pre) %>%
  mutate(betting_intensity_pre = total_number_of_bets_pre/ betting_days_frequency_pre) %>%
  mutate(net_loss_pre = total_amount_wagered_pre - total_amount_won_pre) %>%
  print()

# Note: Negative net loss outcomes indicate the person won money overall in the specified time period
wager.summaries.pre.nonsetters.3 %>%
  select(6:12) %>%
  print(n = 30) # Check new columns have added and appear correct

# Check outputs seem correct against a random customer ID
wagers3.tib.workable %>% 
  filter(wager_date < as.Date("2019-10-14") &
           customerid == "****") %>%
  summarise(n_distinct(wager_date)) %>%
  # (adding the "summarise" function to the above checks the total no. betting days is correct)
  print(n = 57)

# And new column for the total number of wagers in the 30 days prior to message 1 (eligibility criteria)----
wager.days.last.30.nonsetters.3<- wagers3.tib.workable %>% 
  filter(wager_date > as.Date("2019-09-13") &
           wager_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  group_by(customerid) %>%
  summarise(
    last30_betting_days_frequency = n_distinct(wager_date)) %>% 
  print(n = 100)

wager.days.last.30.nonsetters.3 %>%
  filter(last30_betting_days_frequency  < 5)

# Add new column for final wager in the day before being sent message 1
last.wager.column<- wagers3.tib.workable %>% 
  filter(wager_date < as.Date("2019-10-14") &
           window_limit_setter == "None") %>%
  arrange(desc (wager_date,
                wager_time, 
                customerid)) %>% 
  distinct(customerid, .keep_all = TRUE) %>%
  mutate(total_payout = payout + bonusbet_payout) %>%
  mutate(lost_last_bet = total_payout == 0) %>% 
  select(customerid,
         lost_last_bet) %>%
  print() # Checked for accuracy against last bets (i.e., without columns selected)

# Add column for SD of daily wager
daily.wager.SD.pre.nonsetters.3<- wagers3.tib.workable %>% 
  filter(wager_date < as.Date("2019-10-14") & 
           window_limit_setter == "None") %>%
  group_by(customerid,
           wager_date) %>%
  summarise(
    dailywager = sum(stake + bonusbet_stake)
    ) %>%
  group_by(customerid) %>% 
  summarise(
    daily_wager_SD_pre = sd(dailywager)
  ) %>%
  print()

# Calculate wager summaries for non-limit setters *post* Message 1
wager.summaries.post.nonsetters.3<-
wagers3.tib.workable %>% 
  filter(wager_date > as.Date("2019-10-14") & 
           wager_date <= as.Date("2020-01-12") & 
           window_limit_setter == "None") %>%
  group_by(customerid) %>%
  summarise(
    total_number_of_bets_post = n(),
    betting_days_frequency_post = n_distinct(wager_date),
    total_amount_wagered_post = sum(stake),
    bonusbet_amount_wagered_post = sum(bonusbet_stake), # Don't include the stake for bonus bets in total stake or net loss calculations as this isn't the customers' money. Only inlcude payout, which is equal to payout - bonus bet stake.
    regular_payout_post = sum(payout),
    bonusbet_payout_post = sum(bonusbet_payout)
  ) %>%
  mutate(total_amount_won_post = (bonusbet_payout_post - bonusbet_amount_wagered_post) + regular_payout_post) %>%  # For bonus bets, customers only keep the amount won and not what was wagered.
  mutate(average_daily_wager_post = total_amount_wagered_post/ betting_days_frequency_post) %>%
  mutate(betting_intensity_post = total_number_of_bets_post/ betting_days_frequency_post) %>%
  mutate(net_loss_post = total_amount_wagered_post - total_amount_won_post) %>%
  print()

# Add column for SD of daily wager
daily.wager.SD.post.nonsetters.3<- wagers3.tib.workable %>% 
  filter(wager_date > as.Date("2019-10-14") & 
           wager_date <= as.Date("2020-01-12") & 
           window_limit_setter == "None") %>%
  group_by(customerid,
           wager_date) %>%
  summarise(
    dailywager = sum(stake + bonusbet_stake)
  ) %>%
  group_by(customerid) %>% 
  summarise(
    daily_wager_SD_post = sd(dailywager)
  ) %>%
  print()

# Add all new datasets (pre/post message wagers for non-limit setters) to main temp non-setter dataset
client.condition.limits.transactions.wagers.nonsettersonly.3<- client.condition.limits.transactionsnonsettersonly.3 %>% 
  full_join(wager.summaries.pre.nonsetters.3, by = "customerid") %>%
  full_join(wager.days.last.30.nonsetters.3, by = "customerid") %>%
  full_join(last.wager.column, by = "customerid") %>% 
  full_join(daily.wager.SD.pre.nonsetters.3, by = "customerid") %>% 
  full_join(wager.summaries.post.nonsetters.3, by = "customerid") %>% 
  full_join(daily.wager.SD.post.nonsetters.3, by = "customerid") %>% 
  print() # Merge with existing dataset

client.condition.limits.transactions.wagers.nonsettersonly.3 %>% 
  filter(customerid == "****504") %>% 
  select(timeout_start_date_post,# Check this customer's use of RG tools (no tools of any type, including self exclusion, used)
         self_exclusion_start_date, # as their gambling seemed to change drastically from pre to post message, despite not setting a limit 
         date_account_closed)  # They didn't close their account either suggesting they just stopped wagering using this account

client.condition.limits.transactions.wagers.nonsettersonly.3 %>% 
  filter(is.na(total_number_of_bets_post)) %>%
  select(customerid,
           date_account_closed,
           timeout_start_date_post,
           self_exclusion_start_date) %>%
  print(n = 359) # There are 359 unique clients with no wagers post message which seems odd
# Only 1 of these self excluded post-message and...
# only 3 closed their account suggesting this can't explain the change.
# Self excluders and those who closed the account will be....
# removed from the data wherever appropriate during the cleaning of the final, combined (all operators) dataset

# Identify new column range to selectively replace NA wager values to allow proper pre and post comparisons 
# — NA values would exclude clients from within subjects comparisons, 
# even if they simply did not wager during this time and didn't close their account/self-exclude)
ncol(client.condition.limits.transactions.wagers.nonsettersonly.3)

client.condition.limits.transactions.wagers.nonsettersonly.3 %>% 
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

# Replace NAs with 0 0 to reflect that no wagers were made, wherever relevant.
client.condition.limits.transactions.wagers.nonsettersonly.3[, c("total_number_of_bets_pre",
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
                                                                   client.condition.limits.transactions.wagers.nonsettersonly.3[, c("total_number_of_bets_pre",
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

client.condition.limits.transactions.wagers.nonsettersonly.3 %>% 
  select(55:68) # Check NAs were removed

# LIMIT SETTERS----
# Calculate wager summaries for Limit Setters *Pre* messages:
wager.summaries.pre.setters.3<- wagers3.tib.workable %>% 
  filter(window_limit_setter == "LimitSetter") %>% 
  mutate(pre.limit.date = window_limit_date - days(90)) %>% 
  filter(wager_date < as.Date(window_limit_date) &
           wager_date >= as.Date(pre.limit.date)) %>% 
  group_by(customerid) %>%
  summarise(
    total_number_of_bets_pre = n(),
    betting_days_frequency_pre = n_distinct(wager_date),
    total_amount_wagered_pre = sum(stake),
    bonusbet_amount_wagered_pre = sum(bonusbet_stake), # Don't include the stake for bonus bets in total stake or net loss calculations as this isn't the customers' money. Only inlcude payout, which is equal to payout - bonus bet stake.
    regular_payout_pre = sum(payout),
    bonusbet_payout_pre = sum(bonusbet_payout)
  ) %>%
  mutate(total_amount_won_pre = (bonusbet_payout_pre - bonusbet_amount_wagered_pre) + regular_payout_pre) %>%  # For bonus bets, customers only keep the amount won and not what was wagered.
  mutate(average_daily_wager_pre = total_amount_wagered_pre/ betting_days_frequency_pre) %>%
  mutate(betting_intensity_pre = total_number_of_bets_pre/ betting_days_frequency_pre) %>%
  mutate(net_loss_pre = total_amount_wagered_pre - total_amount_won_pre) %>%
  print()

wagers3.tib.workable %>%
  mutate(pre.limit.date = window_limit_date - days(90)) %>% # Check accuracy of output against random customer ids 
  filter(wager_date < as.Date(window_limit_date) & 
           wager_date >= as.Date(pre.limit.date) & 
           customerid == "****")  %>% 
  summarise(n_distinct(wager_date)) 

# And new column for the total number of wagers in the 30 days prior to message 1 (eligibility criteria)----
wager.days.last.30.setters.3<- wagers3.tib.workable %>% 
  filter(wager_date > as.Date("2019-09-13") &
           wager_date < as.Date("2019-10-14") &
           window_limit_setter == "LimitSetter") %>%
  group_by(customerid) %>%
  summarise(
    last30_betting_days_frequency = n_distinct(wager_date)) %>% 
  print(n = 100)

# Add new column for final wager in the day before setting their limit:
last.wager.column<- wagers3.tib.workable %>% 
  filter(wager_date < as.Date(window_limit_date) &
           window_limit_setter == "LimitSetter") %>%
  arrange(desc (wager_date, wager.time, customerid)) %>% 
  distinct(customerid, .keep_all = TRUE) %>%
  mutate(total_payout = payout + bonusbet_payout) %>%
  mutate(lost_last_bet = total_payout <= 0) %>% 
  select(customerid, 
         lost_last_bet) %>%
  print() # Checked for accuracy against last bets (i.e., without columns selected)

# Add column for SD of daily wager:
daily.wager.SD.pre.setters.3<- wagers3.tib.workable %>% 
  filter(wager_date < as.Date("2019-10-14") & window_limit_setter == "LimitSetter") %>%
  group_by(customerid,
           wager_date) %>%
  summarise(
    dailywager = sum(stake + bonusbet_stake)
  ) %>%
  group_by(customerid) %>% 
  summarise(
    daily_wager_SD_pre = sd(dailywager)
  ) %>%
  print()

# Calculate wager summaries for Limit Setters Post* messages:
wager.summaries.post.setters.3<- wagers3.tib.workable %>% 
  filter(window_limit_setter == "LimitSetter") %>% 
  mutate(post.limit.date = window_limit_date + days(90)) %>% 
  filter(wager_date > as.Date(window_limit_date) & 
           wager_date <= as.Date(post.limit.date)) %>% 
  group_by(customerid) %>%
  summarise(
    total_number_of_bets_post = n(),
    betting_days_frequency_post = n_distinct(wager_date),
    total_amount_wagered_post = sum(stake),
    bonusbet_amount_wagered_post = sum(bonusbet_stake), # Don't include the stake for bonus bets in total stake or net loss calculations as this isn't the customers' money. Only inlcude payout, which is equal to payout - bonus bet stake.
    regular_payout_post = sum(payout),
    bonusbet_payout_post = sum(bonusbet_payout)
  ) %>%
  mutate(total_amount_won_post = (bonusbet_payout_post - bonusbet_amount_wagered_post) + regular_payout_post) %>%  # For bonus bets, customers only keep the amount won and not what was wagered.
  mutate(average_daily_wager_post = total_amount_wagered_post/ betting_days_frequency_post) %>%
  mutate(betting_intensity_post = total_number_of_bets_post/ betting_days_frequency_post) %>%
  mutate(net_loss_post = total_amount_wagered_post - total_amount_won_post) %>%
  print()

wagers3.tib.workable %>%
  mutate(post.limit.date = window_limit_date + days(90)) %>% # Check accuracy of output against random customer ids 
  filter(wager_date > as.Date(window_limit_date) &
           wager_date <= as.Date(post.limit.date) & 
           customerid == "****")  # %>% summarise(n_distinct(wager_date)) 

# Add column for SD of daily wager:
daily.wager.SD.post.setters.3<- wagers3.tib.workable %>% 
  filter(window_limit_setter == "LimitSetter") %>%
  mutate(post.limit.date = window_limit_date + days(90)) %>% 
  filter(wager_date > as.Date(window_limit_date) & 
           wager_date <= as.Date(post.limit.date)) %>% 
  group_by(customerid, 
           wager_date) %>%
  summarise(
    dailywager = sum(stake + bonusbet_stake)
  ) %>%
  group_by(customerid) %>% 
  summarise(
    daily_wager_SD_post = sd(dailywager)
  ) %>%
  print()

client.condition.limits.transactions.wagerssetterssonly.3<- client.condition.limits.transactionssettersonly.3 %>% 
  full_join(wager.summaries.pre.setters.3, by = "customerid") %>%
  full_join(wager.days.last.30.setters.3, by = "customerid") %>%
  full_join(last.wager.column, by = "customerid") %>%
  full_join(daily.wager.SD.pre.setters.3, by = "customerid") %>%
  full_join(wager.summaries.post.setters.3, by = "customerid") %>% 
  full_join(daily.wager.SD.post.setters.3, by = "customerid") %>%
  print() # Merge with existing dataset

client.condition.limits.transactions.wagerssetterssonly.3 %>% 
  filter(is.na(total_number_of_bets_post)) %>%
  select(customerid,
           date_account_closed,
           timeout_start_date_post,
           self_exclusion_start_date) %>%
  print() # Only two clients in this group didn't wager after messages. Neither self excluded but one (1465007) closed their account


# Replace NAs with 0 0 to reflect that no transactions were made, wherever relevant.


client.condition.limits.transactions.wagerssetterssonly.3[, c("total_number_of_bets_pre",
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
                                                                   client.condition.limits.transactions.wagerssetterssonly.3[, c("total_number_of_bets_pre",
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



client.condition.limits.transactions.wagerssetterssonly.3 %>% 
  select(55:68) # Check NAs were removed

# 
# COMBINE TRANSACTIONS & WAGER DATA SETS FOR BOTH GROUPS ------------------------------------------------------------------------------
# 

client.condition.limits.transactions.wagers.3<- bind_rows(client.condition.limits.transactions.wagerssetterssonly.3, 
                                                          client.condition.limits.transactions.wagers.nonsettersonly.3) %>% 
  print()

# Explore net loss summaries
client.condition.limits.transactions.wagers.3 %>% 
  group_by(window_limit_setter) %>%
  summarise(
    mean(net_loss_pre),
    median(net_loss_pre),
    max(net_loss_pre),
    mean(net_loss_post),
    median(net_loss_post),
    max(net_loss_post)
  ) # summary values appear to have decreased for Limit setters and to a lesser extent non-setters

client.condition.limits.transactions.wagers.3 %>% 
  group_by(window_limit_setter) %>%
  count(lost_last_bet) # Limit setters & non-setters appear to have similar proportions in relation to whether their last bet pre-limit/message was a win/loss

client.condition.limits.transactions.wagers.3 %>% 
  filter(net_loss_pre >= 0) %>%
  group_by(window_limit_setter) %>%
  summarise(
    n() # Most clients in both groups Lost money Prior to messages
  )

client.condition.limits.transactions.wagers.3 %>% 
  filter(net_loss_pre < 0) %>%
  group_by(window_limit_setter) %>%
  summarise(
    n()
  ) # Only a small proportion (6/38 Of limit setters) had a negative net loss (indicating they won money overall)


# ------------------------------------------------------------------------------
# Finale dataset for analyses
client.condition.limits.transactions.wagers.final.3<- client.condition.limits.transactions.wagers.3 %>%  
  mutate(operator = recode(condition,
                              '1' = '3',
                         '2' = '3',
                         '4' = '3',
                         '6' = '3')) %>% # Add operator column and label to identify client operator
  print()

client.condition.limits.transactions.wagers.final.3 %>% 
  select(operator) %>% 
  print(n = 50) # Check new Operator column has added correctly

names(client.condition.limits.transactions.wagers.final.3) # Check all the new names

# Create a CSV file from the final dataset so that It can be easily accessed And merged with Data sets from other operators:
write.csv(client.condition.limits.transactions.wagers.final.3, file = "Analysis data/Operator 3/Final_dataset_Operator3.csv")

# End