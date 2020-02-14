#This script was used to quantify the qualitative data coding process

# *Qual coding scheme used by RA*:*=
# 1 = not interested in responsible gambling/ deposit limits at all
# 2 = dislike
# 3 = suitable message but change required… 
# 4 = like 
# 5 - like very much



#*******POLL 1********
read.csv("RG Messaging Poll 1 qual outcomes.csv")

data1 <- read.csv("RG Messaging Poll 1 qual outcomes.csv")

names(data1)

View(data1)
nrow(data1) #total sample size = 257

######calculate the total number of each type of feedback (i.e., the qual code given) for each message (POLL 1)
 
# 1. Deposit limits are a great way to manage your spending. Have you set yours?
table(data1$Code.1)
# Outcomes
# 1  2  3  4  5 
# 44 36  1 18 15 

#   2. Deposit limits are great for managing your cash flow. Have you set yours?
table(data1$Code.2)
# Outcomes
# 1  2  3  4  5 
# 35 40  3 15 11

#   3. Deposit limits are great for your betting plan. Have you set yours?
table(data1$Code.3)
# Outcomes
# 1  2  3  4  5 
# 33 39  8 14  9

#   4. Deposit limits are great way to help you stick to budget. Have you set yours?
table(data1$Code.4)
# Outcomes
# 1  2  3  4  5 
# 27 41  2 18 14

#   5. Deposit limits are great way to be bet savvy. Have you set yours?
table(data1$Code.5)
# Outcomes
# 1  2  3  4  5 
# 27 43  6 15 12

#######calculate the total number of participants who put a message (in response to any message) saying that they thought deposit limits are irrelevant to them and/or others

All.nay.sayers1<- subset(x = data1,
                         subset =
                           Code.1 <2 |
                           Code.2 <2 |
                           Code.3 <2 |
                           Code.4 <2 |
                           Code.5 <2)

View(data1)
table(All.nay.sayers1$ResponseId)

nrow(All.nay.sayers1) #56 in total were nay sayers at some point
56/257 * 100 # percentage of sample = 21.79

#*******POLL 2**************************************************
read.csv("RG Messaging Poll 2 qual outcomes.csv")

data2 <- read.csv("RG Messaging Poll 2 qual outcomes.csv")

names(data2)

View(data2)
nrow(data2) #total sample size = 219

#######calculate the total number of each type of feedback (i.e., the qual code given) for each message (POLL 2)
# 1.	Most people who use deposit limits find they help them manage their spending. 
table(data2$code)
# Outcomes
# 1  2  3  4  5 
# 15 10 11 11 26 

# # 2.	Experienced punters find deposit limits help them stick to budget. 
table(data2$Code.1)
# # Outcomes
# 1  2  3  4  5 
# 20 10  7 12 26 
 
# # 3.	Savvy punters set deposit limits. 
table(data2$Code.2)
# # Outcomes
# 1  2  3  4  5 
# 23  9  9  8 23

# # 4.	People who set deposit limits find it helps keep gambling affordable. 
table(data2$Code.3)
# # Outcomes
# 1  2  3  4  5 
# 10 10  5 17 19 
 
# # 5.	Deposit limits can be useful for everyone.
table(data2$Code.4)
# # Outcomes
# 1  2  3  4  5 
# 15 12  8 14 22 
 
# ########calculate the total number of each type of feedback for each tagline (TAGLINE POLL outcomes from poll 2 sample)
# # 1.	"Set and forget"
table(data2$Code.5)
# # Outcomes
# 1  2  3  4  5 
# 14 13  6  7 17 

# 2.	'Deposit limits keep you on track, so you can focus on the track'
table(data2$Code.6)
# # Outcomes
# 1  2  3  4  5 
# 15  9  6  7 20 
 
# # 3.	'Keep on track with deposit limits'
table(data2$Code.7)
# # Outcomes
# 1  2  3  4  5 
# 11  5  5 12 16 
 
# # 4.	"Have you set yours?"
table(data2$Code.8)
# # Outcomes
# 1  2  3  4  5 
# 11  6  6  8  9 

# # 5.	Why don't you set one now and see for yourself?"
table(data2$Code.9)
# # Outcomes
 # 1  2  3  4  5 
# 14 10  7  3  3 

# # 6.	"Deposit limits: keep gambling fun"
table(data2$Code.10)
# # Outcomes
# 1  2  3  4  5 
# 12  8  3 11 12 
 
# # 7.	'Budgets are useful, deposit limits make them easy'
table(data2$Code.11)
# # Outcomes
# 1  2  3  4  5 
# 12  8  2  8  8
 
# # 8.	'Budgets=boring, deposit limits = easy. Set yours today'
table(data2$Code.12)
# # Outcomes
# 1  2  3  4  5 
# 10 14  6  4 10


#######calculate the total number of participants who put a message (in response to any message or tagline) saying that they thought deposit limits are irrelevant to them and/or others

All.nay.sayers2<- subset(x = data2,
       subset = code <2|
         Code.1 <2|
         Code.2 <2 |
         Code.3 <2 |
         Code.4 <2 |
         Code.5 <2 |
         Code.6 <2 |
         Code.7 <2 |
         Code.8 <2 |
         Code.9 <2 |
         Code.10 <2 |
         Code.11 <2 |
         Code.12 <2)

table(All.nay.sayers2$ResponseId)

nrow(All.nay.sayers2) #50 in total were nay sayers at some point
50/219 * 100 # percentage of sample = 22.83

#*******POLL 3*************************************************************

data3 <- read.csv("RG Messaging Poll 3 qual outcomes.csv")

names(data3)

View(data3)

nrow(data3) #total sample size = 211

#######calculate the total number of each type of feedback for each message (POLL 3)

# 1.	Gambling should be about fun, not money management: set a deposit limit
table(data3$Code.1) 
# # Outcomes
# 1  2  3  4  5 
# 13  8  3  4 24 

# # 2.	Getting caught up in the moment is fun, set a deposit limit so you can enjoy it
table(data3$Code.2) 
# # Outcomes
# 1  2  3  4  5 
# 13  8  3  4 24 

# # 3.	Deposit limits keep your gambling safe so the only numbers you have to worry about are the final scores
table(data3$Code.3) 
# # Outcomes
# 1  2  3  4  5 
# 14 10  3  5 23 

# # 4.	You shouldn't have to be an accountant to keep gambling fun: set a deposit limit
table(data3$Code.4) 
# # Outcomes
# 1  2  3  4  5 
# 4 27  1  3  8 

# # 5.	Boring things are important too, set a deposit limit so you can keep gambling fun
table(data3$Code.5) 
# # Outcomes
# 1  2  3  4  5 
# 6 14  7  5  7 

# # 6.	The only nags you should have to worry about are on the track. Set a deposit limit to keep gambling fun
table(data3$Code.6) 
# # Outcomes
# 1  2  3  4  5 
# 5 12  4  5 21 

# # 7.	Does future you forget about past you? Set a deposit limit
table(data3$Code.7) 
# # Outcomes
# 1  2  3  4  5 
# 6 29  3  4  6 

# # 8.	Set a deposit limit so future you can focus on having fun
table(data3$Code.8) 
# # Outcomes
# 1  2  3  4  5 
# 10  9  6  5 11 


########calculate the total number of each type of feedback for each tagline (TAGLINE POLL outcomes from poll 3 sample)

# 1.	"Set and forget"
table(data3$Code.9)
# Outcomes
# 1  2  3  4  5 
# 6 15  3  8 13 

# 2.	'Deposit limits keep you on track, so you can focus on the track'
table(data3$Code.10)
# Outcomes
# 1  2  3  4  5 
# 5 16  5  4 10 

# 3.	'Keep on track with deposit limits'
table(data3$Code.11)
# Outcomes
# 1  2  3  4  5 
# 3  9  2  8 11 

# 4.	"Have you set yours?"
table(data3$Code.12)
# Outcomes
# 1  2  3  4  5 
# 4 14  5  7 10 

# 5.	Why don't you set one now and see for yourself?"
table(data3$Code.13)
# Outcomes
# 1  2  3  4  5 
# 6 15  3  5  4 

# 6.	"Deposit limits: keep gambling fun"
table(data3$Code.14)
# Outcomes
# 1  2  3  4  5 
# 3 18  1  5 10

# 7.	'Budgets are useful, deposit limits make them easy'
table(data3$Code.15)
# Outcomes
# 1  2  4  5 
# 5 18  4  4 

# 8.	'Budgets=boring, deposit limits = easy. Set yours today'
table(data3$Code.16)
# Outcomes
# 1  2  3  4  5 
# 5 23  1  5  2 

#######calculate the total number of participants who put a message (in response to any message or tagline) saying that they thought deposit limits are irrelevant to them and/or others

All.nay.sayers3<- subset(x = data3,
                         subset = Code.1 <2 |
                           Code.2 <2 |
                           Code.3 <2 |
                           Code.4 <2 |
                           Code.5 <2 |
                           Code.6 <2 |
                           Code.7 <2 |
                           Code.8 <2 |
                           Code.9 <2 |
                           Code.10 <2 |
                           Code.11 <2 |
                           Code.12 <2 |
                           Code.13 <2 |
                           Code.14 <2 |
                           Code.15 <2 |
                           Code.16 <2)


table(All.nay.sayers3$ResponseId)

nrow(All.nay.sayers3) #22 in total were nay sayers at some point
22/211 * 100 # percentage of sample = 10.43
