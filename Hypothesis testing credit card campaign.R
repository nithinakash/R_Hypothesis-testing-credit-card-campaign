setwd('C:/Users/Nithin/Downloads/Hypothesis testing credit card campaign')

#**************************************************************************************************************#

cust <- read.csv('cust_seg.csv')

#**************************************************************************************************************#

#1.Card usage has been improved significantly from last year usage which is 50. (Hint: Comparing card usage of
#  post campaign of 1 month with last year hypothesized value 50)

# H0 - u <= 50
# Ha - u > 50

# CI - 95%
# P - 0.05

t.test(cust$Post_usage_1month,mu = 50)

#H0 is rejected.

#**************************************************************************************************************#

#2. The last campaign was successful in terms usage of credit card. (Hint: Comparing means for card usage of pre & 
#post usage of campaign)

# H0 - u = last year
# Ha - u <> last year

# CI - 95%
# P - 0.05

t.test(cust$pre_usage,cust$Post_usage_1month)
t.test(cust$pre_usage,cust$post_usage_2ndmonth)
t.test(cust$pre_usage,cust$Latest_mon_usage)

#H0 is rejected.

#**************************************************************************************************************#

#3. Is there any difference between males & females in terms of credit card usage? (Hint: Comparing means of 
#   card usage for males & females)

# H0 - Both same
# Ha - Not same

# CI - 95%
# p - 0.005

t.test(pre_usage~sex,cust, var.equal = FALSE)
t.test(Post_usage_1month~sex,cust, var.equal = FALSE)
t.test(post_usage_2ndmonth~sex,cust, var.equal = FALSE)
t.test(Latest_mon_usage~sex,cust, var.equal = FALSE)

#Ha is rejected

#**************************************************************************************************************#

#4. Is there any difference between segments of customers in terms of credit card usage? (Hint: Comparing means 
#   of card usage of different segment customers)

# H0 - Same Population
# Ha - Different Population Spend Different

# CI - 95%
# P - 0.05

aov_obj <- aov(pre_usage~segment,cust)
aov_obj <- aov(Post_usage_1month~segment,cust)
aov_obj <- aov(post_usage_2ndmonth~segment,cust)
aov_obj <- aov(Latest_mon_usage~segment,cust)
summary(aov_obj)

#H0 is rejected

#**************************************************************************************************************#

#5. Is there any relation between region & Segment? (Hint: Finding the relationship between categorical variables
#   region and Segment)

# H0 - No Relation
# Ha - Relation

# CI - 95%
# P - 0.05

chi_obj <- xtabs(~region+segment,cust)
chisq.test(chi_obj)

#H0 is rejected

#**************************************************************************************************************#

#6. Is the relationship between card usage in the latest month and pre usage of campaign? (Hint: find the 
#   correlation between latest_mon_usage and pre_usage)

# H0 - No Relation
# Ha - Relation

# CI - 95%
# P - 0.05

cor(cust$pre_usage,cust$Latest_mon_usage)

#**************************************************************************************************************#

