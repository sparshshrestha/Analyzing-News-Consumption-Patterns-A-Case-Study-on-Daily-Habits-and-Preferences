"
****************************************************************
Name: Sparsh Shrestha
Student Number: A00241221

QMM1001 Case Study 2
****************************************************************
"

#Reading Personalized data into R
s <- read.csv("Shrestha, Sparsh Personalized Data.csv", header = TRUE)

###########################################################################
#######Part 1: Does watching the news affect how you spend your day?#######
###########################################################################

#######1. Contingency table for news and fruit variables#######

#Creating a contingency table for 'News' and 'Fruit' variables
(news.fruit <- table(s$News, s$Fruit))
prop.table(news.fruit)
#I don't think any of my categorical variables have any relationship with the news variable.
#But compared to the dinner_taste.1.3. variable, fruit variable is more likely to have a relationship with news variable.

#######2. Probability rule questions#######

#Question 1: What is the probability that I do not watch the news?
p.news <- prop.table(table(s$News))[2] #Probability that I watch the news = 0.1444
#Using the rule of compliment
p.news.c <- 1 - p.news
p.news.c #Probability that I do not watch news = 0.8556

#Question 2: What is the probability that I watch the news or eat an apple?
#Using the rule of addition for non disjoint events
prop.table(table(s$News))[2] + prop.table(table(s$Fruit))[1] - prop.table(table(s$News, s$Fruit))[2,1]
#Probability that I watch the news or eat an apple = 0.5333

#Question 3: What is the probability of me eating an 'Apple' given I watched the news that day?
#Using the rule of conditional probability
prop.table(news.fruit, 1)[2,1]
#Probability of me eating an 'Apple' given I watched the news that day = 0.3846

#Question 4: What is the probability of me eating an apple and watching the news?
#Using the rule of multiplication
prop.table(table(s$News, s$Fruit))[2, 1]
#Probability of me eating an apple and watching the news = 0.05555

#######3. Disjoint events#######

#Are any of the events in the contingency table disjoint?
#P(news and lemon)
prop.table(table(s$News, s$Fruit))[2, 4]
#Probability of me eating a lemon and watching the news on the same day = 0%
#Me eating a lemon and watching the news are 2 disjoint events.

#######4. Independent events#######
#Are me watching news and eating an apple independent events?
#P(news and apple) = 0.05555556
prop.table(table(s$News, s$Fruit))[2, 1]
#P(news) * P(apple) = 0.06419753
prop.table(table(s$News))[2] * prop.table(table(s$Fruit))[1]
#P(news and apple) != P(news) * P(apple)
#Since, P(news and apple) is not equal to P(news) * P(apple) the events are not independent events(i.e. dependent events)

###################################################################
#######Part 2: Do you watch the news more than other people?#######
###################################################################
#######1. Population Proportion#######
n <- nrow(s) #Number of days I collected data = 90
(p.hat <- prop.table(table(s$News))[2]) #Proportion of days I watched news = 0.1444444 < 0.59
(q.hat <- 1 - p.hat)#Proportion of days I did not watched news = 0.8555556
#I watch the news less than the general population because I find reading news on twitter more efficient then watching the news.

#######2. Standard Deviation and Mean#######
p <- 0.59 #Mean = 0.59
q <- 1 - p
(sd <- sqrt(p * q / n)) #Standard Deviation = 0.05184378

#######3. Sample Distribution######
pnorm(p.hat, p, sd, TRUE) #4.192541e-18
#The probability of getting a value less than or equal to my sample proportion is 4.192541e-18

#######4. Confidence Interval#######
se <- sqrt(p.hat * q.hat / n) #Standard error
z.critical <- qnorm(0.95 / 2 + 0.5, 0, 1, lower.tail = TRUE)
lower.bound <- p.hat - z.critical * se
upper.bound <- p.hat + z.critical * se
lower.bound #0.0718169
upper.bound #0.217072
# The proportion of the days I watch the news is 7.18% to 21.71%,  when there is a 95% confidence interval.
# I do not watch the news 50% of the time because the upper bound is only 21.71%.

#######5. Hypothesis Testing#######
#Since I proportion of days I watched news is 0.1444444 < 0.59
#Null hypothesis (H0): p = 0.59
#Alternative hypothesis (H1): p < 0.59

z <- (p.hat - p) / sd
p.value <- pnorm(z, 0, 1, lower.tail = TRUE)
p.value #4.192541e-18 < 0.01 (level of significance)
#Reject the null hypothesis because the p-value is much less than the level of significance(0.01).
#From the alternative hypothesis I can conclude that I watch news less than the general Canadian population.


