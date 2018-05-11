#This was my testing and poking around the data code. Normally I wouldn't include this but it's part of my process.
#

#I want to find the percentage of students whose reading scores are 1. Approaching grade level and 2. Meets grade level
#And I want to be able to break it up by school type, gender, and race.


#Loading the data
STAAR <- read.csv("./DATA/STAAR16-17.csv", stringsAsFactors = FALSE)

View(STAAR)

#I'm assuming "CAMPUS" is unique IDs for Texas schools

length(unique(STAAR$CAMPUS))

#

#There are ., -1, and -3 interspersed throughout the data.
#https://rptsvr1.tea.texas.gov/perfreport/tapr/2017/masking.html
#Masking symbols, -2 may also be lurking somewhere unseen

STAAR <- read.csv("./DATA/STAAR16-17.csv", stringsAsFactors = FALSE, na.strings = c(".","-1","-2","-3"))

#The only variable I'm looking for that is easily interpretable is "GRDTYPE" which appears to be school type
unique(STAAR$GRDTYPE)
#This has E, M, and S which were asked for. It also has 2 Bs which stands for "Both" according to TEA
#https://rptsvr1.tea.texas.gov/perfreport/account/2016/download/camprate.html
#Since only E, M, and S were requested I'll omit the B's for now

STAAR <- subset(STAAR, GRDTYPE != "B")

#Onto deciphering the column headers
#Looking at the column header keys the headers contain the following info:
# 1. Demographics - first 4 characters
# 2. Subject - next 5 characters
# 3. Year - next 2 characters
# 4. numerator/denominator/rate - last character

#Example
#CA00 - All students
#AR042 - Reading
#17 - 2017
#D - Denominator

#There is a change from 2016 to 2017 in how standards are labeled
#From Level II and Level III to Approaches Grade Level and Meets Grade Level respectively
#https://tcta.org/node/14492-commissioner_releases_final_rules_on_staar_performance_standards
#For now I'll only look at the 2017 data since thats the labeling specifically asked for
#There appear to be three types of statistics: Numerator, Denominator, and Rate
#Numerator is the number of students that meet grade level standards
#Denominator is the number of students taking the test
#Rate should be numerator/denominator
#I think rate is not useful for what I want to do


#The meets grade level key makes more sense at first glance so I'll start by checking assumptions there


#I want to check if numerator/denominator actually gives rate
#I want to check that if I add up gender or race I'll get the same total as the var that measures all students
#I want to check if denominators are the same between meets and approaches

#It seems like dplyr is probably the cleanest way to do this
#https://stackoverflow.com/questions/10085806/extracting-specific-columns-from-a-data-frame
library(dplyr)

#In order: All num, All denom, All Rate,Male num, Female num, Male denom, Female denom
testing.df.m <- select(STAAR,"CA00AR04217N","CA00AR04217D","CA00AR04217R","CM00AR04217N","CF00AR04217N","CM00AR04217D", "CF00AR04217D")

names(testing.df.m) <- c("all.n", "all.d", "all.r", "male.n", "female.n", "male.d", "female.d")

head(testing.df.m)


#Rate looks like 100*(round(numerator/denominator,2))

testing.df.m$rate.test <- testing.df.m$all.r == (100 * round(testing.df.m$all.n/testing.df.m$all.d,2))

summary(testing.df.m$rate.test)

#23 FALSE to 207 TRUE seems okay.

testing.df.m$total.test.n <- testing.df.m$all.n == testing.df.m$male.n + testing.df.m$female.n
testing.df.m$total.test.d <- testing.df.m$all.d == testing.df.m$male.d + testing.df.m$female.d

summary(c(testing.df.m$total.test.n,testing.df.m$total.test.d))

#0 FALSE also seems okay

#Checking that approach is the same

testing.df.a <- select(STAAR,"CA00AR01S17N","CA00AR01017D","CA00AR01S17R","CM00AR01S17N","CF00AR01S17N","CM00AR01017D", "CF00AR01017D")

names(testing.df.a) <- c("all.n", "all.d", "all.r", "male.n", "female.n", "male.d", "female.d")

testing.df.a$rate.test <- testing.df.a$all.r == (100 * round(testing.df.a$all.n/testing.df.a$all.d,2))

summary(testing.df.a$rate.test)

#23 FALSE to 207 TRUE seems okay still

testing.df.a$total.test.n <- testing.df.a$all.n == testing.df.a$male.n + testing.df.a$female.n
testing.df.a$total.test.d <- testing.df.a$all.d == testing.df.a$male.d + testing.df.a$female.d

summary(c(testing.df.a$total.test.n,testing.df.a$total.test.d))
#0 FALSE again

#Seeing some errors in the rate calculation makes sense to me because rounding is involved

#A quick look at if denominators are the same.
#It seems like they should be, if it is actually students that took the test

testing.denom <- data.frame("approach.all" = testing.df.a$all.d,
                            "meeting.all" = testing.df.m$all.d)
head(testing.denom)

#They clearly are not the same

testing.denom$difference <- testing.denom[,1]- testing.denom[,2]

summary(testing.denom$difference)
#1154 is the largest difference. That's pretty big
#The minimum is zero though which indicates that approach is always the larger of the two denominators
#Denominator being the number of students taking the test is now a kind of questionable assumption
#For now I'm still going to use it because I can't see a good alternative