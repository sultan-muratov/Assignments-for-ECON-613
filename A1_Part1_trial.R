
library(tidyverse)
library(data.table)
library(stringr)
library(dplyr)

rm(list=ls()) #remove all variable
setwd("D:/New Order/Fourth Semester/ECON 613/R-workdir")
datjss <- read.csv('datjss.csv')
datsss <- read.csv('datsss.csv')
datstu <- read.csv('datstu.csv')

# Exercise 1

# number of students is 340823 (number of rows in datstu)

# number of schools

schools <- datstu %>%
  select(schoolcode1:schoolcode6) %>%
  gather(key = "schoolchoice", value = "schoolcode",schoolcode1:schoolcode6)

schools_unq <-subset(schools, !duplicated(schools$schoolcode), select = schoolchoice:schoolcode)

# so there are 641 unique schools

# number of programs

programs <- datstu %>%
  select(choicepgm1:choicepgm6) %>%
  gather(key = "programs", value = "names",choicepgm1:choicepgm6)

programs_unq <-programs_unq[!duplicated(programs$names),]

# so there are 32 unique programs

# number of choices (school, program) is 32*641 = 20512

# Missing test score

havescore <- datstu %>%
  select(score) %>%
  gather(key = "have score", value = "score", na.rm = TRUE , score)

# we have total number of students, and we created the dataset containing students with test scores. 
# The difference between thesse two numbers is missing score
# 340823 - 160936 = 179887  - so this is the number of missing test scores

# Apply to the same school, but different programs

datstu_sc <- drop_na(datstu, schoolcode1:schoolcode6)
nrows = 323089
count_ss = 0

for (i0 in 1:nrows){
  if(datstu_sc$schoolcode1[i0] == datstu_sc$schoolcode2[i0] ||
     datstu_sc$schoolcode1[i0] == datstu_sc$schoolcode3[i0] ||
     datstu_sc$schoolcode1[i0] == datstu_sc$schoolcode4[i0] ||
     datstu_sc$schoolcode1[i0] == datstu_sc$schoolcode5[i0] ||
     datstu_sc$schoolcode1[i0] == datstu_sc$schoolcode6[i0] ||
     datstu_sc$schoolcode2[i0] == datstu_sc$schoolcode3[i0] ||
     datstu_sc$schoolcode2[i0] == datstu_sc$schoolcode4[i0] ||
     datstu_sc$schoolcode2[i0] == datstu_sc$schoolcode5[i0] ||
     datstu_sc$schoolcode2[i0] == datstu_sc$schoolcode6[i0] ||
     datstu_sc$schoolcode3[i0] == datstu_sc$schoolcode4[i0] ||
     datstu_sc$schoolcode3[i0] == datstu_sc$schoolcode5[i0] ||
     datstu_sc$schoolcode3[i0] == datstu_sc$schoolcode6[i0] ||
     datstu_sc$schoolcode4[i0] == datstu_sc$schoolcode5[i0] ||
     datstu_sc$schoolcode4[i0] == datstu_sc$schoolcode6[i0] ||
     datstu_sc$schoolcode5[i0] == datstu_sc$schoolcode6[i0]){
    count_ss = count_ss + 1
  }
}

# count_ss is 116388 so there are that many students applying to same schools

# Apply to less than 6 choices

lessthan6<- datstu %>%
  select(schoolcode6) %>%
  gather(key = "schoolchoice", value =  "school6", na.rm = TRUE, schoolcode6)

# Answer: If student did not apply to school 6 times, then he applied to less than 6 schools - so we can drop na in only schoolcode6 to 
# find out number of students who applied to less than 6 schools

# so there are 340823 - 323735 = 17088 students applying to less than 6 schools


## Exercise 2

#mutate to create a dataset with (school,program) pairs

datstu = datstu %>% 
  mutate(choice1=paste0(schoolcode1,choicepgm1),
         choice2=paste0(schoolcode2,choicepgm2),
         choice3=paste0(schoolcode3,choicepgm3),
         choice4=paste0(schoolcode4,choicepgm4),
         choice5=paste0(schoolcode5,choicepgm5),
         choice6=paste0(schoolcode6,choicepgm6))

# dropping N/As
datstu_new <- drop_na(datstu)

#attaching schoolcode, program name, scores and jssdistrict to dataset

# school_program is the dataset that will contain (school,program) pair in each row and other necessary variables
school_program <- datstu_new %>%
  select(choice1:choice6) %>%
  gather(key = "choice", value = "school_program",choice1:choice6)

school_program = school_program %>% 
  mutate(schoolcode= gsub('\\D','', school_program),pgm=gsub('\\d','', school_program))

scores = rep(datstu_new$score, 6)

school_program$score <- scores

jssdistrict = rep(datstu_new$jssdistrict, 6)

school_program$jssdistrict <- jssdistrict

# we find size of the admitted students for each schoolcode

datstu_1 = school_program %>%
  group_by(schoolcode) %>%
  summarize(size=n())

# we find more detailed statistics about students for each school code 

datstu_2 = school_program %>%
  group_by(schoolcode,score) %>%
  summarise(size=n(),groups = "keep")

# removing repeated schoolcodes from the dataset

datsss_unq <-subset(datsss, !duplicated(datsss$schoolcode), select = X:ssslat)

school_program$schoolcode <-as.numeric(school_program$schoolcode)

# attaching ssslat and ssslong to our (school,program) dataset
school_program <-left_join(school_program, datsss_unq, by = "schoolcode")

# now our (school,program) dataset has ssslong and ssslat corrdinates

# next we add size and cutoff

# code to find out cutoff
datstu_3 <- subset(datstu_2, !duplicated(datstu_2$schoolcode), select = schoolcode:score)

datstu_3$schoolcode <-as.numeric(datstu_3$schoolcode)

# rename score to cutoff to avoid confusion

colnames(datstu_3) <- c("schoolcode","cutoff")

# Now we can add cutoff and size to the (school,program) dataset

school_program <-left_join(school_program, datstu_3, by = "schoolcode")

datstu_1$schoolcode <-as.numeric(datstu_1$schoolcode)
school_program <-left_join(school_program, datstu_1, by = "schoolcode")

# As a result, we have created a dataset where each row corresponds to (school,program) pair.
# Additionally we have cutoff, size, latitude, longitude and jssdistrict for each (school,program) pair.
# I could not attach the jsslat and jsslong because strings for jssdistrict in datstu and datjss are different.
# I also could not estimate quality for my dataset



## Exercise 3 #########################################



# because I couldnot add the jsslat and jsslong to my dataset, I will just present the function to compute the distance, as if I had needed data
# the code is commented out

# numrows = 910182 # number of rows in my (school,program) dataset

# zeros = rep(0, numrows)
# dist = matrix(c(zeros), ncol = 1)
# for (i1 in 1:numrows){
#   dist[1,i1] = sqrt((69712 * (ssslong[i1] - point_x[i1]) * cos(point_y[i1])/57.3)^2 + (69172 *(ssslat[i1] - point_y[i1]))^2)
# }

# school_program$dist <-dist

# this code will compute distance and attach it as new column to (school,program) dataset