
#write your NAME And TP number.
yName = readline(prompt = "Enter the your name: ")
yTPNumber = as.integer(readline(prompt = "Enter the your TP number: TP"))



#package import 

  #manip tools
  install.packages("dplyr")

  #tidy messy
  install.packages("tidyr")
  
  ## Use melt() function for bar chart
  install.packages("reshape2")

  #graph
  install.packages("ggplot2")
  
  #pie3D
  install.packages("plotrix")

  #clean data
  install.packages("janitor")
  
  
  library(dplyr)
  library(tidyr)
  library(reshape2)
  library(ggplot2)
  library(plotrix)
  library(janitor)
  

#read_file
student_data = read.csv("C:\\Users\\woonx\\Desktop\\R studio\\student.csv",header=TRUE) 
student_data
View(student_data)


#cleaning data
student_data<-clean_names(student_data)
colnames(student_data)




# Question 1: which school's student total score better? which school most suitable student to choose? why?
Q1 = filter(student_data, g1&g2&g3 > 10)


#Analysis 1-1
School = Q1 %>% group_by(school) %>% summarise(n()) %>% select(school)
TotalOfPass = Q1 %>% group_by(school) %>% summarise(TotalOfPass=n()) %>% select(-school)
TotalOfStudent = group_by(student_data,school) %>% summarise(TotalOfStudent=n()) %>% select(-school)


Q1A1DF <- data.frame(TotalOfPass, TotalOfStudent,School)
Q1A1DF

Q1A1DFMelt <- melt(Q1A1DF, id='school') #reshape it, id = group by which column
head(Q1A1DFMelt)

ggplot(Q1A1DFMelt, aes(x=school, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value),position = position_dodge(1),vjust = -0.5) 
#position_doge(low=left, high=right), vjust= (low=move up, high=move down)




#Percentages
PGP = Q1A1DF[1,1]/ Q1A1DF[1,2] 
PercentGP = PGP * 100

PMS = Q1A1DF[2,1]/ Q1A1DF[2,2] 
PercentMS = PMS * 100

PercentagesOfQ1A1 = data.frame(GP= round(PercentGP, digits=2),MS= round(PercentMS,digits = 2))
PercentagesOfQ1A1



#Analysis 1-2
Q1 %>% group_by(school,absences) %>% summarise(Total=n()) %>% summarise(sum(Total)) #total absence each school
Q1A2 = Q1 %>% group_by(school,absences) %>% summarise(Total=n(), .groups='drop') %>% group_by(school) #group by absences #allow more than 1 group
nrow(Q1 %>% group_by(absences) %>% summarise(Total=n())) #25 type 
arrange(Q1 %>% group_by(absences) %>% summarise(Total=n()), desc(absences)) #0-54 types of absences


#start draw
Q1A2GP = filter(Q1A2, school == "GP") %>% mutate(rangeOfAbsences = cut(absences,c(-1,10,20,30,40,50,Inf))) %>% 
  group_by(rangeOfAbsences) %>% summarise(TotalGP=sum(Total))
Q1A2MS = filter(Q1A2, school == "MS") %>% mutate(rangeOfAbsences = cut(absences,c(-1,10,20,30,40,50,Inf))) %>%
  group_by(rangeOfAbsences) %>% summarise(TotalMS=sum(Total))


#GP Absences Level
ggplot(Q1A2GP, aes(x=rangeOfAbsences, y=TotalGP)) +
  geom_point(aes(shape = factor(rangeOfAbsences), colour = factor(rangeOfAbsences)), size=3) + 
  geom_text(aes(label=TotalGP),position = position_dodge(0.9),vjust = -1) +
  ggtitle("School GP Total Range of Absences")


#MS Absences Level
ggplot(Q1A2MS, aes(x=rangeOfAbsences, y=TotalMS)) +
  geom_point(aes(shape = factor(rangeOfAbsences), colour = factor(rangeOfAbsences)),size=3) + 
  geom_text(aes(label=TotalMS),position = position_dodge(0.9),vjust = -1) +
  ggtitle("School MS Total Range of Absences")





#Analysis 1-3
Q1A3Total = Q1 %>% group_by(schoolsup,school) %>% summarise(TotalSupport = n(), .groups = 'drop')  %>% select(TotalSupport)
Q1A3School = Q1 %>% group_by(schoolsup,school) %>% summarise(TotalSupport = n(), .groups = 'drop') %>% select(school)
Q1A3Support = Q1 %>% group_by(schoolsup,school) %>% summarise(TotalSupport = n(), .groups = 'drop') %>% select(schoolsup)


#draw bar chart
Q1A3DF = data.frame(Q1A3School, Q1A3Total,Q1A3Support)

ggplot(Q1A3DF, aes(x=school, y=TotalSupport, fill=schoolsup)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=TotalSupport),position = position_dodge(1),vjust = -0.5) 


#find out which type of student will got school support 
check = filter(student_data, schoolsup == "yes")
nrow(check)

checkNo = filter(student_data, schoolsup == "no")
nrow(checkNo)


#reason 1
check %>% group_by(failures) %>% summarise(Total = n())

  #Why no failures still got school support
  check %>% group_by(failures) %>% filter(failures == 0) %>% group_by(higher) %>% summarise(n()) #this 0 is choose by they
  check %>% group_by(failures) %>% filter(failures == 0) %>% group_by(paid) %>% summarise(n())
  check %>% group_by(failures) %>% filter(failures == 0) %>% group_by(reason) %>% summarise(total=n()) %>% 
    mutate(Percent= (total/ sum(total) * 100 ))

  #all student without not failures student
  check %>% group_by(failures) %>% filter(failures != 0) %>% group_by(reason) %>% summarise(total=n()) %>% 
    mutate(Percent= (total/ sum(total) * 100 ))

#reason 2
check %>% group_by(paid) %>% summarise(n())

#reason 3
check %>% group_by(freetime) %>% summarise(total = n()) %>% mutate(Percent = total/sum(total) * 100)
check %>% group_by(freetime) %>% summarise(total = n()) %>% mutate(Percent = total/sum(total) * 100) %>% 
  summarise(compareA = Percent[1]+Percent[2], compareB = Percent[3]+Percent[4]+Percent[5])






#Analysis 1-4
Q1A4_1 = Q1 %>% group_by(address) %>% summarise(TotalAddress = n()) 

ggplot(Q1A4_1, aes(x=address, y=TotalAddress, fill=address)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=TotalAddress),position = position_dodge(1),vjust = -0.5) 


Q1A4_2 = Q1 %>% group_by(traveltime) %>% summarise(HomeToSchoolTime = n()) 

ggplot(Q1A4_2, aes(x=traveltime, y=HomeToSchoolTime)) + 
  geom_line() +
  geom_text(aes(label=HomeToSchoolTime),position = position_dodge(1),vjust = 0) 





#Question 2
Q2 = filter(student_data, g1&g2 < 10, g3>=10 & g3<15)



#Analysis 2-1
Q2A1 = Q2 %>% group_by(famrel, famsup) %>% summarise(Count = n())

ggplot(Q2A1, aes(x=famrel, y=Count, fill=famsup)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Count),position = position_dodge(1),vjust = -0.5) 




#Analysis 2-2 
Q2A2 = Q2 %>% group_by(studytime) %>% summarise(CountTime = n())

ggplot(Q2A2, aes(x=studytime, y=CountTime, fill=studytime)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=CountTime),position = position_dodge(1),vjust = -0.5) 




#Analysis 2-3
Q2A3DA = Q2 %>% group_by(dalc) %>% summarise(TotalDaily = n())

ggplot(data = Q2A3DA, aes(dalc, TotalDaily)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  geom_text(aes(label=TotalDaily),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Calculate daily alcohol consumption for students",
       subtitle = "(The x-axis represents how often students drink alcohol, from low to high.)",
       y = "Count Of Students Number", x = "Daily Alcohol Consumption") 



Q2A3WA = Q2 %>% group_by(walc) %>% summarise(TotalWeekly = n())

ggplot(data = Q2A3WA, aes(walc, TotalWeekly)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  geom_text(aes(label=TotalWeekly),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Calculate weekly alcohol consumption for students",
       subtitle = "(The x-axis represents how often students drink alcohol, from low to high.)",
       y = "Count Of Students Number", x = "Weekly Alcohol Consumption") 





#Analysis 2-4 
Q2A4 = Q2 %>% group_by(internet) %>% summarise(Total = n()) 
with(Q2A4,pie3D(Total,labels = Total, main = "Percentage Of Internet", explode = 0.2,
                col = rainbow(length(Total)), height = 0.17, labelcex = 1.8, labelcol = "black"))



#Features 2-1
Q2F1 = Q2 %>% group_by(higher) %>% summarise(Count=n())
with(Q2F1,pie3D(Count,labels = higher, main = "Percentage of student want take higher education level", 
                explode = 0.2, col = hcl.colors(length(Count),"TealRose"), height = 0.17, labelcex = 1.2,theta = 0.9, labelcol = "black"))



#Features 2-2
Q2F2A2 = Q2 %>% group_by(absences) %>% mutate(rangeOfAbsences = cut(absences,c(-1,10,20,30,40,50,Inf))) %>% 
  group_by(rangeOfAbsences) %>% summarise(TotalRange = n())

ggplot(Q2F2A2, aes(x=rangeOfAbsences, y=TotalRange)) +
  geom_point(aes(shape = factor(rangeOfAbsences), colour = factor(rangeOfAbsences)), size=3) + 
  geom_text(aes(label=TotalRange),position = position_dodge(0.9),vjust = -1) +
  ggtitle("Total Range of Absences")



#Features Analysis 2-3 
Q2F2A3 = Q2 %>% group_by(romantic) %>% summarise(Total = n()) 
with(Q2F2A3,pie3D(Total,labels = romantic, main = "Percentage Of romantic", explode = 0.2,
                  col = rainbow(length(Total)), height = 0.17, labelcex = 1.5, theta = 0.8))





#Question 3
Q3 = filter(student_data, g1&g2&g3 >= 15)
nrow(Q3)



#Analysis 3-1
Q3A1 = Q3 %>% group_by(reason) %>% summarise(Total = n())

with(Q3A1,pie3D(Total,labels = reason, main = "Total Best Student Reason To Choose This School",
                explode = 0.2, col = hcl.colors(length(Total), "Spectral"),
                height = 0.17, labelcex = 1.5,labelcol = "red", border="black",theta = 0.9))



#Analysis 3-2
Q3A2 = Q3 %>% group_by(absences) %>% mutate(rangeOfAbsences = cut(absences,c(-1,10,20,30,40,50,Inf))) %>%
  group_by(rangeOfAbsences) %>% summarise(TotalRange = n()) 

ggplot(Q3A2, aes(x=rangeOfAbsences, y=TotalRange, fill=rangeOfAbsences)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=TotalRange),position = position_dodge(1),vjust = -0.5) 



#Analysis 3-3
Q3A3 = Q3 %>% group_by(famrel, famsup) %>% summarise(Count = n())


ggplot(Q3A3, aes(x=famrel, y=Count, fill=famsup)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Count),position = position_dodge(1),vjust = -0.5) +
  scale_fill_manual("famsup", values = c("no" = "#FA8072", "yes" = "#228B22")) #light red & forest green color



#Analysis 3-4
Q3A4 = Q3 %>% group_by(romantic) %>% summarise(Count = n())

with(Q3A4,pie(Count,labels = romantic, main = "Count of Best Student Romantic Status", 
               col = hcl.colors(length(Count), "TealGrn"),clockwise = TRUE))



#Analysis 3-5 
Q3A5FE = Q3 %>% group_by(fedu) %>% summarise(TotalFatherEducationLevel = n()) 
Q3A5 = Q3 %>% group_by(medu) %>% summarise(TotalMotherEducationLevel = n()) %>% mutate(Q3A5FE)


Q3A5DF = data.frame(Q3A5)
Q3A5Melt = melt(Q3A5DF[,c("medu","TotalMotherEducationLevel","TotalFatherEducationLevel")], id.vars=1)

Q3A4Bar <- ggplot(Q3A5Melt, aes(x=medu, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value),position = position_dodge(1),vjust = -0.5) 

print(Q3A4Bar + labs(title = "Count Of Parent Education Level", y = "Count", x = "Parent Education Level"))




#Analysis 3-6
Q3A6DA = Q3 %>% group_by(dalc) %>% summarise(TotalDaily = n())

ggplot(data = Q3A6DA, aes(dalc, TotalDaily)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  geom_text(aes(label=TotalDaily),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Calculate daily alcohol consumption for students",
       subtitle = "(The x-axis represents how often students drink alcohol, from low to high.)",
       y = "Count Of Students Number", x = "Daily Alcohol Consumption") 



Q3A6WA = Q3 %>% group_by(walc) %>% summarise(TotalWeekly = n())

ggplot(data = Q3A6WA, aes(walc, TotalWeekly)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  geom_text(aes(label=TotalWeekly),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Calculate weekly alcohol consumption for students",
       subtitle = "(The x-axis represents how often students drink alcohol, from low to high.)",
       y = "Count Of Students Number", x = "Weekly Alcohol Consumption") 







#Question 4
Q4 = filter(student_data, (g1&g2 >=10 & g3 < 10))
nrow(Q4) #19 students



#Analysis 4-1
Q4A1 = Q4 %>% group_by(internet) %>% summarise(Count= n())

with(Q4A1,pie(Count,labels = internet, main = "Count of students accessing the Internet", col = Count))



#Analysis 4-2
Q4A2 = Q4 %>% group_by(activities) %>% summarise(Count = n())

with(Q4A2,pie3D(Count,labels = activities, main = "Count of students extra-curricular activities", 
                explode = 0.25, col = hcl.colors(length(Count), "Temps"), height = 0.17, 
                labelcex = 1.5,labelcol = "black", border="black", theta = 0.9))



#Analysis 4-3
Q4A3 = Q4 %>% group_by(goout) %>% summarise(Total=n())

ggplot(Q4A3, aes(x=goout, y=Total)) + 
  geom_bar(stat = "identity",width = 0.5,color="white",fill="orange")+
  geom_text(aes(label=Total),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Count the number of times students and friends go out",
       y = "Total number of students", x = "Number of time go out") 




#Analysis 4-4
Q4A4DA = Q4 %>% group_by(dalc) %>% summarise(TotalDaily = n())

ggplot(data = Q4A4DA, aes(dalc, TotalDaily)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  geom_text(aes(label=TotalDaily),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Calculate daily alcohol consumption for students",
       subtitle = "(The x-axis represents how often students drink alcohol, from low to high.)",
       y = "Count Of Students Number", x = "Daily Alcohol Consumption") 



Q4A4WA = Q4 %>% group_by(walc) %>% summarise(TotalWeekly = n())

ggplot(data = Q4A4WA, aes(walc, TotalWeekly)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  geom_text(aes(label=TotalWeekly),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Calculate weekly alcohol consumption for students",
       subtitle = "(The x-axis represents how often students drink alcohol, from low to high.)",
       y = "Count Of Students Number", x = "Weekly Alcohol Consumption") 




#Analysis 4-5 
Q4A5ST = Q4 %>% group_by(studytime) %>% summarise(CountStudyTime=n())
Q4A5FT = Q4 %>% group_by(freetime) %>% summarise(CountFreeTime = n())

Q4A5DF = data.frame(Q4A5ST, Q4A5FT)

ggplot(Q4A5DF, aes(y=CountStudyTime)) + 
  geom_line(aes(x = studytime), color = "darkred") + 
  geom_line(aes(x = freetime), color="steelblue", linetype="twodash") 
  
  



#Analysis 4-6
Q4A6 = Q4 %>% group_by(paid) %>% summarise(Count = n())

with(Q4A6,pie(Count,labels = paid, main = "Count of students paid extra class", 
                col = hcl.colors(length(Count), "Purp"), clockwise = TRUE))







#Question 5
Q5 = filter(student_data, g1&g2&g3 < 10)
nrow(Q5)




#Analysis 5-1
Q5A1Percent = round(Q5 %>% group_by(internet) %>% summarise(Count = n()) %>% summarise(Percent = Count/ sum(Count) * 100), digits = 2) 
Q5A1 = Q5 %>% group_by(internet) %>% summarise(Count = n()) %>% mutate(Q5A1Percent)
Q5A1Lab = paste(Q5A1$internet, "\nTotal:", Q5A1$Count,"\n" ,Q5A1$Percent, "%")

with(Q5A1,pie3D(Count,labels = Q5A1Lab, main = "Count of students access inernet", 
              explode = 0.25, col = hcl.colors(length(Count), "Purp"), height = 0.17, labelcex = 1.4,
              labelcol = "black", border="black", theta = 0.7))




#Analysis 5-2
Q5A2Percent = round(Q5 %>% group_by(paid) %>% summarise(Count = n()) %>% summarise(Percent = Count/ sum(Count) * 100), digits = 2) 
Q5A2 = Q5 %>% group_by(paid) %>% summarise(Count = n()) %>% mutate(Q5A2Percent)
Q5A2Lab = paste(Q5A2$paid, "\nTotal:", Q5A2$Count,"\n", Q5A2$Percent, "%")

with(Q5A2,pie(Count,labels = Q5A2Lab, main = "Count of students paid extra class", 
              col = hcl.colors(length(Count), "Greens"), clockwise = TRUE))



#Analysis 5-3
Q5A3ST = Q5 %>% group_by(studytime) %>% summarise(TotalStudyTime = n()) 
Q5A3GO = Q5 %>% group_by(goout) %>% summarise(TotalGoOut = n()) 

Q5A3ST[nrow(Q5A3ST)+1, ] <- 5

Q5A3DF = data.frame(Q5A3ST, Q5A3GO)
Q5A3DF[5,2] = 0

ggplot(Q5A3DF, aes(x=studytime)) + 
  geom_line(aes(y = TotalStudyTime), color = "darkred") + 
  geom_line(aes(y = TotalGoOut), color="steelblue", linetype="twodash") 




#Analysis 5-4
Q5A4Percent =round(Q5 %>% group_by(famsize) %>% summarise(TotalFamilySize = n()) %>% 
                     summarise(Percent = TotalFamilySize/ sum(TotalFamilySize) * 100), digits = 2) 
Q5A4 = Q5 %>% group_by(famsize) %>% summarise(TotalFamilySize = n()) %>% mutate(Q5A4Percent)
Q5A4Lab = paste(Q5A4$famsize, "\nTotal:", Q5A4$TotalFamilySize,"\n", Q5A4$Percent, "%")

with(Q5A4,pie3D(Percent,labels = Q5A4Lab, main = "Count of students's family size", 
              explode = 0.4, col = hcl.colors(length(TotalFamilySize), "Greens"), height = 0.17, labelcex = 1.6,
              labelcol = "red", border="black", theta = 0.9))




#Analysis 5-5
Q5A5DA = Q5 %>% group_by(dalc) %>% summarise(TotalDaily = n())

ggplot(data = Q5A5DA, aes(dalc, TotalDaily)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  geom_text(aes(label=TotalDaily),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Calculate daily alcohol consumption for students",
       subtitle = "(The x-axis represents how often students drink alcohol, from low to high.)",
       y = "Count Of Students Number", x = "Daily Alcohol Consumption") 



Q5A5WA = Q5 %>% group_by(walc) %>% summarise(TotalWeekly = n())

ggplot(data = Q5A5WA, aes(walc, TotalWeekly)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  geom_text(aes(label=TotalWeekly),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Calculate weekly alcohol consumption for students",
       subtitle = "(The x-axis represents how often students drink alcohol, from low to high.)",
       y = "Count Of Students Number", x = "Weekly Alcohol Consumption") 





#Analysis 5-6
Q5A6FE = Q5 %>% group_by(fedu) %>% summarise(TotalFatherEducationLevel = n()) 
Q5A6ME = Q5 %>% group_by(medu) %>% summarise(TotalMotherEducationLevel = n())

Q5A6FE[nrow(Q5A6FE)+1, ] <- 0

Q5A6DF = data.frame(Q5A6FE, Q5A6ME)
Q5A6Melt = melt(Q5A6DF[,c("medu","TotalMotherEducationLevel","TotalFatherEducationLevel")], id.vars=1)

ggplot(Q5A6Melt, aes(x=medu, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Count Of Parent Education Level", y = "Count", x = "Parent Education Level")




#Analysis 5-7
Q5A7Percent =round(Q5 %>% group_by(schoolsup) %>% summarise(TotalSchoolSupport = n()) %>% 
                     summarise(Percent = TotalSchoolSupport/ sum(TotalSchoolSupport) * 100), digits = 2) 
Q5A7 = Q5 %>% group_by(schoolsup) %>% summarise(TotalSchoolSupport = n()) %>% mutate(Q5A7Percent)
Q5A7Lab = paste(Q5A7$schoolsup, "\nTotal:", Q5A7$TotalSchoolSupport,"\n", Q5A7$Percent, "%")

with(Q5A7,pie3D(Percent,labels = Q5A7Lab, main = "Percentages of student got school education support", 
                explode = 0.4, col = hcl.colors(length(TotalSchoolSupport), "OrYel"), height = 0.17, labelcex = 1.6,
                labelcol = "black", border="black", theta = 0.9))







#Question 6
Q6 = filter(student_data, g1&g2&g3 > 10)
nrow(Q6) #491



#Analysis 6-1
Q6A1 = Q6 %>% group_by(address, reason) %>% summarise(Total= n(), .groups = 'drop') 

ggplot(Q6A1, aes(x=address, y=Total, fill=reason)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Total),position = position_dodge(0.9),vjust = -0.5) 


#Analysis 6-2

Q6A2Percent =round(Q6 %>% group_by(romantic) %>% summarise(Total = n()) %>%  summarise(Percent = Total/ sum(Total) * 100), digits = 2) 
Q6A2 = Q6 %>% group_by(romantic) %>% summarise(Total = n()) %>% mutate(Q6A2Percent)
Q6A2Lab = paste(Q6A2$romantic, "\nTotal:", Q6A2$Total,"\n", Q6A2$Percent, "%")

with(Q6A2,pie(Percent,labels = Q6A2Lab, main = "Total Percentages of student got romantic relationship", 
                col = hcl.colors(length(Total), "Peach"), clockwise = TRUE))




#Analysis 6-3
Q6A3DA = Q6 %>% group_by(dalc) %>% summarise(TotalDaily = n())

ggplot(data = Q6A3DA, aes(dalc, TotalDaily)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  geom_text(aes(label=TotalDaily),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Calculate daily alcohol consumption for students",
       subtitle = "(The x-axis represents how often students drink alcohol, from low to high.)",
       y = "Count Of Students Number", x = "Daily Alcohol Consumption") 



Q6A3WA = Q6 %>% group_by(walc) %>% summarise(TotalWeekly = n())

ggplot(data = Q6A3WA, aes(walc, TotalWeekly)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color="steelblue") + 
  geom_text(aes(label=TotalWeekly),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Calculate weekly alcohol consumption for students",
       subtitle = "(The x-axis represents how often students drink alcohol, from low to high.)",
       y = "Count Of Students Number", x = "Weekly Alcohol Consumption") 



#Analysis 6-4
Q6A4 = Q6 %>% group_by(absences) %>% mutate(rangeOfAbsences = cut(absences,c(-1,10,20,30,40,50,Inf))) %>%
  group_by(rangeOfAbsences) %>% summarise(TotalRange = n()) 

ggplot(Q6A4, aes(x=rangeOfAbsences, y=TotalRange, fill=rangeOfAbsences)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=TotalRange),position = position_dodge(1),vjust = -0.5) 




#Analysis 6-5
Q6A5FE = Q6 %>% group_by(fedu) %>% summarise(TotalFatherEducationLevel = n()) 
Q6A5 = Q6 %>% group_by(medu) %>% summarise(TotalMotherEducationLevel = n()) %>% mutate(Q6A5FE)


Q6A5DF = data.frame(Q6A5)
Q6A5Melt = melt(Q6A5DF[,c("medu","TotalMotherEducationLevel","TotalFatherEducationLevel")], id.vars=1)

 ggplot(Q6A5Melt, aes(x=medu, y=value, fill=variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value),position = position_dodge(1),vjust = -0.5) +
  labs(title = "Count Of Parent Education Level", y = "Count", x = "Parent Education Level")




 
 
 
 

#Extra Features


#separate the range
RangeOfGradeG3 = student_data %>% group_by(g3) %>% mutate(rangeOfG3 = cut(g3, c(-1,5,10,15,20))) %>% group_by(rangeOfG3) %>% summarise(TotalG3 = n())
RangeOfGradeG2 = student_data %>% group_by(g2) %>% mutate(rangeOfG2 = cut(g2, c(-1,5,10,15,20))) %>% group_by(rangeOfG2) %>% summarise(TotalG2 = n())
RangeOfGradeG1 = student_data %>% group_by(g1) %>% mutate(rangeOfGrade = cut(g1, c(-1,5,10,15,20))) %>% group_by(rangeOfGrade) %>% summarise(TotalG1 = n())


#store in data frame and reshape the data
RangeOfGrade = data.frame(RangeOfGradeG1,RangeOfGradeG2,RangeOfGradeG3)
RangeOfGradeMelt = melt(RangeOfGrade, id=c("rangeOfGrade","rangeOfG2","rangeOfG3"), value.name = "Total")


#draw multiple line in wrap function, include line & point & label text
ggplot(data=RangeOfGradeMelt, aes(x=rangeOfGrade, y=Total, group=3, col=variable)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label=Total),position = position_dodge(1),vjust=-0.5) +
  facet_wrap(facets = vars(variable)) +
  theme_bw()







# 6 Question, 35 Analysis, 1 Extra Features