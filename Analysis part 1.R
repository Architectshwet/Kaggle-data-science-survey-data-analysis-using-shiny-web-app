require(highcharter)
require(dplyr)
require(tidyr)
require(shiny)
require(shinydashboard)
require(data.table)
require(stringr)

setwd("C:/Users/Architect_shwet/Desktop/data science projects/kaggle data science survey")
Surveydf <- read.csv('multipleChoiceResponses.csv')

#rm(Surveydf)
str(Surveydf)
head(Surveydf)
names(Surveydf)

table(Surveydf$GenderSelect)

hchart(Surveydf$GenderSelect,type="bar",name="count",color="green") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of gender",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#barplot of Emp_status
hchart(Surveydf$EmploymentStatus,type="bar",name="count",color="red") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Employment Status",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 


#barplot of country
hchart(Surveydf$Country,type="bar",name="Count",color="blue") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Country or participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#treemap of top 10  countries  of participant
countryCount <-as.data.frame(table(Surveydf$Country)) %>%  top_n(10) 
head(countryCount)
hchart(countryCount,hcaes(Var1,value=Freq,color=Freq),name="Count of participants",type="treemap") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Tree map of top 10 countries of participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 


#So most of the participants were Male and were full time employed. Secondly most of the participants 
#are form USA followed by India in the second place. This shows that people from these countries are 
#very much interested in Data science and its related subfields, also shows large number of people 
#in the field of Datascience.


# We can also check this relationship between Age and employment status of the participants using a Boxplot
hcboxplot(x= Surveydf$Age , name="Age of participants", var = Surveydf$EmploymentStatus,color="purple",
          outliers = FALSE) %>%
  hc_title(text="Boxplot of Ages of the participants and their Employment Status",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary()) %>% 
  hc_chart(type="column") #for vertical box plot

#Hence the above Boxplot can actually tell us about the bi-variate relation and the statistical 
#distribution between Ages and the Employment status of the survey participants. We can notice that 
#the young participants between age of 20-25 are mostly Unemployed, of looking for job opportunities.

# Let's see how many are students ?

table(Surveydf$StudentStatus)#most didn't fill this field

#let's check if participants are learning DS or not
table(Surveydf$LearningDataScience) #most of them didn't answered this too
hchart(Surveydf$LearningDataScience,name="count",type="column",color="#99FF33") %>%
  hc_title(text="Barplot of Learning Data science field",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

#Hence we can notice that both these variables were not answered by more than 90% of the participants.
#So there is less scope of interpretting something from these.

#Let's see how many are Coders

hchart(Surveydf$CodeWriter,name="count",type="column",color="#99FF33") %>%
  hc_title(text="Barplot of number of coders",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

#let's now check which country has most coders

#Making a new dataframe grouped by country and Code wirter variables and summarized by the count of each
codeCountry<-Surveydf %>% group_by(CodeWriter,Country) %>% 
  select(CodeWriter,Country) %>%
  filter(CodeWriter %in% c("Yes","No")) %>%
  summarize(total=n())

codeCountry<-Surveydf %>% group_by(CodeWriter,Country) %>% 
  select(CodeWriter,Country) %>%
  filter(CodeWriter %in% c("Yes","No")) %>%
  tally()

#getting top 10 countries and their total coders and non coders
TopCoders<-codeCountry %>% top_n(10) %>% arrange(desc(total))

hchart(TopCoders,type="column",name=c("Do not write code","Code Writers"),hcaes(x=Country,y=total,
    group=CodeWriter),color=c("black","#FF4040") ) %>%
  hc_title(text="Barplot of Countries grouped by Coder Writers",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

#let's make a dataframe to plot a barplot
jobdf<-as.data.frame(table(Surveydf$CurrentJobTitleSelect))
jobdf
jobdf[1,1]<-"Not answered"
jobdf<-na.omit(jobdf)

jobdf %>% arrange(desc(Freq)) %>%   hchart(hcaes(x=Var1,y=Freq),name="Count",
        color="#751A75",type="column") %>%
  hc_title(text="Barplot of Current Job titles of the participants",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

countryJobs<-Surveydf %>% group_by(Country,CurrentJobTitleSelect) %>%
  filter(Country %in% countryCount$Var1) %>%
  select(Country,CurrentJobTitleSelect) %>%
  summarize(total=n()) %>%
  arrange(desc(total))

countryJobs[1:2,]<-NA
countryJobs<-na.omit(countryJobs)

#Colors vectors for plotting
USIndJobs<-countryJobs %>% filter(Country %in% c("United States","India"))

colors <- c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#7f8c8d","#000004", "#3B0F70",
            "#8C2981", "#DE4968", "#FE9F6D", "#FCFDBF","#ffb3b3","#66ff33","#00b3b3","#4d4dff")

colors2<-c("#d35400", "#2980b9", "#2ecc71", "#f1c40f", "#2c3e50", "#7f8c8d","#000004", "#3B0F70", 
           "#8C2981", "#DE4968", "#FE9F6D", "#FCFDBF","#ffb3b3","#66ff33","#00b3b3","#4d4dff","7D8A16")


hchart(USIndJobs,type="column",hcaes(Country, y=total,group=CurrentJobTitleSelect),color=colors) %>%
  hc_title(text="Barplot of Jobs of Country's participants",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

#USA INDIA and RUSSIA-Top 3 countries with maximum response 

JobsMajorCountry<-countryJobs %>% filter(Country %in% c("United States","India","Russia"))

JobsMajorCountry[13,2]<-NA

hchart(na.omit(JobsMajorCountry),type="column",hcaes(Country, y=total,group=CurrentJobTitleSelect),color=colors) %>%
  hc_title(text="Barplot of Jobs of Country's participants",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

#The above plot shows the comparative current job titles of survey participants form India and USA.

#1)From India Most of the participants were Software developers followed by Data scientists.

#2)From USA most of the participants were Data scientists followed by software developers.

#Let's check the where the participants were Employed?

Employer<-as.data.frame(table(Surveydf$CurrentEmployerType)) %>% top_n(15) %>% arrange(desc(Freq))
Employer[1,]<-NA
names(Employer)<-c("EmployerType","Count")
hchart(na.omit(Employer),type="column",hcaes(x=EmployerType,y=Count),color="#0E2E93") %>%
  hc_title(text="Barplot of top 15 Type of Employers",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())

EmployerType<-as.data.frame(table(Surveydf$CurrentEmployerType,Surveydf$Country)) %>% 
  top_n(20) %>% arrange(desc(Freq))
#assigning NA values to missing ones
EmployerType[c(1,2,8,12,13,16,19,20),1]<-NA

colnames(EmployerType)<-c("EmployerType","Country","Count")

#plotting data
hchart(na.omit(EmployerType),type="column",hcaes(x=EmployerType, y=Count,group=Country),color=c("#DE4968","#f1c40f","black")) %>%
  hc_title(text="Barplot of Type of Employer and Country",align="center") %>%
  hc_exporting(enabled=TRUE) %>%
  hc_add_theme(hc_theme_elementary())



