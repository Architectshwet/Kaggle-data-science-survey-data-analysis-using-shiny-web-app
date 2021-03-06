require(data.table)
require(highcharter)
require(ggplot2)
require(tidyverse)
setwd("C:/Users/Architect_shwet/Desktop/data science projects/kaggle data science survey")
Surveydf<- read.csv("multipleChoiceResponses.csv") #for faster data reading

attach(Surveydf)

#Part 3 - This section will analyze and study the professional lives of the participants, 
#their major degree ,time spend studying data science topics, what job titles they hold,
#which ML method they actually use in the industries , which bolgs the participants prefer 
#the most for studying data science etc.

#Lets get started 

#Let's start with the most preferred blog sites for learning datascience- This is a 
#multiple answer field. Let's find the top 15 most preferred answers.

blogs<-Surveydf %>% group_by(BlogsPodcastsNewslettersSelect) %>%
  summarise(count=n()) %>% 
  top_n(15) %>% 
  arrange(desc(count))

#removing NA value
blogs[1,1]<-NA
colnames(blogs)<-c("Blogname","Count")

#let's plot them
hchart(na.omit(blogs),hcaes(x=Blogname,y=Count),type="column",color="#062D67") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of most preferred blogs for learning",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 
#hence one can see that one of the most famous and preferred blog sites are R bloggers and Kdnuggets.

#Let's now study how long aprticipants have been learning data science-

table(LearningDataScienceTime)

hchart(Surveydf$LearningDataScienceTime,type="pie",name="count")

#So most of the participants have started learning data science in the past year itself or its been less than a year since they started studying learning data science.

#Let's check the age distribution of the particpiants and for how long they have been learning 
#data science.

hcboxplot(x=Surveydf$Age,var=Surveydf$LearningDataScienceTime,outliers = F,color="#09870D",name="Age Distribution") %>%
  hc_chart(type="column")  %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Boxplot of ages and the learning time of participants",align="center") %>%
  hc_add_theme(hc_theme_elementary()) 

#The above plot was quiet predictable as people with less time learning data science are younger.

#3) Let's now study what participants entered and feel which skills are important for becoming 
#a data scientist?

#Let's do some data wrangling and transformations.
#Making a separate data frame for each variable, for easier understanding.

#let's make a function to ease things
#function takes argument as a dataframe and the categorical variable which we want summarize and group
aggr<-function(df,var) 
{
  require(dplyr)
  var <- enquo(var) #quoting
  dfname<-df %>% 
    group_by_at(vars(!!var)) %>%  ## Group by variables selected by name:
    summarise(count=n()) %>%
    arrange(desc(count))
  
  dfname#function returns a summarized dataframe
  
}

RSkill<-aggr(Surveydf,JobSkillImportanceR)
RSkill[1,]<-NA
SqlSkill<-aggr(Surveydf,JobSkillImportanceSQL)
SqlSkill[1,]<-NA
PythonSkill<-aggr(Surveydf,JobSkillImportancePython)
PythonSkill[1,]<-NA
BigDataSkill<-aggr(Surveydf,JobSkillImportanceBigData)
BigDataSkill[1,]<-NA
StatsSkill<-aggr(Surveydf,JobSkillImportanceStats)
StatsSkill[1,]<-NA
DegreeSkill<-aggr(Surveydf,JobSkillImportanceDegree)
DegreeSkill[1,]<-NA
EnterToolsSkill<-aggr(Surveydf,JobSkillImportanceEnterpriseTools)
EnterToolsSkill[1,]<-NA
MOOCSkill<-aggr(Surveydf,JobSkillImportanceMOOC)
MOOCSkill[1,]<-NA
DataVisSkill<-aggr(Surveydf,JobSkillImportanceVisualizations)
DataVisSkill[1,]<-NA
KaggleRankSkill<-aggr(Surveydf,JobSkillImportanceKaggleRanking)
KaggleRankSkill[1,]<-NA

hchart(na.omit(RSkill),hcaes(x=JobSkillImportanceR,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of R skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())


hchart(na.omit(PythonSkill),hcaes(x=JobSkillImportancePython,y=count),type="pie",name="Count") %>%
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Python skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

hchart(na.omit(SqlSkill),hcaes(x=JobSkillImportanceSQL,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of SQL skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

hchart(na.omit(BigDataSkill),hcaes(x=JobSkillImportanceBigData,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Big Data skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

hchart(na.omit(StatsSkill),hcaes(x=JobSkillImportanceStats,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Statistics kill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

hchart(na.omit(DataVisSkill),hcaes(x=JobSkillImportanceVisualizations,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Data Viz skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

hchart(na.omit(DegreeSkill),hcaes(x=JobSkillImportanceDegree,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Degree",align="center") %>%
  hc_add_theme(hc_theme_elementary())

hchart(na.omit(EnterToolsSkill),hcaes(x=JobSkillImportanceEnterpriseTools,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Enterprise Tools skill",align="center") %>%
  hc_add_theme(hc_theme_elementary())

hchart(na.omit(MOOCSkill),hcaes(x=JobSkillImportanceMOOC,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of MOOCs",align="center") %>%
  hc_add_theme(hc_theme_elementary())

hchart(na.omit(KaggleRankSkill),hcaes(x=JobSkillImportanceKaggleRanking,y=count),type="pie",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Piechart of importance of Kaggle Rankings",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#We can see from the above plot that the most unnecessary skill amongst all is having 
#a knowledge of Enterprise tools, Degree, Kaggle Rankings and MOOCs. These have higher 
#count of unnecessary skills entered by the participants.

#Whereas, Knowledge of Statistics,Python,R and Big data skills are most necessary and 
#Nice to have skills as per answers entered by the survey participants.

#What proves that you have good Data science knowledge?

knowlegdeDf<-Surveydf %>% group_by(ProveKnowledgeSelect) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

knowlegdeDf[1,]<-NA 

hchart(na.omit(knowlegdeDf),hcaes(x=ProveKnowledgeSelect,y=count),type="column",color="#049382",name="count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of what proves you have Datascience knowledge",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Let's check the most famous machine learning technique in which participants consider themselves competent?

Mltechique<-Surveydf %>% group_by(MLTechniquesSelect) %>%
summarise(count=n()) %>% 
arrange(desc(count)) %>%
top_n(20)

Mltechique[1,]<-NA

hchart(na.omit(Mltechique),hcaes(x=MLTechniquesSelect,y=count),type="column",color="purple",name="count") %>% 
hc_exporting(enabled = TRUE) %>%
hc_title(text="Barplot of competent ML techniques of participants",align="center") %>%
hc_add_theme(hc_theme_elementary())

#So we cant notice that Logistic regression, Decision trees, Random forests are the top 2 
#most competent techniques in which the participants are competent and can successfully implement 
#and are most efficient in implementing.

#Let's check which Learning algorithm participants use at work ?

#Now we will check which machine learning algorithm is most used by the participants at their work.

MLalgoWork<-Surveydf %>% group_by(WorkAlgorithmsSelect) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  top_n(20)

MLalgoWork[c(1,3),]<-NA


hchart(na.omit(MLalgoWork),hcaes(x=WorkAlgorithmsSelect,y=count),type="column",color="green",name="count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Most used ML algorithms at Work",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Again as we can see from the above plot, Regression,Logistic regression and decision trees 
#lead the pack as the most used learning algorithms which used at work by participants.

#Now let's check which tools are used most at work?

ToolatWork<-Surveydf %>% group_by(WorkToolsSelect) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  top_n(20)

ToolatWork[c(1),]<-NA


hchart(na.omit(ToolatWork),hcaes(x=WorkToolsSelect,y=count),type="column",color="#7C0E3E",name="count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Most used data science tools used at Work",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#From the above plot we can see that Python and R are collectively used by datascientists 
#the most as entered by the survey participants. Hence Python and R still tops the most 
#used tools at work according to the survey.

#Most used ML method at work?
  
MethodatWork<- Surveydf %>% group_by(WorkMethodsSelect) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  top_n(20)

MethodatWork[c(1,3),]<-NA


hchart(na.omit(MethodatWork),hcaes(x=WorkMethodsSelect
                                   ,y=count),type="column",color="#F14B5B",name="count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Most used ML and DS methods used at Work",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Let's check for how much time the Emloyer has been using ML?

#Let's first check the different tyepes of employers and the industry entered by the 
#participants. This can give us some details about the Employers involvement in Data science and ML.

table(EmployerIndustry)

Employerdf<-as.data.frame(table(EmployerIndustry)) %>% arrange(desc(Freq))

Employerdf[1,]<-NA

hchart(Employerdf,hcaes(x=EmployerIndustry,y=Freq),type="column",color="lightgreen")

employerCountrydf<-Surveydf %>%
  group_by(EmployerIndustry,Country) %>%
  filter(EmployerIndustry %in% Employerdf$EmployerIndustry, Country %in% countryCount$Var1) %>%
  summarise(count=n()) %>% 
  arrange(desc(count))

USEmployerdf<-employerCountrydf  %>%  filter(Country=="United States")
IndiaEmployerdf<-employerCountrydf  %>%  filter(Country=="India")


hchart(USEmployerdf,hcaes(x=EmployerIndustry,y=count),type="column",color="#00008b",name="Count") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of of Employer industry of United states survey  participants",align="center") %>%
  hc_add_theme(hc_theme_elementary())

hchart(IndiaEmployerdf,name="Count",hcaes(x=EmployerIndustry,y=count),type="column",color="#33ccff") %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_title(text="Barplot of Employer industry of Indian survey participants ",align="center") %>%
  hc_add_theme(hc_theme_elementary())








