# remove variable store 
rm(list=ls())
#import librarys
library(dplyr)
library(ggplot2)
#load data
data <- read.csv("C:/Users/anise/OneDrive/Desktop/R_Project/BankingMarketingData/bank-additional-full.csv", 
                 header = TRUE, sep = ";")

#coulmns names
colnames(data)

#encoding y values 1/0 from yes/no for data manupulation

data <- data %>%
  mutate(y=ifelse(y=="no", 0, 1))

data$y <- as.integer(data$y)

sum(data$y)         # total number of positive conversions
nrow(data)          # total number of records
sum(data$y)/nrow(data)*100.0 

#conversion by age
conversionsAgeGroup <- data %>%
  group_by(AgeGroup=cut(age, breaks=seq(20, 70, by=10))) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100)

conversionsAgeGroup$AgeGroup <- as.character(conversionsAgeGroup$AgeGroup)
conversionsAgeGroup$AgeGroup[6] <- "70+"

#visualise
ggplot(data=conversionsAgeGroup, aes(x=AgeGroup, y=ConversionRate)) +
  geom_bar(width=0.5, stat="identity", fill="darkgreen") + 
  labs(title="Conversion Rates by Age Group")

### Conversions by age group and marital status
conversionsAgeMarital <- data %>%
  group_by(AgeGroup=cut(age, breaks=seq(20,70, by=10)),
           Marital=marital) %>%
  summarize(Count=n(), NumConversions=sum(y)) %>%
  mutate(TotalCount=sum(Count)) %>%
  mutate(ConversionRate=NumConversions/TotalCount*100)

conversionsAgeMarital$AgeGroup <- as.character(conversionsAgeMarital$AgeGroup)
conversionsAgeMarital$AgeGroup[is.na(conversionsAgeMarital$AgeGroup)] <- "70+"

#visulaise
ggplot(conversionsAgeMarital, aes(x=AgeGroup, y=ConversionRate, fill=Marital)) +
  geom_bar(width=0.5, stat = "identity") +
  labs(title="Conversion Rates by Age Group and Marital Status")



#conversion by job

conversionsJob <- data %>%
  group_by(Job=job) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#order the jobs DESC for the bar chart
conversionsJob$Job <- factor(conversionsJob$Job, 
                             levels = conversionsJob$Job[order(-conversionsJob$ConversionRate)])

# visualizing conversions by job
ggplot(conversionsJob, aes(x=Job, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Job") +
  theme(axis.text.x = element_text(angle = 90))

#conversion by education

conversionsEdu <- data %>%
  group_by(Education=education) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#order DESC for the bar chart
conversionsEdu$Education <- factor(conversionsEdu$Education, 
                                   levels = conversionsEdu$Education[order(-conversionsEdu$ConversionRate)])
#visualizing conversions by education
ggplot(conversionsEdu, aes(x=Education, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Education") +
  theme(axis.text.x = element_text(angle = 90))


#not default credit

conversionsDefaultCredit <- data %>%
  group_by(HasCredit=default) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#visualizing the data
ggplot(conversionsDefaultCredit, aes(x=HasCredit, y=ConversionRate, fill=HasCredit)) +
  geom_bar(width=0.5, stat = "identity") +
  labs(title="Conversion Rates by Default Credit")

#conversion using personal loan
conversionsHousing <- data %>%
  group_by(HousingLoan=housing) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#visualizing the data
ggplot(conversionsHousing, aes(x=HousingLoan, y=ConversionRate, fill=HousingLoan)) +
  geom_bar(width=0.5, stat = "identity") +
  labs(title="Conversion Rates by Housing Loan")

#group the data - personal loan
conversionsLoan <- data %>%
  group_by(Loan=loan) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#visualizing the data
ggplot(conversionsLoan, aes(x=Loan, y=ConversionRate, fill=Loan)) +
  geom_bar(width=0.5, stat = "identity") +
  labs(title="Conversion Rates by Personal Loan")
#contact type
conversionsContact <- data %>%
  group_by(Contact=contact) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

head(conversionsContact)

#last contact mmonth of  year
conversionsMonth <- data %>%
  group_by(Month=month) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#reorder DESC
conversionsMonth$Month <- factor(conversionsMonth$Month, 
                                 levels = conversionsMonth$Month[order(-conversionsMonth$ConversionRate)])
#visualizing the data
ggplot(conversionsMonth, aes(x=Month, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Last Contact Month") +
  theme(axis.text.x = element_text(angle = 90))
```

People who were contacted last time in March, December, September, and October convert much better than others

#last contact of week
conversionsDayOfWeek <- data %>%
  group_by(Day_Of_Week=day_of_week) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#reorder DESC
conversionsDayOfWeek$Day_Of_Week <- factor(conversionsDayOfWeek$Day_Of_Week, 
                                           levels = c("mon", "tue", "wed", "thu", "fri"))
#visualizing the data
ggplot(conversionsDayOfWeek, aes(x=Day_Of_Week, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Last Contact Day of Week") +
  theme(axis.text.x = element_text(angle = 90))


#Correlation between subscribing to a term deposit and call duration

data_duration <- data %>%
  group_by(Subscribed=y) %>%
  summarise(Average_Duration=mean(duration))
head(data_duration)


#Conversions by the number of contacts performed during the campaign
conversionsCamp <- data %>%
  group_by(Campaign=campaign) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

#outcome from previous campian
conversionsPOutcome <- data %>%
  group_by(Previous_Outcome=poutcome) %>%
  summarize(TotalCount=n(), NumberConversions=sum(y)) %>%
  mutate(ConversionRate=NumberConversions/TotalCount*100) %>%
  arrange(desc(ConversionRate))

# visualizing the data
ggplot(conversionsPOutcome, aes(x=Previous_Outcome, y=ConversionRate)) +
  geom_bar(width=0.5, stat = "identity", fill="darkgreen") +
  labs(title="Conversion Rates by Outcome of the Previous Campaign")
