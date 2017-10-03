---
title: "R Toolbox Assignment"
author: "Ya-Hsuan Chuo"
date: "2017/05/01"
output:
  html_document: default
  pdf_document:
    fig_caption: yes
  word_document: default
runtime: shiny
---

## 1. Data Exploration

#### People make appointments before seeing a doctor. However, we tend to forget to cancel and resulted in no-show to the appointment. The data is from Kaggle https://www.kaggle.com/joniarroba/noshowappointments which is a dataset that uploaded two months ago. A new dataset made me more space to analyze it. 

#### This data was collected in Brazil Public Sector hospital. When people make a doctor appointment, and receive the SMS text message notification all in all they still have a lot of people no-show. The data records 300,000 people from 2013 to 2015. 

## 2. Finding Problem

#### I am looking for the reasons or some facts that shows what certain patterns people not showing after they register the doctor appointment. This data collected the patients with their age, gender, the disease they have, the appointments happened year,month, week of day and the appointments day they made. Also, they waiting time between these day and the status shows that they do show up or not. 


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(ggvis)
library(shiny)
```

```{r data import, include = FALSE}
medical <- read_csv("No-show-Issue-Comma-300k.csv")
```

```{r head, echo=FALSE}
head(medical)
```

## 3. Data cleaning

#### **Status** is the most important variables in the medical dataset. It records the showing or not for a person. First, Show-Up and No-Show are string, I should change it to binary variables. 

#### The data provider indicated that the **Scholarship** column refers to the patient¡¦s Bolsa Familia scholarship status; in Brazil, poor families may benefit by receiving financial aid. 

#### Second, I reorganize the name of variables and adjust some variables to make it reasonable. For example, the **AwaitingTime** (waiting time) is negative while it records the days between the appointment had made and the time of appointment, (**AppointmentRegistration - AppointmentData**) so it is negative value, thus I take the abs() function to make it positive. 

```{r}
medical$Status[medical$Status=="Show-Up"] <- 1
medical$Status[medical$Status=="No-Show"] <- 0

medical <- medical %>% rename(Handicap=Handcap, Bolsa_Familia_recipient=Scholarship)

medical$AwaitingTime <- abs(medical$AwaitingTime)
```


#### There are seven disease-related data are recorded. **Diabetes, Alcoolism, HiperTesion, Handicap, Smokes, Scholarship, Tuberculosis.** For **Handicap** , it is not a binary variable. It contains 0~4. Most of the people are without Handicap. As the table shows, it almost includes a great among of the samples. 

```{r}
medical %>% group_by(Handicap) %>% summarise(n=n())
```

#### I turn the time variables of **AppointmentRegistation, AppointmentData, DayOfWeek** in a seperate column. 

```{r}
medical <- medical %>% 
           mutate(Ryear=format(medical$AppointmentRegistration,'%Y'),
                  Rmonth=format(medical$AppointmentRegistration,'%m'),
                  Ayear=format(medical$ApointmentData,'%Y'),
                  Amonth=format(medical$ApointmentData,'%m'))

head(medical %>% select(Ryear,Rmonth,Ayear,Amonth))
```


#### The appointment would be reminded by SMS. The maximum number of SMS is 2. Most of the people are reminded with 0-1 SMS. 

```{r, fig.width=5}
medical %>% group_by(Sms_Reminder) %>% summarise(n=n()) %>%
            ggplot(aes(x=Sms_Reminder,y=n,fill=as.factor(Sms_Reminder)))+
            geom_bar(stat="identity")+ylab("number of people")+     
            geom_text(aes(label=n),vjust=-0.2,col="black")+
            theme(legend.position="none")
```

#### **Age** should be positive above zero, so I filter out the age that are below zero and also the age above 100. I would like to see the normal cases and also those ages below zero may be typos.
#### The mean of male and female are similar. However, the male has a higher variance than female.   

```{r,fig.width=5}
medical <- medical %>% filter(Age >0& Age <=100) 
medical %>% ggplot(aes(x=Gender,y=Age,fill=Gender))+
           geom_boxplot()+
           scale_fill_manual(values = c("#fa9fb5", "#9ecae1"))+
           theme(legend.position="none")
  
```

#### So this is the summary of all the variables.

```{r }
summary(medical)
```

## 4. Visualization

### Plot1: Status

#### First, we can see the number of people who showed and not-showed. This dataset has more people show than not show. However, there are still 30% of people not show. 

```{r, echo=FALSE, fig.width=5}
medical %>% ggplot(aes(Status,fill=Status))+geom_bar()+ 
  geom_text(stat='count',aes(label=..count..))+
  scale_fill_manual(values = c("#e34a33", "grey"))+
  scale_x_discrete(labels=c("show","no-show"))+
  ylab("Numbers of people") +
  ggtitle("No-Show Rate of about 30,000 people")+
  theme(legend.position="none")
```

### Plot2: Disease - the precentage of people showing or not corresponds with the 5 disease. 

```{r, echo=FALSE,fig.width = 12}
a1 <- as.data.frame(medical %>% filter(Diabetes==1) %>% 
      summarise(show=sum(Status==1)/n(),not=sum(Status==0)/n())) %>% 
      gather(`show`, `not`, key = "condition", value = "rate") %>%    
      ggplot(aes(condition,rate,fill=as.factor(rate)))+geom_bar(stat="identity",position="dodge")+
      scale_fill_manual(values = c("#e34a33", "grey"))+
      scale_x_discrete(labels=c("no-show","show"))+
      scale_y_continuous(limits = c(0, 0.8))+labs(x='Diabetes')+ylab("percentage")+
      theme(legend.position="none")

a2 <- as.data.frame(medical %>% filter(Alcoolism==1) %>% 
      summarise(show =sum(Status==1)/n(),not=sum(Status==0)/n())) %>% 
      gather(`show`, `not`, key = "condition", value = "rate") %>%  
      ggplot(aes(condition,rate,fill=as.factor(rate)))+geom_bar(stat="identity", position="dodge")+
      scale_fill_manual(values = c("#e34a33", "grey"))+
      scale_x_discrete(labels=c("no-show","show"))+
      scale_y_continuous(limits = c(0, 0.8))+labs(x='Alcoolism')+ylab("")+
      theme(legend.position="none")

a3 <- as.data.frame(medical %>% filter(HiperTension==1) %>% 
      summarise(show=sum(Status==1)/n(), not=sum(Status==0)/n())) %>% 
      gather(`show`, `not`, key = "condition", value = "rate") %>%    
      ggplot(aes(condition,rate,fill=as.factor(rate)))+geom_bar(stat="identity", position="dodge")+
      scale_fill_manual(values = c("#e34a33", "grey"))+
      scale_x_discrete(labels=c("no-show","show"))+
      scale_y_continuous(limits = c(0, 0.8))+labs(x='HiperTension')+ylab("")+
      theme(legend.position="none")

a4 <- as.data.frame(medical %>% filter(Smokes==1) %>% 
      summarise(show=sum(Status==1)/n(),not=sum(Status==0)/n())) %>% 
      gather(`show`, `not`, key = "condition", value = "rate") %>%
      ggplot(aes(condition,rate,fill=as.factor(rate)))+geom_bar(stat="identity", position="dodge")+
      scale_fill_manual(values = c("#e34a33", "grey"))+
      scale_x_discrete(labels=c("no-show","show"))+
      scale_y_continuous(limits = c(0, 0.8))+labs(x='Smokes')+ylab("")+
      theme(legend.position="none")

a5 <- as.data.frame(medical %>% filter(Bolsa_Familia_recipient==1) %>%    
      summarise(show=sum(Status==1)/n(),not=sum(Status==0)/n())) %>% 
      gather(`show`, `not`, key = "condition", value = "rate") %>%
      ggplot(aes(condition,rate,fill=as.factor(rate)))+geom_bar(stat="identity", position="dodge")+
      scale_fill_manual(values = c("#e34a33", "grey"))+
      scale_x_discrete(labels=c("no-show","show"))+
      scale_y_continuous(limits = c(0, 0.8))+labs(x='Bolsa_Familia_recipient')+ylab("")+
      theme(legend.position="none")

grid.arrange( a1, a2, a3, a4, a5,ncol=5)
```

### Plot3: Disease- Different Disease and the number of people showing or not. 

```{r, echo=FALSE,fig.width=12}
p1 <- medical %>% filter(Diabetes==1) %>% 
      ggplot()+geom_bar(aes(x=Status,fill=as.factor(Status)))+
      scale_fill_manual(values = c("#e34a33", "grey"))+
      scale_y_continuous(limits = c(0, 50000))+
      scale_x_discrete(labels=c("no-show","show"))+
      labs(x='Diabetes',y='number of people')+
      theme(legend.position="none")
      
p2 <- medical %>% filter(Alcoolism==1) %>% 
      ggplot()+geom_bar(aes(x=Status,fill=as.factor(Status)))+
      scale_fill_manual(values = c("#e34a33", "grey"))+
      scale_y_continuous(limits = c(0, 50000))+
      scale_x_discrete(labels=c("no-show","show"))+
      labs(x='Alcoolism',y='')+
      theme(legend.position="none")

p3 <- medical %>% filter(HiperTension==1) %>%
      ggplot()+geom_bar(aes(x=Status,fill=as.factor(Status)))+
      scale_fill_manual(values = c("#e34a33", "grey"))+
      scale_y_continuous(limits = c(0, 50000))+
      scale_x_discrete(labels=c("no-show","show"))+
      labs(x='HiperTension',y='')+
      theme(legend.position="none")

p4 <- medical %>% filter(Smokes==1) %>% 
      ggplot()+geom_bar(aes(x=Status,fill=as.factor(Status)))+
      scale_fill_manual(values = c("#e34a33", "grey"))+
      scale_y_continuous(limits = c(0, 50000))+
      scale_x_discrete(labels=c("no-show","show"))+
      labs(x='Smokes',y='')+
      theme(legend.position="none")

p5 <- medical %>% filter(Bolsa_Familia_recipient==1) %>% 
      ggplot()+geom_bar(aes(x=Status,fill=as.factor(Status)))+
      scale_fill_manual(values = c("#e34a33", "grey"))+
      scale_y_continuous(limits = c(0,50000))+
      scale_x_discrete(labels=c("no-show","show"))+
      labs(x='Bolsa_Familia_recipient',y='')+
      theme(legend.position="none")

grid.arrange( p1, p2, p3, p4, p5, ncol=5)
```

#### In Plot2, people with the disease of  ** Alcoolism,Smokes and Bolsa_Familia_recipient** have more possibilities that they will "not show". But in the Plot 3, Most of the patients have Hipertension. People with hypertension easily showed because they may need medicine more their health. 

### Plot4: Gender 

#### Select all the people with either one or more than one disease. The graph compares the number of people who have disease either show or not based on gender. 

#### Females are more than males. **It looks like the showing or not doesn't have relationships with Gender.**


```{r}
medical %>% filter(Diabetes==1 |Alcoolism==1| HiperTension==1 | Handicap==1 |Smokes==1) %>% 
            ggplot(aes(Status,fill=Gender))+geom_bar()+
            scale_fill_manual(values = c("#fa9fb5", "#9ecae1"))+
            scale_x_discrete(labels=c("no-show","show"))+
            ylab("number of people")+
            ggtitle("Showing or not based on gender")
```

### Plot5: Week day

#### I calculate the no-show rate with the number of no-show peolpe devided by all of the people in each of the day. The rate are high in Saturday and Monday, and the lowest goes to Sunday. 

```{r}
byweek <- medical%>%
  group_by(DayOfTheWeek) %>%
  count(Status)
byweek <- spread(byweek,key=Status, value=n)
colnames(byweek) <- c("DayOfTheWeek","Noshow", "Show")
byweek <-
  byweek %>% 
  mutate(Noshowrate = round(Noshow / (Noshow + Show), digits = 3))
```

```{r, fig.width=5}
byweek %>%
  ggplot() +
  aes(x = DayOfTheWeek, y = Noshowrate, fill = DayOfTheWeek) +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", 
                              "Thursday", "Friday", "Saturday", "Sunday"),
                   labels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))+
  xlab("Day of Week") +
  ylab("No-Show Rate") +
  ggtitle("No-Show Rate of the Day of Week") +
  theme(legend.position="none")
```

### Plot6: Age

#### It shows that the peak of no-show rate is in the age of 20-30. Young generation tend to not show. The age of 60-80 has the lowest no-show rate. **This could mean that elderly would attend the appointment because they have chronic illness and must see a doctor once they make their appointment.**


```{r}
byage <- medical%>%
  group_by(Age) %>%
  count(Status)
byage <- spread(byage,key=Status, value=n)
colnames(byage) <- c("Age","Noshow", "Show")
byage <- byage %>% 
  mutate(Noshowrate = round(Noshow / (Noshow + Show), digits = 3))
byage <- na.omit(byage)
byage$cate <- c(rep(0,25),rep(1,50),rep(2,24))
```

```{r, fig.width=12}
ggplot(byage,aes(x = Age, y = Noshowrate,fill=cate)) +
  geom_bar(stat="identity",alpha=0.8)+
  geom_smooth(se=FALSE, size=1, col='red')+
  xlab("Age") +
  ylab("No-Show Rate") +
  ggtitle("The trend of No-Show Rate based on Age") +
  theme(legend.position="none")
```

### Plot 7: Age 

#### This plot also demonstrate the number of people showing or not based on their age. 

```{r}
library(shiny)
byage <- as.matrix(byage)
dimnames(byage) <- list(c(1:99), c("Age","Noshow","Show","Noshowrate","cate"))

shinyApp(
   
  ui = fluidPage(
    selectInput("status", "Status:", 
                choices =c("Noshow","Show","Noshowrate")),
    plotOutput("Plot")
  ),
  
  server = function(input, output) {
    output$Plot <- renderPlot({
      barplot(byage[,input$status], 
               ylab = "Number of people / precentage", xlab = "Age")
     })
   },
  
   options = list(height = 500) )
```

### SMS reminder

#### The no-show rate based on the number of SMS are exactly the same. (around 0.3).

```{r}
SMS <- medical%>% group_by(Sms_Reminder) %>% count(Status)
SMS <- spread(SMS,key=Status, value=n)
colnames(SMS) <- c("SMS","Noshow", "Show")

SMS <- SMS %>% 
  mutate(Noshowrate = round(Noshow / (Noshow + Show), digits = 3))
print(SMS)
```

### Plot 8: SMS reminder based on Gender

#### Female who receives more text messages will have higher no-show rate. People would be annoyed about the SMS messages and it also shows that SMS are not a very useful tool to remind people to come. 

```{r}
SMS1<- medical%>% group_by(Sms_Reminder,Gender) %>% count(Status)
SMS1<- spread(SMS1,key=Status,value=n)
colnames(SMS1) <- c("sms","Gender","Noshow", "Show")
SMS1 <- SMS1 %>% 
  mutate(Noshowrate = round(Noshow / (Noshow + Show), digits = 3))
```

```{r, fig.width=12}
s1 <- subset(SMS1,Gender=="F") %>% 
      ggplot(aes(x=sms,y=Noshowrate))+geom_bar(stat="identity",fill="#fa9fb5")+
      ggtitle("Female")
s2 <- subset(SMS1,Gender=="M") %>%    
      ggplot(aes(x=sms,y=Noshowrate))+geom_bar(stat="identity",fill="#9ecae1")+
      ggtitle("Male")+ylab("")
grid.arrange(s1,s2, ncol=2)

```

### Plot 9: Month

#### This plot shows the no-show rate based on month. In the fall season, especially in December, the no-show rate is tend to be high. 

```{r}
byyear <- medical%>%
  group_by(Ryear,Rmonth) %>%
  count(Status)
byyear <- spread(byyear,key=Status, value=n)
colnames(byyear) <- c("Year","Month","Noshow","Show")
byyear <- byyear %>% 
  mutate(Noshowrate = round(Noshow / (Noshow + Show), digits = 3))
byyear <- na.omit(byyear)
```

```{r, fig.width=5}
byyear %>%
  ggplot() +
  aes(x = Month, y = Noshowrate) +
  geom_bar(stat = "identity",fill="grey") +
  xlab("Year") +
  ylab("No-Show Rate") +
  ggtitle("No-Show Rate of Month") +
  theme(legend.position="none")
```

### Plot 10: Waitingtime 

#### This shows the frequency of the waiting time of people. Most of the waiting time is below 10 days.

```{r echo=FALSE}
sliderInput("bins", "Number of bins:", min = 1, max = 100, value = 30)

renderPlot({
  WaitingTime <- medical$AwaitingTime[medical$AwaitingTime < 70] # Old Faithful Geyser data
  bins <- seq(min(WaitingTime), max(WaitingTime), length.out = input$bins + 1)

  # draw the histogram with the specified number of bins
  hist(WaitingTime, breaks = bins, col = 'darkgray', border = 'white')
})
```

### Conclusion
#### 1. Elderly will show up for the appointments and others tend to not show. 
#### 2. People don't show on Saturday.
#### 3. The more SMS reminder that sent to patients couldn't motivate people come to the appointment.
#### 4. Chronic disease such as hypertension or diabetes.    


