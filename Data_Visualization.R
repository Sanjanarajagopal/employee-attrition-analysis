#Load the required libraries
library(caret)
library(dplyr)
library(arules)

########################################3
#Visualizations
####################################################
#1. How does attrition vary across each Departments and environment satisfaction? - Categorised Bar charts
##########################################################
employee_data <- normalized_empl_data
dept_attr_data <- tapply(as.numeric(employee_data$Attrition), list(employee_data$Department,employee_data$EnvironmentSatisfaction), FUN = sum)
dept_attr_data

colnames(dept_attr_data) <- c('Department', 'EnvironmentSatisfaction', 'Attrition')

barplot(t(dept_attr_data)
        ,beside = T
        , col = c('green4', 'red4', 'tan4', 'grey')
        
        )
plot(employee_data$HourlyRate, employee_data$NumCompaniesWorked)

################################################################
#BubbleChart
###########################################
#BubbleChart
par(mar=c(1,1,1,1))
View(final_data)
attr_data <- aggregate(as.numeric(final_data$Attrition), list(final_data$RelationshipSatisfaction,final_data$JobSatisfaction), FUN = sum)
View(attr_data)
symbols(as.numeric(attr_data$Group.1), as.numeric(attr_data$Group.2), circles=attr_data$x)
str(attr_data)
radius <- sqrt(attr_data$x/pi)

symbols(attr_data$Group.1, attr_data$Group.2, circles=radius, inches=0.35,
        fg="white", bg="red", xlab="RelationshipSatisfaction Score", ylab="JobSatisfaction Score", xlim =c(0,10), main="Attrition based on Relationship and Job Satisfaction")
text(attr_data$Group.1, attr_data$Group.2, attr_data$x, cex=0.6)


###########################################################################
#Stacked barplot
####################################################################



##################################################################
#Correlation Matrix
#######################################################
install.packages("corrplot")
library(corrplot)
corrplot(cor(sapply(final_data,as.integer)),method = "color")


####################################################################
#Stacked BarPlot for entire data
#################################################

install.packages("ggplot2")
library("ggplot2")
library("dplyr")

#Travel
a2<- final_data %>%
  group_by(BusinessTravel, Attrition) %>%
  tally() %>%
  ggplot(aes(x = BusinessTravel, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Business Travel", y="Number Attriation")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

View(final_data)
a2

#Gender
a3<- final_data %>%
  group_by(Gender, Attrition) %>%
  tally() %>%
  ggplot(aes(x = Gender, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Gender", y="Number Attrition")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

a3

#Department
a4<- final_data %>%
  group_by(Department, Attrition) %>%
  tally() %>%
  ggplot(aes(x = Department, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Gender", y="Number Attrition")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))
a4

##########################################
# stacked barchart for Sales Dataset
###########################
#SAlesDataset
sales_D<-final_data[final_data$Department=="Sales",]
colnames(sales_D)


#Gender
s_a3<- sales_D %>%
  group_by(Gender, Attrition) %>%
  tally() %>%
  ggplot(aes(x = Gender, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Gender", y="Number Attrition")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

s_a3

#Travel
t_a2<- sales_D %>%
  group_by(BusinessTravel, Attrition) %>%
  tally() %>%
  ggplot(aes(x = BusinessTravel, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Business Travel", y="Number Attriation")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

t_a2

#Education
E_a2<- sales_D %>%
  group_by(Education, Attrition) %>%
  tally() %>%
  ggplot(aes(x = Education, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Business Travel", y="Number Attriation")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

E_a2

#Marital Status
M_a2<- sales_D %>%
  group_by(MaritalStatus, Attrition) %>%
  tally() %>%
  ggplot(aes(x = MaritalStatus, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Business Travel", y="Number Attriation")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

#sales Job Role
M_a2<- sales_D %>%
  group_by(JobRole, Attrition) %>%
  tally() %>%
  ggplot(aes(x = JobRole, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Business Travel", y="Number Attriation")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))


M_a2

##############################################
#Sales Representative Dataset
#######################################################################
sales_repD<-sales_D[sales_D$JobRole=="Sales Representative",]


#######################################################################
#Sales Representative Attrition Characteristics
###########################################################
sales_repD_A<-sales_repD[sales_repD$Attrition=="Yes",]

#travel
t1<- sales_repD_A %>%
  group_by(BusinessTravel, Attrition) %>%
  tally() %>%
  ggplot(aes(x = BusinessTravel, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Business Travel", y="Number Attriation")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

t1

#WorkLifeBalance
t2<- sales_repD_A %>%
  group_by(WorkLifeBalance, Attrition) %>%
  tally() %>%
  ggplot(aes(x = WorkLifeBalance, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Business Travel", y="Number Attriation")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

t2


#######################################################################
#Sales Representative Non Attrition Characteristics
###############################################################
sales_repD_NA<-sales_repD[sales_repD$Attrition=="No",]

#travel
t2<- sales_repD_NA %>%
  group_by(BusinessTravel, Attrition) %>%
  tally() %>%
  ggplot(aes(x = BusinessTravel, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Business Travel", y="Number Attriation")+
  ggtitle("Attrition according to the Business Travel")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

t2

#Work Life Balance
t2<- sales_repD_NA %>%
  group_by(WorkLifeBalance, Attrition) %>%
  tally() %>%
  ggplot(aes(x = WorkLifeBalance, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Business Travel", y="Number Attriation")+
  ggtitle("Attrition according to the Work Life Balance")+
  geom_text(aes(label = n), vjust = -0.5, position = position_dodge(0.9))

t2





