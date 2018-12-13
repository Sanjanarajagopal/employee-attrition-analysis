#Load the required libraries
library(caret)
library(dplyr)
library(arules)
library(corrplot)

#################################
#Data Exploration
#################################
#1. Show the departments 
dept_data <- table(final_data$Department)
dept_data

pie(dept_data)

#2. Show the gender
gender_data <- table(final_data$Gender)
gender_data

barplot(gender_data)

#3. Age distribution
boxplot(employee_data$Age)
hist(employee_data$Age)

#4. Correlation between attributes

corrplot(cor(sapply(final_data,as.integer)),method = "color")



########################################3
#Visualizations
####################################################
#1. How does attrition vary across each Departments and environment satisfaction? - Categorised Bar charts
##########################################################
par(mfrow = c(1,1))
dept_attr_data <- tapply(as.numeric(final_data$Attrition), list(final_data$Department,final_data$EnvironmentSatisfaction), FUN = sum)
dept_attr_data

colnames(dept_attr_data) <- c('Department', 'EnvironmentSatisfaction', 'Attrition')


barplot(t(dept_attr_data)
        ,beside = T
        , col = c('green4', 'red4', 'tan4', 'grey')
        
)


###########################################
#BubbleChart
par(mfrow=c(1,1))

attr_data <- aggregate(as.numeric(final_data$Attrition), list(final_data$RelationshipSatisfaction,final_data$JobSatisfaction), FUN = sum)
symbols(as.numeric(attr_data$Group.1), as.numeric(attr_data$Group.2), circles=attr_data$x)
str(attr_data)
radius <- sqrt(attr_data$x/pi)

symbols(attr_data$Group.1, attr_data$Group.2, circles=radius, inches=0.35,
        fg="white", bg="red", xlab="RelationshipSatisfaction Score", ylab="JobSatisfaction Score", xlim =c(0,10), main="Attrition based on Relationship and Job Satisfaction")
text(attr_data$Group.1, attr_data$Group.2, attr_data$x, cex=0.6)


###########################################################################
#Stacked barplot
####################################################################



####################################################################
#Stacked BarPlot for entire data
#################################################


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
  labs(x="Gender", y="Number Attrition")+
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
t2<- sales_repD %>%
  group_by(WorkLifeBalance, Attrition) %>%
  tally() %>%
  ggplot(aes(x = WorkLifeBalance, y = n,fill=Attrition)) +
  geom_bar(stat = "identity") +
  theme_minimal()+
  labs(x="Work-Life Balance", y="Number of Attritions")+
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





#Education Levels
table(final_data$Education)


educ_data<- table(final_data$Education, final_data$Attrition)
colnames(educ_data)

barplot(t(educ_data)
        , beside = T
        )


barplot(t(table(final_data$JobLevel, final_data$Attrition)),
        beside = T)
