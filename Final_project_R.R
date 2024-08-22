library(tidyverse)
library(corrplot)
library(dplyr)

customers <- read.csv("Customers.csv")
view(customers)
str(customers)
summary(customers)

#----------------------------Scatter Plot-----------------------------------------
ggplot(Customers,
       aes(FamilySize, 
           SpendingScore, 
           color = Gender)) + 
  geom_point(fill ="turquoise2", size = 2)
ggsave('P1.png')

ggplot(Customers,
       aes(Age, WorkExperience,
           color = Profession)) + 
  geom_point(fill= "palegreen1")
ggsave('p2.png')

ggplot(Customers,
       aes(SpendingScore, AnnualIncome,
           color = Profession)) + 
  geom_point(fill= "skyblue1")
ggsave('p3.png')

#--------------------------Box Plot-------------------------------------------
ggplot(Customers,
       aes(AnnualIncome, 
           Profession)) + 
  geom_boxplot(fill= "thistle2")
ggsave('p4.png')

ggplot(Customers,
       aes(SpendingScore, 
           Profession, 
           color = Profession)) + 
  geom_boxplot(fill ="turquoise2")
ggsave('p5.png')

#---------------------Bar Chart------------------------------

ggplot(Customers,
       aes(SpendingScore, 
           color = Gender)) + 
  geom_bar(fill ="plum")
ggsave('p6.png')

#------------------Histogram----------------------------------
ggplot(Customers,
       aes(SpendingScore, 
           color = Profession)) + 
  geom_histogram(fill="snow")
ggsave('p7.png')

# ------------------- Co-relation ------------------------------------
customers <- read.csv("Customers.csv")
cor.test(customers$Age, customers$FamilySize, method = "spearman")
cor(select(customers, Age, FamilySize))
cus<- pairs(select(customers, Age, FamilySize,SpendingScore))
summary(cus)

customers <- read.csv("Customers.csv")
#customers <- filter(customers, customers$Profession_numeric == 8)
cor.test(customers$WorkExperience, customers$FamilySize, method = "spearman")
cor(select(customers, WorkExperience, FamilySize))
pairs(select(customers, WorkExperience, FamilySize,SpendingScore))

# ----------------- Linear regression --------------------------------
customers <- read.csv("Customers.csv")
linear_customer <- lm(customers$FamilySize ~ customers$AnnualIncome, data = customers)
summary(linear_customer)
ggplot(customers, aes(FamilySize, AnnualIncome)) +
  geom_point() +
  geom_smooth(method=lm)
ggsave("Linear_regression1.png")

#customers <- filter(customers, customers$Profession_numeric == 8 & customers$Gender_numeric == 1)
linear_customer <- lm(customers$AnnualIncome ~ customers$WorkExperience, data = customers)
summary(linear_customer)
ggplot(customers, aes(AnnualIncome, WorkExperience)) +
  geom_point() +
  geom_smooth(method=lm)
ggsave("Linear_regression2.png")

#--------------------- T-test--------------------------------------------
customers <- read.csv("customers.csv")
males <- subset(customers, Gender == "Male")
females <- subset(customers, Gender == "Female")
t.test(males$SpendingScore, females$SpendingScore, var.equal = FALSE)


Age_A <- subset(customers, customers$Age > 50)
Age_B <- subset(customers, customers$Age <= 50)
t.test(Age_A$SpendingScore, Age_B$SpendingScore, var.equal = FALSE)




