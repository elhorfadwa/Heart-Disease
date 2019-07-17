###############################################################
#                                                             #
#           Heart Disease Prediction                          #
#                                                             #
###############################################################

# -------INTRODUCTION:
#
# Heart disease is currently the leading cause of death across the globe.
# It is anticipated that the development of computation methods that can 
#predict the presence of heart disease will significantly reduce heart
#disease caused mortalities while early detection could lead to substantial
#reduction in health care costs

#--------Data Set:
#
#Datasets : 1 cleveland.data
#           2 hungarian.data
#           3 switzerland.data
#           4 long-beach-va.data,
#
#####################################################################################################"


#####Package used
library(dplyr)
library(ggplot2) 

getwd()

#####Loading the data


data_clev1<- read.csv('processed_cleveland_data.csv', stringsAsFactors = FALSE, header = FALSE)
data_hung1<- read.csv('processed_hungarian_data.csv',stringsAsFactors = FALSE, header = FALSE)
data_swiss1<- read.csv('processed_switzerland_data.csv', stringsAsFactors = FALSE, header = FALSE)
data_va1<- read.csv('processed_va_data.csv',stringsAsFactors = FALSE, header = FALSE)

# data combined by Row

data_heart<- rbind(data_clev1,data_hung1,data_swiss1,data_va1)

# gave name to each column


names(data_heart) <- c('Age', 'Gender', 'CP', 'Trestbps', 'Chol', 'FBS', 'RestECG',
                 'Thalach', 'Exang', 'Oldpeak', 'Slope', 'CA', 'Thal', 'Goal')
head(data_heart)

summary(data_heart) # get statistic view  for our variables
str(data_heart)

##### Visualise the data

#-----Two way to get table for all dataframe
View(data_tab <- sapply(data_heart, function(x) table(x))) 
data_tab <- sapply(data_heart, table) # use table without (),  !!!!!!§Error data_tab Error in table() : rien à tabuler data_tab<- sapply(data_heart,  table())

#------- Table only for character variable
sapply(data_heart[sapply(data_heart, is.character)], table)

##### Attach our data_heart

attach(data_heart)


############################# Data Cleaning#################################

#### --- NA values

na_count <- sapply(data_heart, function(x) sum(is.na(x)))  # Check if we have NA values

####---  Modify Gender Values : 1 to M and 0 to F

View(table(data_heart$Gender))

data_heart$Gender <- ifelse(data_heart$Gender == 1, "M","F")

#----Gender <- ifelse(Gender == 1, "M","F") <-- modification not applied to the table why??

##### CHOL Variable

# ---- The missing value are replaced by '?' in the original dataset
# the column Chol is one of most important data to analyse, that's why we will deleate the missing value

data_heart <- data_heart[data_heart$Chol!= '?',]
table(data_heart$Chol)


View(data_heart[which(data_heart$Chol== '?'),])
View(data_heart <- data_heart[data_heart$Chol!= '?',])

# Chol variable is Character, we will put it as integer

data_heart$Chol = as.integer(na.omit(data_heart$Chol))


#-- group variable Goal into 2 categories:‘no heart disease’ = 0 and ‘displaying heart disease’ = 1

data_heart$Goal[data_heart$Goal == 2] <- 1
data_heart$Goal[data_heart$Goal == 3] <- 1
data_heart$Goal[data_heart$Goal == 4] <- 1

View(table(data_heart$Goal))

#####--- Add new variable to our dataframe : Disease
data_heart$Disease <- factor(data_heart$Goal, labels = c("No Disease", "Heart Disease"))

View(table(data_heart$Disease))

####################################################################################

### age distribution

ggplot(data_heart,aes(x=Age))+xlab("Age")+ylab("number of people")+geom_histogram(bins =50,fill ="blue")

ggplot(data_heart,aes(x=Age))+xlab("Age")+ylab("density")+ ggtitle("Age density plot")+
geom_histogram(aes(y= ..density..),colour="black",bins = 30, fill ="blue")+ 
  geom_density(alpha=.2,fill="#FF6666", color="red")


### Gender distribution

ggplot(data_heart,aes(x=Gender, fill = Gender))+xlab("Gender")+ylab("Count")+geom_bar()+
stat_count(aes(label = ..count..), geom = "text") # stat_count allowed to write value in top of each bar


ratioF = 100*(count(data_heart[data_heart$Gender== 'F',])/nrow(data_heart))

print(paste(" The Female pourcentage from Gender is: ", as.integer(ratioF), "%"))


#Age variation VS  GEnder

ggplot(data_heart,aes(x=Age,fill=Gender))+xlab("Age")+ylab("count")+
geom_bar()+facet_grid(cols=vars(Gender))+ ggtitle("Variation of Age VS Gender")




#### Cholesterol variation 


ggplot(data_heart,aes(x=Chol))+xlab("Cholesterol")+ylab("Count")+
geom_histogram(bins =50, fill ="deepskyblue4")+ ggtitle("Variation of cholestrol")

data_heart2 <- data_heart[data_heart$Chol!= 0,] # Remove "0" value from Chol variable

ggplot(data_heart2,aes(x=Chol))+xlab("Cholesterol")+ylab("Count")+
  geom_histogram(bins =50, fill ="deepskyblue3") +ggtitle("Variation of cholestrol")+
  geom_vline(xintercept = 240, linetype="dashed",color = "Red", size=1.5)
      


data_heart$CP<- as.character(data_heart$CP)

#### Chest pain variation 

ggplot(data_heart,aes(x=CP, fill = CP))+xlab("Chest Pain Level")+ylab("Count")+geom_bar()+
  stat_count(aes(label = ..count..), geom = "text")+ ggtitle("Distributiion of Chest Pain")
             

