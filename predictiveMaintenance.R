## ----setup, include=FALSE----------------------------------------------------------

knitr::opts_chunk$set(echo = TRUE)

## ----getlibs,echo=FALSE,message=FALSE,warning=FALSE--------------------------------
library(readr)
library(knitr)
library(tidyverse)
library(caret)
library(GGally)


## ----intro_code--------------------------------------------------------------------
# Read in the CSV file
FN <- "C:/Users/kingt/Documents/CapstonePredictiveMaintenance/data/predictive_maintenance.csv"

x<-spec_csv(FN,col_types = cols())

read_csv(FN)%>%apply(., 2, function(x) {length(unique(x))})%>%kable(col.names = c('Length'))


## ----EDA_code----------------------------------------------------------------------

# Import the data with defined field types
maintlog<-read_csv(FN,col_types = 'icfddiddlf')

# Display the 1st 10 rows
maintlog%>%head()%>%kable()


## ----fg1---------------------------------------------------------------------------
# Remove the squre brackets from the vars
names(maintlog)<-names(maintlog)%>%sub("\\s\\[.{1}.*\\]$", "", .)

# Replace the spaces with '_' in the column names (Easier to work with)
names(maintlog)<-names(maintlog)%>%gsub("\\s", "_", .)

value_features<-names(maintlog)[4:8]

# Create a pair plot of 
vf<-maintlog[,4:10]
# vf%>%pairs(col = 'blue', #modify color
#      main = 'Features vs. Machine Failure')


## ----fg2---------------------------------------------------------------------------
vf%>%qplot(Air_temperature, geom ="histogram", data = .,
colour = I("#2EA7CE"), fill = I("#8E87CE"),
main = "Air Temperature", bins=50)

summary(vf$Air_temperature)



## ----fg3---------------------------------------------------------------------------
vf%>%qplot(Process_temperature, geom ="histogram", data = .,
colour = I("#2EA7CE"), fill = I("#8E87CE"),
main = "Process Temperature", bins=50)

vf$Process_temperature%>%summary()



## ----fg4---------------------------------------------------------------------------
vf%>%qplot(Rotational_speed, geom ="histogram", data = .,
colour = I("#2EA7CE"), fill = I("#8E87CE"),
main = "Rotational Speed", bins=50)

vf$Rotational_speed%>%summary()



## ----fg5---------------------------------------------------------------------------
vf%>%qplot(Torque, geom ="histogram", data = .,
colour = I("#2EA7CE"), fill = I("#8E87CE"),
main = "Torque", bins=50)

vf$Torque%>%summary()


## ----fg6---------------------------------------------------------------------------
vf%>%qplot(Tool_wear, geom ="histogram", data = .,
colour = I("#2EA7CE"), fill = I("#8E87CE"),
main = "Tool wear", bins=50)

vf$Tool_wear%>%summary()



## ----fig7--------------------------------------------------------------------------

maintlog$Type <- factor(maintlog$Type, levels=c('L', 'M', 'H'))

maintlog$Type%>%table()%>%kable()

maintlog%>%ggplot(aes(Type, fill = Failure_Type)) + 
  geom_histogram(stat="count") + ggtitle("Distribution of Machine Types")




## ----fg8---------------------------------------------------------------------------
# Air Temp vs. Proccess Temp
p1 <- ggplot(vf)+geom_point(aes(x = Air_temperature, y = Process_temperature,colour = Failure_Type), na.rm = TRUE)

# Air Temp vs. Rotation Speed
p2 <- ggplot(vf)+geom_point(aes(x = Air_temperature, y = Rotational_speed,colour = Failure_Type), na.rm = TRUE)

# Air Temp vs. Torque
p3 <- ggplot(vf)+geom_point(aes(x = Air_temperature, y = Torque,colour = Failure_Type), na.rm = TRUE)

# Air Temp vs. Tool wear
p4 <- ggplot(vf)+geom_point(aes(x = Air_temperature, y = Tool_wear,colour = Failure_Type), na.rm = TRUE)

# Process temp vs. Rotation Speed
p5 <- ggplot(vf)+geom_point(aes(x = Process_temperature, y = Rotational_speed,colour = Failure_Type), na.rm = TRUE)

# Process temp vs. Torque
p6 <- ggplot(vf)+geom_point(aes(x = Process_temperature, y = Torque,colour = Failure_Type), na.rm = TRUE)

# Process temp vs. Tool wear
p7 <- ggplot(vf)+geom_point(aes(x = Process_temperature, y = Tool_wear,colour = Failure_Type), na.rm = TRUE)

# Rotation Speed vs. Torque
p8 <- ggplot(vf)+geom_point(aes(x = Rotational_speed, y = Torque,colour = Failure_Type), na.rm = TRUE)

# Rotation Speed vs. Tool wear
p9 <- ggplot(vf)+geom_point(aes(x = Rotational_speed, y = Tool_wear,colour = Failure_Type), na.rm = TRUE)

# Torque vs. Tool wear
p10 <- ggplot(vf)+geom_point(aes(x = Torque, y = Tool_wear,colour = Failure_Type), na.rm = TRUE)


# observe how these features interact under different failure conditions. 
gridExtra::grid.arrange(p1,p2,p3,p4,p5)
gridExtra::grid.arrange(p6,p7,p8,p9,p10)




## ----------------------------------------------------------------------------------
# Need to check the R version since the syntax is different for R version>3.6
# Set the random seed value
if(base::getRversion()>'3.6'){ set.seed(1, sample.kind="Rounding") }else{set.seed(1) }

# Partition the data by creating an index where 10% is for the hold out data and
# 90% for the remain
test_index <- createDataPartition(y = maintlog$UDI, times = 1, p = 0.1, list = FALSE)
train_set <- maintlog[-test_index,]
test_set <- maintlog[test_index,]

plt<-table(train_set$Type)/length(train_set$Type)*100
barplot(plt, col = c(I("#a7ce2e"),I("#8EA7CE"),I("#CE0000")),main = "Percentages by Type")

plt<-table(train_set$Target)/length(train_set$Target)*100
barplot(plt, col = c(I("#a7ce2e"),I("#CE0000")),main = "Percentages by Type")

train_qvar <- train_set[, - 2]
Target<- as.factor(train_set$Target)
ggpairs(train_qvar, aes(colour=Target))




## ----------------------------------------------------------------------------------

train_set[,-1]%>% 
  ggplot(aes(Rotational_speed, Torque, fill = Target, color=Failure_Type)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm", lwd = 1.5)
# Logistic regression

# train_qda <- train(Failure_Type ~ ., method = "qda", data = train_set[,-1])



