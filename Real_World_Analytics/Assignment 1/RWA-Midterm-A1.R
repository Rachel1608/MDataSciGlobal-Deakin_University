
##################################
#Question 1 - Understand the Data
##################################
#i)set the working directory
setwd("G:/My Drive/R program Dataset/Mid term assessment/")
getwd()

#access the library readr for reading the dataset and plotrix for visualization
library(readr)
library(plotrix)

#ii)Assigning the data to a matrix 
the.data <- as.matrix(read.table("ENB_2023.txt",header=FALSE))  
summary(the.data)

# using student ID number for reproducible sampling with the seed function
set.seed(224234147)

#iii)To investigate Y, generating subset of 340 with numerical data
my.data <- the.data[sample(1:671,340),c(1:6)] 

#change column names as per the problem statement for easily defining and analysis
colnames(my.data, do.NULL = FALSE)
colnames(my.data)<-c("X1","X2","X3","X4","X5","Y")
my.data
summary(my.data)

# iv)Using scatter plots and histograms to understand the relationship between each of the 
# variables X1, X2, X3, X4, X5, and variable of interest Y.
#Accessing library of ggplot2 for data visualization, RColorBrewer for different colorcodes, moments for analysing skewness, kurtosis, dplyr for better understanding of data 
library(ggplot2)
library(RColorBrewer)
library(moments)
library(dplyr)

class(my.data)
my.data<- as.data.frame(my.data)
class(my.data)

# Create 5 scatterplots function (for each X variable against the variable of interest Y) 
ggplot(my.data, aes(x=X1, y=Y))+
  geom_point(color = "blue", size = 3)+
  labs(title= "X1 vs Y", x="X1: Temperature in kitchen", y=" Energy use")
png(file="X1 vs Y.jpeg")

ggplot(my.data, aes(x=X2, y=Y))+
  geom_point(color = "purple", size = 3)+
  labs(title= "X2 vs Y", x="X2: Humidity in kitchen", y="Energy use")
png(file="X2 vs Y.jpeg")

ggplot(my.data, aes(x=X3, y=Y))+
  geom_point(color = "green", size = 3)+
  labs(title= "X3 vs Y", x="X3:Temperature from weather station", y="Energy use")
png(file="X3 vs Y.jpeg")

ggplot(my.data, aes(x=X4, y=Y))+
  geom_point(color = "black", size = 3)+
  labs(title= "X4 vs Y", x="X4:Humidity from weather station", y="Energy use")
png(file="X4 vs Y.jpeg")

ggplot(my.data, aes(x=X5, y=Y))+
  geom_point(color = "red", size = 3)+
  labs(title= "X5 vs Y", x="X5: Visibility from weather station", y="Energy use")
png(file="X5 vs Y.jpeg")


# Create 6 histograms for each X variable and Y
create_histogram <- function(x, xlab="", pos="topright", rounding=2){
  mean <- round(mean(x), rounding)
  meanlabel <- paste("Mean   (", mean, ")")
  median <- round(median(x), rounding)
  medianlabel <- paste("Median (", median, ")")
  min <- round(min(x), rounding)
  minlabel <- paste("Min    (", min, ")")
  max <- round(max(x),rounding)
  maxlabel <- paste("Max    (", max, ")")
  
  title = paste("Histogram of ", xlab)
  
  hist(x, xlab=xlab, main=title)
  abline(v = mean, col = 2, lwd=3)
  abline(v = median, col = 7, lwd=3)
  abline(v = min, col = 4, lwd=3)
  abline(v = max, col = 4, lwd=3)
  legend(x = pos,                    # Position 
         cex = 0.55,                 # Size of text
         legend = c(meanlabel, medianlabel, minlabel, maxlabel),  # Legend texts
         lty = c(1),                 # Line types
         col = c(2, 7, 4, 4),        # Line colors
         lwd = 3)                    # Line width
}
create_histogram(my.data$X1, "X1","topright")
png(file="H1X1.jpeg")
create_histogram(my.data$X2, "X2","topright")
png(file="H2X2.jpeg")
create_histogram(my.data$X3, "X3","topright")
png(file="H3X3.jpeg")
create_histogram(my.data$X4, "X4","topright")
png(file="H4X4.jpeg")
create_histogram(my.data$X5, "X5","topright")
png(file="H5X5.jpeg")
create_histogram(my.data$Y, "Y","topright")
png(file="H6Y.jpeg")

#creating dataframe to store the skewness and its interpretation
variables <- rbind("X1", "X2", "X3", "X4","X5","Y")

# Store the skewness in order of variable names defined above
skewness_vals  <- rbind(round(skewness(my.data$"X1"),2), round(skewness(my.data$"X2"),2), round(skewness(my.data$"X3"),2), round(skewness(my.data$"X4"),2), round(skewness(my.data$"X5"),2), round(skewness(my.data$"Y"),2))

# Interpret the skewness per variable according to the values stored above
inferences <- c()
for(skew_val in skewness_vals){
  if(between(skew_val, -0.5, 0.5)){
    i <-  "Fairly Symmetrical"
  }
  else if(between(skew_val, -1, -0.5) ){
    i <- "Moderately left skewed"
  } 
  else if (between(skew_val, 0.5, 1)){
    i <- "Moderately right skewed"
  }
  else if((skew_val < -1 )| (skew_val > 1)){
    i <-  "Highly Skewed"
  }
  inferences <- append(inferences, i)
}  
# Combine the variable names, skewness values, and their corresponding inferences in one dataframe.
skew_df <- data.frame(variables, skewness_vals, inferences)

skew_df

#Finding correlation between the variables
corr<- round(cor(my.data),2)
corr1<- as.data.frame(corr)

################################
#Question 2 - Transform the Data
################################

# Choose any four X variables (from X1, X2, X3, X4, X5) and Y
I <- c("X1","X2","X4","X5","Y") #chosen X1,X2,X4,X5
variables_to_transform <- my.data[,I]  # obtain a 340 by 5 matrix

# Treating outliers and imputing or remove them with reasonable judgement
treat_outliers_matrix <- function(mat) {
  treated_mat <- mat
  
  for (i in 1:ncol(mat)) {
    column <- mat[, i]
    q <- quantile(column, c(0.25, 0.75))
    iqr <- q[2] - q[1]
    lower_bound <- q[1] - 1.5 * iqr
    upper_bound <- q[2] + 1.5 * iqr
    
    # Replace outliers with values at the bounds
    treated_mat[column < lower_bound, i] <- lower_bound
    treated_mat[column > upper_bound, i] <- upper_bound
  }
  
  return(treated_mat)
}

# Apply outlier treatment to my.data
my.data1 <- treat_outliers_matrix(variables_to_transform)

# Print the treated matrix
print(my.data1)
summary(variables_to_transform)
summary(my.data1)

#Visualizing the outliers
boxplot(variables_to_transform$X1, main = paste("Boxplot for", "X1"))
png(file="outliers X1.jpeg")

boxplot(variables_to_transform$X2, main = paste("Boxplot for", "X2"))
png(file="outliers X2.jpeg")

boxplot(variables_to_transform$X4, main = paste("Boxplot for", "X4"))
png(file="outliers X4.jpeg")

boxplot(variables_to_transform$X5, main = paste("Boxplot for", "X5"))
png(file="outliers X5.jpeg")

boxplot(variables_to_transform$Y, main = paste("Boxplot for", "Y"))
png(file="outliers y.jpeg")

# A Minmax scaling of each variable
## minmax normalisation
minmax <- function(x){
  (x - min(x))/(max(x)-min(x))
}


# z-score standardisation and scaling to unit interval for transformation
unit.z <- function(x){
  0.15*((x-mean(x))/sd(x)) + 0.5
}

data.transformed<-minmax(my.data1)
data.transformed
summary(data.transformed)

data.transformed[,1]<-unit.z(data.transformed[,1])
data.transformed[,2]<-unit.z(data.transformed[,2])
data.transformed[,3]<-unit.z(data.transformed[,3])
data.transformed[,4]<-unit.z(data.transformed[,4])
head(data.transformed)

create_histogram(data.transformed[,1], "X1","topright")
png(file="z1.jpeg")
create_histogram(data.transformed[,2], "X2","topright")
png(file="z2.jpeg")
create_histogram(data.transformed[,3], "X4","topright")
png(file="z4.jpeg")
create_histogram(data.transformed[,4], "X5","topright")
png(file="z5.jpeg")


summary(data.frame(data.transformed))



# Save this transformed data to a text file
write.table(data.transformed, "Rachel-transformed.txt") 


##########################################
#Question 3 - Build models and investigate
##########################################

#using lpsolve library to use "AggWaFit718.R" for performing model
library(lpSolve)
source("AggWaFit718.R")

data.transformed_copy <- as.matrix(read.table("Rachel-transformed.txt"))  # import your saved data

# Get weights for Weighted Arithmetic Mean with fit.QAM() 

fit.QAM(data.transformed_copy, output.1="QAM-output.txt",stats.1="QAM-stats.txt", g=AM, g.inv=invAM)
# Get weights for Power Mean p=0.5 and p=2 with fit.QAM()

fit.QAM(data.transformed_copy,output.1="PM05output.txt",stats.1="PM05stats.txt", g=PM05,g.inv = invPM05)

fit.QAM(data.transformed_copy,output.1="QMoutput.txt",stats.1="QMstats.txt", g=QM,g.inv = invQM)

# Get weights for Ordered Weighted Average with fit.OWA()

fit.OWA(data.transformed_copy,output.1="OWAoutput.txt",stats.1="OWAstats.txt")

# Get weights for Choquet Integral with fit.choquet() - Optional
fit.choquet(data.transformed_copy,output.1="choquetoutput.txt",stats.1="choquetstats.txt",kadd=(ncol(data.transformed_copy)-1))




#######################################
#Question 4 - Use Model for Prediction
#######################################

# new_input has 
X1=22 
X2=38 
X3=4 
X4=88.2 
X5=34

new_input_to_transform <- matrix(c(X1,X2,X4,X5),nrow=1)
new_input_to_transform
# transforming the four variables in the same way as in question 2 


new_input_to_transform[,1]<-(new_input_to_transform[,1]-min(my.data1))/(max(my.data1)-min(my.data1))
new_input_to_transform[,2]<-(new_input_to_transform[,2]-min(my.data1))/(max(my.data1)-min(my.data1))
new_input_to_transform[,3]<-(new_input_to_transform[,3]-min(my.data1))/(max(my.data1)-min(my.data1))
new_input_to_transform[,4]<-(new_input_to_transform[,4]-min(my.data1))/(max(my.data1)-min(my.data1))
new_input_to_transform

data.transform.1<-minmax(my.data1)
new_input_to_transform[,1]<-0.15*((new_input_to_transform[,1]-mean(data.transform.1[,1]))/sd(data.transform.1[,1]))+0.5
new_input_to_transform[,2]<-0.15*((new_input_to_transform[,2]-mean(data.transform.1[,2]))/sd(data.transform.1[,2]))+0.5
new_input_to_transform[,3]<-0.15*((new_input_to_transform[,3]-mean(data.transform.1[,3]))/sd(data.transform.1[,3]))+0.5
new_input_to_transform[,4]<-0.15*((new_input_to_transform[,4]-mean(data.transform.1[,4]))/sd(data.transform.1[,4]))+0.5


new_input_to_transform
# applying the transformed variables to the best model selected from Q3 for Y prediction
fm.weights<-c(0,0,0,0,0,0,0,0,0.0759932672635167,0,0.494757758808006,0,0.0759932672635167,0,0.999999999999968)
model_out<-choquet(new_input_to_transform,fm.weights)

# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer
min(my.data)
max(my.data)

reverse_transform<-function(x,min,max) x*(max-min)+min
# Compare your prediction with the measured value of Y, Y=100.
reverse_transform(model_out,0.01150171,200)

# Outof all models Choquet Integral performed better when compared with measured value Y, Y=100 and has low RMSE error and spearman correlation is high when compared with OWA model which has second low RMSE error.
# CHOQUET INTEGRAL PREDICTION: 95.417 


#############################################################################################
# References 
#############################################################################################
# 1) Luis M. Candanedo, Veronique Feldheim, Dominique Deramaix. Data driven prediction models of energy use of appliances in a low-energy house, Energy and Buildings, Volume 140, 1 April 2017, Pages 81-97, ISSN 0378-7788.
# http://archive.ics.uci.edu/ml/datasets/Appliances+energy+prediction

# 2) Wickham H, Hester J, Bryan J (2023). readr: Read Rectangular Text Data. https://readr.tidyverse.org, https://github.com/tidyverse/readr.

# 3) J, L. (2006). Plotrix: a package in the red light district of R. In R-News (Vol. 6, Issue 4, pp. 8–12).

# 4) Wickham, H. (2016). Ggplot2: Elegant graphics for data analysis (2nd ed.) [PDF]. Springer International Publishing.

# 5) Erich Neuwirth [aut, cre],Cynthia Brewer.ColorBrewer Palettes.http://colorbrewer2.org. Repository CRAN. License Apache License 2.0. Date Version 1.1-3,Date 2022-04-03

# 6) Lukasz Komsta, Frederick Novomestky. Moments. GPL-2 | GPL-3 [expanded from: GPL (≥ 2)]. Version:	0.14.1. Published:	2022-05-02

# 7) Wickham H, François R, Henry L, Müller K, Vaughan D (2023). dplyr: A Grammar of Data Manipulation. R package version 1.1.4, https://github.com/tidyverse/dplyr, https://dplyr.tidyverse.org.

# 8) Michel Berkelaar and others, Gábor Csárdi. lpSolve: Interface to 'Lp_solve' v. 5.5 to Solve Linear/Integer Programs. 	https://github.com/gaborcsardi/lpSolve. License:	LGPL-2. Version:	5.6.19. Published:	2023-09-13

# 9) Zack Gancarz. source(“AggWaFit718.R”). Academic Journal Ranking Data Analysis. 2017