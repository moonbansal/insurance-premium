df = read.csv("insurance.csv", header = TRUE)
num_cols <- unlist(lapply(df, is.numeric)) #found all numeric columns
plot(df[,num_cols]) #plot to see how age, bmi etc affect the charges

correlations <- round(cor(df[,num_cols]), 2) #viewing correlations between 4 factors

smoker = as.factor(df$smoker)
sex = as.factor(df$sex)
region = as.factor(df$region)

#make box plots to observe relation bw charges and smoking, age, region
boxplot(df$charges ~ smoker, main='smoker')
boxplot(df$charges ~ sex, main='sex')
boxplot(df$charges ~ region, main='region')

#make a linear model of the data
model1 = lm(charges ~ . , data = df)

summary(model1)
