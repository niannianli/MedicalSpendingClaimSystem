#query dataset
View(Group2DatasetFinal)
#descriptive statistics: mean, max, min, median, standard deviation
library(psych)
describe(Group2DatasetFinal)
#descriptive statistics: mode
smode <-function(x){
  xtab<-table(x)
  modes<-xtab[max(xtab)==xtab]
  mag<-as.numeric(modes[1])
  themodes<-names(modes)
  mout<-list(themodes=themodes,modeval=mag)
  return(mout)
}
x = as.data.frame(Group2DatasetFinal$Avg_Spending_Per_Episode_Hospital)
smode(x)
#create frequency table
xtabs(~ Hospital_Name + Avg_Spending_Per_Episode_Hospital, data=Group2DatasetFinal)
#chi square test
partial.data = table(Group2DatasetFinal$Hospital_Name, Group2DatasetFinal$Claim_Type)
print(chisq.test(partial.data))
#random distribution
partial.data <- data.frame(Group2DatasetFinal$Avg_Spending_Per_Episode_Hospital)
describe(partial.data)
rnorm(29999, mean = 1701.07, sd = 4298.72)
#pearson correlation
x = as.data.frame(Group2DatasetFinal$Avg_Spending_Per_Episode_Hospital)
y = as.data.frame(Group2DatasetFinal$Percent_of_Spending_Hospital)
cor(x, y,  method = "pearson", use = "complete.obs")
#correlation test
res <- cor.test(Group2DatasetFinal$Avg_Spending_Per_Episode_Hospital, Group2DatasetFinal$Percent_of_Spending_Hospital, method = "pearson")
res
#t-test
x = as.data.frame(Group2DatasetFinal$Avg_Spending_Per_Episode_Hospital)
t.test(x, mu = 0, alternative = "two.sided")
