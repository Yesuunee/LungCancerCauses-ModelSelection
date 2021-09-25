#Sta 138 Final Project 
data=read.csv("/Users/Sia/Downloads/Byssinosis.csv")

library(bestglm)
data(SAheart)
mydata=SAheart
fullModel = glm(chd ~.,data=mydata,family = binomial())
nullModel = glm(chd ~ 1,data=mydata,family = binomial())

bestForwardAIC=step(nullModel,scope = list(lower = nullModel, upper = fullModel),direction = "forward")

bestBackwardAIC=step(fullModel,scope = list(lower = nullModel, upper = fullModel),direction = "backward")

bestBidirectionAIC=step(fullModel,scope = list(lower = nullModel, upper = fullModel),direction = "both")


bestSubsetAIC = bestglm(Xy = mydata, family = binomial(),IC = "AIC",method = "exhaustive") #last column of mydata should have response y 

bestSubsetBIC = bestglm(Xy = mydata, family = binomial(),IC = "BIC",method = "exhaustive")


#Poisson regression example
library(ISwR)
data(eba1977)
names(eba1977)
#Lung cancer incidence in four Danish cities during 1968‐1971 

#This data set contains counts of incident lung cancer cases and population size in four neighbouring Danish cities by age group
#‘cases’ the number of lung cancer cases
#'pop' number of inhabitants

#These data were “at the center of public interest in Denmark in 1974”, according to Erling Andersen's paper. 
#The city of Fredericia has a substantial petrochemical industry in the harbour area.

#model the rate \lambda per unit population
#poisson(\lambda*population_size)
eba.glm <- glm(cases ~ city+age+offset(log(pop)),family=poisson,data=eba1977) 
summary(eba.glm)


