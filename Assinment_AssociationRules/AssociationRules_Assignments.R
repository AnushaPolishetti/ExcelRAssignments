# Import required libraries
library(arules)
library(arulesViz)

#import the datasets
movies <- read.csv('C:/Users/Anusha/Desktop/ExcelRProjects/dataset/my_movies.csv')
View(movies)

#So here we have the Data represented in Transaction as well as Binary Variables
#we will consider the Binary Variables to prepare the Rules

#Support basically tells us how significant in the entire scheme of things
#the support value of  0.05 %means- of all the transactions have the consequent and anticedent
#confidence value of 0.8 means out of all antecedents there need to be atleast 80% of the transactions which would also have the consequent
rule1 = apriori(as.matrix(movies[,6:15]),parameter = list(support=0.05,confidence=0.8))
rule1
inspect(head(rule1,5))

# ordering the rules based on confidence value from high to low
rule1_conf=sort(rule1,by='confidence',decreasing = TRUE)
rule1_conf
inspect(head(rule1_conf,5))

# As the confidence value is same for all the top transactions 
#lets order them by lift value

rule1_lift= sort(rule1,by='lift',decreasing = TRUE)
inspect(head(rule1_lift,5))

# finding top 20 rules using rule1 transactions
toprules <- head(rule1, by = "confidence", n=20)

inspect(head(toprules))

#Visualization using scatter plot and grouped Matrix plot 
plot(rule1)
plot(rule1,method = 'grouped')

plot(toprules, method = "graph", engine = "htmlwidget")
#In this plot we can see that the movie LOTR movies was watched the most along with Green mile and Gladiator

#Create other rules with different minlen parameter value
rule2<- apriori(as.matrix(movies[,6:15]),parameter= list(support = 0.05,confidence=0.8,minlen=3))
inspect(head(rule2,5))
rule2

# organizing the rule2 based on lift value
rule2_lift = sort(rule2,by='lift',decreasing = TRUE)
inspect(head(rule2_lift,5))

rule2<- apriori(as.matrix(movies[,6:15],parameter= list(support = 0.8,confidence=1)))
rule2
rule2_lift = sort(rule2,by='lift',decreasing = TRUE)
inspect(head(rule2_lift,5))

plot(rule2_lift)
plot(rule2_lift,method ="grouped")
# we can observe from the above rule2 that top rule and strong one is 
#who ever watches gladiator and green.mile moves watches LOTR with a lift ratio of 10,
#10% of times with 100% confidence



# as we can see no much differece in the rules,lets create rules with minlength = 2

rule3<- apriori(as.matrix(movies[,6:15]),parameter= list(support = 0.05,confidence=0.8,minlen=3))
inspect(head(rule3,5))
rule3
plot(rule3, method = "two-key plot")

rule3_lift = sort(rule3,by='lift',decreasing = TRUE)
inspect(head(rule3_lift,5))
plot(rule3_lift,method = "grouped")


### Books Data Set


Books<- read.csv('C:/Users/Anusha/Desktop/ExcelRProjects/dataset/book.csv')
#Here we have the Data represented in Binary Variables so we need not perform 
# further transformations

View(Books)
# generating frequents using association rules

rule1 = apriori(as.matrix(Books),parameter = list(support=0.05,confidence=0.8))
rule1
inspect(head(rule1,5))
rule1_lift= sort(rule1,by='lift',decreasing = TRUE)
inspect(head(rule1_lift,5))
plot(rule1_lift)
plot(rule1_lift,method = "grouped")


rule2 = apriori(as.matrix(Books),parameter = list(support=0.1,confidence= 0.8))
rule2
inspect(head(rule2,5))
rule2_lift= sort(rule2,by='lift',decreasing = TRUE)
inspect(head(rule2_lift,5))
plot(rule2_lift)
plot(rule2_lift,method = "grouped")


rule3 = apriori(as.matrix(Books),parameter = list(support=0.1,confidence= 0.8,minlen=3))
rule3
inspect(head(rule3,5))
rule3_lift= sort(rule3,by='lift',decreasing = TRUE)
inspect(head(rule3_lift,6))
plot(rule3_lift)
plot(rule3_lift,method = "grouped") 
