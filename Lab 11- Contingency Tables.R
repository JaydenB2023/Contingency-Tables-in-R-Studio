##Lab 11: Contingency Tables
#J.Brown 04/05/22

#import ANES data
library(readr)
anes <- read.csv("~/Downloads/ANES_2016.csv")
#create a table with pres.vote (depen) & party.id (indep)
pres <-
Table_1 <- table(anes$vote.pres,
                 anes$party.id)
print(Table_1)
#total, row, column marginals 
margin.table(Table_1, margin = 1) #rows
margin.table(Table_1, margin = 2) #columns
margin.table(Table_1) #table total
#column percentages
prop.table(Table_1) #total %
print(Table_1/20000)
#chi square test of independence
chisq.test(Table_1)
#create a table with pres.vote (depen) & bible (indep)
Table_2 <- table(anes$vote.pres, anes$bible)
print(Table_2)
#total, row, column marginals
margin.table(Table_2, margin = 1) #rows
margin.table(Table_2, margin = 2) #columns
margin.table(Table_2) #table total
#column percentages
prop.table(Table_2) #total %
print(Table_2/20000)
#chi square test of independence
chisq.test(Table_2)
#create a table with pres.vote (depen) & game.thrones (indep)
Table_3 <- table(anes$vote.pres, anes$game.thrones)
print(Table_3)
#total, row, column marginals
margin.table(Table_3, margin = 1) #rows
margin.table(Table_3, margin = 2) #columns
margin.table(Table_3) #table total
#column percentages
prop.table(Table_3) #total %
print(Table_3/20000)
#chi square test of independence
chisq.test(Table_3)

#subset Democrats only
democrat <- (anes$party.id == "Dem")
Table_4 <- table(democrat, anes$game.thrones)
print(Table_4)
prop.table(Table_4)
chisq.test(Table_4)
#subset Independents only
indep <- (anes$party.id == "ind")
Table_5 <- table(indep, anes$game.thrones)
print(Table_5)
prop.table(Table_5)
chisq.test(Table_5)
#subset Republicans only
repub <- (anes$party.id == "Rep")
Table_6 <- table(repub, anes$game.thrones)
print(Table_6)
prop.table(Table_6)
chisq.test(Table_6)