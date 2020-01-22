setwd("\\Users\\Asvin\\Documents\\RData\\STAC53")
CUIS<-read.table(file = "CUIS2012-SubsetData.txt", header = TRUE, sep = ",")
#names(CUIS) <- c("CaseID", "Age of Respondant", "Past 12 months online order", "Concerned CC")
attach(CUIS)
names(CUIS)
table(gcagegr6)
table(ec_q01)
table(ps_q02)
CUIS2<-CUIS[which(gcagegr6 <= 6),]
detach(CUIS)
attach(CUIS2)
CUIS3<-CUIS2[which(ec_q01<=3),]
detach(CUIS2)
attach(CUIS3)
table(gcagegr6)
table(ec_q01)
table(ps_q02)
# change the name of gcagegr6 to something meaningful
Age.Of.Respondant <- gcagegr6
# change the name of ec_q01 to something meaningful
Online.Shopping <- ec_q01
# change the name of ps_q02 to something meaningful
CC.Concerned <- ps_q02
# Research Question 1
# change category names from numbers to names for age of respondant
Age.Of.Respondant<-factor(Age.Of.Respondant, 
                          levels=c(1,2,3,4,5,6),
                          labels = c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 and older"))
# change category names from numbers to names for online shopping
Online.Shopping <-factor(Online.Shopping,
                         levels = c(1, 2),
                         labels = c("Yes", "No"))
# Perform exploratory analysis to see if there is an apparent relationship between
# age of respondant and whether or not respondant has purchased anything online in
# the past 12 months
# Create table for comparing age and whether or not respondant did online shopping in the last 12 months
Table <- table(Online.Shopping, Age.Of.Respondant)
Table

# create margins for each column
addmargins(Table, 1)
# create margins for each row
addmargins(Table, 2)

# Create barplot to obtain frequency of responses by Whether they online shop or not
barplot(table(Online.Shopping, Age.Of.Respondant), beside = TRUE,
        main = "Age of the respondant by whether the respondant done online shopping in the past 12 months",
        xlab = "Age of the respondant",
        ylim = c(0,3000),
        col=rainbow(2))
# add legend to bar plot
legend("topright",
       title = "Done online shopping in the past 12 months?",
       legend = rownames(table(Online.Shopping, Age.Of.Respondant)),
       fill = rainbow(2),
       cex = 0.65)

# calculate marginal proportions into the table
marginal.prop<-prop.table(margin.table(Table, 2))
marginal.prop
marginal.prop<-prop.table(margin.table(Table,1))
marginal.prop
# row proportions
Row.prop<-prop.table(Table,1)
Row.prop
#Column proportions
Col.Prop<-prop.table(Table,2)
Col.Prop
#Calculate Joint Proportions
Joint.Prop<-prop.table(Table)
Joint.Prop
# Perform Chi-Squared test of Independence
chisq.test(Table)
#add margins to the table
addmargins(Table)

detach(CUIS3)
attach(CUIS)
names(CUIS)
CUIS2<-CUIS[which(ps_q02<=3 & ec_q01 <=5),]
detach(CUIS)
attach(CUIS2)
table(ec_q01)
table(ps_q02)

# Research Question 2
# change category names from numbers to names for age of respondant
CC.Concerned<-factor(CC.Concerned, 
                          levels=c(1,2,3),
                          labels = c("Not at all Concerned", "Concerned", "Very Concerned"))

# Perform exploratory analysis to see if there is an apparent relationship between
# age of respondant and whether or not respondant has purchased anything online in
# the past 12 months
# Create table for comparing age and whether or not respondant did online shopping in the last 12 months
Table <- table(Online.Shopping, CC.Concerned)
Table

# create margins for each column
addmargins(Table, 1)
# create margins for each row
addmargins(Table, 2)

# Create barplot to obtain frequency of responses by Whether they online shop or not
barplot(table(Online.Shopping, CC.Concerned), beside = TRUE,
        main = "Perceived concern of using a credit card on the internet by whether the respondant done online shopping in the past 12 months",
        xlab = "Perceived concern of using a credit card on the internet",
        ylim = c(0,6000),
        col=rainbow(2))
# add legend to bar plot
legend("topright",
       title = "Done online shopping in the past 12 months?",
       legend = rownames(table(Online.Shopping, CC.Concerned)),
       fill = rainbow(2),
       cex = 0.65)

# calculate marginal proportions into the table
marginal.prop<-prop.table(margin.table(Table, 2))
marginal.prop
marginal.prop<-prop.table(margin.table(Table,1))
marginal.prop
# row proportions
Row.prop<-prop.table(Table,1)
Row.prop
#Column proportions
Col.Prop<-prop.table(Table,2)
Col.Prop
#Calculate Joint Proportions
Joint.Prop<-prop.table(Table)
Joint.Prop
# Perform Chi-Squared test of Independence
chisq.test(Table)
#add margins to the table
addmargins(Table)
prop.test(c(2635,1096),c(9168,7449),correct = FALSE)

