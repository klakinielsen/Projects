
install.packages('foreign')
require('foreign')

# Reading the datasets

ses.2000 <- read.spss('C:/Users/niels/OneDrive/NYU/Business Analytics/Final Project/ETH_2000_HICES_v01_M_v01_A_SHIP_SPSS/ETH_2000_E_P.sav')

ses.2004 <- read.spss('C:/Users/niels/OneDrive/NYU/Business Analytics/Final Project/ETH_2004_HICES_v01_M_v01_A_SHIP_SPSS/ETH_2004_HICES_v01_M_v01_A_SHIP_SPSS/ETH_2004_E_P.sav')

expend201216 <- read.csv('C:/Users/niels/OneDrive/NYU/Business Analytics/Final Project/FoodConsumption2012_16.csv')

agr.value.added <- read.csv('C:/Users/niels/OneDrive/NYU/Business Analytics/Final Project/agricultureValueAdded.csv')

# Calculating household fod consumption as a share of total household consumption and its respective score
# Using linear extrapolation between known datapoints (2000, 2004, 2010 and 2012)

food.con.ratio.2000 <- mean((ses.2000$TOTFOOD)/(ses.2000$HHEXP_N))*100

food.con.ratio.2004 <- mean(na.exclude((ses.2004$TOTFOOD)/(ses.2004$HHEXP_N)))*100

interval1 <- (food.con.ratio.2004 - food.con.ratio.2000)/4

food.con.ratio.2001 <- food.con.ratio.2000 + interval1
food.con.ratio.2002 <- food.con.ratio.2000 + 2*interval1
food.con.ratio.2003 <- food.con.ratio.2000 + 3*interval1

food.con.ratio.2010 <- 46.1 #From report on food security in Ethiopia

interval2 <- (food.con.ratio.2010 - food.con.ratio.2004)/6

food.con.ratio.2005 <- food.con.ratio.2004 + interval2
food.con.ratio.2006 <- food.con.ratio.2004 + 2*interval2
food.con.ratio.2007 <- food.con.ratio.2004 + 3*interval2
food.con.ratio.2008 <- food.con.ratio.2004 + 4*interval2
food.con.ratio.2009 <- food.con.ratio.2004 + 5*interval2

food.con.ratio.2012 <- expend201216$Percentage[1]

food.con.ratio.2011 <- food.con.ratio.2010 + ((food.con.ratio.2012 - food.con.ratio.2010)/2)

food.con.ratio <- c(food.con.ratio.2000, food.con.ratio.2001, food.con.ratio.2002, food.con.ratio.2003,
                    food.con.ratio.2004, food.con.ratio.2005, food.con.ratio.2006, food.con.ratio.2007,
                    food.con.ratio.2008, food.con.ratio.2009, food.con.ratio.2010, food.con.ratio.2011, food.con.ratio.2012)

years <- c('2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012')

df.food.con.ratio <- data.frame(years,food.con.ratio)

df.food.con.ratio

# Predicting the unknown scores using linear regression

y <- expend201216$Score
x <- expend201216$Percentage

ratio.regression <- lm(y ~ x)

score.extrapolation <- predict(ratio.regression,data.frame(x=food.con.ratio))

df.scoreboard <- data.frame(years, food.con.ratio, score.extrapolation)

df.scoreboard
