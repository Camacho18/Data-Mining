# Data-Mining

## Index
- [First Practice](#first-practice)
- [Second Practice (List)](#second-practice)
- [Third Practice (Fibonnacci)](#third-practice)
- [Four Practice (Pearson Correlation)](#four-practice)
- [Five Practice (20 Functions Sales.csv](#five-practice)

## First Practice

Try the *Law of Large Numbers* for N random numbers normally distributed with mean = 0, stdev = 1: Create an R script that counts how many of these numbers fall between 
-1 and 1 and divide by the total amount of N 
You know that E (X) = 68.2%.  
Verify the mean (Xn) -> E (X) while running your script again while increasing N Innuendo:
- Initialize sample size.  
- Initialize counter loop for (i in rnorm (size)).  
- Check if the iterated variable falls.  
- Increase the counter if the condition is true return a result <- counter / N

``` R
size <- 99999999
cont <- 0
mean <- 0
stdev <- 1
for(i in rnorm(size,mean,stdev)){
  if(i>=-1 && i<=1)
    cont <- cont+1
}
result <- cont/size
result <- result*100
paste(format(round(result, 2), nsmall = 2),"%")
```

## Second Practice

#### **Practice find 20 more funtions in R and make an example of it.**  
x<-c(14,15,16,20,14)
 **A generic function which combines its arguments.**  
Sys.getenv(x)
 **Get the value of the environment variable "x".**  
abs(x)         **The absolute value of "x".**  
append(x,14)   **Add elements to a vector.**   
cat(x)         **Prints the arguments.**  
unique(x)      **Remove duplicate entries from vector.**  
Sys.time()     **Return system time.**  
Sys.Date()     **Return system date.**  
getwd()        **Return working directory.**  
setwd("/home") **Set working directory.**  
sqrt (x) **Square root of x.**  
ceiling (x) **Integer smaller than xf.**  
floor (x) **An integer larger than x.**  
trunc (x) **Truncation of x.**  
round (x, digits = n) **Rounds x to a specific number of   decimals.**  
log (x, base = n) **Logarithms.**  
**Generate random numbers.**  
str(rnorm)  
function (n, mean = 0, sd = 1)  
rnorm(3)

## Third Practice

<p style="text-align: justify">Scenario: You are a Data Scientist working for a consulting firm.
One of your colleagues from the Auditing Department has asked you
to help them assess the financial statement of organization X.

You have been supplied with two vector of data: mounthly revenue and
expenses for the financial year in quiestion. Your task is to calculate
the following financial matrics:</p>
  
- profit for each mounth
- profit after tax for each month (the tax rate is 30%)
- profit margin for each month - equal to profit after tax divided by revenue
- good months - where the profit after tax was greater than the mean for the year
- bad months - where the profit after tax was less then the mean for years
- the best month - where the profit after tax was max for the year
- the worst month - where the profit after tax was min for the year

All results need to be presented as vectors.

Results for dollar values need to be calculate with $0.01 precision, but need to be
presented in Units of $1,000(i.e. 1k) with no decimal point.

Results for the profit margin ratio needed to be presented in units of % with no
decimal points.

Note: Your collegue has warned you that it is okay for tax for any given month to be
negative (in accounting terms, negative tax translates into a deferred tax asset).

Hint 1
Use:
```R
round()
mean()
max()
min()
```

#Data
```R
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)    

expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)
```

### Solution
Calculate Profit As The Differences Between Revenue And Expenses
``` R
profit <- revenue- expenses 
profit
```

Calculate Tax As 30% Of Profit And Round To 2 Decimal Points
```R
tax <- round(0.30 * profit, 2)
tax 
```

Calculate Profit Remaining After Tax Is Deducted
``` R
profit.after.tax <- revenue - profit
profit.after.tax
```

Calculate The Profit Margin As Profit After Tax Over Revenue  
Round To 2 Decimal Points, Then Multiply By 100 To Get %
```R
profit.margin <- round(profit.after.tax / revenue, 2) * 100
profit.margin
```

Calculate The Mean Profit After Tax For The 12 Months
```R
mean_pat <- mean(profit.after.tax)
mean_pat
```

Find The Months With Above-Mean Profit After Tax
``` R
good.months <- profit.after.tax > mean_pat
good.months
```

Bad Months Are The Opposite Of Good Months !
``` R
bad.months <- !good.months
bad.months
```

The Best Month Is Where Profit After Tax Was Equal To The Maximum
``` r
best.month <- profit.after.tax == max(profit.after.tax)
best.month
```

The Worst Month Is Where Profit After Tax Was Equal To The Minimum
``` R
worst.month <- profit.after.tax == min(profit.after.tax)
worst.month
```

Convert All Calculations To Units Of One Thousand Dollars
``` R
revenue.1000 <- round(revenue / 1000, 0)
expenses.1000 <- round(expenses / 1000, 0)
profit.1000 <- round(profit / 1000, 0)
profit.after.tax.1000 <- round(profit.after.tax / 1000, 0)
```

Print Results
``` R
revenue.1000
expenses.1000
profit.1000
profit.after.tax.1000
profit.margin
good.months
bad.months
best.month
worst.month
```

#### BONUS:
Preview Of What's Coming In The Next Section
``` R
M <- rbind(
  revenue.1000,
  expenses.1000,
  profit.1000,
  profit.after.tax.1000,
  profit.margin,
  good.months,
  bad.months,
  best.month,
  worst.month
)
```

Print The Matrix
```
M
```

### Four Practice
Dear Student,  
Welcome to the dataset for the homework exercise.  

Instructions for this dataset:
You have only been supplied vectors. You will need  to create the matrices yourself.
 Matrices:
 - FreeThrows
 - FreeThrowAttempts

Sincerely,  
Kirill Eremenko  
www.superdatascience.com

Copyright: These datasets were prepared using publicly available data.  
However, theses scripts are subject to Copyright Laws.  
If you wish to use these R scripts outside of the R Programming Course by Kirill Eremenko, you may do so by referencing www.superdatascience.com in your work.

Comments:  
Seasons are labeled based on the first year in the season  
E.g. the 2012-2013 season is preseneted as simply 2012

Notes and Corrections to the data:  
Kevin Durant: 2006 - College Data Used  
Kevin Durant: 2005 - Proxied With 2006 Data  
Derrick Rose: 2012 - Did Not Play  
Derrick Rose: 2007 - College Data Used  
Derrick Rose: 2006 - Proxied With 2007 Data  
Derrick Rose: 2005 - Proxied With 2007 Data  

#Seasons  
Seasons <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")  
Seasons  
#Players  
Players <- c("KobeBryant", "JoeJohnson", "LeBronJames", "CarmeloAnthony", "DwightHoward", "ChrisBosh", "ChrisPaul","KevinDurant", "DerrickRose", "DwayneWade")  
Players  
#Free Throws  
KobeBryant_FT <- c(696,667,623,483,439,483,381,525,18,196)  
JoeJohnson_FT <- c(261,235,316,299,220,195,158,132,159,141)  
LeBronJames_FT <- c(601,489,549,594,593,503,387,403,439,375)  
CarmeloAnthony_FT <- c(573,459,464,371,508,507,295,425,459,189)  
DwightHoward_FT <- c(356,390,529,504,483,546,281,355,349,143)  
ChrisBosh_FT <- c(474,463,472,504,470,384,229,241,223,179)  
ChrisPaul_FT <- c(394,292,332,455,161,337,260,286,295,289)  
KevinDurant_FT <- c(209,209,391,452,756,594,431,679,703,146)  
DerrickRose_FT <- c(146,146,146,197,259,476,194,0,27,152)  
DwayneWade_FT <- c(629,432,354,590,534,494,235,308,189,284)    

#Matrix 
FreeThrows <- rbind(KobeBryant_FT, JoeJohnson_FT, LeBronJames_FT, CarmeloAnthony_FT, DwightHoward_FT, ChrisBosh_FT, ChrisPaul_FT, KevinDurant_FT, DerrickRose_FT, DwayneWade_FT)  
FreeThrows  
#Remove vectors - we don't need them anymore  
rm(KobeBryant_FT, JoeJohnson_FT, CarmeloAnthony_FT, DwightHoward_FT, ChrisBosh_FT, LeBronJames_FT, ChrisPaul_FT, DerrickRose_FT, DwayneWade_FT, KevinDurant_FT)  
FreeThrows  
#Rename the columns  
colnames(FreeThrows) <- Seasons  
#Rename the rows  
rownames(FreeThrows) <- Players

#Check the matrix  
FreeThrows

#
########### second matrix ############  
#Free Throw Attempts  
KobeBryant_FTA <- c(819,768,742,564,541,583,451,626,21,241)  
JoeJohnson_FTA <- c(330,314,379,362,269,243,186,161,195,176)  
LeBronJames_FTA <- c(814,701,771,762,773,663,502,535,585,528)  
CarmeloAnthony_FTA <- c(709,568,590,468,612,605,367,512,541,237)  
DwightHoward_FTA <- c(598,666,897,849,816,916,572,721,638,271)  
ChrisBosh_FTA <- c(581,590,559,617,590,471,279,302,272,232)  
ChrisPaul_FTA <- c(465,357,390,524,190,384,302,323,345,321)  
KevinDurant_FTA <- c(256,256,448,524,840,675,501,750,805,171)  
DerrickRose_FTA <- c(205,205,205,250,338,555,239,0,32,187)  
DwayneWade_FTA <- c(803,535,467,771,702,652,297,425,258,370)  

#Matrix  

FreeThrowAttempts

FreeThrowAttempts <- rbind(KobeBryant_FTA, JoeJohnson_FTA, LeBronJames_FTA, CarmeloAnthony_FTA, DwightHoward_FTA, ChrisBosh_FTA, ChrisPaul_FTA, KevinDurant_FTA, DerrickRose_FTA, DwayneWade_FTA)  
rm(KobeBryant_FTA, JoeJohnson_FTA, LeBronJames_FTA, CarmeloAnthony_FTA, DwightHoward_FTA, ChrisBosh_FTA, ChrisPaul_FTA, KevinDurant_FTA, DerrickRose_FTA, DwayneWade_FTA)  
colnames(FreeThrowAttempts) <- Seasons  
rownames(FreeThrowAttempts) <- Players  
FreeThrowAttempts
#
########### function ############  
myplot <- function(z, who=1:10) {
  matplot(t(z[who,,drop=F]), type="b", pch=15:18, col=c(1:4,6), main="Basketball Players Analysis")
  legend("bottomleft", inset=0.01, legend=Players[who], col=c(1:4,6), pch=15:18, horiz=F)
}  

myplot(FreeThrows)  
myplot(FreeThrowAttempts)

#Part 1 - Free Throw Attempts Per Game   
#(You will need the Games matrix)  
#Games   
KobeBryant_G <- c(80,77,82,82,73,82,58,78,6,35)  
JoeJohnson_G <- c(82,57,82,79,76,72,60,72,79,80)  
LeBronJames_G <- c(79,78,75,81,76,79,62,76,77,69)  
CarmeloAnthony_G <- c(80,65,77,66,69,77,55,67,77,40)  
DwightHoward_G <- c(82,82,82,79,82,78,54,76,71,41)  
ChrisBosh_G <- c(70,69,67,77,70,77,57,74,79,44)  
ChrisPaul_G <- c(78,64,80,78,45,80,60,70,62,82)  
KevinDurant_G <- c(35,35,80,74,82,78,66,81,81,27)  
DerrickRose_G <- c(40,40,40,81,78,81,39,0,10,51)  
DwayneWade_G <- c(75,51,51,79,77,76,49,69,54,62)  

#Matrix  
Games <- rbind(KobeBryant_G, JoeJohnson_G, LeBronJames_G, CarmeloAnthony_G, DwightHoward_G, ChrisBosh_G, ChrisPaul_G, KevinDurant_G, DerrickRose_G, DwayneWade_G)  
rm(KobeBryant_G, JoeJohnson_G, CarmeloAnthony_G, DwightHoward_G, ChrisBosh_G, LeBronJames_G, ChrisPaul_G, DerrickRose_G, DwayneWade_G, KevinDurant_G)  
colnames(Games) <- Seasons  
rownames(Games) <- Players  

myplot(FreeThrowAttempts/Games)

myplot(FieldGoals/FieldGoalAttempts)

#Part 2 - Free Throw Accuracy  
myplot(FreeThrows/Games)


#Part 3 - Player Style Patterns Excluding Free Throws  
myplot((FieldGoalAttempts-FreeThrows)/Games)


### Five Practice
stats <- read.csv(file.choose())  
stats  
#Filter countries by Low income  
head(stats)  
filter <- stats$Income.Group == "Low income" #TRUE  
filter  
stats[filter,]  
#Filter countries by Lower middle income  
head(stats)  
filter <- stats$Income.Group == "Lower middle income" #TRUE  
filter  
stats[filter,]  
#Filter countries by Upper middle income  
head(stats)  
filter <- stats$Income.Group == "Lower middle income" #TRUE  
filter  
stats[filter,]  
