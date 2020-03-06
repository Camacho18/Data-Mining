# Data-Mining

## Index
- [First Practice](#first-practice)
- [Second Practice (List)](#second-practice)
- [Third Practice (Fibonnacci)](#third-practice)
- [Four Practice (Pearson Correlation)](#four-practice)
- [Five Practice (20 Functions Sales.csv](#five-practice)

###First Practice
Try the *Law of Large Numbers* for N random numbers normally distributed with mean = 0, stdev = 1: 
Create an R script that counts how many of these numbers fall between -1 and 1 and divide by the total amount of N 
You know that E (X) = 68.2% 
Verify the mean (Xn) -> E (X) while running your script again while increasing N Innuendo: 
-Initialize sample size 
-Initialize counter loop for (i in rnorm (size)) 
-Check if the iterated variable falls 
-Increase the counter if the condition is true return a result <- counter / N
```
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

###Second Practice
**Practice find 20 more funtions in R and make an example of it.**
x<-c(14,15,16,20,14) # A generic function which combines its arguments 
Sys.getenv(x)  # Get the value of the environment variable "x"
abs(x)         # The absolute value of "x"
append(x,14)   # Add elements to a vector
cat(x)         # Prints the arguments
unique(x)      # Remove duplicate entries from vector
Sys.time()     # Return system time
Sys.Date()     # Return system date
getwd()        # Return working directory
setwd("/home") # Set working directory

sqrt (x) #Square root of x
ceiling #(x) Integer smaller than xf
floor (x) #An integer larger than x
trunc (x) #Truncation of x
round (x, digits = n) #Rounds x to a specific number of decimals
log (x, base = n) #Logarithms
#Generate random numbers
str(rnorm)
function (n, mean = 0, sd = 1)
rnorm(3)

###Third Practice
Scenario: You are a Data Scientist working for a consulting firm.
One of your colleagues from the Auditing Department has asked you
to help them assess the financial statement of organization X.

You have been supplied with two vector of data: mounthly revenue and
expenses for the financial year in quiestion. Your task is to calculate
the following financial matrics:
  
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
```
round()
mean()
max()
min()
```

#Data
```
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)
```

#Solution
#Calculate Profit As The Differences Between Revenue And Expenses
```
profit <- revenue- expenses 
profit
```

#Calculate Tax As 30% Of Profit And Round To 2 Decimal Points
```
tax <- round(0.30 * profit, 2)
tax 
```

#Calculate Profit Remaining After Tax Is Deducted
```
profit.after.tax <- revenue - profit
profit.after.tax
```

#Calculate The Profit Margin As Profit After Tax Over Revenue
#Round To 2 Decimal Points, Then Multiply By 100 To Get %
```
profit.margin <- round(profit.after.tax / revenue, 2) * 100
profit.margin
```

#Calculate The Mean Profit After Tax For The 12 Months
```
mean_pat <- mean(profit.after.tax)
mean_pat
```

#Find The Months With Above-Mean Profit After Tax
```
good.months <- profit.after.tax > mean_pat
good.months
```

#Bad Months Are The Opposite Of Good Months !
```
bad.months <- !good.months
bad.months
```

#The Best Month Is Where Profit After Tax Was Equal To The Maximum
```
best.month <- profit.after.tax == max(profit.after.tax)
best.month
```

#The Worst Month Is Where Profit After Tax Was Equal To The Minimum
```
worst.month <- profit.after.tax == min(profit.after.tax)
worst.month
```

#Convert All Calculations To Units Of One Thousand Dollars
```
revenue.1000 <- round(revenue / 1000, 0)
expenses.1000 <- round(expenses / 1000, 0)
profit.1000 <- round(profit / 1000, 0)
profit.after.tax.1000 <- round(profit.after.tax / 1000, 0)
```

#Print Results
```
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

#BONUS:
#Preview Of What's Coming In The Next Section
```
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

#Print The Matrix
```
M
```

###Four Practice
#Instructions for this dataset:
# You have only been supplied vectors. You will need
# to create the matrices yourself.
# Matrices:
# - FreeThrows
# - FreeThrowAttempts
#Kirill Eremenko
#www.superdatascience.com

#Copyright: These datasets were prepared using publicly available data.
#           However, theses scripts are subject to Copyright Laws. 
#           If you wish to use these R scripts outside of the R Programming Course
#           by Kirill Eremenko, you may do so by referencing www.superdatascience.com in your work.

#Comments:
#Seasons are labeled based on the first year in the season
#E.g. the 2012-2013 season is preseneted as simply 2012

#Notes and Corrections to the data:
#Kevin Durant: 2006 - College Data Used
#Kevin Durant: 2005 - Proxied With 2006 Data
#Derrick Rose: 2012 - Did Not Play
#Derrick Rose: 2007 - College Data Used
#Derrick Rose: 2006 - Proxied With 2007 Data
#Derrick Rose: 2005 - Proxied With 2007 Data

#Seasons
Seasons <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")

#Players
Players <- c("KobeBryant","JoeJohnson","LeBronJames","CarmeloAnthony","DwightHoward","ChrisBosh","ChrisPaul","KevinDurant","DerrickRose","DwayneWade")

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

#Matrix for Free Throws
#Bind the given vectors to form the matrix
#Remove vectors - we don't need them anymore
#Rename the columns
#Rename the rows
#Check the matrix


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

#Matrix for Free Throw Attempts
#Bind the given vectors to form the matrix

#Remove vectors - we don't need them anymore

#Rename the columns
colnames(FreeThrowAttempts) <- Seasons
#Rename the rows
rownames(FreeThrowAttempts) <- Players

#Check the matrix
FreeThrowAttempts

#Re-create the plotting function
#Visualize the new matrices
#Part 1 - Free Throw Attempts Per Game 
#Part 2 - Free Throw Accuracy

#Field Goal Accuracy was exceptional:
#Part 3 - Player Style Patterns Excluding Free Throws

###Five Practice
