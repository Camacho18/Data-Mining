#Practice_5
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
