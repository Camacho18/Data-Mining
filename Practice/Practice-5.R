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

#Filter by countrie Malta
head(stats)
filter <- stats$Country.Name == "Malta" #TRUE
filter
stats[filter,]
#Filter by countrie Qatar
head(stats)
filter <- stats$Country.Name == "Qatar" #TRUE
filter
stats[filter,]
#Filter by countrie Netherlands
head(stats)
filter <- stats$Country.Name == "Netherlands" #TRUE
filter
stats[filter,]
#Filter by countrie Norway
head(stats)
filter <- stats$Country.Name == "Norway" #TRUE
filter
stats[filter,]
