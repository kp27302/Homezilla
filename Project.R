#Task 2
library(readxl)
data = read_excel("D:/UCONN/SEM 1/OPIM - 5603 - SBA/Project/W16604-XLS-ENG.xlsx")
View(data)

cleaned_data = data[data$type != "vacant", ]
View(cleaned_data)

library(moments)

Stats = data.frame(Mean = mean(cleaned_data$price, na.rm = TRUE),
Median = median(cleaned_data$price, na.rm = TRUE),
SD = sd(cleaned_data$price, na.rm = TRUE),
Minimum = min(cleaned_data$price, na.rm = TRUE),
Maximum = max(cleaned_data$price, na.rm = TRUE),
CV = SD/Mean,
Skewness = skewness(cleaned_data$price, na.rm = TRUE),
Kurtosis = kurtosis(cleaned_data$price, na.rm = TRUE)
)
View(Stats)

cleaned_data$z_score = (cleaned_data$price - mean(cleaned_data$price, na.rm = TRUE)) / sd(cleaned_data$price, na.rm = TRUE)
z_score

positive_outliers = cleaned_data[cleaned_data$z_score > 3, ]
positive_outliers
negative_outliers = cleaned_data[cleaned_data$z_score < -3, ]
negative_outliers

library(ggplot2)

ggplot(cleaned_data, aes(x = price)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of House Prices", x = "Price", y = "Frequency")

qqnorm(cleaned_data$price)
qqline(cleaned_data$price, col = "red")

#Task 3

attach(cleaned_data)

lmdata = cleaned_data[c("subtype","sqfoot","bedrooms","bathrooms")]
model=lm(price ~ ., data=lmdata)
final_model = step(model, direction = "backward")
summary(final_model)

#Task 4

browsing_data = read_excel("D:/UCONN/SEM 1/OPIM - 5603 - SBA/Project/W16604-XLS-ENG.xlsx", 
                           sheet = "Browsing Data")
View(browsing_data)
browsing_data = browsing_data[!is.na(Timestamp), ]

attach(browsing_data)

TVmean= mean(`Time Viewed`,na.rm = TRUE)
TVmedian=median(`Time Viewed`,na.rm = TRUE)
TVsd=sd(`Time Viewed`,na.rm = TRUE)
TVmin=min(`Time Viewed`,na.rm = TRUE)
TVmax=max(`Time Viewed`,na.rm = TRUE)
TVcv=TVsd/TVmean
TVskew=skewness(`Time Viewed`,na.rm = TRUE)
TVkurtosis=kurtosis(`Time Viewed`,na.rm = TRUE)

TVdf=data.frame(TVmean,TVmedian,TVsd,TVmin,TVmax,TVcv,TVskew,TVkurtosis)
View(TVdf)

z_score=scale(`Time Viewed`)
z_score

positive_outliers = `Time Viewed`[z_score > 3]
positive_outliers
negative_outliers =`Time Viewed`[z_score < -3]
negative_outliers


hist(browsing_data$`Time Viewed`)

qqnorm(browsing_data$`Time Viewed`)
qqline(browsing_data$`Time Viewed`)

tv= `Time Viewed`
tv.sort=sort(tv)
n=length(tv)
qqexp=qexp(seq(0,1-(1/n),1/n))
plot(qqexp, tv.sort)
qqline(tv,distribution = qexp, col="red")

#Task 5

browsing_data = read_excel("D:/UCONN/SEM 1/OPIM - 5603 - SBA/Project/W16604-XLS-ENG.xlsx", 
                             sheet = "Browsing Data")
tags_data= read_excel("D:/UCONN/SEM 1/OPIM - 5603 - SBA/Project/HomezillaTags.xlsx", 
                      sheet = "Dummy Variables")

browsing_data= browsing_data[c("Time Viewed","Timestamp","Direction","Customer ID")]
tempdf=tags_data
tempdf$Direction = browsing_data$Direction
tempdf$timeviewed= browsing_data$`Time Viewed`
tempdf$timestamp = browsing_data$Timestamp
tempdf$customerid= browsing_data$`Customer ID`

tempdf=tempdf[tempdf$timeviewed<=120,]
tempdf=tempdf[!is.na(tempdf$timestamp),]
tag_model = lm(timeviewed~.,data=tempdf)
final_tagmodel=step(tag_model,direction = "backward")
summary(final_model)