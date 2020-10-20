# Load the data set and view it#
fullDataSet = read.csv("2020-XTern-DS.csv")
fullDataSet

## Clean the data set to do analysis on ratings ##
cleanedDataSet = subset(fullDataSet, !(Votes == "-" & Reviews == "-"))
cleanedDataSet

## Plot the distribution of average ratings to seeif analysis is applicable ##
plot(cleanedDataSet$Rating)

## Convert cleaned Rating and Votes column into numbers from factor level ##
class(cleanedDataSet$Rating)
class(cleanedDataSet$Votes)

numericRatings = as.numeric(as.character(cleanedDataSet$Rating))
numericVotes = as.numeric(as.character(cleanedDataSet$Votes))

## Plot Rating vs Votes to see if linear regression is applicable ##
plot(numericRatings ~ numericVotes, xlab = "Votes", ylab = "Rating")

## Transform the data by taking the log(Votes) to standardize data and remove large values ##
logVotes = log(numericVotes)
plot(numericRatings ~ logVotes, xlab = "log(Votes)", ylab = "Rating")

## Build linear regression model ##
model1 = lm(numericRatings ~ logVotes)
summary(model1)
abline(model1, col = "red")

## Add logVotes to the data set for easier analysis ##
cleanedDataSet$logVotes = logVotes

## Calculate score using linear regression coefficient ##
alpha = model1$coefficients[2]
score = ((1 - alpha) * numericRatings) + (alpha * logVotes)

## Put the score in the cleaned data set. Set any values greater than 5 to 5 ##
cleanedDataSet$score = score
cleanedDataSet$score[cleanedDataSet$score >= 5] = 5.0
max(cleanedDataSet$score)

## Loop through data set and create a new data set containing the information of only the desired cuisine type ##
dataLength = length(cleanedDataSet$Cuisines)
dataList = list()
counter = 0
for (i in 1:dataLength){
  if ("Fast Food" %in% cleanedDataSet$Cuisine[i]) {
    dataList[[counter]] = cleanedDataSet[i,]
    counter = counter + 1
  }
}
cuisineTypeList = do.call(rbind, dataList)

## Find the ID of the restaurant with the highest score, lowest average cost, and lowest minimum order cost. ##
## Strip the $ to make a vector of numbers, find the min, and then turn the min back into a string ##
numericAverageCost = as.double(gsub("\\$", "", cuisineTypeList$Average_Cost))
minAverageCost = formatC(min(numericAverageCost), format = 'f', flag = '0', digits = 2)
factorMinAverageCost = paste("$", minAverageCost, sep = "")

numericMinimumOrder = as.double(gsub("\\$", "", cuisineTypeList$Minimum_Order))
minMinimumOrder = formatC(min(numericMinimumOrder), format = 'f', flag = '0', digits = 2)
factorMinimumOrder = paste("$", minMinimumOrder, sep = "")

highestScore = max(cuisineTypeList$score)

## Initialize variables for flow control and to store the ID_values ##
cuisineLength = length(cuisineTypeList$score)
orderList = list()
costList = list()
cuisineCounter1 = 1
cuisineCounter2 = 1

## Loop through the data set to find the restaurant ids of the desired type ##
for (j in 1:cuisineLength) {
  if (factorMinAverageCost %in% cuisineTypeList$Average_Cost[j]) {
    print("Success")
    costList[[cuisineCounter1]] = cuisineTypeList$ï..Restaurant[j]
    cuisineCounter1 = cuisineCounter1 + 1
  }
  if (factorMinimumOrder %in% cuisineTypeList$Minimum_Order[j]) {
    orderList[[cuisineCounter2]] = cuisineTypeList$ï..Restaurant[j]
    cuisineCounter2 = cuisineCounter2 + 1
  }
  if (highestScore == cuisineTypeList$score[j]) {
    highScoreID = cuisineTypeList$ï..Restaurant[j]
  }
}

## Find the restaurant closest to the given restaurant and return score and cooking time ##
## Initialize variables. Set givenID to an arbitrary ID in the data set. Can be changed by input from the user ##
minDistance = 1
location = 1
givenID = "ID_332"
currentLat = 0
currentLong = 0

## Loop through the data set and find the latitude and longitude of the desired restaurant ID ##
for (k in 1:dataLength) {
  if (givenID %in% cleanedDataSet$ï..Restaurant[k]) {
    currentLat = cleanedDataSet$Latitude[k]
    currentLong = cleanedDataSet$Longitude[k]
  }
}

## Loop the data set and calculate the absolute value in the difference of distance between given restaurant and every other  ##
## restaurant. If that distance is smaller than the current minimum distance, that distance becomes the new minimum distance. ##
## Store the location of that minimum distance with a variable and use it to access the desired restaurant information.       ##

for (l in 1:dataLength) {
  tempDistance = abs(currentLat - cleanedDataSet$Latitude[l]) + abs(currentLong - cleanedDataSet$Longitude[l])
  if (tempDistance < minDistance) {
    minDistance = tempDistance
    location = l
  }
}

closestScore = cleanedDataSet$score[location]
closestCookTime = cleanedDataSet$Cook_Time[location]