library(tidyverse)
library(readr)

buckets <- read_csv("C:/Users/dchio/Downloads/OKC/shots_data.csv")

# get total Attempts, basically all rows in file
totalAttempts = length(buckets$team)

# Want to make a column to filter shots into 3 possible shots (2pt, Corner 3, Non Corner 3)
# Then we can just count up the values in each zone to get total shot attempts 
zone = numeric()
i = 1
while (i <= totalAttempts){
  dist = sqrt((buckets$x[i] - 0)**2 + (buckets$y - 0)**2)
  if((buckets$x[i] > 22 | buckets$x[i] < -22) && buckets$y[i] <= 7.8 ){
    zone[i] = "C3"
  }else if(dist >= 23.75 && buckets$y[i] > 7.8){
    zone[i] = "NC3"
  }else if((buckets$x[i] < 22 | buckets$x[i] > -22) && buckets$y[i] <= 7.8){
    zone[i] = "2pt"
  }else if((dist < 23.75 && buckets$y[i] > 7.8)){
    zone[i] = "2pt"
  }
  i = i+1
}
buckets$zone = zone


# Split up team A and team B using filter()
tA = filter(buckets,team == "Team A") 
tB = filter(buckets,team == "Team B")

# Team A
tA_totalshots = length(tA$zone)

# We will use sum to add up all attempts in desired zones and divide over total
# Check shot distribution of NC3 zone
print(sum(tA$zone == "NC3") / tA_totalshots)

# Check shot distribution of C3 zone
print(sum(tA$zone == "C3") / tA_totalshots)

# Check shot distribution of 2pt zone
print(sum(tA$zone == "2pt") / tA_totalshots)

# Check EFG of NC3 Zone
# We want the sum again but this time with the condition that the shot goes in
print(0.5 * sum(tA$zone == "NC3" & tA$fgmade == 1) / (sum(tA$zone == "NC3")))

# Check EFG of C3 Zone
print(0.5 * sum(tA$zone == "C3" & tA$fgmade == 1) / (sum(tA$zone == "C3")))

# Check EFG of 2pt Zone
print(sum(tA$zone == "2pt" & tA$fgmade == 1) / (sum(tA$zone == "2pt")))

# Team B
tB_totalshots = length(tB$zone)
#print(tB_totalshots)

# Check shot distribution of NC3 zone
print(sum(tB$zone == "NC3") / tB_totalshots)

# Check shot distribution of C3 zone
print(sum(tB$zone == "C3") / tB_totalshots)

# Check shot distribution of 2pt zone
print(sum(tB$zone == "2pt") / tB_totalshots)

# Check EFG of NC3 Zone
# We want the sum again but this time with the condition that the shot goes in
print(0.5 * sum(tB$zone == "NC3" & tB$fgmade == 1) / (sum(tB$zone == "NC3")))

# Check EFG of C3 Zone
print(0.5 * sum(tB$zone == "C3" & tB$fgmade == 1) / (sum(tB$zone == "C3")))

# Check EFG of 2pt Zone
print(sum(tB$zone == "2pt" & tB$fgmade == 1) / (sum(tB$zone == "2pt")))
  