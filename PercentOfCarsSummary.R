setwd("/Users/hannahwalsh/Desktop/Parking_Data/www/cleanedData/")
files = list.files(pattern="*.csv")
Complete_Data = read.csv(files[1], sep = ",", header = TRUE)
for (i in 2:length(files))
{
  dat = read.csv(files[i], sep = ",", header = TRUE)
  Complete_Data = rbind(Complete_Data, dat)
}
t = table(Complete_Data$License)
t = sort(t, decreasing = TRUE)

HighTickets <- function(tickets)
{
  lim = which(t<tickets)[1] - 1
  tot_high = sum(t[1:(lim)])
  percentOfCars = (lim)/length(t) * 100
  percentOfTickets = tot_high/length(t) * 100
  stats = data.frame(tickets, lim, tot_high, percentOfCars, percentOfTickets)
  
  return(stats)
}


## For 3 or more tickets
HighTickets(3)
# 22619 cars
# 112155 tickets
# 9.6% of cars
# 47.4% of tickets

## For 4 or more tickets
HighTickets(4)
# 12395 cars
# 81483 tickets
# 5.2% of cars
# 34.3% of tickets

## For 5 or more tickets
HighTickets(5)
# 7626 cars
# 62407 tickets
# 3.2% of cars
# 26.4% of tickets

## For 6 or more tickets
HighTickets(6)
# 5092 cars
# 49737 tickets
# 2.2% of cars
# 21.0% of tickets

## For 7 or more tickets
HighTickets(7)
# 3638 cars
# 41013 tickets
# 1.5% of cars
# 17.3% of tickets



## Look for repeat offenders (>4 tickets) and plot them
repeat_offenders = names(t[t>4])



