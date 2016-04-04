rm(list = )
setwd("/Users/rdoraiswamy/mygit/RepData_PeerAssessment1/");

#Let us remove the variables
rm(list=ls()) # Remove everything from environment
cat("\014")   # Clear Console
setwd("/Users/rdoraiswamy/mygit/RepData_PeerAssessment1/");
# Load the necessary packages
library(ggplot2);
# Read the CSV file and store the same
act <- read.table("activity.csv", sep = ",", header = TRUE);
# Let's review the first rows
head(act);
str(act);


## What is mean total number of steps taken per day?
# Let us compute the sum of steps by date
step_sum <- aggregate(. ~ act$date, act[1], sum);
# Let us compute the mean and median of steps
step_mean <- mean(step_sum$steps);
step_median <- median(step_sum$steps);
head(step_sum); # Display the first 6 rows of the step summary
#Let's print the mean and median
cat("Mean number of steps: ", step_mean);
cat("Median number of steps: ", step_median);
hist(step_sum$steps, col=step_sum$`act$date`, xlab = "Number of Steps per Day", ylab = "Frequency" , main = "Histogram of number of steps", labels = TRUE, ylim = c(0,40));



## What is the average daily activity pattern?
#Let us create a new plot
plot.new();
#Build the plot of steps vs date
g <- ggplot(step_sum, aes(step_sum$`act$date`, step_sum$steps));
#Add the labels and titles
g <- g + labs(title = "Daily Steps") + xlab ("Date") + ylab("Steps") + geom_bar(stat = "identity", color = step_sum$`act$date`) ;
# Set the lables of x-axis to be 90degrees
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1));
print(g);

# What is the average daily activity pattern?

## Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) -->```{r echo=TRUE}
step_interval <- aggregate(.~interval, act[-2], sum);
step_intv_mean <- aggregate(.~interval, act[-2], mean);
g <- ggplot(step_interval, aes(step_interval$interval, step_interval$steps)) 
g <- g + labs(title = "Interval Steps") + xlab ("Interval") + ylab("Steps") 
g <- g+ geom_bar(stat = "identity", color = step_interval$interval) ;
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1));
print(g);

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
cat("Top 6 intervals by number of steps");
head(step_interval[order(-step_interval$steps),]);
cat("The maximum number of steps are taken at the interval: ", head(step_interval[order(-step_interval$steps),])$interval[1]);

## Imputing missing values
#Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.
###============================================

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)
cat("Out of the total ", dim(act)[1], "number of rows ", sum(is.na(act$steps)), " are missing steps");

###============================================

#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Create a copy of the data frame for both Q2 and Q3 below as well
act_new <- act;
#Replace the NA values with the step_intv_mean values corresponding to the interval
act_new$steps <- ifelse(is.na(act_new$steps) == TRUE, step_intv_mean$steps[step_intv_mean$interval %in% act_new$interval], act_new$steps);
# Let us verify a sample NA value replacement
head(act[is.na(act$steps),]);
#Now let us see the replaced values and the corresponding interval means
act_new[act_new$date == "2012-10-01" & act$interval == 0,];step_intv_mean[step_intv_mean$interval == 0,];

###============================================

#3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
### This is the new data fram act_new created in Q2
sum(is.na(act_new$steps));
cat("Since the new array has no NA values, we can continue to step 4");
###============================================


#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
# Let us compute the New sum of steps by date
nstep_sum <- aggregate(. ~ act_new$date, act_new[1], sum);
# Let us compute the mean and median of steps
nstep_mean <- mean(nstep_sum$steps);
nstep_median <- median(nstep_sum$steps);
head(nstep_sum); # Display the first 6 rows of the step summary
#Let's print the mean and median
cat("Mean number of steps: ", nstep_mean);
cat("Median number of steps: ", nstep_median);

x <- hist(nstep_sum$steps, col=nstep_sum$`act_new$date`, xlab = "Number of Steps per Day", ylab = "Frequency" , main = "Histogram of number of steps after replacing NAs with mean values",labels = TRUE, ylim = c(0,40));
y <- hist(step_sum$steps, col=step_sum$`act$date`, xlab = "Number of Steps per Day", ylab = "Frequency" , main = "[Original] Histogram of number of steps", labels = TRUE, ylim = c(0,40));

#Let us print the histogram counts
cat("Original histogram counts", y$counts);
cat("New histogram counts", x$counts);

### The range 10K-15K steps per day has increased by 8 days. Also the original plot was short by 8 days out of the 61 days total, and the updated data frame has 61 days, a better statistics (at least IMHO)

###============================================
##Are there differences in activity patterns between weekdays and weekends?

##For ths part the 𝚠𝚎𝚎𝚔𝚍𝚊𝚢𝚜() function may be of some help here. Use the dataset with the filled-in missing values for this part.

## Create a new ```{r echo=TRUE}
act_new$day <- ifelse(weekdays.Date(as.Date(act_new$date)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday");
library(sqldf);
act_summ_data <- sqldf("select interval, day, round(avg(steps)) avg_steps from act_new group by interval, day");
str(act_summ_data);
ggplot(act_summ_data, aes(interval, avg_steps)) + geom_line() + facet_wrap(~day,nrow = 2) + labs(title = "Interval vs Steps by Weekday and Weekend") + xlab ("Interval") + ylab(" Number of Steps") 





