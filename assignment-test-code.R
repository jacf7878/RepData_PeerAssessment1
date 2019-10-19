# Loading and preprocessing the data
        library(dplyr)
        library(ggplot2)
        df <- read.csv("activity.csv", header=TRUE, sep=",", na.strings = "NA")
        dim(df)
        str(df)
        df$dateOK <- as.Date(df$date, "%Y-%m-%d")
        head(df)

#1.What is mean total number of steps taken per day?

#1.1 Calculate the total number of steps taken per day
        dfPerDay <- df %>% group_by(dateOK) %>% summarise(totalSteps = sum(steps, na.rm = TRUE))  
        head(dfPerDay)

#1.2 Make a histogram of the total number of steps taken each day
        ggplot(dfPerDay, aes(x=totalSteps)) + 
                geom_histogram(bins = 30, position="identity", alpha=0.4, col="orange", fill="green")
        
        
#1.3 Calculate and report the mean and median of the total number of steps taken per day
        #total mean
        mean(dfPerDay$totalSteps)
        #total median
        median(dfPerDay$totalSteps)
        #mean per day
        dfMeanPerDay <- df %>% group_by(dateOK) %>% summarise(meanSteps = mean(steps, na.rm = TRUE))  
        head(dfMeanPerDay)
        #median per day
        dfMedianPerDay <- df %>% group_by(dateOK) %>% summarise(meanSteps = median(steps, na.rm = TRUE))  
        head(dfMedianPerDay)
        
#2.What is the average daily activity pattern?

#2.1  Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average 
#number of steps taken, averaged across all days (y-axis)
        #calculate means per interval (across all days)
        dfPerInterval <- df %>% group_by(interval) %>% summarise(meanSteps = mean(steps, na.rm = TRUE))  
        head(dfPerInterval)
        
        #plotting
        ggplot(dfPerInterval, aes(x=interval, y=meanSteps)) + 
                geom_line(color="#00AFBB", size=1) +
                theme_minimal()


#2.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum
#number of steps?        
        
        dfPerInterval[dfPerInterval$meanSteps == max(dfPerInterval$meanSteps),]
        
#3. Imputing missing values

#3.1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

        sapply(df, function(x) sum(is.na(x)))
        
#3.2 Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the mean/median for that day,
#or the mean for that 5-minute interval, etc.

#Strategy: using mean of the 5-minute interval to fill NAs occurrences
        
#3.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.
        dfOK <- merge(df, dfPerInterval)
        dfOK <- dfOK %>% mutate(stepsOK = ifelse(is.na(steps),meanSteps,steps))                

#3.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean 
#and median total number of steps taken per day. Do these values differ from the estimates from the first part
#of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
        #prepare data
        dfPerDayOK <- dfOK %>% group_by(dateOK) %>% summarise(totalSteps = sum(stepsOK))  
        head(dfPerDayOK)
        
        #Making a histogram 
        ggplot(dfPerDayOK, aes(x=totalSteps)) + 
                geom_histogram(bins = 30, position="identity", alpha=0.4, col="orange", fill="blue")
        
        
        #Calculating mean and median total
        mean(dfPerDayOK$totalSteps)
        median(dfPerDayOK$totalSteps)
        #Calculating mean and median per day
        dfMeanPerDayOK <- dfOK %>% group_by(dateOK) %>% summarise(meanSteps = mean(stepsOK))  
        head(dfMeanPerDayOK)
        #median per day
        dfMedianPerDayOK <- dfOK %>% group_by(dateOK) %>% summarise(medianSteps = median(stepsOK))  
        head(dfMedianPerDayOK)

#4. Are there differences in activity patterns between weekdays and weekends?
        
#4.1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
#indicating whether a given date is a weekday or weekend day.
        #instead of using weekdays function I prefer use as.POSIXlt()$wday construction
        dfOK <- dfOK %>% mutate(daytype = ifelse(as.POSIXlt(dfOK$dateOK)$wday==0,"weekend",ifelse(as.POSIXlt(dfOK$dateOK)$wday==6,"weekend","weekday")))                       
        View(dfOK)
#4.2 Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval 
#(x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).        
        
        #calculate means per interval and per daytype 
        dfPerIntervalOK <- dfOK %>% group_by(interval,daytype) %>% summarise(meanSteps = mean(stepsOK, na.rm = TRUE))  
        head(dfPerIntervalOK)
        
        #plotting
        ggplot(dfPerIntervalOK, aes(x=interval, y=meanSteps)) +
                facet_grid(daytype~.) +
                geom_line(color="blue", size=1) +
                theme_minimal()
        
