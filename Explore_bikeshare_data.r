###############
#
# Author: Karajo Kafka
#
# Description: Use information from csv files to analyse data
#   and answer three questions in R programming
#   with different charts and summaries.
#
###############

nyc = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(nyc)

head(wash)

head(chi)

# What is the most popular time of month and city to travel from?

# Make sure library is enabled for plot use
library(ggplot2)

# Using Function to get travel start month without rest of date/time
getStartMonthNumber = function (city.Time){
  return (format(as.Date(city.Time), "%m"))
}

# Create initial plot with Chicago's start months, setting the points to be red circles 
plot.default(as.data.frame(table(getStartMonthNumber(chi$Start.Time))), type = "o", col = 'blue',
            # Set the axis labels and graph title
            xlab = "Month", ylab = "Travelers", main = "Popular Travel Months",
            # Set range of entire graph to include maximum range of all the points
              # or else subsequent lines won't appear.
            ylim = c(0,22500))
# Plot green line for NYC on Chicago Graph
lines.default(as.data.frame(table(getStartMonthNumber(nyc$Start.Time))), type = "o", col = 'green')
# Plot blue line for Washington on Chicago graph
lines.default(as.data.frame(table(getStartMonthNumber(wash$Start.Time))), type = "o", col = 'red')

# Reference for Legend: https://www.r-graph-gallery.com/119-add-a-legend-to-a-plot.html
legend("topleft",     # Location of legend
  legend = c("Chicago", "NYC", "Washington"), # Name of Lines
  col = c('blue', 'green', 'red'), # Color of lines
  pch = c(1,1,1),     # Symbol Type
  bty = "n",          # Hide surrounding box
  pt.cex = 1.5)       # Symbol Size

# Summary
# Get data frame of number of Chicago travelers over the 6 months
chicagoTravelers = as.data.frame(table(getStartMonthNumber(chi$Start.Time)))

# Combine with other data frames
Travelers <- cbind(chicagoTravelers, 
    # Add number of NYC travelers over 6 months as a new column to Chicaco
    as.data.frame(table(getStartMonthNumber(nyc$Start.Time)))[2],
    # Add number of Washington travelers over 6 months as a new column to Chicaco
    as.data.frame(table(getStartMonthNumber(wash$Start.Time)))[2])

# Using For-loop to rename columns of data frame table
chart.Titles = c("Month", "Chicago", "NYC", "Washington")
count = 1 
for (name in chart.Titles){
    names(Travelers)[count] = name # Set the name of the columns as listed in chart.Titles
    count = count + 1              # Increase the index per loop
}

# Print Travelers Table
cat("List of Travelers per city per month:") # 'cat' works better than printing
Travelers                                    # Show table

# Summary Function to get max, min, quartile, median, mean
cat("\nSummary of max, min, quartile, median, mean:") # 'cat' allows formatting
summary(Travelers)

# Analysis:
  # The most popular time of month to travel is June for all three cities as shown in the graph (highest point on 6th month)
  # The summary supports this with the 6th month having the highest value of 2816, 14000, and 20335 
    # for Chicago, NYC, and Washington respectfully.

# Which city has the most number of subscribers?

# Primary Source for double plotting on ggplot and also for getting legend by Philip Merkle: 
  # https://stackoverflow.com/questions/42186060/overlay-two-bar-plots-with-geom-bar
# Secondary source not used but still informational with an alternative way: 
  # https://stackoverflow.com/questions/34204198/how-to-superimpose-bar-plots-in-r

# Getting first table with chiUsers
washUsers <- as.data.frame(table(wash$User.Type))

# Combining chicago users with NYC and Washington
travelerTypes <- cbind(washUsers, 
                       as.data.frame(table(nyc$User.Type))[2], 
                       as.data.frame(table(chi$User.Type))[2])

# Rename columns in table
names(travelerTypes)[1] <- "UserTypes"
names(travelerTypes)[2] <- "Washington"
names(travelerTypes)[3] <- "NYC"
names(travelerTypes)[4] <- "Chicago"

# Remove first row with people that are neither customers or subscribers
travelerTypes = travelerTypes[-1,]

# Include library to convert values into long format
library(reshape2)
travelerTypeLong <- melt(travelerTypes, id.vars = c("UserTypes")) # Converting to long format
                                                                    # Where "variable" equals the columns and
                                                                    # "value" equals the numerical values
ggplot(data=travelerTypeLong, aes(x=UserTypes, y=value, fill=variable, color=variable, alpha=variable)) +
  geom_bar(stat="identity", position ="identity") +
  # Set graph title and axis labels
  labs(x = "User Types", y = "Number of Users", title = "Number of Customers and Subscribers") +
  # Formatting the legend
  scale_colour_manual(values=c("red", "green", "blue")) +   # Border color
  scale_fill_manual(values=c("red", "green", "blue")) +     # Bar fill color
  scale_alpha_manual(values=c(.3, .3, .3))                  # Transparency

# Summary
cat("List of User Types per each city:")
travelerTypes # Display table count
  # Even though a subset filter was used to factor out the null variables, 
    # there are still some blank values where users were not customers or subscribers.
  # Because a row had to be removed from the table, there's a row number in the summary showing the first row missing.


# Analysis
  # For all three cities, there were more Subscribers than Customers as shown in the graph.
  # This is supported by the resulting table that shows Washington, NYC, and Chicago with more
    # Subscribers (65600, 49093, and 6883 respectively) than Customers (2345, 5558, and 1746 respectively)

# Which city do users travel the longest?

plot(wash$Trip.Duration, type = "h", col = "red",
     xlab = "Duration", ylab = "Travelers", main = "Duration of Travelers per City",
     ylim = c(0,1100000), xlim = c(0,91000))
lines(nyc$Trip.Duration, col = "green", type = 'h')
lines(chi$Trip.Duration, col = "blue", type = 'h')

# Reference for Legend: https://www.r-graph-gallery.com/119-add-a-legend-to-a-plot.html
legend("topleft",     # Location of legend
  legend = c("Chicago", "NYC", "Washington"), # Name of Lines
  col = c('blue', 'green', 'red'), # Color of lines
  pch = c(16,16,16),     # Symbol Type
  bty = "n",          # Hide surrounding box
  pt.cex = 1.5)       # Symbol Size

# Summary to find stats of female versus male travel duration
cat("\nSummary of max, min, quartile, median, mean:")
cat("\n\nChicago:")
summary(as.data.frame(chi$Trip.Duration)) # Get min, max, mean, median, and quartile
cat("New York City:")
summary(as.data.frame(nyc$Trip.Duration))
cat("Washington:")
summary(as.data.frame(wash$Trip.Duration))

# Analysis: 
  # From the graph it is apparent that the the longest duration is by a person in NYC, though,
  # using the summary, Washington users travel more than NYC and Chicago users on average.
# Notes:
  # Couldn't combine the tables using cbind because their lengths were different

system('python -m nbconvert Explore_bikeshare_data.ipynb')
