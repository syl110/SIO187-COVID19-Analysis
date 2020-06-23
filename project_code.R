#clear global environment
rm(list=ls())

#load csv files
covid19_county = read.csv('C:\\Users\\sylvi\\OneDrive\\Documents\\UC San Diego\\S20\\SIO187\\Project\\us-counties.csv')
election_results_2016_raw = read.csv('C:\\Users\\sylvi\\OneDrive\\Documents\\UC San Diego\\S20\\SIO187\\Project\\2016_US_County_Level_Presidential_Results.csv')

#remove unwanted cols in election results
election_results_2016 = data.frame(election_results_2016_raw$per_dem, election_results_2016_raw$per_gop, election_results_2016_raw$county_name, election_results_2016_raw$state_abbr)
colnames(election_results_2016) = c('percent_dem', 'percent_rep', 'county', 'state')


# Cleaning Data Sets ------------------------------------------------------

#Alaska is not separated into counties, so the entire state is reported as a county in the 2016 election data.
#We will treat Alaska as one county for this project.


#combine alaska entries, add one entry for the state
election_results_2016 = subset(election_results_2016, county!= 'Alaska', select = c('percent_dem', 'percent_rep', 'county', 'state'))
alaska = data.frame(0.3771595, 0.52887, 'Alaska', 'AK')
names(alaska)= c('percent_dem', 'percent_rep', 'county', 'state')
election_results_2016 = rbind(election_results_2016, alaska)

#add col with party
party = c()
for (percent in election_results_2016$percent_dem){
  if (percent>0.5){
    party = c(party, 'democratic')
  }else {
    party = c(party, 'republican')
  }
}
election_results_2016$party = party

voted_dem = subset(election_results_2016, party=='democratic', select = c('percent_dem', 'percent_rep', 'county', 'state', 'party'))
voted_rep = subset(election_results_2016, party == 'republican', select = c('percent_dem', 'percent_rep', 'county', 'state', 'party'))

#remove 'County' at the end of county names
test_dem = sub(' County', '', voted_dem$county, fixed = T)
voted_dem = subset(voted_dem, select = c('percent_dem', 'percent_rep', 'state', 'party'))
voted_dem$county = test_dem

test_rep = sub(' County', '', voted_rep$county, fixed = T)
voted_rep = subset(voted_rep, select = c('percent_dem', 'percent_rep', 'state', 'party'))
voted_rep$county = test_rep

# list of rep and dem counties
dem_list = tolower(c(voted_dem$county))
rep_list = tolower(c(voted_rep$county))

# make counties in covid19 dataframe lowercase
temp_cov_counties = tolower(covid19_county$county)
covid19_county$county=temp_cov_counties

#add party column to covid 19 data frame
covid_party = c()
for (county in covid19_county$county){
  if (any(dem_list==county)){
    covid_party = c(covid_party, 'democratic')
  }
  else {
    covid_party = c(covid_party, 'republican')
  }
}
covid19_county = data.frame(covid19_county, stringsAsFactors = F)
covid19_county = cbind(covid19_county, covid_party)

#split covid19 data to democratic and republican
covid19_county_dem = subset(covid19_county, covid_party=='democratic', select = c('date', 'county', 'state', 'fips', 'cases', 'deaths', 'covid_party'))
covid19_county_rep = subset(covid19_county, covid_party=='republican', select = c('date', 'county', 'state', 'fips', 'cases', 'deaths', 'covid_party'))


#split by date
attempt = as.character(covid19_county_dem$date)
covid19_county_dem$date = attempt
covid19_county_rep$date = as.character(covid19_county_rep$date)

#insert desired dates:
dates = as.character(seq.Date(as.Date('2020-04-01'), as.Date('2020-05-01'), 'day'))


#creates two lists that have the daily total number of cases for the counties that voted for a certain party using the vector 'dates'
daily_totals_dem = c()

for (i in dates){
  temp_df = subset(covid19_county_dem, covid19_county_dem$date==i)
  daily_totals_dem = c(daily_totals_dem, sum(temp_df$cases))
  
}

daily_totals_rep = c()

for (i in dates){
  temp_df = subset(covid19_county_rep, covid19_county_rep$date==i)
  daily_totals_rep = c(daily_totals_rep, sum(temp_df$cases))
  
}

#uniform distribution of the expected values, divides total number of cases for that day in half
expected_vals = c()

for (i in 1:30){
  to_append = (daily_totals_dem[i]+daily_totals_rep[i])/2
  expected_vals = c(expected_vals, to_append)
}


# 2-sample t-test ------------------------------------------------------------------

# Number of new cases per day:
dem_new_cases = c(diff(daily_totals_dem))
rep_new_cases = c(diff(daily_totals_rep))


# Equal variance
var.test(dem_new_cases, rep_new_cases)

# Normality
shapiro.test(dem_new_cases)
shapiro.test(rep_new_cases)

t.test(dem_new_cases, rep_new_cases, var.equal = T)


# Figures -----------------------------------------------------------------

# normality check for t-test
hist(dem_new_cases, main = 'New Cases of COVID19 in Democratic counties in April',
     xlab = 'Number of new cases', ylab = 'Count')
hist(rep_new_cases, main = 'New Cases of COVID19 in Republican counties in April',
     xlab = 'Number of new cases', ylab = 'Count')

# total cases by time in each party
all_dates_dem = as.character(seq.Date(as.Date('2020-01-21'), as.Date('2020-05-12'), 'day'))
all_dates_rep = as.character(seq.Date(as.Date('2020-01-26'), as.Date('2020-05-12'), 'day'))
all_daily_totals_dem = c()

for (i in all_dates_dem){
  temp_df = subset(covid19_county_dem, covid19_county_dem$date==i)
  all_daily_totals_dem = c(all_daily_totals_dem, sum(temp_df$cases))
  
}

all_daily_totals_rep = c()

for (i in all_dates_rep){
  temp_df = subset(covid19_county_rep, covid19_county_rep$date==i)
  all_daily_totals_rep = c(all_daily_totals_rep, sum(temp_df$cases))
  
}

all_daily_totals_rep = c(0, 0, 0, 0, 0, all_daily_totals_rep)

axis_dates = as.Date(all_dates_dem)
plot(axis_dates, all_daily_totals_dem,type="l",col="blue", main = 'Total COVID19 Cases in Democratic and Republican Counties', xlab = 'Date', ylab = 'COVID19 Cases')
lines(axis_dates, all_daily_totals_rep,col="red")
legend('topleft', legend=c("Republican Counties", "Democratic Counties"),
       col=c("red", "blue"), lwd = 1, cex=0.8)

#total cases in US
axis_dates = as.Date(all_dates_dem)
plot(axis_dates, all_daily_totals_rep+all_daily_totals_dem,type="l", main = 'Total COVID19 Cases in the US', xlab = 'Date', ylab = 'COVID19 Cases')

