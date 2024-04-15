library(rvest)
library(ggplot2)

ipl_players_df = data.frame()


csk_link = 'https://www.cricbuzz.com/api/html/series/5945/most-runs/3/58/IPL'
rcb_link = 'https://www.cricbuzz.com/api/html/series/5945/most-runs/3/59/IPL'
dc_link = 'https://www.cricbuzz.com/api/html/series/5945/most-runs/3/61/IPL'
mi_link = 'https://www.cricbuzz.com/api/html/series/5945/most-runs/3/62/IPL'
kkr_link = 'https://www.cricbuzz.com/api/html/series/5945/most-runs/3/63/IPL'
rr_link = 'https://www.cricbuzz.com/api/html/series/5945/most-runs/3/64/IPL'
pbks_link = 'https://www.cricbuzz.com/api/html/series/5945/most-runs/3/65/IPL'
srh_link = 'https://www.cricbuzz.com/api/html/series/5945/most-runs/3/255/IPL'
lsg_link = 'https://www.cricbuzz.com/api/html/series/5945/most-runs/3/966/IPL'
gt_link = 'https://www.cricbuzz.com/api/html/series/5945/most-runs/3/971/IPL'


links = c(csk_link, rcb_link, dc_link, mi_link, kkr_link, rr_link, pbks_link, srh_link, lsg_link, gt_link)
teams = c('CSK', 'RCB', 'DC', 'MI', 'KKR', 'RR', 'PBKS', 'SRH', 'LSG', 'GT')

for (link in links) {
  page = read_html(link)
  table_body = html_nodes(page, 'tbody')
  
  table_rows = html_nodes(table_body, 'tr')
  
  player_name = html_nodes(table_rows, 'td:nth-child(2)')
  
  # player innings in 4th column
  player_innings = html_nodes(table_rows, 'td:nth-child(4)')
  
  # player runs in 5th column
  player_runs = html_nodes(table_rows, 'td:nth-child(5)')
  
  # player average in 6th column
  player_average = html_nodes(table_rows, 'td:nth-child(6)')
  
  # player strike rate in 7th column
  player_strike_rate = html_nodes(table_rows, 'td:nth-child(7)')
  
  # append the data to the data frame
  ipl_players_df = rbind(ipl_players_df, data.frame(
    team = teams[links == link],
    name = html_text(player_name),
    innings = html_text(player_innings),
    runs = html_text(player_runs),
    average = html_text(player_average),
    strike_rate = html_text(player_strike_rate)
  ))
}

#Data Integration
# Define the new data
new_data <- data.frame(
  team = c("CSK", "MI", "MI"),
  name = c("Mitchell Santner", "Behrendorff Jason Behrendorff", "Jofra Archer"),
  innings = c(2, 1, 1),
  runs = c(1, 3, 1),
  average = c(2.00, 3.00, 1.00),
  strike_rate = c(50.00, 75.00, 50.00)
)

# Bind the new data to the existing data frame
ipl_players_df <- rbind(ipl_players_df, new_data)


ipl_players_df



# save as csv
write.csv(ipl_players_df, file = 'ipl_players.csv', row.names = FALSE)

# preprocessing the data

## Data Cleaning

### Smooth noisy data

ipl_players_df$runs = as.numeric(gsub('[-]', NA, ipl_players_df$runs))
ipl_players_df$average = as.numeric(gsub('[-]', NA, ipl_players_df$average))
ipl_players_df$strike_rate = as.numeric(gsub('[-]', NA, ipl_players_df$strike_rate))

### Remove duplicates

ipl_players_df = ipl_players_df[!duplicated(ipl_players_df), ]

### Remove rows with NA values

ipl_players_df = na.omit(ipl_players_df)


## Remove rows with 0 runs

ipl_players_df = ipl_players_df[ipl_players_df$runs != 0, ]

## remove rows with 0 innings

ipl_players_df = ipl_players_df[ipl_players_df$innings != 0, ]

## remove rows with 0 average

ipl_players_df = ipl_players_df[ipl_players_df$average != 0, ]

## Handle missing data 

### Replace missing values with mean

ipl_players_df$runs[is.na(ipl_players_df$runs)] = mean(ipl_players_df$runs, na.rm = TRUE)
ipl_players_df$average[is.na(ipl_players_df$average)] = mean(ipl_players_df$average, na.rm = TRUE)
ipl_players_df$strike_rate[is.na(ipl_players_df$strike_rate)] = mean(ipl_players_df$strike_rate, na.rm = TRUE)

# Data Munging 

## Create a new column for player type

ipl_players_df$type = 'Bowler'



## Assign player type based on average and strike rate

ipl_players_df$type[(ipl_players_df$average > 10) & (ipl_players_df$strike_rate > 100)] = 'Batsman'
ipl_players_df$type[(ipl_players_df$runs < 10) & (ipl_players_df$strike_rate < 100)] = 'Bowler'
ipl_players_df$type[(ipl_players_df$average > 35) & (ipl_players_df$strike_rate > 140)] = 'Power Hitter'
ipl_players_df$type[(ipl_players_df$average >= 25) & (ipl_players_df$average <= 35) & (ipl_players_df$strike_rate > 130)] = 'Aggressive Batsman'
ipl_players_df$type[(ipl_players_df$average >= 25) & (ipl_players_df$average <= 35) & (ipl_players_df$strike_rate >= 100) & (ipl_players_df$strike_rate <= 130)] = 'Batting All-rounder'
ipl_players_df$type[(ipl_players_df$average < 25) & (ipl_players_df$strike_rate > 130)] = 'Slogger'
ipl_players_df$type[(ipl_players_df$average < 25) & (ipl_players_df$strike_rate >= 100) & (ipl_players_df$strike_rate <= 130)] = 'Bowling All-rounder'
ipl_players_df$type[(ipl_players_df$average < 15) & (ipl_players_df$strike_rate < 100)] = 'Tailender'


ipl_players_df






### Convert runs, average and strike rate to numeric

ipl_players_df$runs = as.numeric(ipl_players_df$runs)
ipl_players_df$average = as.numeric(ipl_players_df$average)
ipl_players_df$strike_rate = as.numeric(ipl_players_df$strike_rate)

# Data Discretization 

## Create a new column for player performance

ipl_players_df$performance = 'Average'

## Assign player performance based on runs, average and strike rate


mean_runs = mean(ipl_players_df$runs)
mean_average = mean(ipl_players_df$average)
mean_strike_rate = mean(ipl_players_df$strike_rate)

ipl_players_df$performance[ipl_players_df$runs > mean_runs*1.2 & ipl_players_df$average < mean_average & ipl_players_df$strike_rate < mean_strike_rate] = 'Inconsistent'
ipl_players_df$performance[ipl_players_df$runs < mean_runs & ipl_players_df$average > mean_average*1.2 & ipl_players_df$strike_rate < mean_strike_rate] = 'Steady'
ipl_players_df$performance[ipl_players_df$runs < mean_runs & ipl_players_df$average < mean_average & ipl_players_df$strike_rate > mean_strike_rate*1.2] = 'Impactful'
ipl_players_df$performance[ipl_players_df$runs > mean_runs & ipl_players_df$average > mean_average & ipl_players_df$strike_rate > mean_strike_rate] = 'Excellent'
ipl_players_df$performance[ipl_players_df$runs > mean_runs & ipl_players_df$average > mean_average & ipl_players_df$strike_rate < mean_strike_rate] = 'Good'
ipl_players_df$performance[ipl_players_df$runs > mean_runs & ipl_players_df$average < mean_average & ipl_players_df$strike_rate < mean_strike_rate] = 'Average'
ipl_players_df$performance[ipl_players_df$runs < mean_runs & ipl_players_df$average < mean_average & ipl_players_df$strike_rate > mean_strike_rate*0.8] = 'Poor'
ipl_players_df$performance[ipl_players_df$runs < mean_runs & ipl_players_df$average < mean_average & ipl_players_df$strike_rate < mean_strike_rate*0.8] = 'Bad'
ipl_players_df$performance[ipl_players_df$runs > mean_runs*1.2 & ipl_players_df$average > mean_average*1.2 & ipl_players_df$strike_rate > mean_strike_rate*1.2] = 'Dominant'
ipl_players_df$performance[ipl_players_df$runs > mean_runs*1.2 & ipl_players_df$average > mean_average*1.2 & ipl_players_df$strike_rate < mean_strike_rate] = 'Solid'
ipl_players_df$performance[ipl_players_df$runs < mean_runs*0.8 & ipl_players_df$average > mean_average & ipl_players_df$strike_rate > mean_strike_rate] = 'Consistent'






ipl_players_df

# Data Visualization


## Bar plot for player type

ggplot(ipl_players_df, aes(x = type, fill = type)) + geom_bar() + theme_bw() + labs(title = 'Player Type', x = 'Type', y = 'Count')


## Bar plot for player performance

ggplot(ipl_players_df, aes(x = performance, fill = performance)) + geom_bar() + theme_bw() + labs(title = 'Player Performance', x = 'Performance', y = 'Count')

## Box plot for runs

ggplot(ipl_players_df, aes(x = type, y = runs, fill = type)) + geom_boxplot() + theme_bw() + labs(title = 'Runs', x = 'Type', y = 'Runs')


## Measure the team performance by runs, strike rate, average, moving average and moving average of strike rate

### Runs

ggplot(ipl_players_df, aes(x = team, y = runs, fill = team)) + geom_boxplot() + theme_bw() + labs(title = 'Runs', x = 'Team', y = 'Runs')

### Strike Rate

ggplot(ipl_players_df, aes(x = team, y = strike_rate, fill = team)) + geom_boxplot() + theme_bw() + labs(title = 'Strike Rate', x = 'Team', y = 'Strike Rate')


### Average

ggplot(ipl_players_df, aes(x = team, y = average, fill = team)) + geom_boxplot() + theme_bw() + labs(title = 'Average', x = 'Team', y = 'Average')


#


