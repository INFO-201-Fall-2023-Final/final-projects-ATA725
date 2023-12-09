library(dplyr)
library(ggplot2)

EV_pop <- read.csv("Electric_Vehicle_Population_Data.csv")
EV_pop <- EV_pop[EV_pop$State == "WA",]
EV_pop$Model.Year <- EV_pop$Model.Year-1
EV_pop <- subset(EV_pop, Model.Year > 2011, select = c("Model.Year", "County", "City","Make"))

EV_pop_df_region <- group_by(EV_pop,Model.Year, City, County)
EV_pop_df_region <- summarise(EV_pop_df_region,
                              total_cars = n())

unique_df <- unique(EV_pop[c("County","City")])


EV_pop_df_yr <- group_by(EV_pop, Model.Year)
EV_pop_df_yr <- summarise(EV_pop_df_yr,
                          total_cars = n())

GHG_em <- read.csv("GHG_Reporting_Program_Publication.csv")

GHG_em_region <- GHG_em[, c("Year", "County", "City", "Total.Emissions..MTCO2e.")]

GHG_em_yr <- group_by(GHG_em_region, Year)
GHG_em_yr <- summarise(GHG_em_yr,
                       total_emissions = sum(Total.Emissions..MTCO2e.))

yr_df <- merge(x = EV_pop_df_yr, y = GHG_em_yr, by.x = "Model.Year", by.y = "Year", all.x = TRUE)

region_df <- merge(x = EV_pop_df_region, y = GHG_em_region, by.x = c("Model.Year","County", "City"), by.y = c("Year","County", "City"), all.x = TRUE)
mean <- mean(yr_df$total_emissions, na.rm = TRUE)
yr_df <- mutate(yr_df, emission_category = ifelse(total_emissions > mean, "High", "Low"))


#ch1
total_cars_yr <- function(df, year) {
  filtered_data <- df[df$Model.Year >= year[1] & df$Model.Year <= year[2], ]
  total_cars <- sum(filtered_data$total_cars)
  return(total_cars)
}

ggplot(EV_pop_df_yr, aes(x=Model.Year, y=total_cars)) + geom_line() + labs(title="EV Growth Over Years")

#ch2

# Find the range of total cars and total emissions to set the breaks for the secondary axis
total_cars_in_yr <- function(df, year) {
  filtered_data <- df[df$Model.Year == year, ]
  total_cars <- filtered_data$total_cars
  return(total_cars)
}

total_em_in_yr <- function(df, year) {
  if (year > 2021) {
    return("NULL")
  }
  filtered_data <- df[df$Year == year, ]
  total_emissions <- filtered_data$total_emissions
  return(total_emissions)
}

max_cars <- max(EV_pop_df_yr$total_cars, na.rm = TRUE)
max_emissions <- max(GHG_em_yr$total_emissions, na.rm = TRUE)

ratio <- max_emissions / max_cars

p <- ggplot() +
  geom_line(data=EV_pop_df_yr, aes(x=Model.Year, y=total_cars), color="blue") +
  geom_line(data=GHG_em_yr, aes(x=Year, y=total_emissions / ratio), color="red", linetype="dashed") +
  scale_y_continuous(name="Total EVs",
                     sec.axis=sec_axis(~ . * ratio, name="Total Emissions (MTCO2e)")) +
  labs(title="EV Adoption vs GHG Emissions Over Time", x="Year")

print(p)

#ch3
ev_trend_fun <- function(df, selected_year, selected_city, selected_county) {
  filtered_data <- df[df$Model.Year == selected_year & df$City == selected_city & df$County == selected_county, ]
  if (nrow(filtered_data) == 0) {
    return("Data not available for the selected year, city, and county.")
  }
  total_ev_selected_year <- sum(filtered_data$total_cars)
  total_ev_previous_year <- sum(df[df$Model.Year == (selected_year - 1) & df$City == selected_city & df$County == selected_county, ]$total_cars)
  if (total_ev_selected_year > total_ev_previous_year) {
    return("increasing")
  } else {
    return("decreasing")
  }
}


ggplot(EV_pop_df_region, aes(x=Model.Year, y=total_cars)) +
  geom_point() +
  labs(title="EV Adoption Trends Over Time")

#ch4
#ggplot(region_df, aes(x=total_cars, y=Total.Emissions..MTCO2e.)) + geom_point() + theme_minimal()
