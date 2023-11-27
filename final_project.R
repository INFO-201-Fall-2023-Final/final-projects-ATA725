library(dplyr)

EV_pop <- read.csv("Electric_Vehicle_Population_Data.csv")
EV_pop <- EV_pop[EV_pop$State == "WA",]
EV_pop <- subset(EV_pop, Model.Year > 2011, select = c("Model.Year", "County", "City","Make"))

EV_pop_df_region <- group_by(EV_pop,Model.Year, City, County)
EV_pop_df_region <- summarise(EV_pop_df_region,
                       total_cars = n())

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
