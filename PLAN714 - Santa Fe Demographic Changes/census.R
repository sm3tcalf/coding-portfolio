#
# load packages
#

library(sf)
library(tidycensus)
library(tidyverse)
library(tmap)
options(tigris_use_cache = TRUE)

#
# load geoms
#

santaFeBoundaries <- st_read("C:/Users/samdm/OneDrive - University of North Carolina at Chapel Hill/Fall2025/PLAN714/santaFeJurisBound.json") |>
  st_transform(4269) # santa fe municipal boundary

myGeom2023 <- get_acs(
  geography = "tract",
  variables = c(pop = "B01001A_001",
                mdHomeValue = "B25077_001",
                mdOwned = "B25097_002",
                mdRented = "B25097_003",
                mdGrossRent = "B25064_001",
                totalOccupied = "B25002_002",
                ownerOccupied = "B25003_002",
                totalVacant = "B25002_003",
                totalUnits = "B25004_001"
  ),
  state = "NM",
  county = 049,
  year = 2023,
  output = "wide",
  geometry = TRUE
)
glimpse(myGeom2023)

myGeom2020 <- get_acs(
  geography = "tract",
  variables = c(pop = "B01001A_001",
                mdOwned = "B25097_002",
                mdGrossRent = "B25064_001",
                totalOccupied = "B25002_002",
                ownerOccupied = "B25003_002",
                totalVacant = "B25002_003",
                totalUnits = "B25004_001"
                ),
  state = "NM",
  county = 049,
  year = 2020,
  output = "wide",
  geometry = TRUE
)
glimpse(myGeom2020)

myGeom2010 <- get_acs(
  geography = "tract",
  variables = c(pop = "B01001A_001",
                mdOwned = "B25097_002",
                mdGrossRent = "B25064_001",
                totalOccupied = "B25002_002",
                ownerOccupied = "B25003_002",
                totalVacant = "B25002_003",
                totalUnits = "B25004_001"
                ),
  state = "NM",
  county = 049,
  year = 2010,
  output = "wide",
  geometry = TRUE
)
glimpse(myGeom2010)

myGeom2000 <- get_decennial(
  geography = "tract",
  variables = c(pop = "H009001",
                mdOwned = "H085001",
                mdGrossRent = "H063001",
                totalOccupied = "H006001",
                ownerOccupied = "H006002",
                totalVacant = "H006003",
                totalUnits = "H001001"
                ),
  state = "NM",
  county = 049,
  year = 2000,
  output = "wide",
  geometry = TRUE
)
glimpse(myGeom2000)

# 
# calculating fields
#

myGeom2023pcts <- mutate(myGeom2023, 
                         year = 2023,
                         totalUnits = totalOccupiedE + totalVacantE,
                         pctVacant = (totalVacantE / totalUnits)*100,
                         pctOccupied = (totalOccupiedE / totalUnits)*100,
                         pctOwned = (ownerOccupiedE / totalUnits)*100) |>
  rename(pop = popE,
         mdHomeValue = mdHomeValueE,
         mdOwned = mdOwnedE,
         mdRented = mdRentedE,
         mdGrossRent = mdGrossRentE,
         totalOccupied = totalOccupiedE,
         ownerOccupied = ownerOccupiedE,
         totalVacant = totalVacantE) |>
  select(year, GEOID, geometry, NAME, pop, mdOwned, mdGrossRent, totalOccupied, ownerOccupied, totalVacant, totalUnits, pctVacant, pctOccupied, pctOwned)

myGeom2020pcts <- mutate(myGeom2020, 
                         year = 2020,
                         totalUnits = totalOccupiedE + totalVacantE,
                         pctVacant = (totalVacantE / totalUnits)*100,
                         pctOccupied = (totalOccupiedE / totalUnits)*100,
                         pctOwned = (ownerOccupiedE / totalUnits)*100) |>
                  rename(pop = popE,
                         mdHomeValue = mdHomeValueE,
                         mdOwned = mdOwnedE,
                         mdRented = mdRentedE,
                         mdGrossRent = mdGrossRentE,
                         totalOccupied = totalOccupiedE,
                         ownerOccupied = ownerOccupiedE,
                         totalVacant = totalVacantE) |>
                  select(year, GEOID, geometry, NAME, pop, mdOwned, mdGrossRent, totalOccupied, ownerOccupied, totalVacant, totalUnits, pctVacant, pctOccupied, pctOwned)

myGeom2010pcts <- mutate(myGeom2010, 
                         year = 2010,
                         totalUnits = totalOccupiedE + totalVacantE,
                         pctVacant = (totalVacantE / totalUnits)*100,
                         pctOccupied = (totalOccupiedE / totalUnits)*100,
                         pctOwned = (ownerOccupiedE / totalUnits)*100) |>
                  rename(pop = popE,
                         mdHomeValue = mdHomeValueE,
                         mdOwned = mdOwnedE,
                         mdRented = mdRentedE,
                         mdGrossRent = mdGrossRentE,
                         totalOccupied = totalOccupiedE,
                         ownerOccupied = ownerOccupiedE,
                         totalVacant = totalVacantE) |>
                  select(year, GEOID, geometry, NAME, pop, mdOwned, mdGrossRent, totalOccupied, ownerOccupied, totalVacant, totalUnits, pctVacant, pctOccupied, pctOwned)
                  
myGeom2000pcts <- mutate(myGeom2000, 
                         year = 2000,
                         totalUnits = totalOccupied + totalVacant,
                         pctVacant = (totalVacant / totalUnits)*100,
                         pctOccupied = (totalOccupied / totalUnits)*100,
                         pctOwned = (ownerOccupied / totalUnits)*100)

combinedDataframe <- bind_rows(myGeom2020pcts, myGeom2010pcts, myGeom2000pcts) |>
  st_intersection(santaFeBoundaries)

#
# intersect municipal boundaries w/ county census data
#

cuts = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000)

tm_shape(combinedDataframe) +
  tm_polygons("pop",
              fill.legend = tm_legend("Total Population")
  ) +
  tm_facets_wrap("year", ncol = 2, nrow = 2)

tm_shape(combinedDataframe) +
  tm_polygons("pctVacant",
              fill.legend = tm_legend("Housing Vacancy %"),
              fill.scale = tm_scale_intervals(
                values = "greens",
                label.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%")))
  ) +
  tm_facets_wrap("year", ncol = 3)

tm_shape(combinedDataframe) +
  tm_polygons("mdOwned",
              fill.legend = tm_legend("Median Mortgage Value"),
              fill.scale = tm_scale_intervals(
                values = "oranges",
                label.format=list(fun=function(x) paste0("$", formatC(x, digits=0, format="f"))))
  ) +
  tm_facets_wrap("year", ncol = 3)

tm_shape(combinedDataframe) +
  tm_polygons("mdGrossRent",
              fill.legend = tm_legend("Median Gross Rent"),
              fill.scale = tm_scale_intervals(
                values = "blues",
                label.format=list(fun=function(x) paste0("$", formatC(x, digits=0, format="f"))))
  ) +
  tm_facets_wrap("year", ncol = 3)

tm_shape(combinedDataframe) +
  tm_polygons("pctOwned",
              fill.legend = tm_legend("Percent of Units that are Owner-Occupied"),
              fill.scale = tm_scale_intervals(
                values = "reds",
                label.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%")))
  ) +
  tm_facets_wrap("year", ncol = 3)


#
# stats
#

stats <- combinedDataframe |>
  as.data.frame() |>
  drop_na() |>
  group_by(year) |>
  summarize(
    `Median Value of Owner-Occupied Units` = median(mdOwned),
    `Median Gross Rent` = median(mdGrossRent),
    `Total # of Vacancies` = sum(totalVacant),
    `Total # of Units` = sum(totalUnits),
    `Median Tract Vacancy %` = median(pctVacant),
    `Median Tract Occupancy %` = median(pctOccupied),
    `Median Tract % of Units Owned` = median(pctOwned)
  )

