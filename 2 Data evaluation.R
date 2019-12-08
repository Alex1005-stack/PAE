library(eeptools)
library(janitor)
library(odbc)
library(DBI)
library(dplyr)
library(lubridate)
library(readr)
library(scales)
library(tidyverse)
library(httr)
library(readxl)
library(Hmisc)
library(gt)

#1. Load relevant datasets

OFFENDER_SUPERVISED_FULL <- read_rds("clean-data/OFFENDER_SUPERVISED_FULL.rds")
INMATE_RELEASE_FULL <- read_rds("clean-data/INMATE_RELEASE_FULL.rds")
INMATE_ACTIVE_FULL <- read_rds("clean-data/INMATE_ACTIVE_FULL.rds")

#2. Attach all data to a county and consolidate the 3 data sets

  # Read in zip codes
  Zip_codes <- read.csv("raw-data/Florida_zip_codes.csv")
  colnames(Zip_codes)[colnames(Zip_codes)=="ï..ZipCode"] <- "ZipCode"

  # Join Counties to Offenders
  OFFENDER_SUPERVISED_COUNTIES <- OFFENDER_SUPERVISED_FULL %>%
    left_join(Zip_codes, by = c("ZipCode", "ZipCode")) %>%
    select(DCNumber, race_descr, County)

  Zip_codes$ZipCode <- as.character(Zip_codes$ZipCode)
  INMATE_RELEASE_FULL$ZipCode <- as.character(INMATE_RELEASE_FULL$ZipCode)
  
  # Join Counties to Released
  INMATE_RELEASE_COUNTIES <- INMATE_RELEASE_FULL %>%
    left_join(Zip_codes, by = c("ZipCode", "ZipCode")) %>%
    select(DCNumber, race_descr, County)
 
  # Limit Active inmates
  INMATE_ACTIVE_COUNTIES <- INMATE_ACTIVE_FULL %>%
    select(DCNumber, race_descr, County)
  
  # Unite data sets for which County is available and that are allowed to vote according to the amendment's literal interpretation
  NEW_VOTING_POPULATION <- rbind(INMATE_RELEASE_COUNTIES, INMATE_ACTIVE_COUNTIES, OFFENDER_SUPERVISED_COUNTIES) %>% filter(!is.na(County))
  
  
#3. Expected partisan effect
  # VOX piece as basis for rough assumption: https://www.vox.com/the-big-idea/2018/11/2/18049510/felon-voting-rights-amendment-4-florida
  # Crist restored the right to vote to about 150,000 ex-felons convicted of less serious offenses. Although Republican Gov. Rick Scott rescinded the policy in 2011, these 150,000 ex-felons remain eligible to vote.  
  #While black voters within this population overwhelmingly register with the Democratic Party (87 percent), nonblack voters within this population were more likely to register as Republicans (40 percent) than as a Democrats (34 percent). The fact that 26 percent of the remaining nonblack voters affiliate with neither party suggests that their votes may not reliably be cast for either party.
  # We find that just 16 percent of black and 12 percent of nonblack ex-felons voted. (We defined nonblack as white, Hispanic, Asian, and other due to data limitations.)
  
  n_rows <- NEW_VOTING_POPULATION %>% nrow()
  
  NEW_VOTING_POPULATION$race_descr <- as.character(NEW_VOTING_POPULATION$race_descr) %>% tolower() %>% capitalize()
  NEW_VOTING_POPULATION$County <- as.character(NEW_VOTING_POPULATION$County) %>% tolower() %>%  capitalize()
  NEW_VOTING_POPULATION$County[NEW_VOTING_POPULATION$County == "Saint johns"] <- "St. johns"
  NEW_VOTING_POPULATION$County[NEW_VOTING_POPULATION$County == "Saint lucie"] <- "St. lucie"
  NEW_VOTING_POPULATION$County[NEW_VOTING_POPULATION$County == "De soto"] <- "Desoto"
  
    NEW_VOTING_POPULATION_COUNTIES <- NEW_VOTING_POPULATION %>% 
    filter(County != "Interstate") %>%
    group_by(County, race_descr) %>%
    summarise(Total_pot_vote = n()) %>%
    mutate(Estimated_D_vote = 
             ifelse(race_descr == "Black", Total_pot_vote*0.16*.87, Total_pot_vote*0.12*.34),
           Estimated_R_vote = 
             ifelse(race_descr == "Black", Total_pot_vote*0.16*.1, Total_pot_vote*0.12*.44),
           Estimated_O_vote = 
             ifelse(race_descr == "Black", Total_pot_vote*0.16*.03, Total_pot_vote*0.12*.36)) %>%
    group_by(County) %>%
    summarise(Total_pot_vote = sum(Total_pot_vote),
              Estimated_D_vote = sum(Estimated_D_vote),
              Estimated_R_vote = sum(Estimated_R_vote),
              Estimated_O_vote = sum(Estimated_O_vote), 
              Share_total_pot_vote = Total_pot_vote/n_rows,
              Share_estimated_D_vote = Estimated_D_vote/n_rows,
              Share_estimated_R_vote = Estimated_R_vote/n_rows,
              Share_estimated_O_vote = Estimated_O_vote/n_rows)
  
#4. Scaling:
    # Assumption: 1,487,847 million people can now vote
    # https://www.sentencingproject.org/publications/6-million-lost-voters-state-level-estimates-felony-disenfranchisement-2016/
  
  NEW_VOTING_POPULATION_COUNTIES_2 <- NEW_VOTING_POPULATION_COUNTIES %>% 
    select(County, Share_total_pot_vote, Share_estimated_D_vote, Share_estimated_R_vote, Share_estimated_O_vote) %>% 
    mutate(Total_pot_vote = round(Share_total_pot_vote*1487847,0),
  Estimated_D_vote = round(Share_estimated_D_vote*1487847,0),
  Estimated_R_vote = round(Share_estimated_R_vote*1487847,0),
  Estimated_O_vote = round(Share_estimated_O_vote*1487847,0))
     
#5. Read in Data from last state wide elections in Florida (2018 Senate elections) per county
  
  Elections_2018 <- read.csv("raw-data/Past-elections/2018_elections_senate.csv")

  Elections_2018_2 <- Elections_2018 %>% group_by(PartyCode, Juris2num) %>%
    summarise(Votes = sum(CanVotes))
  
  Elections_2018_3 <- Elections_2018_2 %>%
    spread(key = PartyCode, value=Votes)
  
  colnames(Elections_2018_3)[colnames(Elections_2018_3)=="Juris2num"] <- "County"
  Elections_2018_3$County <- as.character(Elections_2018_3$County)
  Elections_2018_3$County[Elections_2018_3$County == "Miami-Dade"] <- "Miami-dade"
  
  
#6. Combine data sets to show counties at the margin
  
  Elections_2020 <- Elections_2018_3 %>%
    left_join(NEW_VOTING_POPULATION_COUNTIES_2 , by = c("County", "County")) %>%
    select(County, DEM, REP, Total_pot_vote, Estimated_D_vote, Estimated_R_vote) %>%
    adorn_totals("row")%>%
    mutate(`Incumbent party` = ifelse(DEM > REP, "Democratic Party", "Republican Party"),
           `2018 margin of victory` = abs(DEM-REP),
           `Re-enfranchised voters` = Total_pot_vote,
           `Potential of Amendment 4 to swing the vote` = 
             ifelse(`2018 margin of victory` < `Re-enfranchised voters`, "Yes", "No"),
           `Expected add. Democratic vote` = Estimated_D_vote,
           `Expected add. Republican vote` = Estimated_R_vote,
           `Expected add. margin` = 
             abs(`Expected add. Democratic vote`-`Expected add. Republican vote`),
           `Expected marginal advantage` = 
             ifelse(`Expected add. Democratic vote`>`Expected add. Republican vote`, "Democratic Party", "Republican Party"),
           `Amendment 4 expected to swing vote` = ifelse(`2018 margin of victory` < `Expected add. margin`, "Yes", "No"))
             
#7. Visualize results in table

  Elections_2020$County[Elections_2020$County == "Total"] <- "Statewide"
    
  Elections_2020_table <- Elections_2020 %>%
    select(County, `Incumbent party`, `2018 margin of victory`, `Re-enfranchised voters`, `Potential of Amendment 4 to swing the vote`, `Expected add. margin`, `Expected marginal advantage`, `Amendment 4 expected to swing vote`)
  
  table_1 <- Elections_2020_table %>%
    gt()%>%
    tab_header(
      title = "The electoral significance of Amendment 4 in 2020") %>%
    tab_spanner(
      label = "2018 midterm results",
      columns = vars("Incumbent party", "2018 margin of victory")) %>%
    tab_spanner(
      label = "2020 projection",
      columns = vars("Re-enfranchised voters", "Potential of Amendment 4 to swing the vote", "Expected add. margin", "Expected marginal advantage", "Amendment 4 expected to swing vote")) %>%
    tab_style(
      style = list(
        cell_fill(color = "#FFA500"),
        cell_text(weight = "bold")
      ),
      locations = cells_data(
        columns = vars(`Re-enfranchised voters`,`Potential of Amendment 4 to swing the vote`),
        rows = `Potential of Amendment 4 to swing the vote` == "Yes"))%>%
  tab_style(
    style = list(
      cell_fill(color = "#add8e6"),
      cell_text(weight = "bold")
    ),
  locations = cells_data(
    columns = vars(`Expected add. margin`, `Expected marginal advantage`, `Amendment 4 expected to swing vote`),
    rows = `Amendment 4 expected to swing vote` == "Yes"))
  
  # NEED TO: Add clarifier for total that it is based on the accumulated margin in 2018 and not on the gap in 2016, which was 112911. If that is the basis for assessment, it would not swing.
  # Highlight assumptions in table (Note: Highly simplified mehtodology based on Vox article)
  