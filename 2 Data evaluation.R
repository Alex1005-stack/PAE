library(eeptools)
library(janitor)
library(odbc)
library(gridExtra)
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
library(webshot)
library(tm)
library(stringr)
library(ggthemes)

#1. Load relevant datasets

OFFENDER_SUPERVISED_FULL <- read_rds("clean-data/OFFENDER_SUPERVISED_FULL.rds")
INMATE_RELEASE_FULL <- read_rds("clean-data/INMATE_RELEASE_FULL.rds")
INMATE_ACTIVE_FULL <- read_rds("clean-data/INMATE_ACTIVE_FULL.rds")
Registration_data_mtach <- read_rds("clean-data/registration_data_red.rds")

glimpse(OFFENDER_SUPERVISED_FULL)
glimpse(INMATE_RELEASE_FULL)
glimpse(INMATE_ACTIVE_FULL)

#2. Attach all data to a county and consolidate the 3 data sets

  # Read in zip codes
  Zip_codes <- read.csv("raw-data/Florida_zip_codes.csv")
  colnames(Zip_codes)[colnames(Zip_codes)=="ï..ZipCode"] <- "ZipCode"

  # Join Counties to Offenders
  OFFENDER_SUPERVISED_COUNTIES <- OFFENDER_SUPERVISED_FULL %>%
    left_join(Zip_codes, by = c("ZipCode", "ZipCode")) %>%
    select(DCNumber, LastName, FirstName, Sex, BirthDate, race_descr, County)

  Zip_codes$ZipCode <- as.character(Zip_codes$ZipCode)
  INMATE_RELEASE_FULL$ZipCode <- as.character(INMATE_RELEASE_FULL$ZipCode)
  
  # Join Counties to Released
  INMATE_RELEASE_COUNTIES <- INMATE_RELEASE_FULL %>%
    left_join(Zip_codes, by = c("ZipCode", "ZipCode")) %>%
    select(DCNumber, LastName, FirstName, Sex, BirthDate, race_descr, County)
 
  glimpse(INMATE_RELEASE_FULL)
  
  # Limit Active inmates
  INMATE_ACTIVE_COUNTIES <- INMATE_ACTIVE_FULL %>%
    select(DCNumber, LastName, FirstName, Sex, BirthDate, race_descr, County)
  
  # Unite data sets for which County is available and that are allowed to vote according to the amendment's literal interpretation
  NEW_VOTING_POPULATION_prep <- rbind(INMATE_RELEASE_COUNTIES, INMATE_ACTIVE_COUNTIES, OFFENDER_SUPERVISED_COUNTIES) %>% filter(!is.na(County))
  
  
#3. Prepare corrrectional data set for matching
  # Matching logik:changing all text in both databases to lowercase, removing all punctuation, concatenating a string consisting of a person’s first name, last name, name suffix, date of birth, race code, and sex code, and then matching the concatenated strings across both datasets.
  
  # 3.a Add a new column in format of Race and Birth Date in correctional data to match voter registration files
  
  Race_code <- read_csv("raw-data/Race_code.csv")
  
  NEW_VOTING_POPULATION_prep_1 <- NEW_VOTING_POPULATION_prep %>% separate(BirthDate, into = c("Year", "Month", "Day"), sep = "-") %>% mutate(`Birth Date` = paste(Month, Day, Year, sep = ""))
  
  NEW_VOTING_POPULATION_prep_2 <-NEW_VOTING_POPULATION_prep_1 %>%
                              left_join(Race_code, by = c("race_descr" = "Correctional"))
  
  # 3.b Removing all punctuation
  
  NEW_VOTING_POPULATION_prep_2$LastName <- removePunctuation(as.character(NEW_VOTING_POPULATION_prep_2$LastName))
  
  NEW_VOTING_POPULATION_prep_2$FirstName <- removePunctuation(as.character(NEW_VOTING_POPULATION_prep_2$FirstName))
  
  NEW_VOTING_POPULATION_prep_2$`Birth Date` <- removePunctuation(NEW_VOTING_POPULATION_prep_2$`Birth Date`)

  # 3.b Concatennating it all to one string
  
  NEW_VOTING_POPULATION_prep_2 <- NEW_VOTING_POPULATION_prep_2 %>% 
    mutate(string = paste(LastName, FirstName, Sex, Number, `Birth Date`, sep=""))
  
  # 3.c Make string lower case
  
  NEW_VOTING_POPULATION_prep_2$string <- tolower(NEW_VOTING_POPULATION_prep_2$string)
  
  glimpse(NEW_VOTING_POPULATION_prep_2)
  
  
#4. Match voter registration files to correctional data:  
  
  # People registered to vote
  NEW_VOTING_POPULATION_full_registered <-NEW_VOTING_POPULATION_prep_2 %>%
    inner_join(Registration_data_mtach, by = c("string" = "string"))
  
  # Party affiliation of people registered to vote
  NEW_VOTING_POPULATION_full_registered %>% 
    dplyr:: group_by(`Party Affiliation`) %>% 
    dplyr:: summarise(n = dplyr:: n())
  
  # Update addresses
    # For People registered to vote, assign a county based on ZIP
    NEW_VOTING_POPULATION_REG_COUNTIES <- NEW_VOTING_POPULATION_full_registered %>%
      left_join(Zip_codes, by = c("Residence Zipcode" = "ZipCode")) %>%
      select(DCNumber, race_descr, County.x, County.y, Gender)
    
    # See share of appropriate county assignment based on correctional data
    NEW_VOTING_POPULATION_REG_COUNTIES %>% 
      mutate(County_acc = ifelse(as.character(County.x) == as.character(County.y), "TRUE", "FALSE")) %>% 
      dplyr:: group_by(County_acc) %>% 
      dplyr:: summarise(n = dplyr::n())
    
    # See share of appropriate ZIP code assignment based on correctional data
    
      # Create a new dataset that includes ZIP and street name, based on DCNumbers 
    INMATE_RELEASE_COUNTIES_ZIP_STREET <- INMATE_RELEASE_FULL %>% 
      select(DCNumber, ZipCode, AddressLine1)
    
    
    OFFENDER_SUPERVISED_COUNTIES_ZIP_STREET <- OFFENDER_SUPERVISED_FULL %>% 
      select(DCNumber, ZipCode, AddressLine1)
    
    # Unite data sets for which ZIP & Street are available and that are allowed to vote according to the amendment's literal interpretation
    NEW_VOTING_POPULATION_ZIP_STREET <- rbind(INMATE_RELEASE_COUNTIES_ZIP_STREET, OFFENDER_SUPERVISED_COUNTIES_ZIP_STREET) %>% filter(!is.na(ZipCode), !is.na(AddressLine1))
    
    
    NEW_VOTING_POPULATION_REG_ZIP_STREET <- NEW_VOTING_POPULATION_full_registered %>% left_join(NEW_VOTING_POPULATION_ZIP_STREET, by = c("DCNumber" = "DCNumber"))
    
    # ZIP share
    NEW_VOTING_POPULATION_REG_ZIP_STREET %>% 
      mutate(ZIP_acc = ifelse(as.character(`Residence Zipcode`) == as.character(ZipCode), "TRUE", "FALSE")) %>% 
      dplyr:: group_by(ZIP_acc) %>% 
      dplyr:: summarise(n = dplyr::n())
    
    # See share of appropriate exact address based on correctional data
    
    NEW_VOTING_POPULATION_REG_ZIP_STREET$`Residence Address Line 1` <- removePunctuation(as.character(NEW_VOTING_POPULATION_REG_ZIP_STREET$`Residence Address Line 1`)) 
    
    NEW_VOTING_POPULATION_REG_ZIP_STREET$AddressLine1 <- removePunctuation(as.character(NEW_VOTING_POPULATION_REG_ZIP_STREET$AddressLine1)) %>% str_replace_all(AddressLine1, fixed(" "), "")
    
    NEW_VOTING_POPULATION_REG_ZIP_STREET$`Residence Address Line 1` <- gsub('\\s+', '', NEW_VOTING_POPULATION_REG_ZIP_STREET$`Residence Address Line 1`)

    NEW_VOTING_POPULATION_REG_ZIP_STREET$AddressLine1 <- gsub('\\s+', '', NEW_VOTING_POPULATION_REG_ZIP_STREET$AddressLine1)
    
    # Make string lower case
    
    NEW_VOTING_POPULATION_REG_ZIP_STREET$AddressLine1 <- tolower(NEW_VOTING_POPULATION_REG_ZIP_STREET$AddressLine1)
    NEW_VOTING_POPULATION_REG_ZIP_STREET$`Residence Address Line 1` <- tolower(NEW_VOTING_POPULATION_REG_ZIP_STREET$`Residence Address Line 1`)
    
    # Share of Addresses 
    NEW_VOTING_POPULATION_REG_ZIP_STREET %>% 
      mutate(Address_acc = ifelse(as.character(`Residence Address Line 1`) == as.character(AddressLine1 ), "TRUE", "FALSE")) %>% 
      dplyr:: group_by(Address_acc) %>% 
      dplyr:: summarise(n = dplyr::n())
    
    # Update addresses 
    NEW_VOTING_POPULATION_REG_COUNTIES_update <- NEW_VOTING_POPULATION_REG_COUNTIES %>% select(DCNumber,County.y, Gender)
    
    NEW_VOTING_POPULATION_full <-NEW_VOTING_POPULATION_prep_2 %>%
    left_join(Registration_data_mtach, by = c("string" = "string"))
  
  # Reduce to just county, race, sex 
  NEW_VOTING_POPULATION_v1 <- NEW_VOTING_POPULATION_prep %>%
    left_join(NEW_VOTING_POPULATION_REG_COUNTIES_update, by = c("DCNumber","DCNumber")) %>% mutate(County_new = ifelse(is.na(County.y), as.character(County), as.character(County.y))) %>% select(DCNumber, race_descr, County_new, Sex)
  
  # Harmonize county names and race description
  NEW_VOTING_POPULATION_v1$County_new <- as.character(NEW_VOTING_POPULATION_v1$County_new) %>% tolower() %>%  capitalize() 
  
  NEW_VOTING_POPULATION_v1$County_new[NEW_VOTING_POPULATION_v1$County_new == "Saint johns"] <- "St. johns"
  NEW_VOTING_POPULATION_v1$County_new[NEW_VOTING_POPULATION_v1$County_new == "Saint lucie"] <- "St. lucie"
  NEW_VOTING_POPULATION_v1$County_new[NEW_VOTING_POPULATION_v1$County_new == "De soto"] <- "Desoto"
  
  NEW_VOTING_POPULATION_v1$race_descr <- as.character(NEW_VOTING_POPULATION_v1$race_descr) %>% tolower() %>% capitalize()
  
  NEW_VOTING_POPULATION_v1 <- NEW_VOTING_POPULATION_v1 %>%
    filter(County_new != "Interstate")
  
  
#5. Create timeline for voter registration
  
  # Turn registration date into an actual date of Months and years

  NEW_VOTING_POPULATION_full_registered$`Registration Date` <- 
  
  NEW_VOTING_POPULATION_full_registered_1 <- NEW_VOTING_POPULATION_full_registered %>%
    mutate(Reg_date = as.Date(`Registration Date`, "%m/%d/%Y")) %>%
    filter(Reg_date >= "2018-10-01")
      
  
  # Visualize timeline
  NEW_VOTING_POPULATION_full_registered_1 %>% 
    ggplot(aes(x = Reg_date))+
    geom_bar()+
    geom_vline(xintercept = as.Date("2018-11-06"),size = 0.2, color = "red", show.legend = FALSE)+
    geom_text(aes(x = as.Date("2018-11-06"), 
                  y = 140,
                  label = "1", 
                  vjust = 1,
                  hjust= 1.5,
                  color = "red")) +
    geom_vline(xintercept = as.Date("2019-01-08"),size = 0.2, color = "red", show.legend = FALSE)+
    geom_text(aes(x = as.Date("2019-01-08"), 
                  y = 140,
                  label = "2", 
                  vjust = 1,
                  hjust= 1.5,
                  color = "red")) +
    geom_vline(xintercept = as.Date("2019-05-03"),size = 0.2, color = "red", show.legend = FALSE)+
    geom_text(aes(x = as.Date("2019-05-03"), 
                  y = 140,
                  label = "3", 
                  vjust = 1,
                  hjust= 1.5,
                  color = "red")) +
    geom_vline(xintercept = as.Date("2019-07-01"),size = 0.2, color = "red", show.legend = FALSE)+
    geom_text(aes(x = as.Date("2019-07-01"), 
                  y = 140,
                  label = "4", 
                  vjust = 1,
                  hjust= 1.5,
                  color = "red")) +
    geom_vline(xintercept = as.Date("2019-10-18"),size = 0.2, color = "red", show.legend = FALSE)+
    geom_text(aes(x = as.Date("2019-10-18"), 
                  y = 140,
                  label = "5", 
                  vjust = 1,
                  hjust= 1.5,
                  color = "red")) +
    geom_vline(xintercept = as.Date("2020-01-16"),size = 0.2, color = "red", show.legend = FALSE)+
    geom_text(aes(x = as.Date("2020-01-16"), 
                  y = 140,
                  label = "6", 
                  vjust = 1,
                  hjust= 1.5,
                  color = "red"))+
    scale_y_continuous(expand = c(0, 0), limits = c(0,150))+
    
    # add labels
    
    labs(x="Registration date", y = "# of people registering",
         caption = "Source: Florida Division of Elections, Florida Department of Corrections, own Analysis.")+
    
    # Aesthetics
    
    theme_fivethirtyeight()+
    theme(axis.title = element_text(colour = "black", size = 12),
          plot.title = element_text(size = 14, hjust = 0),
          plot.subtitle = element_text(size = 10, hjust = 0),
          plot.caption = element_text(size = 10, hjust = 0))+ 
    theme(legend.position = "none")
  

#6. Create table to show which demographic feels engaged (is registered) and which demographic is not yet registered  (County, Race, Sex, Age, Party affiliation (for those registered))
  
  # Registered / Unregistered

  # Racial split (%)
   Racial_split_reg <- NEW_VOTING_POPULATION_full_registered_1 %>% 
      dplyr:: group_by(race_descr) %>%
      dplyr:: summarise(Registered = dplyr:: n()) %>% 
      mutate(Share = Registered/ sum(Registered))
   
   Racial_split_tot <- NEW_VOTING_POPULATION_v1 %>% 
     dplyr:: group_by(race_descr) %>%
     dplyr:: summarise(Total = dplyr:: n()) %>% 
     mutate(Share = Total/ sum(Total))
  
   Racial_split <- Racial_split_tot %>%
     left_join(Racial_split_reg, by = c("race_descr","race_descr"))
   
   # Table on racial split
      
      # Formate columns
   
   names(Racial_split) <- c("Race", "No_a", "Share_a", "No_r", "Share_r")
   Racial_split$Race <- Racial_split$Race %>% tolower() %>% capitalize()
   
   Racial_split_1 <- Racial_split %>% 
     arrange(desc(Share_a)) %>%
     adorn_totals("row") %>%
     mutate(Share_a = round(Share_a * 100, 0), 
            Share_r = round(Share_r * 100, 0)) 
      
      # Create gt table   
   table_racial_split <-
     Racial_split_1 %>%
     gt()%>%
     tab_header(
       title = "Racial split between registered enfranchised voters and total",
       subtitle = "December 2019") %>%
     tab_spanner(
       label = "All enfranchised voters",
       columns = vars("No_a", "Share_a")) %>%
     tab_spanner(
       label = "Registered enfranchised voters",
       columns = vars("No_r", "Share_r")) %>%
     tab_source_note(
       source_note = "Source: Florida Division of Elections, Florida Department of Corrections, own analysis.")
       
    # Sex split (%)
   Sex_split_reg <- NEW_VOTING_POPULATION_full_registered_1 %>% 
      dplyr:: group_by(Sex) %>%
      dplyr:: summarise(Registered = dplyr:: n()) %>% 
      mutate(Share = Registered/ sum(Registered))
    
   Sex_split_tot <- NEW_VOTING_POPULATION_v1 %>% 
      dplyr:: group_by(Sex) %>%
      dplyr:: summarise(Total = dplyr:: n()) %>% 
      mutate(Share = Total/ sum(Total))
   
   Sex_split <- Sex_split_tot %>%
     left_join(Sex_split_reg, by = c("Sex", "Sex"))

   # Table on sex split
   
   # Formate columns
   
   names(Sex_split) <- c("Sex", "No_a", "Share_a", "No_r", "Share_r")
   
   Sex_split_1 <- Sex_split %>% 
     arrange(desc(Share_a)) %>%
     adorn_totals("row") %>%
     mutate(Share_a = round(Share_a * 100, 0), 
            Share_r = round(Share_r * 100, 0)) 
   
   # Create gt table   
   table_sex_split <-
     Sex_split_1 %>%
     gt()%>%
     tab_header(
       title = "Gender split between registered enfranchised voters and total",
       subtitle = "December 2019") %>%
     tab_spanner(
       label = "All enfranchised voters",
       columns = vars("No_a", "Share_a")) %>%
     tab_spanner(
       label = "Registered enfranchised voters",
       columns = vars("No_r", "Share_r")) %>%
     tab_source_note(
       source_note = "Source: Florida Division of Elections, Florida Department of Corrections, own analysis.")
   
  # Party affiliation split (%)  
   Party_split <- NEW_VOTING_POPULATION_full_registered_1 %>% 
      dplyr:: group_by(`Party Affiliation`) %>%
      dplyr:: summarise(Registered = dplyr:: n()) %>% 
      mutate(Share = Registered/ sum(Registered))
    
    # Table on party affiliation split
   
   names(Party_split) <- c("Party", "No_r", "Share_r")
   
   Party_split_1 <- Party_split %>% 
     arrange(desc(Share_r)) %>%
     adorn_totals("row") %>%
     mutate(Share_r = round(Share_r * 100, 0))
    
    table_party_split <-
      Party_split_1 %>%
      gt()%>%
      tab_header(
        title = "Party affiliation for registered enfranchised voters",
        subtitle = "December 2019") %>%
      tab_source_note(
        source_note = "Source: Florida Division of Elections, Florida Department of Corrections, own analysis.")
    
    
#7. Reduce the voting population based on estimation of outstanding Legal Financial Obligations at county level
  
  LFOs <- read.csv("raw-data/Legal Financial Obligations.csv")
  
  LFOs <- LFOs %>% 
    mutate(Join = paste(County, Race, sep = "")) %>%
    select(Join, Total.share.of.eligible.voters)
  
  NEW_VOTING_POPULATION_COUNTIES_v1 <- NEW_VOTING_POPULATION_v1 %>% 
    dplyr:: group_by(County_new, race_descr) %>% 
    dplyr:: summarise(Total_individuals = dplyr::n()) %>%
    mutate(Race_new = ifelse(race_descr == "Black", "Black", "White & others"), Join = paste(County_new, Race_new, sep = "")) %>%
    left_join(LFOs, by = c("Join","Join")) %>%
    mutate(Total_exp_pot_vote = Total_individuals * Total.share.of.eligible.voters)
  

#8. Turnout shares for scaling: 
  
# Assumptions  

  # Total eligible population - from scaling Excel, cell Total number of individuals qualified as having paid 
  
  n_active <- INMATE_ACTIVE_ROOT_2 %>% nrow()
  n_released <- INMATE_RELEASE_Count %>% nrow()
  n_offenders <- OFFENDER_SUPERVISED_Count %>% nrow()
  
  total_sample_pop <- sum(n_active, n_released, n_offenders)
  
  total_sample_pop_eligible <- round(sum(NEW_VOTING_POPULATION_COUNTIES_v1$Total_exp_pot_vote),0)
  
  # Scaling:
  # Assumption: 355108 million people can now vote
  # https://www.sentencingproject.org/publications/6-million-lost-voters-state-level-estimates-felony-disenfranchisement-2016/
  
  total_pop_eligible <-  1487847*(total_sample_pop_eligible/total_sample_pop)
  
  # Basis for share
  n_medium <- round(sum(NEW_VOTING_POPULATION_COUNTIES_v1$Total_exp_pot_vote),0)
  
  # Partisan allocation
  Party_split
  D_reg <- Party_split$Share_r[Party_split$Party == "DEM"]
  R_reg <- Party_split$Share_r[Party_split$Party == "REP"]
  O_reg <- 1 - R_reg - D_reg
  
  # VOX piece as basis for rough assumption: https://www.vox.com/the-big-idea/2018/11/2/18049510/felon-voting-rights-amendment-4-florida
  # Crist restored the right to vote to about 150,000 ex-felons convicted of less serious offenses. Although Republican Gov. Rick Scott rescinded the policy in 2011, these 150,000 ex-felons remain eligible to vote.  
  #While black voters within this population overwhelmingly register with the Democratic Party (87 percent), nonblack voters within this population were more likely to register as Republicans (40 percent) than as a Democrats (34 percent). The fact that 26 percent of the remaining nonblack voters affiliate with neither party suggests that their votes may not reliably be cast for either party.
  # We find that just 16 percent of black and 12 percent of nonblack ex-felons voted. (We defined nonblack as white, Hispanic, Asian, and other due to data limitations.)
  
  D_B_vox <- .87
  D_W_vox <- .34
  R_B_vox <- .1
  R_W_vox <- .44
  O_B_vox <- .03
  O_W_vox <- .22
  
  
  # Spillover effect for each additional person voting:
  spill <- 1.72
  
  #A. All potential 
  
  NEW_VOTING_POPULATION_COUNTIES_all <- NEW_VOTING_POPULATION_COUNTIES_v1 %>% 
    dplyr:: group_by(County_new) %>% 
    dplyr:: summarise(Total_sample_pot_vote_county = round(sum(Total_exp_pot_vote),0)) %>% # 1. Determine expected number
    mutate(Share_pot_county = Total_sample_pot_vote_county / total_sample_pop_eligible, # 2. Determine share of county for expected number
           Pred_pot_county_scaled = round(Share_pot_county*total_pop_eligible,0),
           # 3. Scale county number based on share and total_pop_eligible
           Pred_pot_county_scaled_incl_spill = round(Pred_pot_county_scaled + Pred_pot_county_scaled *spill,0))
            # 4. Add spillover
   
  #B. Medium scenario w/ party affiliation
  
 NEW_VOTING_POPULATION_COUNTIES_medium <- NEW_VOTING_POPULATION_COUNTIES_v1 %>% 
   # 1. Determine expected number on Race and County Basis (total, R_vox, D_vox, O_vox, R_reg, D_reg, O_reg)
   mutate(Vote_turnout_medium_randc = 
            ifelse(race_descr == "Black", round(Total_exp_pot_vote*0.16,0), round(Total_exp_pot_vote*0.12, 0)),
          Estimated_D_vote_vox = 
            ifelse(race_descr == "Black", Vote_turnout_medium_randc*D_B_vox, Vote_turnout_medium_randc*D_W_vox),
          Estimated_R_vote_vox = 
            ifelse(race_descr == "Black", Vote_turnout_medium_randc*R_B_vox, Vote_turnout_medium_randc*R_W_vox),
          Estimated_O_vote_vox = 
            ifelse(race_descr == "Black", Vote_turnout_medium_randc*O_B_vox, Vote_turnout_medium_randc*O_W_vox),
          Estimated_D_vote_reg = Vote_turnout_medium_randc*D_reg,
          Estimated_R_vote_reg = Vote_turnout_medium_randc*R_reg,
          Estimated_O_vote_reg = Vote_turnout_medium_randc*O_reg) %>%
   # 2. Summarise on County level
   dplyr:: group_by(County_new) %>% 
   dplyr:: summarise(
     Total_sample_medium_vote_county = round(sum(Vote_turnout_medium_randc),0),
     Pred_sample_medium_vox_D_vote_county = round(sum(Estimated_D_vote_vox),0),
     Pred_sample_medium_vox_R_vote_county = round(sum(Estimated_R_vote_vox),0),
     Pred_sample_medium_vox_O_vote_county = round(sum(Estimated_O_vote_vox),0),
     Pred_sample_medium_reg_D_vote_county = round(sum(Estimated_D_vote_reg),0),
     Pred_sample_medium_reg_R_vote_county = round(sum(Estimated_R_vote_reg),0),
     Pred_sample_medium_reg_O_vote_county = round(sum(Estimated_O_vote_reg),0)) %>% 
   mutate(
    # 3. Determine share of county for expected number
       Share_medium_county = Total_sample_medium_vote_county / total_sample_pop_eligible,
     Share_medium_vox_D_county = Pred_sample_medium_vox_D_vote_county / total_sample_pop_eligible,
     Share_medium_vox_R_county = Pred_sample_medium_vox_R_vote_county / total_sample_pop_eligible,
     Share_medium_vox_O_county = Pred_sample_medium_vox_O_vote_county / total_sample_pop_eligible,
     Share_medium_reg_D_county = Pred_sample_medium_reg_D_vote_county / total_sample_pop_eligible,
     Share_medium_reg_R_county = Pred_sample_medium_reg_R_vote_county / total_sample_pop_eligible,
     Share_medium_reg_O_county = Pred_sample_medium_reg_O_vote_county / total_sample_pop_eligible,
   # 4. Scale county number based on share and total_pop_eligible        
  Pred_medium_county_scaled = round(Share_medium_county*total_pop_eligible,0),
  Pred_medium_vox_D_county_scaled = round(Share_medium_vox_D_county*total_pop_eligible,0),
  Pred_medium_vox_R_county_scaled = round(Share_medium_vox_R_county*total_pop_eligible,0),
  Pred_medium_vox_O_county_scaled = round(Share_medium_vox_O_county*total_pop_eligible,0),
  Pred_medium_reg_D_county_scaled = round(Share_medium_reg_D_county*total_pop_eligible,0),
  Pred_medium_reg_R_county_scaled = round(Share_medium_reg_R_county*total_pop_eligible,0),
  Pred_medium_reg_O_county_scaled = round(Share_medium_reg_O_county*total_pop_eligible,0),
   # 5. Add spillover         
  Pred_medium_county_scaled_incl_spill = round(Pred_medium_county_scaled + Pred_medium_county_scaled *spill,0),
  Pred_medium_county_vox_D_scaled_incl_spill = round(Pred_medium_vox_D_county_scaled + Pred_medium_vox_D_county_scaled *spill,0),
  Pred_medium_county_vox_R_scaled_incl_spill = round(Pred_medium_vox_R_county_scaled + Pred_medium_vox_R_county_scaled *spill,0),
  Pred_medium_county_vox_O_scaled_incl_spill = round(Pred_medium_vox_O_county_scaled + Pred_medium_vox_O_county_scaled *spill,0),
  Pred_medium_county_reg_D_scaled_incl_spill = round(Pred_medium_reg_D_county_scaled + Pred_medium_reg_D_county_scaled *spill,0),
  Pred_medium_county_reg_R_scaled_incl_spill = round(Pred_medium_reg_R_county_scaled + Pred_medium_reg_R_county_scaled *spill,0),
  Pred_medium_county_reg_O_scaled_incl_spill = round(Pred_medium_reg_O_county_scaled + Pred_medium_reg_O_county_scaled *spill,0)
  )
 
 

 
 
 # C. Low scenario w/ party affiliation
 
 NEW_VOTING_POPULATION_COUNTIES_low <- NEW_VOTING_POPULATION_COUNTIES_v1 %>% 
   mutate(Vote_turnout_low = round(Total_exp_pot_vote*0.05,0),
          Estimated_D_vote_vox = 
            ifelse(race_descr == "Black", Vote_turnout_low*D_B_vox, Vote_turnout_low*D_W_vox),
          Estimated_R_vote_vox = 
            ifelse(race_descr == "Black", Vote_turnout_low*R_B_vox, Vote_turnout_low*R_W_vox),
          Estimated_O_vote_vox = 
            ifelse(race_descr == "Black", Vote_turnout_low*O_B_vox, Vote_turnout_low*O_W_vox),
          Estimated_D_vote_reg = Vote_turnout_low*D_reg,
          Estimated_R_vote_reg = Vote_turnout_low*R_reg,
          Estimated_O_vote_reg = Vote_turnout_low*O_reg) %>%   
   # 2. Summarise on County level
 dplyr:: group_by(County_new) %>% 
   dplyr:: summarise(
     Total_sample_low_vote_county = round(sum(Vote_turnout_low),0),
     Pred_sample_low_vox_D_vote_county = round(sum(Estimated_D_vote_vox),0),
     Pred_sample_low_vox_R_vote_county = round(sum(Estimated_R_vote_vox),0),
     Pred_sample_low_vox_O_vote_county = round(sum(Estimated_O_vote_vox),0),
     Pred_sample_low_reg_D_vote_county = round(sum(Estimated_D_vote_reg),0),
     Pred_sample_low_reg_R_vote_county = round(sum(Estimated_R_vote_reg),0),
     Pred_sample_low_reg_O_vote_county = round(sum(Estimated_O_vote_reg),0)) %>% 
   mutate(
     # 3. Determine share of county for expected number
     Share_low_county = Total_sample_low_vote_county / total_sample_pop_eligible,
     Share_low_vox_D_county = Pred_sample_low_vox_D_vote_county / total_sample_pop_eligible,
     Share_low_vox_R_county = Pred_sample_low_vox_R_vote_county / total_sample_pop_eligible,
     Share_low_vox_O_county = Pred_sample_low_vox_O_vote_county / total_sample_pop_eligible,
     Share_low_reg_D_county = Pred_sample_low_reg_D_vote_county / total_sample_pop_eligible,
     Share_low_reg_R_county = Pred_sample_low_reg_R_vote_county / total_sample_pop_eligible,
     Share_low_reg_O_county = Pred_sample_low_reg_O_vote_county / total_sample_pop_eligible,
     # 4. Scale county number based on share and total_pop_eligible        
     Pred_low_county_scaled = round(Share_low_county*total_pop_eligible,0),
     Pred_low_vox_D_county_scaled = round(Share_low_vox_D_county*total_pop_eligible,0),
     Pred_low_vox_R_county_scaled = round(Share_low_vox_R_county*total_pop_eligible,0),
     Pred_low_vox_O_county_scaled = round(Share_low_vox_O_county*total_pop_eligible,0),
     Pred_low_reg_D_county_scaled = round(Share_low_reg_D_county*total_pop_eligible,0),
     Pred_low_reg_R_county_scaled = round(Share_low_reg_R_county*total_pop_eligible,0),
     Pred_low_reg_O_county_scaled = round(Share_low_reg_O_county*total_pop_eligible,0),
     # 5. Add spillover         
     Pred_low_county_scaled_incl_spill = round(Pred_low_county_scaled + Pred_low_county_scaled *spill,0),
     Pred_low_county_vox_D_scaled_incl_spill = round(Pred_low_vox_D_county_scaled + Pred_low_vox_D_county_scaled *spill,0),
     Pred_low_county_vox_R_scaled_incl_spill = round(Pred_low_vox_R_county_scaled + Pred_low_vox_R_county_scaled *spill,0),
     Pred_low_county_vox_O_scaled_incl_spill = round(Pred_low_vox_O_county_scaled + Pred_low_vox_O_county_scaled *spill,0),
     Pred_low_county_reg_D_scaled_incl_spill = round(Pred_low_reg_D_county_scaled + Pred_low_reg_D_county_scaled *spill,0),
     Pred_low_county_reg_R_scaled_incl_spill = round(Pred_low_reg_R_county_scaled + Pred_low_reg_R_county_scaled *spill,0),
     Pred_low_county_reg_O_scaled_incl_spill = round(Pred_low_reg_O_county_scaled + Pred_low_reg_O_county_scaled *spill,0)
   )
 
 
  # D. High scenario w/ party affiliation

 NEW_VOTING_POPULATION_COUNTIES_high <- NEW_VOTING_POPULATION_COUNTIES_v1 %>% 
   mutate(Vote_turnout_high = round(Total_exp_pot_vote*0.35,0),
          Estimated_D_vote_vox = 
            ifelse(race_descr == "Black", Vote_turnout_high*D_B_vox, Vote_turnout_high*D_W_vox),
          Estimated_R_vote_vox = 
            ifelse(race_descr == "Black", Vote_turnout_high*R_B_vox, Vote_turnout_high*R_W_vox),
          Estimated_O_vote_vox = 
            ifelse(race_descr == "Black", Vote_turnout_high*O_B_vox, Vote_turnout_high*O_W_vox),
          Estimated_D_vote_reg = Vote_turnout_high*D_reg,
          Estimated_R_vote_reg = Vote_turnout_high*R_reg,
          Estimated_O_vote_reg = Vote_turnout_high*O_reg) %>%   
   # 2. Summarise on County level
   dplyr:: group_by(County_new) %>% 
   dplyr:: summarise(
     Total_sample_high_vote_county = round(sum(Vote_turnout_high),0),
     Pred_sample_high_vox_D_vote_county = round(sum(Estimated_D_vote_vox),0),
     Pred_sample_high_vox_R_vote_county = round(sum(Estimated_R_vote_vox),0),
     Pred_sample_high_vox_O_vote_county = round(sum(Estimated_O_vote_vox),0),
     Pred_sample_high_reg_D_vote_county = round(sum(Estimated_D_vote_reg),0),
     Pred_sample_high_reg_R_vote_county = round(sum(Estimated_R_vote_reg),0),
     Pred_sample_high_reg_O_vote_county = round(sum(Estimated_O_vote_reg),0)) %>% 
   mutate(
     # 3. Determine share of county for expected number
     Share_high_county = Total_sample_high_vote_county / total_sample_pop_eligible,
     Share_high_vox_D_county = Pred_sample_high_vox_D_vote_county / total_sample_pop_eligible,
     Share_high_vox_R_county = Pred_sample_high_vox_R_vote_county / total_sample_pop_eligible,
     Share_high_vox_O_county = Pred_sample_high_vox_O_vote_county / total_sample_pop_eligible,
     Share_high_reg_D_county = Pred_sample_high_reg_D_vote_county / total_sample_pop_eligible,
     Share_high_reg_R_county = Pred_sample_high_reg_R_vote_county / total_sample_pop_eligible,
     Share_high_reg_O_county = Pred_sample_high_reg_O_vote_county / total_sample_pop_eligible,
     # 4. Scale county number based on share and total_pop_eligible        
     Pred_high_county_scaled = round(Share_high_county*total_pop_eligible,0),
     Pred_high_vox_D_county_scaled = round(Share_high_vox_D_county*total_pop_eligible,0),
     Pred_high_vox_R_county_scaled = round(Share_high_vox_R_county*total_pop_eligible,0),
     Pred_high_vox_O_county_scaled = round(Share_high_vox_O_county*total_pop_eligible,0),
     Pred_high_reg_D_county_scaled = round(Share_high_reg_D_county*total_pop_eligible,0),
     Pred_high_reg_R_county_scaled = round(Share_high_reg_R_county*total_pop_eligible,0),
     Pred_high_reg_O_county_scaled = round(Share_high_reg_O_county*total_pop_eligible,0),
     # 5. Add spillover         
     Pred_high_county_scaled_incl_spill = round(Pred_high_county_scaled + Pred_high_county_scaled *spill,0),
     Pred_high_county_vox_D_scaled_incl_spill = round(Pred_high_vox_D_county_scaled + Pred_high_vox_D_county_scaled *spill,0),
     Pred_high_county_vox_R_scaled_incl_spill = round(Pred_high_vox_R_county_scaled + Pred_high_vox_R_county_scaled *spill,0),
     Pred_high_county_vox_O_scaled_incl_spill = round(Pred_high_vox_O_county_scaled + Pred_high_vox_O_county_scaled *spill,0),
     Pred_high_county_reg_D_scaled_incl_spill = round(Pred_high_reg_D_county_scaled + Pred_high_reg_D_county_scaled *spill,0),
     Pred_high_county_reg_R_scaled_incl_spill = round(Pred_high_reg_R_county_scaled + Pred_high_reg_R_county_scaled *spill,0),
     Pred_high_county_reg_O_scaled_incl_spill = round(Pred_high_reg_O_county_scaled + Pred_high_reg_O_county_scaled *spill,0)
   ) 
 

# 9. Table of impact predictions
 
 table_all <-
   NEW_VOTING_POPULATION_COUNTIES_all %>%
   gt()%>%
   tab_header(
     title = "Potential impact prediction on county level through Amendment 4",
     subtitle = "Including all potential voters") %>%
   tab_source_note(
     source_note = "Source: See assumptions and own analysis.")
 
 
 glimpse(NEW_VOTING_POPULATION_COUNTIES_all)
 
 table_medium <-
   NEW_VOTING_POPULATION_COUNTIES_medium %>%
   select(County = County_new, Pred_D_1 = Pred_medium_county_vox_D_scaled_incl_spill, Pred_R_1 = Pred_medium_county_vox_R_scaled_incl_spill, Pred_O_1 = Pred_medium_county_vox_O_scaled_incl_spill, Pred_D_2 = Pred_medium_county_reg_D_scaled_incl_spill, Pred_R_2 = Pred_medium_county_reg_R_scaled_incl_spill, Pred_O_2 = Pred_medium_county_reg_O_scaled_incl_spill) %>%
   gt()%>%
   tab_header(
     title = "Impact prediction on county level through Amendment 4",
     subtitle = "Medium: Voter turnout at 16% and 12% for enfranchised black and non-black voters") %>%
   tab_source_note(
     source_note = "Source: See assumptions and own analysis.")

 
 table_low <-
   NEW_VOTING_POPULATION_COUNTIES_low %>%
   select(County = County_new, Pred_D_1 = Pred_low_county_vox_D_scaled_incl_spill, Pred_R_1 = Pred_low_county_vox_R_scaled_incl_spill, Pred_O_1 = Pred_low_county_vox_O_scaled_incl_spill, Pred_D_2 = Pred_low_county_reg_D_scaled_incl_spill, Pred_R_2 = Pred_low_county_reg_R_scaled_incl_spill, Pred_O_2 = Pred_low_county_reg_O_scaled_incl_spill) %>%
   gt()%>%
   tab_header(
     title = "Impact prediction on county level through Amendment 4",
     subtitle = "Low: Voter turnout at 5% for enfranchised voters") %>%
   tab_source_note(
     source_note = "Source: See assumptions and own analysis.")

table_high <-
   NEW_VOTING_POPULATION_COUNTIES_high %>%
   select(County = County_new, Pred_D_1 = Pred_high_county_vox_D_scaled_incl_spill, Pred_R_1 = Pred_high_county_vox_R_scaled_incl_spill, Pred_O_1 = Pred_high_county_vox_O_scaled_incl_spill, Pred_D_2 = Pred_high_county_reg_D_scaled_incl_spill, Pred_R_2 = Pred_high_county_reg_R_scaled_incl_spill, Pred_O_2 = Pred_high_county_reg_O_scaled_incl_spill) %>%
   gt()%>%
   tab_header(
     title = "Impact prediction on county level through Amendment 4",
     subtitle = "High: Voter turnout at 35% for enfranchised voters") %>%
   tab_source_note(
     source_note = "Source: See assumptions and own analysis.")
    
  
#10. Read in Data from last state wide elections in Florida (2018 Senate elections) per county
  
  Elections_2018 <- read.csv("raw-data/Past-elections/2018_elections_senate.csv")

  Elections_2018_2 <- Elections_2018 %>% group_by(PartyCode, Juris2num) %>%
    summarise(Votes = sum(CanVotes))
  
  Elections_2018_3 <- Elections_2018_2 %>%
    spread(key = PartyCode, value=Votes)
  
  colnames(Elections_2018_3)[colnames(Elections_2018_3)=="Juris2num"] <- "County"
  Elections_2018_3$County <- as.character(Elections_2018_3$County)
  Elections_2018_3$County[Elections_2018_3$County == "Miami-Dade"] <- "Miami-dade"
  
  
#11. Combine data sets to show counties at the margin
  
  # A. All scenario
  
  glimpse(NEW_VOTING_POPULATION_COUNTIES_all)
  glimpse(Elections_2018_3)
  
  Elections_2020_all <- Elections_2018_3 %>%
    left_join(NEW_VOTING_POPULATION_COUNTIES_all , by = c("County" = "County_new")) %>%
    select(County, Pot_direct_impact = Pred_pot_county_scaled, Pot_impact_incl_spill = Pred_pot_county_scaled_incl_spill, DEM, REP) %>%
    adorn_totals("row")%>%
    mutate(`Incumbent party` = ifelse(DEM > REP, "Democratic Party", "Republican Party"),
           `2018 margin of victory` = abs(DEM-REP),
           `Enfranchised voters` = Pot_direct_impact,
           `Enfranchised voters & spillover` = Pot_impact_incl_spill,
           `Potential of Amendment 4 (direct) to swing the vote` = 
             ifelse(`2018 margin of victory` < `Enfranchised voters`, "Yes", "No"),
            `Potential of Amendment 4 (direct & spillover) to swing the vote` = 
    ifelse(`2018 margin of victory` < `Enfranchised voters & spillover`, "Yes", "No")) 
  
  # %>%
  #   filter(`Potential of Amendment 4 (direct & spillover) to swing the vote` == "Yes")
  # 
  
  
  Elections_2020_all_map <- Elections_2020_all %>% 
    select(County, `2018 margin of victory`, `Enfranchised voters`) %>%
    mutate(`Direct impact` = `Enfranchised voters`/ `2018 margin of victory`)
  
  write.csv(Elections_2020_all_map, file = "Elections_2020_all_map.csv")
 
   
  # B. Low Scenario
  
  glimpse(NEW_VOTING_POPULATION_COUNTIES_low)
  
  Elections_2020_low <- Elections_2018_3 %>%
    left_join(NEW_VOTING_POPULATION_COUNTIES_low , by = c("County" = "County_new")) %>%
    select(County, Pred_low_county_scaled, Pred_low_county_scaled_incl_spill, DEM, REP, Pred_low_county_vox_D_scaled_incl_spill, Pred_low_county_vox_R_scaled_incl_spill, Pred_low_county_reg_D_scaled_incl_spill, Pred_low_county_reg_R_scaled_incl_spill) %>%
    adorn_totals("row")%>%
    mutate(`Incumbent party` = ifelse(DEM > REP, "Democratic Party", "Republican Party"),
           `2018 margin of victory` = abs(DEM-REP),
           `Enfranchised voters` = Pred_low_county_scaled,
           `Enfranchised voters & spillover` = Pred_low_county_scaled_incl_spill,
           `Expected add. Democratic vote 1` = Pred_low_county_vox_D_scaled_incl_spill,
           `Expected add. Republican vote 1` = Pred_low_county_vox_R_scaled_incl_spill,
           `Expected add. Democratic vote 2` = Pred_low_county_reg_D_scaled_incl_spill,
           `Expected add. Republican vote 2` = Pred_low_county_reg_R_scaled_incl_spill,
           `Expected add. margin 1` = 
             abs(`Expected add. Democratic vote 1`-`Expected add. Republican vote 1`),
           `Expected add. margin 2` = 
             abs(`Expected add. Democratic vote 2`-`Expected add. Republican vote 2`),
           `Expected marginal advantage 1` = 
             ifelse(`Expected add. Democratic vote 1`>`Expected add. Republican vote 1`, "Democratic Party", "Republican Party"),
           `Expected marginal advantage 2` = 
             ifelse(`Expected add. Democratic vote 2`>`Expected add. Republican vote 2`, "Democratic Party", "Republican Party"),
           `Amendment 4 expected to swing vote 1` = ifelse(`2018 margin of victory` < `Expected add. margin 1`, "Yes", "No"),
           `Amendment 4 expected to swing vote 2` = ifelse(`2018 margin of victory` < `Expected add. margin 2`, "Yes", "No")) 
  
  # %>% 
  #   filter(`Amendment 4 expected to swing vote 1` == "Yes" | `Amendment 4 expected to swing vote 2` == "Yes")
  # 
  
  Elections_2020_low_map <- Elections_2020_low %>% 
    select(County, `2018 margin of victory`, `Expected add. margin 1`, `Expected marginal advantage 1`) %>%
    mutate(`Expected impact` = `Expected add. margin 1`/ `2018 margin of victory`)
  
  write.csv(Elections_2020_low_map, file = "Elections_2020_low_map.csv")
  
  
  # C. Middle Scenario
  
  glimpse(NEW_VOTING_POPULATION_COUNTIES_medium)
  
  Elections_2020_medium <- Elections_2018_3 %>%
    left_join(NEW_VOTING_POPULATION_COUNTIES_medium , by = c("County" = "County_new")) %>%
    select(County, Pred_medium_county_scaled, Pred_medium_county_scaled_incl_spill, DEM, REP, Pred_medium_county_vox_D_scaled_incl_spill, Pred_medium_county_vox_R_scaled_incl_spill, Pred_medium_county_reg_D_scaled_incl_spill, Pred_medium_county_reg_R_scaled_incl_spill) %>%
    adorn_totals("row")%>%
    mutate(`Incumbent party` = ifelse(DEM > REP, "Democratic Party", "Republican Party"),
           `2018 margin of victory` = abs(DEM-REP),
           `Enfranchised voters` = Pred_medium_county_scaled,
           `Enfranchised voters & spillover` = Pred_medium_county_scaled_incl_spill,
           `Expected add. Democratic vote 1` = Pred_medium_county_vox_D_scaled_incl_spill,
           `Expected add. Republican vote 1` = Pred_medium_county_vox_R_scaled_incl_spill,
           `Expected add. Democratic vote 2` = Pred_medium_county_reg_D_scaled_incl_spill,
           `Expected add. Republican vote 2` = Pred_medium_county_reg_R_scaled_incl_spill,
           `Expected add. margin 1` = 
             abs(`Expected add. Democratic vote 1`-`Expected add. Republican vote 1`),
           `Expected add. margin 2` = 
             abs(`Expected add. Democratic vote 2`-`Expected add. Republican vote 2`),
           `Expected marginal advantage 1` = 
             ifelse(`Expected add. Democratic vote 1`>`Expected add. Republican vote 1`, "Democratic Party", "Republican Party"),
           `Expected marginal advantage 2` = 
             ifelse(`Expected add. Democratic vote 2`>`Expected add. Republican vote 2`, "Democratic Party", "Republican Party"),
           `Amendment 4 expected to swing vote 1` = ifelse(`2018 margin of victory` < `Expected add. margin 1`, "Yes", "No"),
           `Amendment 4 expected to swing vote 2` = ifelse(`2018 margin of victory` < `Expected add. margin 2`, "Yes", "No")) 
  
  # %>% 
  #   filter(`Amendment 4 expected to swing vote 1` == "Yes" | `Amendment 4 expected to swing vote 2` == "Yes")
  # 
  
  Elections_2020_medium_map <- Elections_2020_medium %>% 
    select(County, `2018 margin of victory`, `Expected add. margin 1`, `Expected marginal advantage 1`) %>%
    mutate(`Expected impact` = `Expected add. margin 1`/ `2018 margin of victory`)
  
  write.csv(Elections_2020_medium_map, file = "Elections_2020_medium_map.csv")

  # D. High Scenario
  
  glimpse(NEW_VOTING_POPULATION_COUNTIES_high)
  
  Elections_2020_high <- Elections_2018_3 %>%
    left_join(NEW_VOTING_POPULATION_COUNTIES_high , by = c("County" = "County_new")) %>%
    select(County, Pred_high_county_scaled, Pred_high_county_scaled_incl_spill, DEM, REP, Pred_high_county_vox_D_scaled_incl_spill, Pred_high_county_vox_R_scaled_incl_spill, Pred_high_county_reg_D_scaled_incl_spill, Pred_high_county_reg_R_scaled_incl_spill) %>%
    adorn_totals("row")%>%
    mutate(`Incumbent party` = ifelse(DEM > REP, "Democratic Party", "Republican Party"),
           `2018 margin of victory` = abs(DEM-REP),
           `Enfranchised voters` = Pred_high_county_scaled,
           `Enfranchised voters & spillover` = Pred_high_county_scaled_incl_spill,
           `Expected add. Democratic vote 1` = Pred_high_county_vox_D_scaled_incl_spill,
           `Expected add. Republican vote 1` = Pred_high_county_vox_R_scaled_incl_spill,
           `Expected add. Democratic vote 2` = Pred_high_county_reg_D_scaled_incl_spill,
           `Expected add. Republican vote 2` = Pred_high_county_reg_R_scaled_incl_spill,
           `Expected add. margin 1` = 
             abs(`Expected add. Democratic vote 1`-`Expected add. Republican vote 1`),
           `Expected add. margin 2` = 
             abs(`Expected add. Democratic vote 2`-`Expected add. Republican vote 2`),
           `Expected marginal advantage 1` = 
             ifelse(`Expected add. Democratic vote 1`>`Expected add. Republican vote 1`, "Democratic Party", "Republican Party"),
           `Expected marginal advantage 2` = 
             ifelse(`Expected add. Democratic vote 2`>`Expected add. Republican vote 2`, "Democratic Party", "Republican Party"),
           `Amendment 4 expected to swing vote 1` = ifelse(`2018 margin of victory` < `Expected add. margin 1`, "Yes", "No"),
           `Amendment 4 expected to swing vote 2` = ifelse(`2018 margin of victory` < `Expected add. margin 2`, "Yes", "No")) 
              
  
Elections_2020_high_map <- Elections_2020_high %>% 
    select(County, `2018 margin of victory`, `Expected add. margin 1`, `Expected marginal advantage 1`) %>%
    mutate(`Expected impact` = `Expected add. margin 1`/ `2018 margin of victory`)
  
  write.csv(Elections_2020_high_map, file = "Elections_2020_high_map.csv")
  
  glimpse(Elections_2020_high)
  
  
#12. Visualize results in table

  # A. All scenario
  
  Elections_2020_all$County[Elections_2020_all$County == "Total"] <- "Statewide"
    
  Elections_2020_all_table <- Elections_2020_all %>%
    select(County, `Incumbent party`, `2018 margin of victory`, `Enfranchised voters`, `Enfranchised voters & spillover`, `Potential of Amendment 4 (direct) to swing the vote`, `Potential of Amendment 4 (direct & spillover) to swing the vote`)
  
  table_all_1 <- Elections_2020_all_table %>%
    gt()%>%
    tab_header(
      title = "The electoral significance of Amendment 4 in 2020") %>%
    tab_spanner(
      label = "2018 midterm results",
      columns = vars("Incumbent party", "2018 margin of victory")) %>%
    tab_spanner(
      label = "2020 potential impact",
      columns = vars("Enfranchised voters", "Enfranchised voters & spillover", "Potential of Amendment 4 (direct) to swing the vote", "Potential of Amendment 4 (direct & spillover) to swing the vote")) 
  
  
  # B. Low scenario
  
  Elections_2020_low$County[Elections_2020_low$County == "Total"] <- "Statewide"

  Elections_2020_low_table <- Elections_2020_low %>%
    select(County, `Incumbent party`, `2018 margin of victory`, `Enfranchised voters`, `Enfranchised voters & spillover`, `Expected add. margin 1`, `Expected marginal advantage 1`, `Amendment 4 expected to swing vote 1`, `Expected add. margin 2`, `Expected marginal advantage 2`, `Amendment 4 expected to swing vote 2`)
  
  table_low_1 <- Elections_2020_low_table %>%
    gt()%>%
    tab_header(
      title = "The electoral significance of Amendment 4 in 2020") %>%
    tab_spanner(
      label = "2018 midterm results",
      columns = vars("Incumbent party", "2018 margin of victory")) %>%
    tab_spanner(
      label = "2020 predicted impact low Scenario",
      columns = vars("Enfranchised voters", "Enfranchised voters & spillover", "Expected add. margin 1", "Expected marginal advantage 1", "Amendment 4 expected to swing vote 1", "Expected add. margin 2", "Expected marginal advantage 2", "Amendment 4 expected to swing vote 2")) 
  
 

  # C. Medium scenario
  
  Elections_2020_medium$County[Elections_2020_medium$County == "Total"] <- "Statewide"
  
  Elections_2020_medium_table <- Elections_2020_medium %>%
    select(County, `Incumbent party`, `2018 margin of victory`, `Enfranchised voters`, `Enfranchised voters & spillover`, `Expected add. margin 1`, `Expected marginal advantage 1`, `Amendment 4 expected to swing vote 1`, `Expected add. margin 2`, `Expected marginal advantage 2`, `Amendment 4 expected to swing vote 2`)
  
  table_medium_1 <- Elections_2020_medium_table %>%
    gt()%>%
    tab_header(
      title = "The electoral significance of Amendment 4 in 2020") %>%
    tab_spanner(
      label = "2018 midterm results",
      columns = vars("Incumbent party", "2018 margin of victory")) %>%
    tab_spanner(
      label = "2020 predicted impact medium scenario",
      columns = vars("Enfranchised voters", "Enfranchised voters & spillover", "Expected add. margin 1", "Expected marginal advantage 1", "Amendment 4 expected to swing vote 1", "Expected add. margin 2", "Expected marginal advantage 2", "Amendment 4 expected to swing vote 2")) 
  
  # D. High scenario
  
  Elections_2020_high$County[Elections_2020_high$County == "Total"] <- "Statewide"
  view(Elections_2020_high_table)
  Elections_2020_high_table <- Elections_2020_high %>%
    select(County, `Incumbent party`, `2018 margin of victory`, `Enfranchised voters`, `Enfranchised voters & spillover`, `Expected add. margin 1`, `Expected marginal advantage 1`, `Amendment 4 expected to swing vote 1`, `Expected add. margin 2`, `Expected marginal advantage 2`, `Amendment 4 expected to swing vote 2`)
  
  table_high_1 <- Elections_2020_high_table %>%
    gt()%>%
    tab_header(
      title = "The electoral significance of Amendment 4 in 2020") %>%
    tab_spanner(
      label = "2018 midterm results",
      columns = vars("Incumbent party", "2018 margin of victory")) %>%
    tab_spanner(
      label = "2020 predicted impact high scenario",
      columns = vars("Enfranchised voters", "Enfranchised voters & spillover", "Expected add. margin 1", "Expected marginal advantage 1", "Amendment 4 expected to swing vote 1", "Expected add. margin 2", "Expected marginal advantage 2", "Amendment 4 expected to swing vote 2")) 
  
   
  
  # OLD CODE:
  
  
  # 
  #   tab_style(
  #     style = list(
  #       cell_fill(color = "#FFA500"),
  #       cell_text(weight = "bold")
  #     ),
  #     locations = cells_data(
  #       columns = vars(County, `Incumbent party`, `2018 margin of victory`, `Enfranchised voters`, `Potential of Amendment 4 (direct) to swing the vote`, `Potential of Amendment 4 (direct & spillover) to swing the vote`),
  #       rows = `Potential of Amendment 4 (direct) to swing the vote` == "Yes"))%>%
  # tab_style(
  #   style = list(
  #     cell_fill(color = "#add8e6"),
  #     cell_text(weight = "bold")
  #   ),
  # locations = cells_data(
  #   columns = vars(County, `Incumbent party`, `2018 margin of victory`, `Enfranchised voters`, `Potential of Amendment 4 (direct) to swing the vote`, `Potential of Amendment 4 (direct & spillover) to swing the vote`),
  #   rows = `Potential of Amendment 4 (direct & spillover) to swing the vote` == "Yes"))
  
  
  # table_2 <- Elections_2020_table %>%
  #   filter(`Potential of Amendment 4 to swing the vote` =="Yes") %>%
  #   gt()%>%
  #   tab_header(
  #     title = "The electoral significance of Amendment 4 in 2020") %>%
  #   tab_spanner(
  #     label = "2018 midterm results",
  #     columns = vars("Incumbent party", "2018 margin of victory")) %>%
  #   tab_spanner(
  #     label = "2020 projection",
  #     columns = vars("Re-enfranchised voters", "Potential of Amendment 4 to swing the vote", "Expected add. margin", "Expected marginal advantage", "Amendment 4 expected to swing vote")) %>%
  #   tab_style(
  #     style = list(
  #       cell_fill(color = "#add8e6"),
  #       cell_text(weight = "bold")
  #     ),
  #     locations = cells_data(
  #       columns = vars(County, `Incumbent party`, `2018 margin of victory`, `Re-enfranchised voters`, `Potential of Amendment 4 to swing the vote`, `Expected add. margin`, `Expected marginal advantage`, `Amendment 4 expected to swing vote`),
  #       rows = `Amendment 4 expected to swing vote` == "Yes"))
  # 
  # 
  # gtsave(table_1, "Counties.png")
  # 
  #  write.csv(Elections_2020_table, file = "Elections_2020_table.csv")
  # 
  # # NEED TO: Add clarifier for total that it is based on the accumulated margin in 2018 and not on the gap in 2016, which was 112911. If that is the basis for assessment, it would not swing.
  # # Highlight assumptions in table (Note: Highly simplified mehtodology based on Vox article)
  # 