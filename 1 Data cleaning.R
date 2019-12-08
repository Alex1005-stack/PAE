
library(eeptools)
library(odbc)
library(DBI)
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)


#I. Create raw-data and clean-data directory to store the data that is to be downloaded      
dir.create("raw-data")
dir.create("clean-data")

#II. Load Florida data and store each database in an individual tab 
Florida_correctional <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/Alexander Klueber/Desktop/University/Coursework/PAE/Data analysis/PAE/raw-data/FDOC_October_2019.mdb")

#III. Active inmates
  #1. Current inmates: Pull out data 
  
  INMATE_ACTIVE_OFFENSES_CPS <- sqlQuery(Florida_correctional, paste ("select DCNumber, Sequence, County, adjudicationcharge_descr from INMATE_ACTIVE_OFFENSES_CPS"))
  
  INMATE_ACTIVE_OFFENSES_PRPR <- sqlQuery(Florida_correctional, paste ("select DCNumber, Sequence, County, adjudicationcharge_descr from INMATE_ACTIVE_OFFENSES_PRPR"))
  
  INMATE_ACTIVE_ROOT <- sqlQuery(Florida_correctional, paste ("select DCNumber, LastName, FirstName, MiddleName, NameSuffix, Sex, BirthDate, PrisonReleaseDate, ReceiptDate, race_descr from INMATE_ACTIVE_ROOT"))
  
  #2. Filter out inmates that:
    # will not be released before the Florida general election on November 3rd (Need to register by Oct. 5 2020)
  
  INMATE_ACTIVE_ROOT_2 <- INMATE_ACTIVE_ROOT %>%
    filter(PrisonReleaseDate <= as.Date("2020-10-05"))
  
  #3. List inmates that have committed murder or sexual offenses (disqualifying for right to vote) - selection based on offense categories (murder and sex crime) adjuste by hand to approximate literal amendment and bill, using reference:
      # Offense categories : http://www.dc.state.fl.us/offendersearch/offensecategory.aspx#KN
      #Source: 
        # Bill: https://www.tampabay.com/florida-politics/buzz/2019/05/26/how-felons-can-register-to-vote-in-florida-under-new-amendment-4-law/
        # Literal: https://www.pnj.com/story/news/2019/01/23/meaning-of-murder-key-in-florida-felons-voting-rights/2657973002/
        
      #Note: 1. Literally included in the amendment: 
        # Murder: 1st degree murder (Excludes 2-3rd degree murder, other homicides, manslaugther, DUI manslaughter)
        # Sexual offenses: Rape, sexual offenses against children
      #Note: 2. Included in the bill other than fines and restitutions 
      #Note: 3. Fines and restitutions: Don't know how to include yet
    
      # Reading in selection of offenses:
    disq_offenses <- read.csv("raw-data/Disqualifying_offenses.csv")
      
      # Current prison offenses
    INMATE_ACTIVE_OFFENSES_CPS_2 <- INMATE_ACTIVE_OFFENSES_CPS %>%
      mutate(Disq_bill = adjudicationcharge_descr %in% disq_offenses$ï..Offense[which(disq_offenses$Bill == "Yes")],
             Disq_literal = adjudicationcharge_descr %in% disq_offenses$ï..Offense[which(disq_offenses$Literal == "Yes")])
    
    INMATE_ACTIVE_OFFENSES_CPS_3_literal <- INMATE_ACTIVE_OFFENSES_CPS_2 %>%
      filter(Disq_literal == TRUE) %>%
      select(DCNumber) 
    
    INMATE_ACTIVE_OFFENSES_CPS_3_bill <- INMATE_ACTIVE_OFFENSES_CPS_2 %>%
      filter(Disq_bill == TRUE) %>%
      select(DCNumber)
    
      # Prior prison offenses
    
    INMATE_ACTIVE_OFFENSES_PRPR_2 <- INMATE_ACTIVE_OFFENSES_PRPR %>%
      mutate(Disq_bill = adjudicationcharge_descr %in% disq_offenses$ï..Offense[which(disq_offenses$Bill == "Yes")],
             Disq_literal = adjudicationcharge_descr %in% disq_offenses$ï..Offense[which(disq_offenses$Literal == "Yes")])
    
    INMATE_ACTIVE_OFFENSES_PRPR_3_literal <- INMATE_ACTIVE_OFFENSES_PRPR_2 %>%
      filter(Disq_literal == TRUE) %>%
      select(DCNumber)
    
    INMATE_ACTIVE_OFFENSES_PRPR_3_bill <- INMATE_ACTIVE_OFFENSES_PRPR_2 %>%
      filter(Disq_bill == TRUE) %>%
      select(DCNumber)
    
    # Combine the two lists and filter for a list of distinct numbers
 INMATE_ACTIVE_DISQ_literal <- rbind(INMATE_ACTIVE_OFFENSES_CPS_3_literal, INMATE_ACTIVE_OFFENSES_PRPR_3_literal) %>% distinct()
 INMATE_ACTIVE_DISQ_bill <- rbind(INMATE_ACTIVE_OFFENSES_CPS_3_bill, INMATE_ACTIVE_OFFENSES_PRPR_3_bill) %>% distinct()
    
  #4. Identify (and filter out) of the active inmates released in time to whom disqualifications apply:
      
     INMATE_ACTIVE_FULL <- INMATE_ACTIVE_ROOT_2 %>%
       mutate(Disq_bill = DCNumber %in% INMATE_ACTIVE_DISQ_bill$DCNumber,
              Disq_literal = DCNumber %in% INMATE_ACTIVE_DISQ_literal$DCNumber) %>%
       filter(Disq_literal == FALSE)
  
  #5. As proxy for address, use county in which most felonies have been conducted (if an equal number of felonies have been conducted in multiple counties, we go by alphabetical order of counties)
     
     PRPR_CPS_COUNTIES <- rbind(INMATE_ACTIVE_OFFENSES_PRPR_2, INMATE_ACTIVE_OFFENSES_CPS_2)
     
     INMATE_ACTIVE_OFFENSES_COUNTY <- PRPR_CPS_COUNTIES %>%
       filter(DCNumber %in% INMATE_ACTIVE_FULL$DCNumber)%>%
       unite("DCNumber_county", c(DCNumber, County), sep =" / ") %>%
       group_by(DCNumber_county)%>%
       summarize(Frequ = n())   %>%
       separate(DCNumber_county, c("DCNumber", "County"), sep = " / ") %>%
       arrange(desc(Frequ)) 
       
     INMATE_ACTIVE_OFFENSES_COUNTY_FULL <- INMATE_ACTIVE_OFFENSES_COUNTY[!duplicated(INMATE_ACTIVE_OFFENSES_COUNTY$DCNumber), ] %>% select(-Frequ)
     
  #6. Add counties to INMATE_ACTIVE_FULL
     
     INMATE_ACTIVE_FULL <- INMATE_ACTIVE_FULL %>%
       left_join(INMATE_ACTIVE_OFFENSES_COUNTY_FULL, by = c("DCNumber", "DCNumber"))
  
  #7. Save data in clean files:
     
  write_rds(INMATE_ACTIVE_FULL, "clean-data/INMATE_ACTIVE_FULL.rds", compress = "none")
     
  
  
        

#IV. Released inmates
     
     #1. Current inmates: Pull out data 
     
     INMATE_RELEASE_OFFENSES_CPS <- sqlQuery(Florida_correctional, paste ("select DCNumber, adjudicationcharge_descr from INMATE_RELEASE_OFFENSES_CPS"))
     
     INMATE_RELEASE_OFFENSES_PRPR <- sqlQuery(Florida_correctional, paste ("select DCNumber, adjudicationcharge_descr from INMATE_RELEASE_OFFENSES_PRPR"))

     INMATE_RELEASE_RESIDENCE <- sqlQuery(Florida_correctional, paste ("select DCNumber, AddressLine1, AddressLine2, City, State, ZipCode from INMATE_RELEASE_RESIDENCE"))
     
     INMATE_RELEASE_ROOT <- sqlQuery(Florida_correctional, paste ("select DCNumber, LastName, FirstName, MiddleName, NameSuffix, Sex, BirthDate, PrisonReleaseDate, race_descr from INMATE_RELEASE_ROOT"))
     
     
     #3. Filter out inmates that:
     # are likely dead
     # based on life exepectancy at release date and life expectancy acc. to average American in 2016: https://www.ssa.gov/oact/STATS/table4c6.html
     
     # reading in life expectancy
     Life_expectancy <- read.csv("raw-data/Life_expectancy.csv") 
     colnames(Life_expectancy)[colnames(Life_expectancy)=="ï..age"] <- "age_at_release"
     
     INMATE_RELEASE_ROOT_2 <- INMATE_RELEASE_ROOT %>% 
       drop_na(BirthDate, PrisonReleaseDate)%>%
       mutate(BirthDate = as.Date(BirthDate),
              PrisonReleaseDate = as.Date(PrisonReleaseDate),
              ElectionDate = as.Date("2020-10-05"),
              age_at_release = age_calc(BirthDate, PrisonReleaseDate, units = "years", precise = FALSE),
              age_at_election = age_calc(BirthDate, ElectionDate, units = "years", precise = FALSE))

       
    INMATE_RELEASE_ROOT_3 <- INMATE_RELEASE_ROOT_2 %>%
       left_join(Life_expectancy, by = c("age_at_release", "age_at_release"))%>%
        mutate(Life_expectancy_at_release = 
                 ifelse(Sex == "M", Male_life_expectancy, Female_life_expectancy),
               Presumed_alive = 
                 ifelse(Life_expectancy_at_release >= age_at_election - age_at_release, "Yes", "No")) %>%
    filter(Presumed_alive == "Yes")
    
    #3. List inmates that have committed murder or sexual offenses (disqualifying for right to vote) - selection based on offense categories (murder and sex crime) adjuste by hand to approximate literal amendment and bill, using reference:
     # Offense categories : http://www.dc.state.fl.us/offendersearch/offensecategory.aspx#KN
     #Source: 
     # Bill: https://www.tampabay.com/florida-politics/buzz/2019/05/26/how-felons-can-register-to-vote-in-florida-under-new-amendment-4-law/
     # Literal: https://www.pnj.com/story/news/2019/01/23/meaning-of-murder-key-in-florida-felons-voting-rights/2657973002/
     
     #Note: 1. Literally included in the amendment: 
     # Murder: 1st degree murder (Excludes 2-3rd degree murder, other homicides, manslaugther, DUI manslaughter)
     # Sexual offenses: Rape, sexual offenses against children
     #Note: 2. Included in the bill other than fines and restitutions 
     #Note: 3. Fines and restitutions: Don't know how to include yet
     
     # Current prison offenses
     INMATE_RELEASE_OFFENSES_CPS_2 <- INMATE_RELEASE_OFFENSES_CPS %>%
       mutate(Disq_bill = adjudicationcharge_descr %in% disq_offenses$ï..Offense[which(disq_offenses$Bill == "Yes")],
              Disq_literal = adjudicationcharge_descr %in% disq_offenses$ï..Offense[which(disq_offenses$Literal == "Yes")])
     
     INMATE_RELEASE_OFFENSES_CPS_3_literal <- INMATE_RELEASE_OFFENSES_CPS_2 %>%
       filter(Disq_literal == TRUE) %>%
       select(DCNumber) 
     
     INMATE_RELEASE_OFFENSES_CPS_3_bill <- INMATE_RELEASE_OFFENSES_CPS_2 %>%
       filter(Disq_bill == TRUE) %>%
       select(DCNumber)
     
     # Prior prison offenses
     
     INMATE_RELEASE_OFFENSES_PRPR_2 <- INMATE_RELEASE_OFFENSES_PRPR %>%
       mutate(Disq_bill = adjudicationcharge_descr %in% disq_offenses$ï..Offense[which(disq_offenses$Bill == "Yes")],
              Disq_literal = adjudicationcharge_descr %in% disq_offenses$ï..Offense[which(disq_offenses$Literal == "Yes")])
     view(INMATE_RELEASE_OFFENSES_PRPR_2)
     INMATE_RELEASE_OFFENSES_PRPR_3_literal <- INMATE_RELEASE_OFFENSES_PRPR_2 %>%
       filter(Disq_literal == TRUE) %>%
       select(DCNumber)
     
     INMATE_RELEASE_OFFENSES_PRPR_3_bill <- INMATE_RELEASE_OFFENSES_PRPR_2 %>%
       filter(Disq_bill == TRUE) %>%
       select(DCNumber)
     
     # Combine the two lists and filter for a list of distinct numbers
     INMATE_RELEASE_DISQ_literal <- rbind(INMATE_RELEASE_OFFENSES_CPS_3_literal, INMATE_RELEASE_OFFENSES_PRPR_3_literal) %>% distinct()
     INMATE_RELEASE_DISQ_bill <- rbind(INMATE_RELEASE_OFFENSES_CPS_3_bill, INMATE_RELEASE_OFFENSES_PRPR_3_bill) %>% distinct()
     
     #4. Identify (and filter out) of the released inmates still presumed alive at the election to whom disqualifications apply:
     
     INMATE_RELEASE_FULL <- INMATE_RELEASE_ROOT_3 %>%
       mutate(Disq_bill = DCNumber %in% INMATE_RELEASE_DISQ_bill$DCNumber,
              Disq_literal = DCNumber %in% INMATE_RELEASE_DISQ_literal$DCNumber) %>%
       filter(Disq_literal == FALSE)

   #5. Add Addresses to INMATE_RELEASE_FULL and Filter out inmates that:
   # don't have residence in Florida
   INMATE_RELEASE_FULL <- INMATE_RELEASE_FULL %>%
     left_join(INMATE_RELEASE_RESIDENCE, by = c("DCNumber", "DCNumber"))  %>%
     filter(State == "FL") %>%
     select(DCNumber, LastName, FirstName, MiddleName, NameSuffix, Sex, BirthDate, race_descr, Disq_bill, Disq_literal, AddressLine1, AddressLine2, City, State, ZipCode)
   

   #6. Save data in clean files:
   
   write_rds(INMATE_RELEASE_FULL, "clean-data/INMATE_RELEASE_FULL.rds", compress = "none")
   
   
   
      
#V. Supervised population
   #1. Currently supervised: Pull out data 
   
   OFFENDER_OFFENSES_CCS <- sqlQuery(Florida_correctional, paste ("select DCNumber, ProbationTerm, ParoleTerm, adjudicationcharge_descr from OFFENDER_OFFENSES_CCS"))
   
   OFFENDER_RESIDENCE <- sqlQuery(Florida_correctional, paste ("select DCNumber,AddressLine1, AddressLine2, City, State, ZipCode, Country from OFFENDER_RESIDENCE"))
   
   OFFENDER_ROOT <- sqlQuery(Florida_correctional, paste ("select DCNumber, LastName, FirstName, MiddleName, NameSuffix, Sex, BirthDate, SupervisionTerminationDate, supvstatus_description, race_descr from OFFENDER_ROOT"))
   
   #2. Filter out individuals on parole that:
   # will not remain under supervision beyond the Florida general election on November 3rd (registration has to happen by Oct 5th)
   
   OFFENDER_ROOT_2 <- OFFENDER_ROOT %>%
     filter(SupervisionTerminationDate <= as.Date("2020-10-05"))
   
   #3. Filter out supervised that:
   # are likely dead
   # based on life exepectancy at release date and life expectancy acc. to average American in 2016: https://www.ssa.gov/oact/STATS/table4c6.html
   
   OFFENDER_ROOT_3 <- OFFENDER_ROOT_2 %>% 
     drop_na(BirthDate, SupervisionTerminationDate)%>%
     mutate(BirthDate = as.Date(BirthDate),
            SupervisionTerminationDate = as.Date(SupervisionTerminationDate),
            ElectionDate = as.Date("2020-10-05"),
            age_at_release = age_calc(BirthDate, SupervisionTerminationDate, units = "years", precise = FALSE),
            age_at_election = age_calc(BirthDate, ElectionDate, units = "years", precise = FALSE))
   
   OFFENDER_ROOT_4 <- OFFENDER_ROOT_3 %>%
     left_join(Life_expectancy, by = c("age_at_release", "age_at_release"))%>%
     mutate(Life_expectancy_at_termination = 
              ifelse(Sex == "M", Male_life_expectancy, Female_life_expectancy),
            Presumed_alive = 
              ifelse(Life_expectancy_at_termination >= age_at_election - age_at_release, "Yes", "No")) %>%
     filter(Presumed_alive == "Yes")
   
   #4. List supervised that have committed murder or sexual offenses (disqualifying for right to vote) - selection based on offense categories (murder and sex crime) adjuste by hand to approximate literal amendment and bill, using reference:
   # Offense categories : http://www.dc.state.fl.us/offendersearch/offensecategory.aspx#KN
   #Source: 
   # Bill: https://www.tampabay.com/florida-politics/buzz/2019/05/26/how-felons-can-register-to-vote-in-florida-under-new-amendment-4-law/
   # Literal: https://www.pnj.com/story/news/2019/01/23/meaning-of-murder-key-in-florida-felons-voting-rights/2657973002/
   
   #Note: 1. Literally included in the amendment: 
   # Murder: 1st degree murder (Excludes 2-3rd degree murder, other homicides, manslaugther, DUI manslaughter)
   # Sexual offenses: Rape, sexual offenses against children
   #Note: 2. Included in the bill other than fines and restitutions 
   #Note: 3. Fines and restitutions: Don't know how to include yet
   
   # Prior prison offenses
   
   OFFENDER_OFFENSES_CCS_2 <- OFFENDER_OFFENSES_CCS %>%
     mutate(Disq_bill = adjudicationcharge_descr %in% disq_offenses$ï..Offense[which(disq_offenses$Bill == "Yes")],
            Disq_literal = adjudicationcharge_descr %in% disq_offenses$ï..Offense[which(disq_offenses$Literal == "Yes")])
   
   OFFENDER_OFFENSES_CCS_3_literal <- OFFENDER_OFFENSES_CCS_2 %>%
     filter(Disq_literal == TRUE) %>%
     select(DCNumber)
   
   OFFENDER_OFFENSES_CCS_3_bill <- OFFENDER_OFFENSES_CCS_2 %>%
     filter(Disq_bill == TRUE) %>%
     select(DCNumber)
   
      #4. Identify (and filter out) of the supervised released in time to whom disqualifications apply:
   
   OFFENDER_SUPERVISED_FULL <- OFFENDER_ROOT_4 %>%
     mutate(Disq_bill = DCNumber %in% OFFENDER_OFFENSES_CCS_3_bill$DCNumber,
            Disq_literal = DCNumber %in% OFFENDER_OFFENSES_CCS_3_literal$DCNumber) %>%
     filter(Disq_literal == FALSE)
   
   #5. Add Addresses to OFFENDER_SUPERVISED_FULL and Filter out inmates that:
   # don't have residence in US and in Florida
   OFFENDER_SUPERVISED_FULL <- OFFENDER_SUPERVISED_FULL %>%
     left_join(OFFENDER_RESIDENCE, by = c("DCNumber", "DCNumber"))  %>%
     filter(!supvstatus_description %in% c("ABSCONDER/FUGITIVE", "DEPORTED", "MOVED TO OTHER STATE")) %>%
     filter(State == "FL") %>%
     select(DCNumber, LastName, FirstName, MiddleName, NameSuffix, Sex, BirthDate, race_descr, Disq_bill, Disq_literal, AddressLine1, AddressLine2, City, State, ZipCode)
   
   #6. Save data in clean files:
   
   write_rds(OFFENDER_SUPERVISED_FULL, "clean-data/OFFENDER_SUPERVISED_FULL.rds", compress = "none")
   
   
   
   # Write CSV's
   write.csv(OFFENDER_SUPERVISED_FULL, "clean-data/OFFENDER_SUPERVISED_FULL.csv")
   write.csv(INMATE_ACTIVE_FULL, "clean-data/INMATE_ACTIVE_FULL.csv")
   write.csv(INMATE_RELEASE_FULL, "clean-data/INMATE_RELEASE_FULL.csv")
   
   
   # Understanding parole or probation data
    test <- OFFENDER_OFFENSES_CCS %>% group_by(DCNumber) %>%
      summarize(Total_parole = sum(ParoleTerm),
                Total_probation = sum(ProbationTerm))
    
    test %>% filter(Total_parole == 0) %>% nrow()
    test %>% filter(Total_probation == 0) %>% nrow()