library(eeptools)
library(purrr)
library(odbc)
library(DBI)
library(dplyr)
library(plyr)
library(lubridate)
library(readr)
library(tidyverse)
library(RODBC)
library(vroom)
library(tm)


#I. Create raw-data and clean-data directory to store the data that is to be downloaded      
dir.create("raw-data")
dir.create("clean-data")

#II. Load Florida data and store each database in an individual tab 
Florida_correctional <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/Alexander Klueber/Desktop/University/Coursework/PAE/Data analysis/PAE/raw-data/FDOC_Jan_2020.mdb")


#III. Active inmates
  #1. Current inmates: Pull out data 
  
  INMATE_ACTIVE_OFFENSES_CPS <- sqlQuery(Florida_correctional, paste ("select DCNumber, Sequence, County, adjudicationcharge_descr from INMATE_ACTIVE_OFFENSES_CPS"))
  
  INMATE_ACTIVE_OFFENSES_PRPR <- sqlQuery(Florida_correctional, paste ("select DCNumber, Sequence, County, adjudicationcharge_descr from INMATE_ACTIVE_OFFENSES_PRPR"))
  
  INMATE_ACTIVE_ROOT <- sqlQuery(Florida_correctional, paste ("select DCNumber, LastName, FirstName, MiddleName, NameSuffix, Sex, BirthDate, PrisonReleaseDate, ReceiptDate, race_descr from INMATE_ACTIVE_ROOT"))

  
  #2. Filter out inmates that:
    # will not be released before the Florida general election on November 3rd (Need to register by Oct. 5 2020)
  
  INMATE_ACTIVE_ROOT_2 <- INMATE_ACTIVE_ROOT %>%
    filter(PrisonReleaseDate <= as.Date("2020-10-05"))
  
  #3. List inmates that have committed murder or sexual offenses (disqualifying for right to vote) - selection based on offense categories (murder and sex crime), adjusted by hand to approximate literal amendment and bill, using reference:
      # Defining what qualifies as disqualifying offense offenses: 
         
         # Literal: https://www.pnj.com/story/news/2019/01/23/meaning-of-murder-key-in-florida-felons-voting-rights/2657973002/ 
               # Murder: 1st degree murder (Excludes 2nd degree murder, other homicides, etc.)
               # Sexual offenses: Rape, sexual offenses against children
            
         # Bill: https://www.tampabay.com/florida-politics/buzz/2019/05/26/how-felons-can-register-to-vote-in-florida-under-new-amendment-4-law/
         # Bill: SB 7066 - 98.0751 other than Legal Financial Obligations: http://laws.flrules.org/2019/162     
  
  
            # Note 1 - Bill: Legal Financial Obligations: court-ordered fees, fines and restitution:              no centralized database (See Mail by Daniel A. Smith). Include at county level based on               racial-county information metrics in Expert testimony by Daniel A. Smith in Figure 2 on p.             37. and then just on racial grounds could look at how much people tend to own (That would             then happen at the county after all these calculations to reduce the 1.5 million people)
        
  
            # Note 2 - Bill and Literal: Matching respective offenses from statues they violate to                offense codes (AON/FCIC) and then Correctional data: 
               # Offense categories: http://www.dc.state.fl.us/offendersearch/offensecategory.aspx#KN
               # FDLE's Statute Table: https://web.fdle.state.fl.us/statutes/about.jsf
  
        
    
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
  
     INMATE_ACTIVE_FULL_bill <- INMATE_ACTIVE_FULL %>%
        filter(Disq_bill == FALSE)
     
  #5. As proxy for address, use county in which most felonies have been conducted (if an equal number of felonies have been conducted in multiple counties, we go by alphabetical order of counties)
     
     PRPR_CPS_COUNTIES <- rbind(INMATE_ACTIVE_OFFENSES_PRPR_2, INMATE_ACTIVE_OFFENSES_CPS_2)
     
     INMATE_ACTIVE_OFFENSES_COUNTY <- PRPR_CPS_COUNTIES %>%
       filter(DCNumber %in% INMATE_ACTIVE_FULL$DCNumber)%>%
       unite("DCNumber_county", c(DCNumber, County), sep =" / ") %>%
       group_by(DCNumber_county)%>%
       dplyr:: summarize(Frequ = dplyr:: n())   %>%
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
     
     colnames(Life_expectancy)[colnames(Life_expectancy)=="ï..Age"] <- "age_at_release"
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
    
    
    
    #4. List inmates that have committed murder or sexual offenses (disqualifying for right to vote) - selection based on offense categories (murder and sex crime), adjusted by hand to approximate literal amendment and bill, using reference:
    
    # Defining what qualifies as disqualifying offense offenses: 
    # Literal: https://www.pnj.com/story/news/2019/01/23/meaning-of-murder-key-in-florida-felons-voting-rights/2657973002/ 
      # Murder: 1st degree murder (Excludes 2nd degree murder, other homicides, etc.)
      # Sexual offenses: Rape, sexual offenses against children
    
    # Bill: https://www.tampabay.com/florida-politics/buzz/2019/05/26/how-felons-can-register-to-vote-in-florida-under-new-amendment-4-law/
    # Bill: SB 7066 - 98.0751 other than Legal Financial Obligations: http://laws.flrules.org/2019/162     
    
    
         # Note 1 - Bill: Legal Financial Obligations: court-ordered fees, fines and restitution:              no centralized                   database (See Mail by Daniel A. Smith). Include at county level based on               racial-county information metrics             in Expert testimony by Daniel A. Smith in Figure 2 on p.             37. and then just on racial grounds could look at              how much people tend to own (That would             then happen at the county after all these calculations to reduce the             1.5 million people)
    
         # Note 2 - Bill and Literal: Matching respective offenses from statues they violate to offense codes (AON/FCIC) and then Correctional data: 
               # Offense categories: http://www.dc.state.fl.us/offendersearch/offensecategory.aspx#KN
               # FDLE's Statute Table: https://web.fdle.state.fl.us/statutes/about.jsf

    
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

     INMATE_RELEASE_FULL_bill <- INMATE_RELEASE_ROOT_3 %>%
        mutate(Disq_bill = DCNumber %in% INMATE_RELEASE_DISQ_bill$DCNumber,
               Disq_literal = DCNumber %in% INMATE_RELEASE_DISQ_literal$DCNumber) %>%
        filter(Disq_bill == FALSE)
     
   #5. Add Addresses to INMATE_RELEASE_FULL and Filter out inmates that:
   # don't have residence in Florida
   INMATE_RELEASE_FULL <- INMATE_RELEASE_FULL %>%
     left_join(INMATE_RELEASE_RESIDENCE, by = c("DCNumber", "DCNumber"))  %>%
     filter(State == "FL") %>%
     select(DCNumber, LastName, FirstName, MiddleName, NameSuffix, Sex, BirthDate, race_descr, Disq_bill, Disq_literal, AddressLine1, AddressLine2, City, State, ZipCode)
   
   
   # TO get total number: determine how many do not have Florida residence of all
   
  INMATE_RELEASE_Count <- INMATE_RELEASE_ROOT_3 %>%
     left_join(INMATE_RELEASE_RESIDENCE, by = c("DCNumber", "DCNumber"))  %>%
     filter(State == "FL")
   
  INMATE_RELEASE_Count_lit <- INMATE_RELEASE_FULL %>%
        filter(State == "FL")
  
  INMATE_RELEASE_Count_bill <- INMATE_RELEASE_FULL_bill %>%
    left_join(INMATE_RELEASE_RESIDENCE, by = c("DCNumber", "DCNumber"))  %>%
    filter(State == "FL")

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
   
   #4. List inmates that have committed murder or sexual offenses (disqualifying for right to vote) - selection based on offense categories (murder and sex crime), adjusted by hand to approximate literal amendment and bill, using reference:
   
   # Defining what qualifies as disqualifying offense offenses: 
   # Literal: https://www.pnj.com/story/news/2019/01/23/meaning-of-murder-key-in-florida-felons-voting-rights/2657973002/ 
   # Murder: 1st degree murder (Excludes 2nd degree murder, other homicides, etc.)
   # Sexual offenses: Rape, sexual offenses against children
   
   # Bill: https://www.tampabay.com/florida-politics/buzz/2019/05/26/how-felons-can-register-to-vote-in-florida-under-new-amendment-4-law/
   # Bill: SB 7066 - 98.0751 other than Legal Financial Obligations: http://laws.flrules.org/2019/162     
   
   
   # Note 1 - Bill: Legal Financial Obligations: court-ordered fees, fines and restitution:              no centralized                   database (See Mail by Daniel A. Smith). Include at county level based on               racial-county information metrics             in Expert testimony by Daniel A. Smith in Figure 2 on p.             37. and then just on racial grounds could look at              how much people tend to own (That would             then happen at the county after all these calculations to reduce the             1.5 million people)
   
   # Note 2 - Bill and Literal: Matching respective offenses from statues they violate to offense codes (AON/FCIC) and then Correctional data: 
   # Offense categories: http://www.dc.state.fl.us/offendersearch/offensecategory.aspx#KN
   # FDLE's Statute Table: https://web.fdle.state.fl.us/statutes/about.jsf
   
   
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
  
   OFFENDER_SUPERVISED_FULL_bill <- OFFENDER_SUPERVISED_FULL %>%
     filter(Disq_bill == FALSE) 
   
   # TO get total number: determine how many do not have Florida residence of all
   
   OFFENDER_SUPERVISED_Count <- OFFENDER_ROOT_4 %>%
     left_join(OFFENDER_RESIDENCE, by = c("DCNumber", "DCNumber"))  %>%
     filter(State == "FL")
   
   OFFENDER_SUPERVISED_Count_lit <- OFFENDER_SUPERVISED_FULL %>%
     filter(State == "FL")
   
   OFFENDER_SUPERVISED_Count_bill <- OFFENDER_SUPERVISED_FULL_bill %>%
     filter(State == "FL")
   

   #6. Save data in clean files:
   
   write_rds(OFFENDER_SUPERVISED_FULL, "clean-data/OFFENDER_SUPERVISED_FULL.rds", compress = "none")
   
   
#VI. Load and prepare voter registration data 
   
   #!!!! Once voter registration update is available, change here
   
   # 1. Read in text files
      # Get names of all the text files
   list_of_files <- list.files(path = "raw-data/Voter_Registration_20191210", recursive = TRUE,
                               pattern = "\\.txt$", 
                               full.names = TRUE)
   
   datalist = lapply(list_of_files, function(x)read.table(x, header=FALSE, sep="\t", fill = TRUE, quote="\"", stringsAsFactors = FALSE)) 
  

   # 2. Combine registration data to a data table
   
  registration_data <- do.call("rbind.fill", datalist)

  
  # 3. Rename column headers 
  
  colnames(registration_data)<- c("County Code", "Voter ID", "Name Last", "Name Suffix", "Name First", "Name Middle", "Requested public records exemption", "Residence Address Line 1", "Residence Address Line 2", "Residence City (USPS)", "Residence State", "Residence Zipcode", "Mailing Address Line 1", "Mailing Address Line 2", "Mailing Address Line 3", "Mailing City", "Mailing State", "Mailing Zipcode", "Mailing Country", "Gender", "Race", "Birth Date", "Registration Date", "Party Affiliation", "Precinct", "Precinct Group", "Precinct Split", "Precinct Suffix", "Voter Status", "Congressional District", "House District", "Senate District", "County Commission District", "School Board District", "Daytime Area Code", "Daytime Phone Number", "Daytime Phone Extension", "Email address")
  
 
 # 4. Reduce data set to columns relevant for this analysis
  
  registration_data_red <- registration_data %>%
                              select(`County Code`, `Voter ID`, `Name Last`, `Name First`, `Residence Address Line 1`, `Residence Address Line 2`, `Residence State`, `Residence Zipcode`, Gender, Race, `Birth Date`, `Registration Date`, `Party Affiliation`)
  

  # 5. Matching: changing all text in both databases to lowercase, removing all punctuation, concatenating a string consisting of a person’s first name, last name, name suffix, date of birth, race code, and sex code, and then matching the concatenated strings across both datasets.

   # 5.a Removing all punctuation

          registration_data_red$`Name Last` <- removePunctuation(as.character(registration_data_red$`Name Last`))
          
          registration_data_red$`Name First` <- removePunctuation(as.character(registration_data_red$`Name First`))
          
          registration_data_red$Gender <- removePunctuation(as.character(registration_data_red$Gender))
          
          registration_data_red$`Birth Date` <- removePunctuation(as.character(registration_data_red$`Birth Date`))
    
          
  # 5.b Concatennating it all to one string
  
  registration_data_red <- registration_data_red %>% 
                                    mutate(string = paste(`Name Last`, `Name First`, Gender, Race, `Birth Date`, sep=""))
  
  # 5.c Make string lower case

  registration_data_red$string <- tolower(registration_data_red$string)
  

  #6. Save data in clean files:
  
  write_rds(registration_data_red, "clean-data/registration_data_red.rds", compress = "none")
  

#X. OTHER: Refining selections:

    # Refining selection of offenses:
      # Pull out unique offenses in correctional databases
    INMATE_ACTIVE_CPS_OFFENSES <- unique(INMATE_ACTIVE_OFFENSES_CPS$adjudicationcharge_descr)
    INMATE_ACTIVE_PRPR_OFFENSES <- unique(INMATE_ACTIVE_OFFENSES_PRPR$adjudicationcharge_descr)
    INMATE_RELEASE_CPS_OFFENSES <- unique(INMATE_RELEASE_OFFENSES_CPS$adjudicationcharge_descr)
    INMATE_RELEASE_PRPR_OFFENSES <- unique(INMATE_RELEASE_OFFENSES_PRPR$adjudicationcharge_descr)
    OFFENDER_OFFENSES <- unique(OFFENDER_OFFENSES_CCS$adjudicationcharge_descr)
    
      # Combine unique offenses to one list
    write.csv(INMATE_ACTIVE_CPS_OFFENSES, "raw-data/INMATE_ACTIVE_CPS_OFFENSES.csv")
    write.csv(INMATE_ACTIVE_PRPR_OFFENSES, "raw-data/INMATE_ACTIVE_PRPR_OFFENSES.csv")
    write.csv(INMATE_RELEASE_CPS_OFFENSES, "raw-data/INMATE_RELEASE_CPS_OFFENSES.csv")
    write.csv(INMATE_RELEASE_PRPR_OFFENSES, "raw-data/INMATE_RELEASE_PRPR_OFFENSES.csv")
    write.csv(OFFENDER_OFFENSES, "raw-data/OFFENDER_OFFENSES.csv")
    