#' ---
#' title: "ONS LAD 2019 data cleaning"
#' author: "Luis Chaves"
#' date: "Summer 2020"
#' output: html_document, pdf_document
#' ---


#' # Data cleaning report

#+ include = FALSE
knitr::opts_chunk$set(message = F, warning = F)

#' ## Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
theme_set(theme_bw())

library(imputeTS)# time series imputation
library(naniar)# for missing data visualisation

library(knitr) # for rmarkdown rendering

debugSource('time_impute.R', echo = T) # time series wrapper imputation function
debugSource('updateCodes.R', echo = T)
#' ## Load Data

# deaths = read.csv('OriginalData/deathrecords2020.csv') # caveat: only 2020 
# deaths = read.csv('OriginalData/DEATHS02010_2019.csv') # caveat: only by wider regions (around 13)
# -- not using for now as only 2020 deaths

lifeexpect = read.csv('OriginalData/lifeexpectancies.csv')

# population = read_xlsx('OriginalData/popestimates.xlsx',
#                        sheet = 'Dataset',
#                        range = cell_rows(c(3, 121433)))
population = read.csv('OriginalData/POPULATION2018_2019.csv')

suicides = read.csv('OriginalData/suicide2018.csv')

wellbeing = read.csv('OriginalData/wellbeing.csv')

work = read.csv('OriginalData/working.csv')
work_ts = read.csv('OriginalData/working_time_series.csv')

gdp = read_xlsx('OriginalData/regionalgrossdomesticproductgdplocalauthorities.xlsx',
                sheet = 'Table 5',
                range = cell_rows(c(2, 384)))

unemployment = read_xls('OriginalData/unemployment.xls',
                        range = 'A3:HA375', sheet = 3)

#' ## Clean data
#' 
#' Here I take data cleaning on a case-by-case basis: tidying and imputing each data set at a time,
#' indepently of each other. A few comments that apply to all the ONS datasets: all data sets appear to have
#' redundant columns (as in duplicates), secondly for some of them the data corresponds to an interval and
#' for some others the data is available for every year.
#' 
#' ### Deaths table (not used in the end)

# deaths table: some columns are duplicate so we remove those, we also change the week strings to week numbers

# str(deaths)
# 
# deaths = deaths %>%
#   select(-c(Data.Marking,calendar.years,week, cause.of.death, place.of.death, registration.or.occurrence)) %>%
#   rename(Deaths = V4_1, year = time, areaCode = admin.geography,
#          areaName = geography, deathCause = causeofdeath,
#          deathPlace = placeofdeath, week = week.number) %>%
#   mutate(week = as.integer(str_replace(week, 'week-', '')))

#' ### Life expectancy table
#' #### Visualise raw data

str(lifeexpect)

#' #### Modify initial data
#' 
#' As there are many duplicated columns, we will delete those that are either duplicated or empty
#' and give better names to the ones we keep, as well as changing the type of numeric variables that
#' are registered as strings

lifeexpect = lifeexpect %>% 
  select(-c(Data_Marking,
            two.year.intervals,
            life.expectancy.variable,
            birth.cohort)) %>%
  rename(Value = V4_3,
         lowerCI = lower.confidence.limit,
         upperCI = upper.confidence.limit,
         year = time,
         areaCode = admin.geography,
         areaName = geography,
         LEvariable = lifeexpectancyvariable,
         cohort = birthcohort) %>%
  mutate(lowerCI = as.numeric(lowerCI),
         upperCI = as.numeric(upperCI))

#' #### Modify year intervals to years
#' The life expectancy information is available in the year intervals `r{unique(lifeexpect$year)}`,
#' to be consistent with the datasets that are available for every year we turn intervals into years. We do this 
#' by taking the upper value of each interval as the year column. e.g.: turning 2013-15 to 2015 
#' and 2014-16 to 2016 and so forth

lifeexpect$year = as.numeric(lapply(strsplit(lifeexpect$year, '-'), '[[',1))+2

#' #### Check missing data with respect to several factors
#' Now we check missing data in the `Value` column by cohort, type of life expectancy variable and year


kable(table(is.na(lifeexpect$Value), lifeexpect$cohort)) # all cohorts with same amount of data
kable(table(is.na(lifeexpect$Value), lifeexpect$LEvariable)) # lifeexpectancy with the least missing data
kable(table(is.na(lifeexpect$Value), lifeexpect$year)) # 2017 with most missing data


#' #### Shortening strings that are too long

lifeexpect = lifeexpect %>%
  mutate(LEvariable = ifelse(LEvariable == 'Disability-free life expectancy',
                             'DFLE',
                             ifelse(LEvariable == 'Healthy life expectancy',
                                    'HLE',
                                    'LE')),
         cohort = ifelse(cohort == 'Females at age 65',
                         'FemAt65',
                         ifelse(cohort == 'Males at age 65',
                                'MaleAt65',
                                ifelse(cohort == 'Females at birth',
                                       'FemAtBirth',
                                       'MaleAtBirth'))))

#' #### Check number of entries with missing values for all years
#' 
#' Here we wish to know how many entries, defined by unique combinations of the local authority code (`areaCode`),
#' the life expectancy(LE) variable (`LEvariable`, e.g. healthy LE, Disability-free LE...) and cohort (`cohort`, e.g. Male at birth, 
#' Female at 65...) have no available values (aka all values missing) for all available years.

count = 0
combs = 0
for (reg in unique(lifeexpect$areaCode))
  for (var in unique(lifeexpect$LEvariable))
    for(coh in unique(lifeexpect$cohort))
    {
      combs = combs +1
      
      if (all(is.na(lifeexpect %>% filter(areaCode == reg, LEvariable == var, cohort == coh) %>% select(Value)))){
        count = count+1
      }
    }

cat(paste(count, 'combinations out of', combs, 'combinations have all missing entries'))


#' #### Missingness count by number of missing year entries

lifeexpect %>%
  group_by(areaCode,
           LEvariable,
           cohort) %>%
  summarise(NumberOfMissingEntries = sum(is.na(Value)),
            ProprortionOfMissingEntries = paste0(round(mean(is.na(Value)),3)*100,' %')) %>%
  group_by(NumberOfMissingEntries,
           ProprortionOfMissingEntries) %>%
  summarise(Count = n()) %>% 
  kable()

#' #### Time series imputation
#' 
#' First we order the data by year, the time series works assuming the data is ordered.

lifeexpect = lifeexpect %>% arrange(areaCode, LEvariable, cohort, year)

lifeexpect = time_impute(lifeexpect, areaCode, LEvariable, cohort,
                         year_column = 'year')

#' #### Final check on missingness count

lifeexpect %>%
  group_by(areaCode,
           LEvariable,
           cohort) %>%
  summarise(NumberOfMissingEntries = sum(is.na(Value)),
            ProportionOfMissingEntries = mean(is.na(Value))) %>%
  group_by(NumberOfMissingEntries,
           ProportionOfMissingEntries) %>%
  summarise(Count = n()) %>%
  kable()

#' #### Gather data in its final form
#' 
#' We finally filter for the latest year (in this case 2017) and pivot the data to get a tidy format. We 
#' also keep a copy of the tidy time-series data (just in case)

lifeexpect_time = lifeexpect %>%
  pivot_wider(names_from = c(LEvariable, cohort),
              values_from = c(Value, lowerCI, upperCI))

lifeexpect = lifeexpect_time %>%
  filter(year == 2017)

str(lifeexpect)

#' #### Visualise missing data
vis_miss(lifeexpect) 

#' We notice the variables DFLE and HLE have a lot of missingness hence we only keep the LE variables

lifeexpect_time = lifeexpect_time %>% select(year, areaCode, areaName, contains('_LE_'))
lifeexpect = lifeexpect %>% select(year, areaCode, areaName, contains('_LE_'))

#' #### Update old area codes
#' 
#' Pass through custom function to update old codes, then combine the entries with matching area name 
#' and area code
#' 
#' ##### For lifeexpect

lifeexpect = updateCodes(lifeexpect)

combs_dup = lifeexpect %>%
  group_by(areaCode, areaName, year) %>% summarise(duplicat = n()>1)

print(
  lifeexpect[lifeexpect$areaCode %in%
               combs_dup$areaCode[combs_dup$duplicat==T],c(2,3,6,7)])

#' This function combines entries by taking the mean when they are duplicated
for (row in which(combs_dup$duplicat==T)){
  entry = combs_dup[row,]
  
  lifeexpect[(lifeexpect$areaCode == entry$areaCode &
                lifeexpect$areaName == entry$areaName &
                lifeexpect$year == entry$year),
             !colnames(lifeexpect) %in% c('areaCode','areaName', 'year')] = 
    t(apply(lifeexpect[(lifeexpect$areaCode == entry$areaCode &
                lifeexpect$areaName == entry$areaName &
                lifeexpect$year == entry$year),
             !colnames(lifeexpect) %in% c('areaCode','areaName', 'year')],2,mean))
  
}

print(
  lifeexpect[lifeexpect$areaCode %in%
               combs_dup$areaCode[combs_dup$duplicat==T],c(2,3,6,7)])

#' Finally, select unique rows as we have generated duplicated entries in the previous step
lifeexpect = lifeexpect %>% distinct()

#'##### For lifeexpecttime
#'
lifeexpect_time = updateCodes(lifeexpect_time)

combs_dup = lifeexpect_time %>%
  group_by(areaCode, areaName, year) %>% summarise(duplicat = n()>1)

print(
  lifeexpect_time[lifeexpect_time$areaCode %in%
               combs_dup$areaCode[combs_dup$duplicat==T],c(3,6,7)])

#' This function combines entries by taking the mean when they are duplicated
for (row in which(combs_dup$duplicat==T)){
  entry = combs_dup[row,]
  
  lifeexpect_time[(lifeexpect_time$areaCode == entry$areaCode &
                lifeexpect_time$areaName == entry$areaName &
                lifeexpect_time$year == entry$year),
             !colnames(lifeexpect_time) %in% c('areaCode','areaName', 'year')] = 
    t(apply(lifeexpect_time[(lifeexpect_time$areaCode == entry$areaCode &
                          lifeexpect_time$areaName == entry$areaName &
                          lifeexpect_time$year == entry$year),
                       !colnames(lifeexpect_time) %in% c('areaCode','areaName', 'year')],2,mean))
  
}

print(
  lifeexpect_time[lifeexpect_time$areaCode %in%
               combs_dup$areaCode[combs_dup$duplicat==T],c(3,6,7)])

#' Finally, select unique rows as we have generated duplicated entries in the previous step
lifeexpect_time = lifeexpect_time %>% distinct()

#' ### Population table
#' 
#' Out if this dataset we will extract information regarding age statistics and population size

tail(population)

#' We notice below that the column containing the population estimates contains no missing values which is great

sum(is.na(population$v4_0)) # no missing values, thank god, no need for imputation

#' #### (un)select, filter, rename

population = population %>% 
  select(-c(calendar.years,
            mid.year.pop.sex,
            mid.year.pop.age))%>%
  rename(areaName = geography,
         areaCode = admin.geography,
         Value =  v4_0,
         year = time)

#' ##### Update old codes
#' 
#' In this case we update the codes before tidying because we are going to perform aggregations later
#' 
population = updateCodes(population)

#' #### Visualise the age distribution
#' 
#' Distribution of age coloured by gender for 9 random local authority districts
population %>% 
  filter(year == 2018) %>% 
  filter(sex != 'All',
         age != 'Total',
         areaName %in% sample(areaName, 9)) %>%
  mutate(age = as.numeric(ifelse(age == '90+', 90, age))) %>% 
  ggplot(aes(x = age,
             y = Value,
             fill = sex))+
  geom_col()+
  facet_wrap(vars(areaName), scales = 'free')+
  scale_x_continuous(breaks = seq(0,100, by = 20))

#' #### Check if all ages are represented
#' 
#' Check if there are any holes in the age records
unique_ages = data.matrix(unique(population %>% 
                                   filter(age != 'Total') %>%
                                   mutate(age = as.numeric(ifelse(age == '90+', 90, age))) %>%
                                   select(age)))
all(unique_ages %in% c(0:90))

#' For population I can extract:
#'    * age info:
#'        * mean
#'        * median
#'        * mode: when calculating the mode, sometimes for some miracle
#' of the universe the frequency for a given age group and a given sex is the same
#' (See Wrexham in population df as they have 1015 for 46 and 47 y/o males), in these cases
#' I'm taking the mean for those cases (i.e. 46.5 in above example)
#'        * stdev
#'        * ~~range~~
#'        * ~~max~~ 
#'        * ~~min~~ in practice all LADs have at least one 0 year old and one 90+ year old
#'        * age bins, number of people in each age bin
#'        
#'    * pop info:
#'        * total
#'        * total males
#'        * total females
#'      
#' All stratified by gender and not stratified
#' 
#' We define the age bins arbitrarily to represent: children and teens (0-16 y/o), young adults (17-29 y/o),
#' adults (30-63 y/o), old (64-80) and very old (80+).
#' 
#' To calculate things like median and mode I actually 'explode' the age column with the `rep()` function
#' getting the count for each age from the `Value` column and then calculate the statistics in question

age_time = population %>%
  filter(age != 'Total' & sex != 'All') %>%
  mutate(age = as.numeric(ifelse(age == '90+', 90, age))) %>%
  group_by(year,
           areaCode,
           areaName,
           sex) %>% 
  summarise(TotalPeople = sum(Value),
            MeanAge = sum(age*Value)/TotalPeople,
            MedianAge = median(rep(age,Value)),
            ModeAge = mean(age[which(Value == max(Value))]),
            stdevAge = sd(rep(age,Value)),
            NumberOfTeens = sum(Value[which(age<17)]),
            NumberOfYoungAdults = sum(Value[which(age>=17 & age<30)]),
            NumberOfAdults = sum(Value[which(age>=30 & age<64)]),
            NumberOfOld = sum(Value[which(age>=64 & age<81)]),
            NumberOfVeryOld = sum(Value[which(age>80)]))

age = age_time %>% filter(year == 2018)

age_time = age_time %>%
  pivot_wider(names_from = sex,
              values_from = c(TotalPeople,
                              MeanAge,
                              MedianAge,
                              ModeAge,
                              stdevAge,
                              NumberOfTeens,
                              NumberOfYoungAdults,
                              NumberOfAdults,
                              NumberOfOld,
                              NumberOfVeryOld)
  )


age  = age %>%
  pivot_wider(names_from = sex,
              values_from = c(TotalPeople,
                              MeanAge,
                              MedianAge,
                              ModeAge,
                              stdevAge,
                              NumberOfTeens,
                              NumberOfYoungAdults,
                              NumberOfAdults,
                              NumberOfOld,
                              NumberOfVeryOld)
  )

#' #### Total(Female+Male) population statistics
#' 

agetotal_time = population %>%
  filter(age != 'Total' & sex != 'All') %>%
  mutate(age = as.numeric(ifelse(age == '90+', 90, age))) %>%
  group_by(year,
           areaCode,
           areaName) %>% # note we are not aggregating by sex 
  summarise(TotalPeople = sum(Value),
            MeanAge = sum(age*Value)/TotalPeople,
            MedianAge = median(rep(age,Value)),
            ModeAge = mean(age[which(Value == max(Value))]),
            stdevAge = sd(rep(age,Value)),
            NumberOfTeens = sum(Value[which(age<17)]),
            NumberOfYoungAdults = sum(Value[which(age>=17 & age<30)]),
            NumberOfAdults = sum(Value[which(age>=30 & age<64)]),
            NumberOfOld = sum(Value[which(age>=64 & age<81)]),
            NumberOfVeryOld = sum(Value[which(age>80)]))

agetotal = agetotal_time %>% filter(year == 2018)

colnames(agetotal_time)[4:ncol(agetotal_time)] = paste0(
  colnames(agetotal_time)[4:ncol(agetotal_time)], '_All')

colnames(agetotal)[4:ncol(agetotal)] = paste0(
  colnames(agetotal)[4:ncol(agetotal)], '_All')

#'#### Merge into a single table

popclean_time = merge(age_time, agetotal_time, by = c('year','areaCode','areaName'))

popclean = merge(age, agetotal, by = c('year','areaCode','areaName'))

#' #### Update old area codes
#' 
#' Pass through custom function to update old codes, then combine the entries with matching area name 
#' and area code
#' 
#' ##### For population table

popclean = updateCodes(popclean)

combs_dup = popclean %>%
  group_by(areaCode, areaName,year) %>% summarise(duplicat = n()>1)

# In this case luckily we had no bad entries
print(
  popclean[popclean$areaCode %in%
               combs_dup$areaCode[combs_dup$duplicat==T],c(3,6,7)])


#' ##### For popclean_time table

popclean_time = updateCodes(popclean_time)

combs_dup = popclean_time %>%
  group_by(areaCode, areaName,year) %>% summarise(duplicat = n()>1)

print(
  popclean_time[popclean_time$areaCode %in%
               combs_dup$areaCode[combs_dup$duplicat==T],c(3,6,7)])

#'### Suicides table

str(suicides)

#' #### Select, rename

suicides = suicides %>% 
  select(-calendar.years) %>% 
  rename(Value = V4_0,
         year = time,
         areaCode = admin.geography,
         areaName = geography)


#' #### Again no NAs 

sum(is.na(suicides$Value)) # no NAs good

#' According the command below all locations have data available for all years

sum(is.na(suicides %>%
            pivot_wider(names_from = year, values_from = Value)))

#' #### Time-series plot of a few LADs
suicides %>%
  filter(areaName %in% sample(areaName, 9)) %>%
  ggplot(aes(x = year, y = Value))+
  geom_point()+
  facet_wrap(vars(areaName))

#' #### Update codes
#' 
suicides = updateCodes(suicides)

combs_dup = suicides %>%
  group_by(areaCode, areaName, year) %>% summarise(duplicat = n()>1)

print(
  suicides[suicides$areaCode %in%
               combs_dup$areaCode[combs_dup$duplicat==T],c(1)])

#' # No duplicates again!

#'#### Store time-series and most up-to-date one
#'

suicides_time = suicides  %>% rename(NumberOfSuicides = Value)
suicides = suicides_time %>% filter(year == 2018)

#'### Wellbeing table

#'#### Evaluate content of table
tail(wellbeing)

#'#### Missing data
#'
#'We observe quite a lot of data missingness here

sum(is.na(wellbeing$V4_3))
mean(is.na(wellbeing$V4_3))

#'#### Select, rename, mutate
wellbeing = wellbeing %>% 
  select(-c(Data.Marking, 
            yyyy.yy,
            wellbeing.measureofwellbeing,
            wellbeing.estimate)) %>%
  rename(Value = V4_3,
         lowerCI = Lower.limit,
         upperCI = Upper.limit,
         year = time,
         areaCode = admin.geography,
         areaName = geography,
         WellbeingMetric = allmeasuresofwellbeing,
         EstimateBin = estimate) %>% 
  mutate(lowerCI = as.numeric(lowerCI),
         upperCI = as.numeric(upperCI))


#' #### Turn year interval to year
#' 
#' Same operation that we did for the `lifeexpect` table, here the intervals are only 2 years long though.
#' (e.g. 2014-15 --> 2015)

wellbeing$year = as.numeric(lapply(strsplit(wellbeing$year, '-'), '[[',1))+1

#' #### Variable selection
#' 
#' The wellbeing data is very rich, this data was gathered by running a survey with 4 self-assesed
#' questions. For each question the user can score a value between 0 and 10. The `wellbeing` table
#' contains the average score for each category 
#' (i.e. `r{paste(unique(wellbeing$wellbeing.measureofwellbeing), collapse= ', ')}`) as well as the proportion
#' of people in different bins (i.e. `r{paste(unique(wellbeing$estimate), collapse= ', ')}`). I believe the average
#' score is more interpretable than the bins and more concise, though there are some caveats to this. Read more
#' about the methodology behing these metrics in 
#' [the ONS site](https://www.ons.gov.uk/peoplepopulationandcommunity/wellbeing/methodologies/personalwellbeingintheukqmi)
#' 

wellbeing = wellbeing %>% filter(EstimateBin == 'Average (mean)')

#' #### Evaluate missingness after filtering
sum(is.na(wellbeing$Value)) # 117 missing values

#' #### Missingness by area and by wellbeing metric
#' 
#' In this case if the output is 8, all entries are missing for all 8 years the variable was measured

wellbeing %>%
  group_by(areaCode,
           WellbeingMetric) %>%
  summarise(NumberOfMissingEntries = sum(is.na(Value)),
            ProportionOfMissingEntries = mean(is.na(Value))) %>%
  group_by(NumberOfMissingEntries,
           ProportionOfMissingEntries) %>%
  summarise(Count = n()) %>%
  kable()

#'#### Missingness by year
wellbeing %>%
  group_by(year) %>%
  summarise(NumberOfMissingEntries = sum(is.na(Value)),
            ProportionOfMissingEntries = mean(is.na(Value))) %>%
  kable()

#' #### Time-series plot
wellbeing %>% 
  filter(areaName %in% sample(areaName, 9)) %>% 
  ggplot(aes(x = year,
             y = Value,
             color = WellbeingMetric)) +
  geom_point() +
  facet_wrap(vars(areaName))


#' #### Edge-cases
#' 
#' The LADs with all missingness are the City of london and the Isles of Scilly. These two often have
#' no available data from the ONS

#'#### Time-series imputation

#' Order the data before imputation (just in case)
#' 
wellbeing = wellbeing %>% arrange(areaCode, WellbeingMetric, year)

wellbeing = time_impute(wellbeing, areaCode, WellbeingMetric)

#' ##### Results (as expected)
wellbeing %>%
  group_by(areaCode,
           WellbeingMetric) %>%
  summarise(NumberOfMissingEntries = sum(is.na(Value)),
            ProportionOfMissingEntries = mean(is.na(Value))) %>%
  group_by(NumberOfMissingEntries,
           ProportionOfMissingEntries) %>%
  summarise(Count = n()) %>%
  kable()

#' (Updated) missingness by year
wellbeing %>%
  group_by(year) %>%
  summarise(NumberOfMissingEntries = sum(is.na(Value))) %>%
  kable()

#' #### Gather data 
#' 
wellbeing_time = wellbeing %>%
  mutate(Range = upperCI - lowerCI) %>%
  select(Value,
         Range,
         year,
         areaCode,
         areaName,
         WellbeingMetric) %>%
  pivot_wider(names_from = WellbeingMetric,
              values_from = c(Value, Range)) 

#' #### Update codes
#' 
wellbeing_time = updateCodes(wellbeing_time)

combs_dup = wellbeing_time %>%
  group_by(areaCode, areaName, year) %>% summarise(duplicat = n()>1)

print(
  wellbeing_time[wellbeing_time$areaCode %in%
              combs_dup$areaCode[combs_dup$duplicat==T],c(1,2,3,4)] %>%
    arrange(areaCode, areaName,year), n = 100)

#' Many duplicates now
#' 
#' #' This function combines entries by taking the __mean__ when they are duplicated
for (row in which(combs_dup$duplicat==T)){
  entry = combs_dup[row,]
  
  wellbeing_time[(wellbeing_time$areaCode == entry$areaCode &
                wellbeing_time$areaName == entry$areaName &
                wellbeing_time$year == entry$year),
             !colnames(wellbeing_time) %in% c('areaCode','areaName', 'year')] = 
    t(apply(wellbeing_time[(wellbeing_time$areaCode == entry$areaCode &
                          wellbeing_time$areaName == entry$areaName &
                          wellbeing_time$year == entry$year),
                       !colnames(wellbeing_time) %in% c('areaCode','areaName', 'year')],2,mean))
  
}

print(
  wellbeing_time[wellbeing_time$areaCode %in%
               combs_dup$areaCode[combs_dup$duplicat==T],c(1,2,3,4)])

#' Finally, select unique rows as we have generated duplicated entries in the previous step
wellbeing_time = wellbeing_time %>% distinct()

wellbeing = wellbeing_time %>%
  filter(year == 2019)

#'### Work table

#' The ASHE work table contains a column called coefficient of variation which is an estimate of the
#' variation within the sample and it helps evaluate whether the sample can be considered a good
#' representation of the population. Find explanation of the Coefficient of variation (CV) column 
#' [here](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/methodologies/annualsurveyofhoursandearningslowpayandannualsurveyofhoursandearningspensionresultsqmi)

#' Moreover, in this section we have two tables, `work` and `work_ts`, the 1st one contains info for 2019 and the second one contains
#' data from earlier years to enable us to impute the 2019 data.
str(work)

#'#### Select, rename, mutate
work = work %>% 
  select(-c(Data.Marking,
            calendar.years,
            ashe.working.pattern,
            ashe.sex,
            ashe.statistics,
            ashe.workplace.or.residence,
            ashe.hours.and.earnings,
            ashe.working.pattern)) %>%
  rename(Value = V4_2,
         year = time,
         areaCode = admin.geography,
         areaName = geography,
         CoefficientOfVariation = CV) %>%
  mutate(CoefficientOfVariation = as.numeric(CoefficientOfVariation))


#'#### Explore a few columns
#'
unique(work$hoursandearnings)
unique(work$workplaceorresidence)
unique(work$workingpattern)
unique(work$statistics)

#' #### Many, many, many statistics available
#' 
#' Too many stats are available as seen above, we are only interested in a few. We select mean, median,
#' 10th and 90th percentile
work = work %>%
  filter(statistics %in% c('Mean', 'Median', '10th percentile', '90th percentile'),
         workplaceorresidence == 'Workplace')

#' #### Select earnings metric
#' 
#' From the above command we see that hourly pay has the least missingness so we'll go
#' with that one
work %>% 
  group_by(hoursandearnings) %>% 
  summarise(NumberOfMissingEntries = mean(is.na(Value))) %>%
  kable()


#' Now we evaulate the missingess of each earning metric per statistic to see if
#' some combination of these is missing at an unacceptable rate
work %>%
  group_by(hoursandearnings, statistics) %>%
  summarise(NumberOfMissingEntries = mean(is.na(Value))) %>%
  ggplot(aes(x = hoursandearnings,
             y = statistics,
             fill = NumberOfMissingEntries,
             label = round(NumberOfMissingEntries,2)))+
  geom_tile()+
  geom_text(color = 'white')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#' this last graph reveals that the 90th and the 10th percentile stats have too much 
#' missingess hence I am dropping them, it also show how Hourly pay is the
#' most covered stat
#'
#' Based on previous graph I decided to choose hourly pay
#' and mean and median only

work = work %>%
  filter(hoursandearnings == 'Hourly pay - Gross',
         statistics %in% c('Median', 'Mean'))

#' Given that now the `hoursandearnings` and the `worplaceorresidence` columns contain 
#' unique values only I drop them

work = work %>% select(-c(hoursandearnings, workplaceorresidence))

#' #### Work time-series table

#' ##### Select, rename, mutate 
work_ts = work_ts %>% 
  select(-c(Data_Marking,
            calendar.years,
            ashe.working.pattern,
            ashe.sex,
            ashe.statistics,
            ashe.workplace.or.residence,
            ashe.hours.and.earnings,
            ashe.working.pattern)) %>%
  rename(Value = V4_2,
         year = time,
         areaCode = admin.geography,
         areaName = geography,
         CoefficientOfVariation = CV) %>%
  mutate(CoefficientOfVariation = as.numeric(CoefficientOfVariation))


#' Filter the same way as for the `work` dataframe
#' 
work_ts = work_ts %>%
  filter(statistics %in% c('Mean', 'Median', '10th percentile', '90th percentile'),
         workplaceorresidence == 'Workplace')

work_ts = work_ts %>%
  filter(hoursandearnings == 'Hourly pay - Gross',
         statistics %in% c('Median', 'Mean'))

work_ts = work_ts %>% select(-c(hoursandearnings, workplaceorresidence))

#' ##### Check for any disparity between the areas covered by both of these dataframes
#' 
unique(work$areaName[!(work$areaName %in% work_ts$areaName)])

#' Now merge work (data from 2019 only) and work_ts (2016 through 2018)
work = rbind(work, work_ts)

#' #### Start exploring dataset
#' 
#' I believe not all towns have entries for all years
#' in theory there should be 4 entries (2016-19) per combination of
#' areacode, stat, sex and working pattern
work %>%
  group_by(areaCode,
           statistics,
           sex,
           workingpattern) %>%
  summarise(NumberOfYearsWithAvailableData = n()) %>% ## this takes the numbers
  # of entries per unique combination of the variables in the group_by
  group_by(NumberOfYearsWithAvailableData) %>%
  summarise(Count = n()) %>%
  kable()


#' As you can see in the above table for some combinations
#' there are only 3 available variables hence 3 missing variables
#' represent 100% where as in other cases 4 missing varialbes represent 
#' 100% of variables
#' 
work %>%
  group_by(areaCode,
           statistics,
           sex,
           workingpattern) %>%
  summarise(NumberOfMissingEntries = sum(is.na(Value)),
            ProportionOfMissingEntries = mean(is.na(Value))) %>%
  group_by(NumberOfMissingEntries,
           ProportionOfMissingEntries) %>%
  summarise(Count = n()) %>%
  kable()

#' Missingness by year
work %>%
  group_by(year) %>%
  summarise(NumberOfMissingEntries = sum(is.na(Value)),
            ProportionOfMissingEntries = mean(is.na(Value))) %>%
  kable()

#' #### Time-series imputation
#' 
#' Make sure data is ordered by year
work = work %>% arrange(areaCode, statistics, sex, workingpattern, year)
work = time_impute(work, cols_to_impute = c('Value', 'CoefficientOfVariation'), 
                   areaCode, statistics, sex, workingpattern)

#' #### Results: (updated) missingness
work %>%
  group_by(areaCode,
           statistics,
           sex,
           workingpattern) %>%
  summarise(NumberOfMissingEntries = sum(is.na(Value)),
            ProportionOfMissingEntries = mean(is.na(Value))) %>%
  group_by(NumberOfMissingEntries,
           ProportionOfMissingEntries) %>%
  summarise(Count = n())

work %>%
  group_by(year) %>%
  summarise(NumberOfMissingEntries = sum(is.na(Value)),
            ProportionOfMissingEntries = mean(is.na(Value))) %>%
  kable()

#' #### Edge cases
#' 
#' Finally we gotta be careful with those entries where records ends in 2018,
#'  we have to characterise them

work %>% 
  group_by(areaName) %>%
  summarise(latestAvailableYear = max(year)) %>%
  group_by(latestAvailableYear) %>%
  summarise(Count = n()) %>%
  kable()

#' Note: interestingly, by area code you get slightly different numbers than grouping by areaName
work %>% 
  group_by(areaCode) %>%
  summarise(latestAvailableYear = max(year)) %>% 
  group_by(latestAvailableYear) %>%
  summarise(Count = n()) %>%
  kable()

#' Check count per number of available values
work %>% 
  group_by(areaCode,
           areaName,
           statistics,
           sex,
           workingpattern) %>%
  summarise(NumberOfValues = n()) %>%
  group_by(NumberOfValues) %>%
  summarise(Count = n()) %>%
  kable()

#' Check also which area names have several latest years available
tst = work %>% group_by(areaCode, areaName) %>% summarise(latest = max(year))
tab = table(tst$areaName)
tab[tab>1]

#' Ok, so some codes have been updated which is fine, for example see Dorset
#' code from 2018 corresponds to one that has been deprecated, what we'll do now
#' is for each area name choose a single area code (the area code corresponding to the 
#' latest year)
#' 
#' #### Get entries for which latest available data is from 2019
work %>% group_by(areaCode, areaName) %>% summarise(year = max(year)) %>% filter(year == 2018)

#' it further appears that many areaName for which latest figures come
#' from 2018 are so called 'abolished' now (i.e. inactive), see
#' [Suffolk Coastal](https://en.wikipedia.org/wiki/Suffolk_Coastal) or 
#' [East Dorset](https://en.wikipedia.org/wiki/East_Dorset)

#' So I have not check all the 14 entries which have missing data, the robust
#' way to do this would be filtering with nspl (unclear what I meant in this comment)
length(unique(work$areaName))
length(unique(work$areaCode))

#' A final problem which is not really a problem is the fact that because the work 
#' table has data on regions(also UK-wide, England-wide...) as well as local authorities, If
#' a region happens to be called the same than a LA then it'll appear twice (e.g. West Midlands
#' E1200005 - Region code, E1100005 - LA code)

#' #### Gather final data
#' 

work_time = work %>%
  rename(Pay = Value,
         CoV = CoefficientOfVariation) %>%
  pivot_wider(names_from = c(statistics,
                             workingpattern,
                             sex),
              values_from = c(Pay,
                              CoV))

#'#### Update codes

work_time = updateCodes(work_time)

combs_dup = work_time %>%
  group_by(areaCode, areaName, year) %>% summarise(duplicat = n()>1) 

print(
  work_time[work_time$areaCode %in%
                   combs_dup$areaCode[combs_dup$duplicat==T],c(1,2,3,4)] %>%
    arrange(areaCode, areaName,year), n = 100)

#' Many duplicates now
#'
#' This function combines entries by taking the __mean__ when they are duplicated
for (row in which(combs_dup$duplicat==T)){
  entry = combs_dup[row,]
  
  work_time[(work_time$areaCode == entry$areaCode &
                    work_time$areaName == entry$areaName &
                    work_time$year == entry$year),
                 !colnames(work_time) %in% c('areaCode','areaName', 'year')] = 
    t(apply(work_time[(work_time$areaCode == entry$areaCode &
                              work_time$areaName == entry$areaName &
                              work_time$year == entry$year),
                           !colnames(work_time) %in% c('areaCode','areaName', 'year')],2,mean))
  
}

print(
  work_time[work_time$areaCode %in%
                   combs_dup$areaCode[combs_dup$duplicat==T],c(1,2,3,4)])

#' Finally, select unique rows as we have generated duplicated entries in the previous step
work_time = work_time %>% distinct()


work = work_time %>% filter(year == 2019)

#'### GDP table

str(gdp)

#'#### No NAs yay
sum(is.na(gdp$`2018`)) ## good news

#'#### Rename, pivot, mutate
gdp = gdp %>% rename(Region = `NUTS1 Region`,
                     areaCode = `LA code`,
                     areaName = `LA name`) %>% 
  pivot_longer(-c(Region, areaCode, areaName), 
               names_to = 'year',
               values_to = 'GDP(millionGBP)') %>%
  mutate(year = as.numeric(year))

#' Format year as it kep the superscript from the excel sheet
gdp$year = ifelse(gdp$year == 20183, 2018, gdp$year)

#' #### Update codes

gdp = updateCodes(gdp)

combs_dup = gdp %>%
  group_by(areaCode, areaName, year) %>% summarise(duplicat = n()>1)  

print(
  gdp[gdp$areaCode %in%
              combs_dup$areaCode[combs_dup$duplicat==T],c(2,3,4)] %>%
    arrange(areaCode, areaName,year), n = 100)

#' Many duplicates now
#'
#' This function combines entries by taking the __sum__ when they are duplicated
for (row in which(combs_dup$duplicat==T)){
  entry = combs_dup[row,]
  
  gdp[(gdp$areaCode == entry$areaCode &
               gdp$areaName == entry$areaName &
               gdp$year == entry$year),
            !colnames(gdp) %in% c('areaCode','areaName', 'year')] = 
    t(apply(gdp[(gdp$areaCode == entry$areaCode &
                         gdp$areaName == entry$areaName &
                         gdp$year == entry$year),
                      !colnames(gdp) %in% c('areaCode','areaName', 'year')],2,sum))
  
}

print(
  gdp[gdp$areaCode %in%
        combs_dup$areaCode[combs_dup$duplicat==T],c(2,3,4, 5)] %>%
    arrange(areaCode, areaName,year), n = 100)


#' Check what min and max years are
min(gdp$year)
max(gdp$year)

gdp_time = gdp
gdp = gdp_time %>% filter(year == max(year))

#' ### Unemployment

#' This one is easy to handle as it is almost tidy, we just got to choose the
#' column we are interested in which is the one with the unemployment rate in 2019. Because it is 2019 arleady, 
#' we don't even need to update the LAD codes
str(unemployment)

unemployment = unemployment[3:nrow(unemployment), -seq(3, ncol(unemployment), 3)]

#' The date format for this table is confusing so I am not gonna take care of the tidying yet
unemployment_time = unemployment[, c(1,2,seq(3, ncol(unemployment), 2))]
colnames(unemployment_time)[1:2] = c('areaName', 'areaCode')
unemployment_time = unemployment_time %>% pivot_longer(-c(areaName, areaCode),
                                                       names_to = 'year',
                                                       values_to = 'UnempRate')

unemployment = unemployment[, c(1, 2, ncol(unemployment)-1)]

colnames(unemployment) = c('areaName', 'areaCode', 'UnempRate')
unemployment$year = 2019


#' ## Merge Data
#' 
#' For each dataset the data that has been taken it's that for the latest available year, 
#' we'll log what that is for each dataframe
#' 
#' #### Time-series data frame
#' 

ONS_time = Reduce(function(x,y) merge(x = x, y = y,
                                      by = c('areaCode', 'areaName', 'year'),
                                      all = TRUE),
                  list(popclean_time, gdp_time, lifeexpect_time, suicides_time,
                       unemployment, wellbeing_time, work_time))

write.csv(ONS_time, 'CleanData/ONStime.csv')
## unemployment_time year format is impossible to understand and it is too much time to preprocess it

#' #### Most current data

years_for_dfs = data.frame(df = character(), 
                           year = integer(),
                           yearColumn = character())


for (obj in ls()){
  if ('data.frame' %in% class(eval(parse(text = obj))) &
      (obj != 'years_for_dfs' &
      obj != 'tst' &
      !grepl('_time', obj) &
      obj != 'unemployment') &
      obj != 'ONS_time'
      ){
    years_for_dfs[nrow(years_for_dfs)+1, 'df'] = obj
    curr_df = eval(parse(text = obj))
    years_for_dfs[nrow(years_for_dfs), 'year'] = 
      ifelse('year' %in% colnames(curr_df),
             curr_df$year, NA)
    years_for_dfs[nrow(years_for_dfs), 'yearColumn'] = 
      ifelse('year' %in% colnames(curr_df),
             'year', NA)
    
    #remove year column
    if ('year' %in% colnames(curr_df)){
      curr_df = curr_df[,-which(colnames(curr_df) == 'year')]
    }
    
    eval(parse(text = paste(obj, ' = curr_df')))
  }
}
kable(years_for_dfs)
remove(obj)

#'### MERGE!
#' As it can be seen in the function call below we are performing outer joins(`all = TRUE`)
ONS = Reduce(function(x,y) merge(x = x, y = y,
                                 by = c('areaCode', 'areaName'),
                                 all = TRUE),
             list(popclean, gdp, lifeexpect, suicides,
                  unemployment, wellbeing, work))


#' Finally we can visualise the missingness in the final dataset
vis_miss(ONS)+coord_flip()

#' ### Save all data
write.csv(ONS, 'CleanData/ONS.csv')

#' after this run: rmarkdown::render('01_tidyingOriginalData.R',
#'                 output_format = c('html_document','pdf_document'))



