library(RCurl)
library(data.table)
library(ggplot2)
library(XML)
library(xlsx)

#Data comes from three sources
#1) The Counted - Killed by Police from The Guardian website 
#2) Population - Data from U.S. Census Beureau
#3) Crime & Police Stats - Data from FBI

#1) The Counted
#Loading data downloaded from The Counted - http://www.theguardian.com/us-news/ng-interactive/2015/jun/01/about-the-counted
thecounted <- fread("data/the-counted.csv",showProgress=FALSE)

#Counting killings by state, need to calculate # killed to sort in plot. 
#Equivalent in dplyr of: group_by(thecounted,state) %>% summarize(killed=n()) %>% arrange(-killed)
thecounted_by_state <- thecounted[,.(killed=.N),by = .(state)][order(-killed)]

ggplot(thecounted_by_state[1:25,],aes(x=reorder(state,killed),y=killed)) + 
        geom_bar(stat='identity') + 
        coord_flip() + 
        labs(x = "State (Top 25)",y="Number of People Killed by Police") +
        ggtitle("Number of People Killed by Police by State\n2015")


#2) Population
#a) Overall population by state
#As state/race populations vary significantly, counts are not useful for comparison so so demographic data is required

state_pop <- fread("http://www.census.gov/popest/data/state/asrh/2014/files/SCPRC-EST2014-18+POP-RES.csv",showProgress=FALSE)
state_codes <- fread("https://raw.githubusercontent.com/kitjosh1050/dataquest_p04_police_killings/master/state_codes.csv",showProgress = FALSE)

#Exclude national and Puerto Rico, equivalent in ddplyr of 
#Equivalent in dplyr of: filter(state_pop,STATE %in% c(0,72)) %>% select(NAME,POPESTIMATE2014) %>% rename(name=NAME)
state_pop <- state_pop[!(STATE %in% c(0,72)),.(name=NAME,POPESTIMATE2014,STATE)]

state_pop <- merge(state_codes,state_pop,by.x="name",by.y="name")

#b) Percent Male 15-44, Black & Hispanic

race_pop <- fread("http://www.census.gov/popest/data/state/asrh/2014/files/SC-EST2014-ALLDATA6.csv",showProgress = FALSE)


#Violence both from police and perpetrated is generally seen to be the problem of younger men, particularly blacks and hispanic   
state_by_race_gender <- race_pop[,.(population_15to44_male2014 = sum(ifelse(ORIGIN==0 & SEX==1 & AGE>=15 & AGE <= 44,POPESTIMATE2014,0)),
                                    population_black2014 = sum(ifelse(ORIGIN==0 & SEX==0 & RACE==2,POPESTIMATE2014,0)),
                                    population_hispanic2014 = sum(ifelse(ORIGIN==2 & SEX==0,POPESTIMATE2014,0))
),by=.(STATE)]

state_pop_full <- merge(state_pop,state_by_race_gender,by.x="STATE",by.y="STATE")


#Combine 1 & 2
#Used here is chaining, where code in each pair of square brackets is executed in order, 
#left to right, similar to %>% in dplyr

thecounted_pop <- merge(state_pop_full,thecounted_by_state,by.x="state",by.y="state",all.x=TRUE)

thecounted_pop[,c("killed","killed_per100k") := list(ifelse(is.na(killed),0,killed),
                                                          ifelse(is.na(killed),0,killed)/POPESTIMATE2014*100000)
                    ]



#Killed by state by population Cleveland Dot Plot
#set order first using data.table function
setorder(thecounted_pop,-killed_per100k)
ggplot(thecounted_pop[1:25,],aes(x=reorder(state,killed_per100k),y=killed_per100k,size=killed)) + 
        coord_flip() +
        labs(x = " State (Top 25)", y = "Killed per 100k Population") + 
        geom_segment(aes(xend = state, size = .01), yend = 0, colour ="#D2D2F0") +
        geom_point() + 
        scale_colour_brewer(palette ="Set1", guide = FALSE) + theme_bw() + theme(panel.grid.major.y = element_blank()) + 
        ggtitle("Rate of People Killed by Police in the U.S. in 2015 by State")

#3)
#From FBI - Crime in the U.S. in 2014 from FBI 2014  
violent_crime <- data.table(read.xlsx("data/table_5_crime_in_the_united_states_by_state_2014.xls",sheetIndex=1,startRow=4,header=TRUE,stringsAsFactors=FALSE))

#In data.table, to refer to columns by number in j section, with=FALSE argument is required, see data.table FAQ for rationale
numeric_cols <- colnames(violent_crime[,4:14,with=FALSE])

#.SD is Sub Data.table, with which a function can be applied when the applicable columns are specified with .SDcols argument
violent_crime[,(numeric_cols):=lapply(.SD,function(x) as.numeric(ifelse(x==" ",NA,x))),.SDcols=numeric_cols]

#A "fill down" method can be applied by grouping on an aggregate function
violent_crime[,State := State[1], by = cumsum(!is.na(State))]
violent_crime <- violent_crime[grepl("Total",Area) & State != "PUERTO RICO"]

violent_crime[,State := trimws(tolower(gsub("[0-9,]","",violent_crime[,State])))]

thecounted_pop[,name := tolower(name)]
thecounted_pop_crime <- merge(thecounted_pop,violent_crime,by.x="name",by.y="State")

#from same FBI Crime in U.S. - Police Officers 
#Note: first two Male/Female columns are police officers, second two are civilian employees
police <- data.table(read.xlsx("data/Table_77_Full_time_Law_Enforcement_Employess_by_State_2014.xls",sheetIndex=1,startRow=5,header=TRUE,stringsAsFactors=FALSE))
police[,police_officers:=Male+Female]
police[,State := trimws(tolower(gsub("\n","",NA.)))]


#West Viriginia is missing in 2014 because did not submit data to FBI in time, use 2013 instead
police2013 <- data.table(read.xlsx("data/table_77_full_time_law_enforcement_employess_by_state_2013.xls",sheetIndex=1,startRow=5,header=TRUE,stringsAsFactors=FALSE))
police2013[,police_officers:=Male+Female]
police2013[,State := trimws(tolower(gsub("\n","",NA.)))]
police2013 <- police2013[State=="west virginia"]

police <- rbindlist(list(police,police2013))
police <- police[State != " NA"]

thecounted_pop_crime_police <- merge(thecounted_pop_crime,police,by.x="name",by.y="State")


## Drop, rename and reorder columns for better organized data set 
thecounted_pop_crime_police <- thecounted_pop_crime_police[,.(state,
                                                              name,
                                                              POPESTIMATE2014,
                                                              population_15to44_male2014,
                                                              population_black2014,
                                                              population_hispanic2014,
                                                              killed,
                                                              Violent.crime1,
                                                              Murder.and..nonnegligent..manslaughter,
                                                              police_officers)
                                                           ]

setnames(thecounted_pop_crime_police,
         c("name",
           "POPESTIMATE2014",
           "killed",
           "Violent.crime1",
           "Murder.and..nonnegligent..manslaughter",
           "police_officers")
         ,c("state_name",
            "population2014",
            "killedbypolice2015",
            "violent_crime2014",
            "murder_nonnegligent_manslaughter2014",
            "police_officers2014"))

thecounted_pop_crime_police[,c("population_15to44_male2014_percent",
                        "population_black2014_percent",
                        "population_hispanic2014_percent",
                        "killedbypolice2015_per100k",
                        "violent_crime2014_per100k",
                        "murder_nonnegligent_manslaughter2014_per100k",
                        "police_officers2014_per100k"):=
                             list(population_15to44_male2014/population2014,
                                  population_black2014/population2014,
                                  population_hispanic2014/population2014,
                                  killedbypolice2015/population2014*100000,
                                  violent_crime2014/population2014*100000,
                                  murder_nonnegligent_manslaughter2014/population2014*100000,
                                  police_officers2014/population2014*100000)]


# Add back properly capitalized state names for appearance
thecounted_pop_crime_police <- merge(thecounted_pop_crime_police,state_codes,by.x="state",by.y="state")
thecounted_pop_crime_police[,state_name:=name]
thecounted_pop_crime_police[,name:=NULL]

write.csv(thecounted_pop_crime_police,"data/thecounted_and_crime.csv",row.names = FALSE)