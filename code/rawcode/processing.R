library(RCurl)
library(data.table)
library(ggplot2)
library(XML)
library(xlsx)

#Data comes from three sources
#1) The Counted - Killed by Police from The Guardian website 
#2) Population - Data from U.S. Census Beureau
#3) Crime & Police Stats - Data from FBI

#1) 
#Loading data downloaded from The Counted - http://www.theguardian.com/us-news/ng-interactive/2015/jun/01/about-the-counted
thecounted = fread("../../data/the-counted.csv",showProgress=FALSE)

#Counting killings by state, need to calculate # killed to sort in plot. 
#Equivalent in dplyr of: group_by(thecounted,state) %>% summarize(killed=n()) %>% arrange(-killed)
thecounted_by_state = thecounted[,.(killed=.N),by = .(state)][order(-killed)]

ggplot(thecounted_by_state[1:25,],aes(x=reorder(state,killed),y=killed)) + 
        geom_bar(stat='identity') + 
        coord_flip() + 
        labs(x = "State (Top 25)",y="Number of People Killed by Police") +
        ggtitle("Number of People Killed by Police by State\n2015")


#2)
#As state/race populations vary significantly, counts are not useful for comparison so so demographic data is required

state_pop = fread("http://www.census.gov/popest/data/state/asrh/2014/files/SCPRC-EST2014-18+POP-RES.csv",showProgress=FALSE)
state_codes = fread("https://raw.githubusercontent.com/kitjosh1050/dataquest_p04_police_killings/master/state_codes.csv",showProgress = FALSE)

#Exclude national and Puerto Rico, equivalent in ddplyr of 
#Equivalent in dplyr of: filter(state_pop,STATE %in% c(0,72)) %>% select(NAME,POPESTIMATE2014) %>% rename(name=NAME)
state_pop = state_pop[!(STATE %in% c(0,72)),.(name=NAME,POPESTIMATE2014,STATE)]

#This is an inner join in data.table (state_pop looking up values in state_codes on set key and only returning matches)
#Equivalent in base R of: merge(state_pop,state_codes,by.x="name",by.y="name")
setkey(state_codes,name)
state_pop = state_codes[state_pop]

race_pop = fread("http://www.census.gov/popest/data/state/asrh/2014/files/SC-EST2014-ALLDATA6.csv",showProgress = FALSE)
race_pop = race_pop[ORIGIN == 0 & SEX==0]
race_pop[,POPESTIMATE2014_black := ifelse(RACE==2,POPESTIMATE2014,0)]
race_pop = race_pop[,.(population2014=sum(POPESTIMATE2014),population_black2014=sum(POPESTIMATE2014_black)),by=.(STATE)]
setkey(race_pop,STATE)
setkey(state_pop,STATE)

state_pop = state_pop[race_pop
                      ][,.(name,state,population2014=POPESTIMATE2014,population_black2014)]

#This is a left outer join in data.table
#Equivalent in base R of: merge(state_pop,thecounted_by_state,by.x="state",by.y="state",all.x =TRUE)
#Also used here is chaining, where code in each pair of square brackets is executed in order, left to right, similar to %>% in dplyr
setkey(state_pop,state)
setkey(thecounted_by_state,state)
thecounted_by_state = thecounted_by_state[state_pop,nomatch=NA][is.na(killed),killed:=0
                                                                ][,killed_per100k:=killed/population2014*100000
                                                                  ][order(-killed_per100k)]



#Killed by state by population Cleveland Dot Plot

ggplot(thecounted_by_state[1:25,],aes(x=reorder(state,killed_per100k),y=killed_per100k,size=killed)) + 
        coord_flip() +
        labs(x = " State (Top 25)", y = "Killed per 100k Population") + 
        geom_segment(aes(xend = state, size = .01), yend = 0, colour ="#D2D2F0") +
        geom_point() + 
        scale_colour_brewer(palette ="Set1", guide = FALSE) + theme_bw() + theme(panel.grid.major.y = element_blank()) + 
        ggtitle("Rate of People Killed by Police in the U.S. in 2015 by State")

#3)
#From FBI - Crime in the U.S. in 2014 from FBI 2014  
violent_crime = data.table(read.xlsx("../../data/table_5_crime_in_the_united_states_by_state_2014.xls",sheetIndex=1,startRow=4,header=TRUE,stringsAsFactors=FALSE))

#In data.table, to refer to columns by number in j section, with=FALSE argument is required, see data.table FAQ for rationale
numeric_cols = colnames(violent_crime[,4:14,with=FALSE])

#.SD is Sub Data.table, with which a function can be applied when the applicable columns are specified with .SDcols argument
violent_crime[,(numeric_cols):=lapply(.SD,function(x) as.numeric(ifelse(x==" ",NA,x))),.SDcols=numeric_cols]

#A "fill down" method can be applied by grouping on an aggregate function
violent_crime[,State := State[1], by = cumsum(!is.na(State))]
violent_crime = violent_crime[grepl("Total",Area) & State != "PUERTO RICO"]

violent_crime[,State := trimws(tolower(gsub("[0-9,]","",violent_crime[,State])))]

setkey(violent_crime,State)
thecounted_by_state[,name := tolower(name)]
setkey(thecounted_by_state,name)

crime_counted = thecounted_by_state[violent_crime]

#from same FBI Crime in U.S. - Police Officers 
#Note: first two Male/Female columns are police officers, second two are civilian employees
police = data.table(read.xlsx("../../data/Table_77_Full_time_Law_Enforcement_Employess_by_State_2014.xls",sheetIndex=1,startRow=5,header=TRUE,stringsAsFactors=FALSE))
police[,police_officers:=Male+Female]
police[,State := trimws(tolower(gsub("\n","",NA.)))]


#West Viriginia is missing in 2014 because did not submit data to FBI in time, use 2013 instead
police2013 = data.table(read.xlsx("../../data/table_77_full_time_law_enforcement_employess_by_state_2013.xls",sheetIndex=1,startRow=5,header=TRUE,stringsAsFactors=FALSE))
police2013[,police_officers:=Male+Female]
police2013[,State := trimws(tolower(gsub("\n","",NA.)))]
police2013 = police2013[State=="west virginia"]

police =rbindlist(list(police,police2013))
police = police[State != " NA"]

setkey(police,State)

combined_state_stats = police[crime_counted][,.(state,State,population2014,population_black2014,killed,Violent.crime1,Murder.and..nonnegligent..manslaughter,police_officers)]

setnames(combined_state_stats,c("State","killed","Violent.crime1","Murder.and..nonnegligent..manslaughter","police_officers"),c("state_name","killedbypolice2015","violent_crime2014","murder_nonnegligent_manslaughter2014","police_officers2014"))

combined_state_stats[,c("population_black2014_percent",
                        "killedbypolice2015_per100k",
                        "violent_crime2014_per100k",
                        "murder_nonnegligent_manslaughter2014_per100k",
                        "police_officers2014_per100k"):=
                             list(population_black2014/population2014,
                                  killedbypolice2015/population2014*100000,
                                  violent_crime2014/population2014*100000,
                                  murder_nonnegligent_manslaughter2014/population2014*100000,
                                  police_officers2014/population2014*100000)]


write.csv(combined_state_stats,"../../data/thecounted_and_crime.csv",row.names = FALSE)