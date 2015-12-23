setwd("C:/Users/Josh.Josh-PC/Documents/DataScience/9-DataProducts/Projects/killed_by_police")

library(RCurl)
library(data.table)
library(ggplot2)
library(XML)
library(xlsx)

#Loading data
thecounted = fread("data/the-counted-enriched.csv",showProgress = FALSE)
#thecounted = fread("data/the-counted.csv",showProgress=FALSE)


combined_state_stats = fread("data/combined_state_stats.csv",showProgress = FALSE)
#Data by state


#Counting killings by state, need to calculate # killed to sort in plot. 
#Equivalent in dplyr of: group_by(thecounted,state) %>% summarize(killed=n()) %>% arrange(-killed)
thecounted_by_state = thecounted[,.(killed=.N),by = .(state)][order(-killed)]

ggplot(thecounted_by_state[1:25,],aes(x=reorder(state,killed),y=killed)) + 
        geom_bar(stat='identity') + 
        coord_flip() + 
        labs(x = "State (Top 25)",y="Number of People Killed by Police") +
        ggtitle("Number of People Killed by Police by State\n2015")


#As state/race populations vary significantly, counts are not useful for comparison so so demographic data is required
state_pop = fread("http://www.census.gov/popest/data/state/asrh/2014/files/SCPRC-EST2014-18+POP-RES.csv",showProgress = FALSE)
state_codes = fread("https://raw.githubusercontent.com/kitjosh1050/dataquest_p04_police_killings/master/state_codes.csv",showProgress = FALSE)

#Exclude national and Puerto Rico, equivalent in ddplyr of 
#Equivalent in dplyr of: filter(state_pop,STATE %in% c(0,72)) %>% select(NAME,POPESTIMATE2014) %>% rename(name=NAME)
state_pop = state_pop[!(STATE %in% c(0,72)),.(name=NAME,POPESTIMATE2014)]

#This is an inner join in data.table (state_pop looking up values in state_codes on set key and only returning matches)
#Equivalent in base R of: merge(state_pop,state_codes,by.x="name",by.y="name")
setkey(state_codes,name)
state_pop = state_codes[state_pop]

#This is a left outer join in data.table
#Equivalent in base R of: merge(state_pop,thecounted_by_state,by.x="state",by.y="state",all.x =TRUE)
#Also used here is chaining, where code in each pair of square brackets is executed in order, left to right, similar to %>% in dplyr
setkey(state_pop,state)
setkey(thecounted_by_state,state)
thecounted_by_state = thecounted_by_state[state_pop,nomatch=NA][is.na(killed),killed:=0
                                                                ][,killedpermillion:=killed/POPESTIMATE2014*1000000
                                                                  ][order(-killedpermillion)]

#Killed by state by population
ggplot(thecounted_by_state[1:25,],aes(x=reorder(state,killedpermillion),y=killedpermillion)) + 
        geom_bar(stat='identity',fill="#F8766D") + 
        geom_text(aes(label=paste("rate=",round(killedpermillion,1),", n=",killed,sep=''),y=0),hjust=0,size=4) + 
        coord_flip() + 
        labs(x = " State (Top 25)", y = "Killed per Million Population") + 
        ggtitle("Rate of People Killed by Police in the U.S. in 2015 by State")

#getting some extra data to see if violent crime, officers play a role 
violent_crime = data.table(read.xlsx("data/table_5_crime_in_the_united_states_by_state_2014.xls",sheetIndex=1,startRow=4,header=TRUE,stringsAsFactors=FALSE))

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
crime_counted[,c("violentcrimepermillion","murderpermillion"):=list(Violent.crime1/Population*1000000,Murder.and..nonnegligent..manslaughter/Population*1000000)]

#Murder per million vs violent crime per million
g <- ggplot(crime_counted,aes(x=murderpermillion,y=killedpermillion,label=state))
g + geom_point() + geom_smooth(method="lm") + 
        scale_x_continuous(limits=c(0, max(crime_counted$murderpermillion))) + 
        geom_text()


#Violent crime is sum of Murder.and.nonnegligent manslauter, Rape (revised definition), Robbery, Aggravated.assault
g <- ggplot(crime_counted,aes(x=violentcrimepermillion,y=killedpermillion,label=state))
g + geom_point() + geom_smooth(method="lm") + 
        scale_x_continuous(limits=c(0, max(crime_counted$violentcrimepermillion))) + 
        geom_text()

crime_counted_no_dc = crime_counted[state != "DC"]

g <- ggplot(crime_counted_no_dc,aes(x=violentcrimepermillion,y=killedpermillion,label=state))
g + geom_point() + geom_smooth(method="lm") + scale_x_continuous(limits=c(0, max(crime_counted_no_dc$violentcrimepermillion))) + geom_text()


#Murder per million vs violent crime per million - no DC
g <- ggplot(crime_counted_no_dc,aes(x=murderpermillion,y=killedpermillion,label=state))
g + geom_point() + geom_smooth(method="lm") + scale_x_continuous(limits=c(0, max(crime_counted_no_dc$murderpermillion))) + geom_text()


#Note: first two Male/Female columns are police officers, second two are civilian employees
police = data.table(read.xlsx("data/Table_77_Full_time_Law_Enforcement_Employess_by_State_2014.xls",sheetIndex=1,startRow=5,header=TRUE,stringsAsFactors=FALSE))
police[,police_officers:=Male+Female]

police[,State := trimws(tolower(gsub("\n","",NA.)))]


#West Viriginia is missing for some reason in 2014 stats so use 2013
police2013 = data.table(read.xlsx("data/table_77_full_time_law_enforcement_employess_by_state_2013.xls",sheetIndex=1,startRow=5,header=TRUE,stringsAsFactors=FALSE))
police2013[,police_officers:=Male+Female]
police2013[,State := trimws(tolower(gsub("\n","",NA.)))]
police2013 = police2013[State=="west virginia"]

police =rbindlist(list(police,police2013))
police = police[State != " NA"]

setkey(police,State)



combined_state_stats = police[crime_counted][,.(state,State,POPESTIMATE2014,killed,Violent.crime1,Murder.and..nonnegligent..manslaughter,police_officers)]

setnames(combined_state_stats,c("State","POPESTIMATE2014","killed","Violent.crime1","Murder.and..nonnegligent..manslaughter","police_officers"),c("state_name","population2014","killedbypolice2015","violent_crime2014","murder_nonnegligent_manslaughter2014","police_officers2014"))

combined_state_stats[,c("killedbypolice2015_per100k",
                        "violent_crime2014_per100k",
                        "murder_nonnegligent_manslaughter2014_per100k",
                        "police_officers2014_per100k"):=
                             list(killedbypolice2015/population2014*100000,
                                  violent_crime2014/population2014*100000,
                                  murder_nonnegligent_manslaughter2014/population2014*100000,
                                  police_officers2014/population2014*100000)]


combined_state_stats_no_dc = combined_state_stats[state!="DC"]

#Null hypothesis - there is no relationship between violent crime & people killed by police; # police employed & people killed by police, 
#and interaction between violent crime & people killed by police

glm = glm(killedbypolice2015_per100k ~ violent_crime2014_per100k + police_officers2014 + police_officers2014*violent_crime2014_per100k,family="quasipoisson",data=combined_state_stats_no_dc)

glm2 = glm(killedbypolice2015_per100k ~ violent_crime2014_per100k,family="quasipoisson",data=combined_state_stats_no_dc)


#g = ggplot 
g <- ggplot(combined_state_stats_no_dc,aes(x=violent_crime2014_per100k,y=killedbypolice2015_per100k,label=state))
g + geom_point() + geom_smooth(method = "glm", family="poisson") + 
        scale_x_continuous(limits=c(0, max(combined_state_stats_no_dc$violent_crime2014_per100k))) + 
        geom_text()


g <- ggplot(combined_state_stats_no_dc,aes(x=murder_nonnegligent_manslaughter2014,y=killedbypolice2015,label=state))
g + geom_point() + geom_smooth(method = "glm", family="poisson") + 
        geom_text()



#Create address in format required for call to Google geocoding API
thecounted[,address:=gsub(" ","+",gsub("&"," ",paste(streetaddress,city,state,sep =', ')))]



#library(jsonlite)
get_geometrics <- function(address,apikey ="") {
        #With apikey you can make up to 2500 requests a day to Google, without, only 1000
        if (apikey == "") {
                fileUrl = paste("https://maps.googleapis.com/maps/api/geocode/xml?address=%s",address,sep="")
        }
        else {
                fileUrl = paste("https://maps.googleapis.com/maps/api/geocode/xml?address=%s",address,"&key=",apikey,sep="")
        }
        doc = xmlTreeParse(getURL(fileUrl,ssl.verifypeer = FALSE),useInternalNodes = TRUE)
        #There may be more than one result from Google for some addresses, but the first should be acceptable
        long = xpathSApply(doc,"//location/lng",xmlValue)[1]
        lat = xpathSApply(doc,"//location/lat",xmlValue)[1]
        postal_code = xpathSApply(doc,"//address_component[type='postal_code']/short_name",xmlValue)[1]
        county = xpathSApply(doc,"//address_component[type='administrative_area_level_2']/short_name",xmlValue)[1]
        if(length(postal_code) == 0) {postal_code = as.character(NA)}
        if(length(county) == 0) {county = as.character(NA)}
        geometrics = data.frame(long=long,lat=lat,postal_code=postal_code,county=county)
        return(geometrics)
}


thecounted_geo = lapply(thecounted$address, function(x) get_geometrics(address = x, apikey = "AIzaSyCdAo6VW4kSNWY2xUU6hYwncQ-ruUqRzHY"))


#rbindlist is the (faster) data.table version of do.call(rbind,list(...)) 
thecounted_geo <- rbindlist(thecounted_geo)
thecounted <- cbind(thecounted,thecounted_geo)

ggplot(thecounted,aes(x=raceethnicity)) + 
        geom_bar() + 
        coord_flip() + 
        labs(x = "Race/Ethnicity",y="Number of People Killed by Police")


#Killed by alternate option
ggplot(thecounted_by_state[1:25,],aes(x=reorder(state,killedpermillion),y=killedpermillion)) + 
        geom_bar(stat='identity',fill="#F8766D") + 
        geom_text(aes(label=killed,y=killedpermillion),hjust=0,size=4) + 
        coord_flip() + 
        labs(x = " State (Top 25)", y = "Killed per Million Population") + 
        ggtitle("Rate of People Killed by Police in the U.S. in 2015 by State\nNumber Killed Annotated")



#write.csv(thecounted,"the-counted-enriched.csv")

#In this part, we want to look at racial composition of the US Population, and those killed by police, and those killed by police while unarmed
#This is an initial inquiry into a potential racial bias of police in use of lethal force 


g + geom_bar(stat='identity') + coord_flip() + geom_text(position="identity",aes(label=value1),hjust=0,size=4)


#First, we summarize the counted data by race where it is known, grouping Arab-American with white to align with U.S. Census methods
thecounted[,killed_while_unarmed:=ifelse(armed=="No",1,0)]
thecounted_byrace = thecounted[raceethnicity == "Arab-American",raceethnicity:="White"
                               ][!(raceethnicity %in% c("Other","Unknown"))
                                 ,.(killed=.N,killed_while_unarmed=sum(killed_while_unarmed))
                                 ,by=.(raceethnicity)]


#Second we get US Census population estimates (Race & Origin), separting Hispanic/Latino from other races using ORIGIN (1 Non-Hispanic, 2 is Hispanic)
#Care must be taken in interpreting data as a person classified as "White" or "Black" in The Counted may actually be "Hispanic" if using U.S. Census method

racepop = fread("http://www.census.gov/popest/data/state/asrh/2014/files/SC-EST2014-ALLDATA6.csv",showProgress = FALSE)
White = racepop[ORIGIN==1 & SEX==0 & RACE==1,
                 .(Pop=sum(POPESTIMATE2014))]$Pop
Black = racepop[ORIGIN==1 & SEX==0 & RACE==2,
                 .(Pop=sum(POPESTIMATE2014))]$Pop
NativeAmerican = racepop[ORIGIN==1 & SEX==0 & RACE==3,
                          .(Pop=sum(POPESTIMATE2014))]$Pop
AsianPacific = racepop[ORIGIN==1 & SEX==0 & RACE %in% c(4,5),
                        .(Pop=sum(POPESTIMATE2014))]$Pop
HispanicLatino = racepop[ORIGIN==2 & SEX==0,
                          .(Pop=sum(POPESTIMATE2014))]$Pop

population = c(AsianPacific,Black,HispanicLatino,NativeAmerican,White)
raceethnicity = sort(thecounted_byrace$raceethnicity)
population_byrace = data.table(raceethnicity=raceethnicity,population=population)
setkey(population_byrace,raceethnicity)

thecounted_byrace = population_byrace[thecounted_byrace]

#We need data about violent deaths and/or violent crime 


thecounted_byrace = thecounted_byrace[,c("killedpermillion","unarmedkilledpermillion") := 
                                    list(killed/population*1000000,
                                         killed_while_unarmed/population*1000000)]


ggplot(thecounted_byrace,aes(x=reorder(raceethnicity,killedpermillion),y=killedpermillion)) + 
        geom_bar(stat='identity',fill="#F8766D") + 
        geom_text(aes(label=killed,y=killedpermillion),hjust=0,size=4) + 
        coord_flip() + 
        labs(x = "People Killed by Police per Million Population", y = "People Killed by Police per Million (annotation is number killed)")


dt = melt(forplot, id=1:2, measure=list(3:4,5:6), variable.factor=TRUE)
levels(dt$variable) <- list("While Unarmed" = "2","Overall" = "1")
setnames(dt,c("value1","value2"),c("killed","killedpermillion"))

ggplot(dt,aes(x=reorder(raceethnicity,killedpermillion),y=killedpermillion,fill=variable)) + 
        geom_bar(stat='identity',position='dodge') + 
        geom_text(aes(label=paste("rate=",round(killedpermillion,1),", n=",killed,sep=''),y=0),position=position_dodge(width=.9),hjust=0,size=4) + 
        coord_flip() + 
        labs(x = "Race/Ethnicity", y = "People Killed by Police per Million",fill=" Killed by Police") +
        guides( fill = guide_legend( reverse = TRUE)) + ggtitle("People Killed in U.S. by Police in 2015 by Race/Ethnicity")


black_white_murder <- c("4396","5375")

dt[raceethnicity=="Black",murder_by_race:=5375]
dt[raceethnicity=="White",murder_by_race:=4396]

murders = fread("data/murder_offenders.csv",showProgress=FALSE)


dt2 = dt[raceethnicity %in% c("Black","White"),.(raceethnicity,population,"activity"=variable,"number" = killed,"permillion"=killedpermillion)]

murders = fread("data/murder_offenders.csv",showProgress=FALSE)
victims = fread("data/murder_victims.csv",showProgress=FALSE)


murder_dt = data.table(raceethnicity=c("White","Black"),population=c(White,Black),number=c(murders$White,murders$Black),activity="Murder as Offender")
victim_dt = data.table(raceethnicity=c("White","Black"),population=c(White,Black),number=c(victims$White,victims$Black),activity="Murder as Victim")
activities = rbindlist(list(dt2,murder_dt,victim_dt),use.names=TRUE,fill=TRUE)
activities[,permillion:=number/population*1000000]
levels(activities$activity) <- list("Murder as Victim" = "Murder as Victim", "Murder as Offender" = "Murder as Offender", "Killed by Police" = "Overall","Killed by Police while Unarmed" = "While Unarmed")


ggplot(data=activities,aes(x=activity,y=permillion,fill=raceethnicity)) + 
        geom_bar(stat='identity',position='dodge') + 
        coord_flip() + 
        geom_text(aes(label=round(permillion,1),y=0),position=position_dodge(width=.9),hjust=0,size=4)

#Black involvement in violence is significantly higher than white, regardless as victim, offender, or whether killed by police or anyone.


#Focusing on just Blacks & Whites

#From: https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/offenses-known-to-law-enforcement/expanded-homicide/expanded_homicide_data_table_3_murder_offenders_by_age_sex_and_race_2013.xls
murders = data.table(race=c("White","Black"),population=c(White,Black),number=c(4396,5375),activity="Murder as Offender")

#From: https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/offenses-known-to-law-enforcement/expanded-homicide/expanded_homicide_data_table_2_murder_victims_by_age_sex_and_race_2013.xls
victims = data.table(race=c("White","Black"),population=c(White,Black),number=c(5537,6261),activity="Murder as Victim")

White_All = racepop[ORIGIN==0 & SEX==0 & RACE==1,
                .(Pop=sum(POPESTIMATE2014))]$Pop
Black_All = racepop[ORIGIN==0 & SEX==0 & RACE==2,
                .(Pop=sum(POPESTIMATE2014))]$Pop

White_Prop_NonHisp = White/White_All
Black_Prop_NonHisp = Black/Black_All



#Learning Poisson Rate Ratio for WY vs US

us = combined_state_stats[,.(place = "US",population2014 = sum(population2014),killedbypolice2015 = sum(killedbypolice2015))]
wy = combined_state_stats[state=="WY",.(place="WY",population2014,killedbypolice2015)]
wy_us = rbindlist(list(us,wy))
glm.fit <- glm(killedbypolice2015 ~ place,offset=log(population2014),family="poisson",data=wy_us)
summary(glm.fit)
#Rate Ratio = 2.5 higher than US
exp(coef(glm.fit)[2])
exp(confint.default(glm.fit)[2,])

#Difference against US?



us_all = combined_state_stats[,.(state = "US",population2014 = sum(population2014),killedbypolice2015 = sum(killedbypolice2015),violent_crime2014=sum(violent_crime2014),murder_nonnegligent_manslaughter2014=sum(murder_nonnegligent_manslaughter2014))]
combined_state_stats_us = rbindlist(list(combined_state_stats,us_all),fill=TRUE)
combined_state_stats_us[,state:=relevel(factor(state),ref="US")]

glm.fit <- glm(killedbypolice2015 ~ state,offset=log(population2014),family="poisson",data=combined_state_stats_us)
rr_vs_us = cbind(round(exp(coef(glm.fit)),3),round(exp(confint.default(glm.fit)),3),round(summary(glm.fit)$coef[,4],4))
sig = rr_vs_us[rr_vs_us[-1,4]<0.05,]


#GLM FIT POISSON

ggplot(combined_state_stats,aes(x=violent_crime2014_per100k,y=killedbypolice2015_per100k,label=state)) +
        geom_point() + geom_text()

#DC is obvious outlier and must be removed as many crimes committed there, are not done 
#by or to those that live there. This may affect states for Maryland, Virginia as well, but not to an extent to exclude them?

combined_state_stats = combined_state_stats[state!="DC"]
        

#Explanatory = Violent Crime, response = Killed by Police
glm.fit.pop <- glm(killedbypolice2015 ~ log(population2014),family="poisson",data=combined_state_stats)

glm.fit.viol <- glm(killedbypolice2015 ~ log(police_officers2014)*log(violent_crime2014),offset=log(population2014),family="poisson",data=combined_state_stats)

summary(glm.fit.viol)
