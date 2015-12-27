library(data.table)
library(ggplot2)

thecounted_and_crime = fread("../../data/thecounted_and_crime.csv",showProgress=FALSE)

setorder(thecounted_and_crime,-killedbypolice2015_per100k)

#Rate of People Killed by Police in US 
ggplot(thecounted_and_crime[1:25,],aes(x=reorder(state,killedbypolice2015_per100k),y=killedbypolice2015_per100k,size=killedbypolice2015)) + 
        coord_flip() +
        labs(x = " State (Top 25)", y = "Killed per 100k Population") + 
        geom_segment(aes(xend = state, size = .01), yend = 0, colour ="#D2D2F0") +
        geom_point() + 
        scale_colour_brewer(palette ="Set1", guide = FALSE) + theme_bw() + theme(panel.grid.major.y = element_blank()) + 
        ggtitle("Rate of People Killed by Police in the U.S. in 2015 by State") +
        labs(size="Number Killed\nby Police")

#Against population
cor(thecounted_and_crime[,.(population2014,population_black2014,killedbypolice2015,violent_crime2014,murder_nonnegligent_manslaughter2014,police_officers2014)])

#As rate or percent of population
cor(thecounted_and_crime[,.(population_black2014_percent,killedbypolice2015_per100k,violent_crime2014_per100k,murder_nonnegligent_manslaughter2014_per100k,police_officers2014_per100k)])


#Violent Crime vs Killed by Police
g <- ggplot(thecounted_and_crime,aes(x=violent_crime2014_per100k,y=killedbypolice2015_per100k,label=state))
g + geom_point() + geom_smooth(method="lm") + 
        scale_x_continuous(limits=c(0, max(thecounted_and_crime$violent_crime2014_per100k))) + 
        geom_text()

#DC is such an outlier with regards to violent crime as a rate of total population, 
#perhaps because a lot of the crime that happens there is from residents of Virginia and Maryland
#We need to exclude DC, but should we exclude Virginia and Maryland as well?

thecounted_and_crime_no_dc = thecounted_and_crime[state != "DC"]

#Null Hypothesis - The Number of People Killed by Police has no relation to violent crime or the number of employed police officers"
#Alternate Hypothesis
#Violent crime has an effect on number of people killed by police
#Number of police officers employed has an effect on number of people killed by police

#If the residual deviance for the Poisson-model fits the data reasonable,
#we would expect the residual deviance to be roughly equal to the residual degree of freedom
#http://www.sagepub.com/sites/default/files/upm-binaries/21121_Chapter_15.pdf

#Explanatory = Violent Crime, response = Killed by Police
#Since we are dealing with counts data a Generalized Linear Model of Poisson family is suited to model
#The relationship. However, as the response variable - killedbypolice2015 - does not follow the Mean - Variance relationship o fpoisson, we will use quasi-poisson instead
glm.killed <- glm(killedbypolice2015 ~ log(police_officers2014) + log(violent_crime2014) + log(population_black2014),offset=log(population2014),family="quasipoisson",data=thecounted_and_crime_no_dc)


#Examining residuals vs fitted values

killed_fitted <- data.frame(fitted = glm.killed$fitted.values,residuals = resid(glm.killed),actual = thecounted_and_crime_no_dc$killedbypolice2015,state = thecounted_and_crime_no_dc$state)

ggplot(killed_fitted,aes(x=log(fitted),y=residuals,label=state)) + 
        geom_segment(aes(xend = log(fitted)), size = .01, yend = 0, colour ="#E77471") + 
        geom_point(alpha=.25) + 
        geom_text(size=3) +
        labs(x = "Natural Log of Fitted Values", y = "Residuals (working)") + 
        ggtitle("People Killed by Police in the U.S. 2015 - Residuals vs Fitted Values")