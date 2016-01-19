library(data.table)
library(ggplot2)
library(ggvis)
library(MASS)

thecounted_and_crime <- fread("data/thecounted_and_crime.csv",showProgress=FALSE)
thecounted_and_crime_labels <- fread("data/killed_by_police_code_book.csv",showProgress=FALSE)


#Rate of People Killed by Police in US
ggplot(thecounted_and_crime,aes(x=killedbypolice2015_per100k,y=reorder(state,killedbypolice2015_per100k))) +
        geom_segment(aes(yend = state), size=1,xend = 0, colour ="#D2D2F0",guide=FALSE) +
        geom_point(aes(size=killedbypolice2015)) +
        scale_colour_brewer(palette ="Set1", guide = FALSE) +
        theme_bw() +
        theme(panel.grid.major.y = element_blank()) +
        ggtitle("Rate of People Killed by Police in the U.S. in 2015 by State") +
        labs(x = "Killed per 100k Population",y = "State", size="Number Killed\nby Police") +
        theme(axis.text.y = element_text(hjust = grid::unit(c(-1, 0), "points")))


# Basic histogram of Rate Killed by Police, it looks approximately log normal
killedrate_range <- diff(range(thecounted_and_crime$killedbypolice2015_per100k))
ggplot(thecounted_and_crime,aes(x=killedbypolice2015_per100k)) + 
        geom_density(binwidth=killedrate_range/11,fill="white") + 
        stat_function(fun=dlnorm,args=list(meanlog=mean(log(thecounted_and_crime$killedbypolice2015_per100k)),
                                           sd=sd(log(thecounted_and_crime$killedbypolice2015_per100k))),
                      color="red")

# Distribution of outcome variable appears approximately log-normal 
killedratelog_range <- diff(range(log(thecounted_and_crime$killedbypolice2015_per100k)))

ggplot(thecounted_and_crime,aes(x=log(killedbypolice2015_per100k))) + 
        geom_density(binwidth=killedratelog_range/11,fill="white") + 
        stat_function(fun=dnorm,args=list(mean=mean(log(thecounted_and_crime$killedbypolice2015_per100k)),
                                          sd=sd(log(thecounted_and_crime$killedbypolice2015_per100k))),
                      color="red")



# Plotting x vs y

#Violent Crime vs Killed
ggplot(thecounted_and_crime,aes(x=log(violent_crime2014_per100k),y=log(killedbypolice2015_per100k))) + 
        geom_point() + geom_smooth(method="lm")

#Population Black vs Killed
ggplot(thecounted_and_crime,aes(x=median_age2014,y=log(killedbypolice2015_per100k))) + 
        geom_point() + geom_smooth(method="lm")


plot_labels = setNames(thecounted_and_crime_labels$variable,thecounted_and_crime_labels$label)

#thecounted_and_crime = subset(thecounted_and_crime,state!="DC")

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

glm.killed <- glm(killedbypolice2015 ~ log(police_officers2014) + log(violent_crime2014) + 
                          log(population_black2014) + log(population_hispanic2014) +
                          log(median_age2014) + log(population_male2014),
                  offset=log(population2014),family="quasipoisson",data=thecounted_and_crime)

summary(glm.killed)
library(MASS)
glm.killed <- stepAIC(glm.killed,direction="both")
summary(glm.killed)

#Using poisson

glm.mod <- glm(killedbypolice2015 ~ log(violent_crime2014),offset=log(population2014), family=poisson,data = thecounted_and_crime)

library(AER)
#testing for dispersion

glm.mod <- glm(killedbypolice2015 ~ log(violent_crime2014),offset=log(population2014), family=poisson,data = thecounted_and_crime)
dispersiontest(glm.mod)

#Using linear model with log transformed

lm.killed <- lm(log(killedbypolice2015_per100k) ~ log(police_officers2014_per100k) + log(violent_crime2014_per100k) +
                        population_black2014_percent + population_male2014_percent + population_hispanic2014_percent + 
                        median_age2014,data=thecounted_and_crime)

summary(lm.killed)
library(MASS)
lm.killed <- stepAIC(lm.killed,direction="both")
summary(lm.killed)


#Using negative binomial

nb.killed <- glm.nb(killedbypolice2015 ~ median_age2014 + log(violent_crime2014) + 
                            log(police_officers2014) + log(population_black2014) + 
                            log(population_hispanic2014) + log(population_male2014) + offset(log(population2014)),
                    data=thecounted_and_crime)

summary(nb.killed)
glm.killed <- stepAIC(nb.killed,direction="both")
summary(nb.killed)

estimates <- predict(nb.killed, type="response", se.fit=T)$fit
estimates_se <- predict(nb.killed, type="response", se.fit=T)$se.fit
estimates_lower <-  estimates - 1.96*estimates_se
estimates_upper <-  estimates + 1.96*estimates_se

estimates_df <- as.data.frame(cbind(killedbypolice2015= thecounted_and_crime$killedbypolice2015,population2014=thecounted_and_crime$population2014,estimates,estimates_se,estimates_lower,estimates_upper))

ggplot(data=estimates_df,aes(x=log(population2014),y=(killedbypolice2015)) + geom_point()

#Examining residuals vs estimated values

killed_estimates <- data.frame(estimated = glm.killed$fitted.values,residuals = resid(glm.killed),
                               lower = lower,
                               upper = upper,
                               actual = thecounted_and_crime$killedbypolice2015,
                               state = thecounted_and_crime$state
)
killed_estimates$actual_above_estimate = ifelse(killed_estimates$actual > killed_estimates$estimated,"Actual > Estimated","Estimated > Actual")
killed_estimates$actual_above_estimate = factor(killed_estimates$actual_above_estimate)
killed_estimates$actual_estimated_ratio = killed_estimates$actual/killed_estimates$estimated



ggplot(killed_estimates,aes(x=log(estimated),y=residuals,label=state)) +
        geom_segment(aes(xend = log(estimated)), size = .01, yend = 0, colour ="#E77471") +
        geom_point(alpha=.25) +
        geom_text(size=3) +
        labs(x = "Natural Log of Estimated People Killed by Police", y = "Residuals (working)") +
        ggtitle("People Killed by Police in the U.S. 2015 - Residuals vs Estimated Values")




#Fitted vs with confidence intervals
ggplot(killed_estimates,aes(x=log(estimated),y=reorder(state,actual_estimated_ratio),shape=actual_above_estimate)) +
        geom_point(aes(color="blue")) +
        geom_point(dat=killed_estimates,aes(x=log(actual),y=state,colour="red")) +
        labs(x = "Natural Log of Estimated People Killed", y = "State") +
        geom_errorbarh(aes(xmin = log(lower),xmax=log(upper)),colour ="#D2D2F0") +
        scale_colour_brewer(palette ="Set1", guide = FALSE) + theme_bw() + theme(panel.grid.major.y = element_blank()) +
        facet_grid(actual_above_estimate ~  .,scales= "free_y",space="free_y") +
        ggtitle("Rate of People Killed by Police in the U.S. in 2015 by State") +
        guides(shape = FALSE) +
        theme(legend.position="top") +
        scale_colour_manual(name = 'Number Killed by Police',guide = 'legend',
                            values =c('blue'='blue','red'='red'),
                            labels = c('Estimated','Actual')) +
        theme(axis.text.y = element_text(hjust = grid::unit(c(-1.5, 0), "points")))


#Using ggvis
thecounted_and_crime %>%
        ggvis(x=~violent_crime2014_per100k,y=~killedbypolice2015_per100k,opacity:=.5) %>%
        layer_points(size=~killedbypolice2015,fill:="darkred") %>%
        layer_text(text:=~state,opacity:=1) %>%
        layer_model_predictions(model = input_radiobuttons(label = "Choose model for fitted line:", choices = c("loess","lm")), se = TRUE)



#non-interactive



#Interactive

xvalue = input_select(label="Choose x-variable:",
                      choices = c("Violent Crime per 100k Population" = "violent_crime2014_per100k",
                                  "Murder & Non Negligent Manslaughter per 100k Population" = "murder_nonnegligent_manslaughter2014_per100k"),
                      id = "selectedx",
                      map=as.name)

thecounted_and_crime %>%
        ggvis(x = xvalue,
              y=~killedbypolice2015_per100k,opacity:=.5) %>%
        layer_points(fill:="darkred") %>%
        layer_text(text:=~state,opacity:=1) %>%
        add_axis("x",title="Selected X Variable") %>%
        layer_model_predictions(model = input_radiobuttons(label = "Choose model for fitted line:", choices = c("loess","lm")), se = TRUE, formula = xvalue ~ killedbypolice2015_per100k)

#Function
make_plot <- function(xvar,yvar) {
        thecounted_and_crime %>%
                ggvis(x = ~xvar,
                      y=~killedbypolice2015_per100k,opacity:=.5) %>%
                layer_points(fill:="darkred") %>%
                layer_text(text:=~state,opacity:=1) %>%
                layer_model_predictions(model = input_radiobuttons(label = "Choose model for fitted line:", choices = c("loess","lm")), se = TRUE)
        
}


g <- ggplot(thecounted_and_crime,aes(x=violent_crime2014_per100k,y=killedbypolice2015_per100k,label=state))
g + geom_point() + geom_smooth(method="lm") +
        scale_x_continuous(limits=c(0, max(thecounted_and_crime$violent_crime2014_per100k))) +
        geom_text()
