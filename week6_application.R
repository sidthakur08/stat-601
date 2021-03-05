mtcars
library(ggplot2)
library(dplyr)
#install.packages('visreg')
library(visreg)
library(plotly)

colnames(mtcars)

head(mtcars)

summary(mtcars)

mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("Automatic", "Manual"))
  cyl <- factor(cyl, labels=c("Four","Six","Eight"))
})

####################### UNIVARIATE #######################

# cars with different mileage
ggplot(mtcars,aes(mpg)) + 
  geom_density()

# cars with cylinders
ggplot(mtcars,aes(cyl)) + 
  geom_histogram(fill="cornflowerblue",color="white",bins=length(unique(mtcars$cyl)))

# density plot of horsepower of a car
ggplot(mtcars,aes(hp)) + 
  geom_density()

# density plot of the weights of car
ggplot(mtcars,aes(wt)) + 
  geom_density()

# cars with v-shaped and straight engine
ggplot(mtcars,aes(vs)) + 
  geom_histogram(fill="cornflowerblue",color="white",bins=length(unique(mtcars$vs)))

# cars with automatic and manual transmission
ggplot(mtcars,aes(am)) + 
  geom_histogram(fill="cornflowerblue",color="white",bins=length(unique(mtcars$am)))

# cars with number of forward gears
ggplot(mtcars,aes(gear)) + 
  geom_histogram(fill="cornflowerblue",color="white",bins=length(unique(mtcars$gear)))

# cars with number of carburetors
ggplot(mtcars,aes(carb)) + 
  geom_histogram(fill="cornflowerblue",color="white",bins=length(unique(mtcars$carb)))


####################### BIVARIATE #######################

# mpg vs hp
ggplot(mtcars) +
  geom_point(aes(x=hp,y=mpg))

# mpg vs weight
ggplot(mtcars) +
  geom_point(aes(x=wt,y=mpg))

# hp vs weight
ggplot(mtcars) +
  geom_point(aes(x=wt,y=hp))

# boxplot: mpg vs engine
ggplot(mtcars2) + 
  geom_boxplot(aes(x=vs,y=mpg))

# boxplot: mpg vs transmission
ggplot(mtcars2) + 
  geom_boxplot(aes(x=am,y=mpg))

# mpg vs cyl
ggplot(mtcars2) +
  geom_boxplot(aes(x=cyl,y=mpg))

# weight vs engine
ggplot(mtcars2) +
  geom_boxplot(aes(x=vs,y=wt))

# weight vs transmission
ggplot(mtcars2) +
  geom_boxplot(aes(x=am,y=wt))


####################### FUEL CONSUMPTION #######################

# mpg vs hp
ggplot(mtcars) +
  geom_point(aes(x=hp,y=mpg)) +
  labs(title = "Fuel Consumption vs Horsepower of the car",
    x="Horsepower of the Car",
    y="Miles per Gallon",
    caption = "Data Source: mtcars")

# mpg vs weight
ggplot(mtcars) +
  geom_point(aes(x=wt,y=mpg)) +
  labs(title = "Fuel Consumption vs Weight of the car",
     x="Weight of the Car",
     y="Miles per Gallon",
     caption = "Data Source: mtcars")

# boxplot: mpg vs engine
ggplot(mtcars2) + 
  geom_boxplot(aes(x=vs,y=mpg)) +
  labs(title = "Fuel Consumption vs Engine of the car",
       x="Engine of the Car",
       y="Miles per Gallon",
       caption = "Data Source: mtcars")


# boxplot: mpg vs transmission
ggplot(mtcars2) + 
  geom_boxplot(aes(x=am,y=mpg))

# mpg vs cyl
ggplot(mtcars2) +
  geom_boxplot(aes(x=cyl,y=mpg))

####################### MULTIVARIATE #######################
# quantitative -> mpg, hp, wt 
# categorical -> engine(vs), transmission(am), cylinder(cyl)

ggplot(mtcars2) +
  geom_point(aes(x=wt,y=mpg,color=vs)) +
  labs(title="Fuel Consumption vs Weight of the Car and Engine of the Car",
       x="Weight of Car",
       y="Miles per Gallon",
       caption="Data Source: mtcars",
       colour = "Engine")

ggplot(mtcars2) +
  geom_point(aes(x=wt,y=mpg,color=am))+
  labs(title="Fuel Consumption vs Weight of the Car and Transmission of the Car",
       x="Weight of Car",
       y="Miles per Gallon",
       caption="Data Source: mtcars",
       colour = "Transmission")

ggplot(mtcars2) +
  geom_point(aes(x=wt,y=mpg,color=cyl))+
  labs(title="Fuel Consumption vs Weight of the Car and Cylinder of the Car",
       x="Weight of Car",
       y="Miles per Gallon",
       caption="Data Source: mtcars",
       colour = "Cylinder")

ggplot(mtcars2) +
  geom_point(aes(x=hp,y=mpg,color=vs))+
  labs(title="Fuel Consumption vs Horsepower of the Car and Engine of the Car",
       x="Horsepower of Car",
       y="Miles per Gallon",
       caption="Data Source: mtcars",
       colour = "Engine")

ggplot(mtcars2) +
  geom_point(aes(x=hp,y=mpg,color=am))+
  labs(title="Fuel Consumption vs Horsepower of the Car and Transmission of the Car",
       x="Horsepower of Car",
       y="Miles per Gallon",
       caption="Data Source: mtcars",
       colour = "Transmission")

ggplot(mtcars2) +
  geom_point(aes(x=hp,y=mpg,color=cyl))+
  labs(title="Fuel Consumption vs Horsepower of the Car and Cylinder of the Car",
       x="Horsepower of Car",
       y="Miles per Gallon",
       caption="Data Source: mtcars",
       colour = "Cylinder")



####################### SIMPLE LINEAR REGRESSION #######################

lm_weight <- lm(mpg ~ wt, mtcars)
summary(lm_weight)

'lm_hp <- lm(mpg ~ hp, mtcars)
summary(lm_hp)

lm_vs <- lm(mpg ~ vs, mtcars)
summary(lm_vs)

lm_am <- lm(mpg ~ am, mtcars)
summary(lm_am)

lm_cyl <- lm(mpg ~ cyl, mtcars)
summary(lm_cyl)'


ggplot(mtcars,aes(x=wt,y=mpg)) +
  geom_point(colour="cornflowerblue") +
  geom_smooth(formula='y~x',se = TRUE,method='lm') +
  labs(title='Miles per gallon vs Weight of the Car',
       x='Weight of the Car',
       y='Miles per gallon',
       caption = 'Data Source: mtcars') +
  xlim(1.5,5.5)

new_data <- mtcars %>%
  select(mpg,wt) %>%
  filter(wt<5)

ggplot(new_data,aes(x=wt,y=mpg)) +
  geom_point(colour="cornflowerblue") +
  geom_smooth(formula='y~x',se = TRUE,method='lm') +
  labs(title='Miles per gallon vs Weight of the Car',
       x='Weight of the Car',
       y='Miles per gallon',
       caption = 'Data Source: mtcars') +
  xlim(1.5,5.5)

visreg(lm_weight,gg=TRUE)

####################### RESIDUAL PLOTS #######################

data <- mtcars
data$predicted <- predict(lm_weight)
data$residuals <- residuals(lm_weight)

# residuals vs predicted values
ggplot(data,aes(x=predicted,y=residuals))+
  geom_point(color="cornflowerblue") +
  geom_abline(intercept=0, slope=0,linetype=2) +
  ylim(-10,10) +
  xlim(5,35)

colnames(data)

# cyl
ggplot(data,aes(x=cyl,y=residuals))+
  geom_point(color="cornflowerblue") +
  geom_abline(intercept=0, slope=0,linetype=2)

# disp
ggplot(data,aes(x=disp,y=residuals))+
  geom_point(color="cornflowerblue") +
  geom_abline(intercept=0, slope=0,linetype=2)

# hp
ggplot(data,aes(x=hp,y=residuals))+
  geom_point(color="cornflowerblue") +
  geom_abline(intercept=0, slope=0,linetype=2)+
  xlim(0,400)+
  ylim(-6,8) +
  labs(title = "Residual plot against Horsepower",
       x = "Horsepower of the car",
       y = "Residuals",
       caption = "Data Source: mtcars")

# drat
ggplot(data,aes(x=drat,y=residuals))+
  geom_point(color="cornflowerblue") +
  geom_abline(intercept=0, slope=0,linetype=2)

# qsec
ggplot(data,aes(x=qsec,y=residuals))+
  geom_point(color="cornflowerblue") +
  geom_abline(intercept=0, slope=0,linetype=2) +
  xlim(14,23)+
  ylim(-6,8)+
  labs(title = "Residual plot against Quarter Mile time",
       x = "Qsec of the car",
       y = "Residuals",
       caption = "Data Source: mtcars")

# vs
ggplot(data,aes(x=vs,y=residuals))+
  geom_point(color="cornflowerblue") +
  geom_abline(intercept=0, slope=0,linetype=2)

# am
ggplot(data,aes(x=am,y=residuals))+
  geom_point(color="cornflowerblue") +
  geom_abline(intercept=0, slope=0,linetype=2)

# gear
ggplot(data,aes(x=gear,y=residuals))+
  geom_point(color="cornflowerblue") +
  geom_abline(intercept=0, slope=0,linetype=2)

# carb
ggplot(data,aes(x=carb,y=residuals))+
  geom_point(color="cornflowerblue") +
  geom_abline(intercept=0, slope=0,linetype=2)
####################### MULTIPLE LINEAR REGRESSION #######################
lm_2 <- lm(mpg ~ wt+hp, data)
summary(lm_2)

visreg(lm_2,gg=TRUE)

plot_ly(data,z=~mpg,y=~wt,x=~hp,opacity=0.6) %>%
  add_markers()
