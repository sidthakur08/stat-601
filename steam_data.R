library(ggplot2)

x1 <- c(35.3,29.7,30.8,58.8,61.4,71.3,74.4,76.7,70.7,57.5,46.4,28.9,28.1,
        39.1,46.8,48.5,59.3,70.0,70.0,74.5,72.1,58.1,44.6,33.4,28.6)
x2 <- c(20,20,23,20,21,22,11,23,21,20,20,21,21,19,23,20,22,22,11,23,20,21,
        20,20,22)
y <- c(17.8270,17.0443,15.6764,26.0350,28.3908,31.1388,32.9019,37.7660,31.9286,
       24.8575,21.0482,15.3141,15.2673,19.0198,20.6128,20.7972,28.1459,33.2510,
       30.4711,36.1130,35.3671,25.7301,19.9729,16.6504,16.5597)

data <- data.frame(x1 = x1,x2 = x2,y=y)

################### UNIVARIATE ###################
ggplot(data,aes(x=x1))+
  geom_density()+
  labs(title="Univariate plot for x1")

ggplot(data,aes(x=x2))+
  geom_density()+
  labs(title="Univariate plot for x2")

ggplot(data,aes(x=y))+
  geom_density()+
  labs(title="Univariate plot for y")

################### MULTIVARIATE ###################
ggplot(data,aes(x=x1,y=x2))+
  geom_point(color="cornflowerblue")+
  labs(title="x1 vs x2")

ggplot(data,aes(x=x1,y=y))+
  geom_point(color="cornflowerblue")+
  labs(title="x1 vs y")

ggplot(data,aes(x=x2,y=y))+
  geom_point(color="cornflowerblue")+
  labs(title="x2 vs y")

s