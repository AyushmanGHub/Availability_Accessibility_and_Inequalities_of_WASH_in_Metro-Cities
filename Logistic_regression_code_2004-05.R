
# fitting a general linear binomial model

glm(c(0,0,1,1)~c(0,0.1620269,2.341387,2.25532),family = "binomial")

# output
#Call:  glm(formula = c(0, 0, 1, 1) ~ c(0, 0.1620269, 2.341387, 2.25532), 
#           family = "binomial")

#Coefficients:
#  (Intercept)  c(0, 0.1620269, 2.341387, 2.25532)  
#-26.35                               21.86  

#Degrees of Freedom: 3 Total (i.e. Null);  2 Residual
#Null Deviance:	    5.545 
#Residual Deviance: 5.049e-10 	AIC: 4


# log(p(x_i)/(1 - p(x_i))) = beta0 + beta1*x_i


# if probability of wash score being classified as good is more than 50% according to the
# model it is considered good and vice versa
good_wash <- (exp(-26.35 + 21.86*score)/(1+exp(-26.35 + 21.86*score)) > 0.5)
bad_wash <- (exp(-26.35 + 21.86*score)/(1+exp(-26.35 + 21.86*score)) < 0.5)


# code for getting citywise data
good_wash.city <- rep(0,6)
names(good_wash.city)<- cities
bad_wash.city <- rep(0,6)
names(bad_wash.city)<- cities
for(i in 1:6){
  good_wash.city[i] <- 100*sum(good_wash[data1["METRO6"]==cities[i]],na.rm = T)/Sample_sizes[i]
  bad_wash.city[i] <- 100 - good_wash.city[i]
}

# plotting the results 
windows()
barplot(rbind(good_wash.city,bad_wash.city),legend.text = c("good wash score","bad wash score"),
        legend = list(x="topleft",inset = c(0.05,0)),ylim=c(0,130),main = "Overall Wash 2004-05")

# code wash accessibility data
glm(c(0,0,1,1) ~ c(0,0.2193165,2.121601,2.340917),family = "binomial")

# output
#Call:  glm(formula = c(0, 0, 1, 1) ~ c(0, 0.2193165, 2.121601, 2.340917), 
#           family = "binomial")

#Coefficients:
#  (Intercept)  c(0, 0.2193165, 2.121601, 2.340917)  
#-27.93                                23.87  

#Degrees of Freedom: 3 Total (i.e. Null);  2 Residual
#Null Deviance:	    5.545 
#Residual Deviance: 5.567e-10 	AIC: 4

good_wash_acc <- (exp(-27.93 + 23.87*wash.acc)/(1+exp(-27.93 + 23.87*wash.acc)) > 0.5)
bad_wash_acc <- (exp(-27.93 + 23.87*wash.acc)/(1+exp(-27.93 + 23.87*wash.acc)) < 0.5)

good_wash_acc.city <- rep(0,6)
bad_wash_acc.city <- rep(0,6)
names(good_wash_acc.city)<- cities
names(bad_wash_acc.city)<- cities
for(i in 1:6){
  good_wash_acc.city[i] <- 100*sum(good_wash_acc[data1["METRO6"]==cities[i]])/Sample_sizes[i]
  bad_wash_acc.city[i] <- 100 - good_wash_acc.city[i]
}

# plotting the results 
windows()
barplot(rbind(good_wash_acc.city,bad_wash_acc.city),legend.text = c("good wash accessibility","bad wash accessibility"),
        legend = list(x="topleft",inset = c(0.05,0)),ylim=c(0,130),main = "Overall Wash Accessibility 2004-05")


# code wash availability data
glm(c(0,0,1,1) ~ c(0,0.1620269, 2.255789,2.169723),family = "binomial")

# output
#Call:  glm(formula = c(0, 0, 1, 1) ~ c(0, 0.1620269, 2.255789, 2.169723), 
#family = "binomial")

#Coefficients:
#  (Intercept)  c(0, 0.1620269, 2.255789, 2.169723)  
#-26.49                                22.78  

#Degrees of Freedom: 3 Total (i.e. Null);  2 Residual
#Null Deviance:	    5.545 
#Residual Deviance: 5.072e-10 	AIC: 4


good_wash_avl <- (exp(-26.49 + 22.78*wash.avl)/(1+exp(-26.49 + 22.78*wash.avl)) > 0.5)
bad_wash_avl <- (exp(-26.49 + 22.78*wash.avl)/(1+exp(-26.49 + 22.78*wash.avl)) < 0.5)

good_wash_avl.city <- rep(0,6)
bad_wash_avl.city <- rep(0,6)
names(good_wash_avl.city)<- cities
names(bad_wash_avl.city)<- cities
for(i in 1:6){
  good_wash_avl.city[i] <- 100*sum(good_wash_avl[data1["METRO6"]==cities[i]])/Sample_sizes[i]
  bad_wash_avl.city[i] <- 100 - good_wash_avl.city[i]
}

# plotting the results 
windows()
barplot(rbind(good_wash_avl.city,bad_wash_avl.city),legend.text = c("good wash availability","bad wash availability"),
        legend = list(x="topleft",inset = c(0.05,0)),ylim=c(0,130),main = "Overall Wash Availabilty 2004-05")


