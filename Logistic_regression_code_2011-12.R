
# fitting a general linear binomial model
# 0 and 0.1526145 are the worst possible scores and 2.394764 and 2.24215 are best possible scores
glm(c(0,0,1,1)~c(0,0.1526145,2.24215,2.394764),family = "binomial")

# output
#Call:  glm(formula = c(0, 0, 1, 1) ~ c(0, 0.1526145, 2.24215, 2.394764), 
#           family = "binomial")

#Coefficients:
#  (Intercept)  c(0, 0.1526145, 2.24215, 2.394764)  
#-26.13                               21.83  

#Degrees of Freedom: 3 Total (i.e. Null);  2 Residual
#Null Deviance:	    5.545 
#Residual Deviance: 5.176e-10 	AIC: 4


# log(p(x_i)/(1 - p(x_i))) = beta0 + beta1*x_i


# if probability of wash score being classified as good is more than 50% according to the
# model it is considered good and vice versa
good_wash <- (exp(-26.13 + 21.83*score2)/(1+exp(-26.13 + 21.83*score2)) > 0.5)
bad_wash <- (exp(-26.13 + 21.83*score2)/(1+exp(-26.13 + 21.83*score2)) < 0.5)


# code for getting citywise data
good_wash.city <- rep(0,6)
names(good_wash.city)<- cities
bad_wash.city <- rep(0,6)
names(bad_wash.city)<- cities
for(i in 1:6){
  good_wash.city[i] <- 100*sum(good_wash[data2["Metro_new"]==cities[i]])/Sample_sizes[i]
  bad_wash.city[i] <- 100 - good_wash.city[i]
}

# plotting the results 
windows()
barplot(rbind(good_wash.city,bad_wash.city),legend.text = c("good wash score","bad wash score"),
        legend = list(x="topleft",inset = c(0.05,0)),ylim=c(0,130),main = "Overall Wash 2011-12")

# code wash accessibility data
glm(c(0,0,1,1) ~ c(0,0.1608329,2.131676,2.292509),family = "binomial")
# output
#Call:  glm(formula = c(0, 0, 1, 1) ~ c(0, 0.1608329, 2.131676, 2.292509), 
 #          family = "binomial")

#Coefficients:
 # (Intercept)  c(0, 0.1608329, 2.131676, 2.292509)  
#-26.49                                23.11  

#Degrees of Freedom: 3 Total (i.e. Null);  2 Residual
#Null Deviance:	    5.545 

good_wash_acc <- (exp(-26.49 + 23.11*wash.acc2)/(1+exp(-26.49 + 23.11*wash.acc2)) > 0.5)
bad_wash_acc <- (exp(-26.49 + 23.11*wash.acc2)/(1+exp(-26.49 + 23.11*wash.acc2)) < 0.5)

good_wash_acc.city <- rep(0,6)
bad_wash_acc.city <- rep(0,6)
names(good_wash_acc.city)<- cities
names(bad_wash_acc.city)<- cities
for(i in 1:6){
  good_wash_acc.city[i] <- 100*sum(good_wash_acc[data2["Metro_new"]==cities[i]])/Sample_sizes[i]
  bad_wash_acc.city[i] <- 100 - good_wash_acc.city[i]
}

# plotting the results 
windows()
barplot(rbind(good_wash_acc.city,bad_wash_acc.city),legend.text = c("good wash accessibility","bad wash accessibility"),
        legend = list(x="topleft",inset = c(0.05,0)),ylim=c(0,130),main = "Overall Wash Accessibility 2011-12")


# code wash availability data
glm(c(0,0,1,1) ~ c(0,0.1526145,2.344405,2.497019),family = "binomial")
# output
#Call:  glm(formula = c(0, 0, 1, 1) ~ c(0, 0.1526145, 2.344405, 2.497019), 
#family = "binomial")

#Coefficients:
#  (Intercept)  c(0, 0.1526145, 2.344405, 2.497019)  
#-25.99                                20.82  

#Degrees of Freedom: 3 Total (i.e. Null);  2 Residual
#Null Deviance:	    5.545 
#Residual Deviance: 5.144e-10 	AIC: 4

good_wash_avl <- (exp(-25.99 + 20.82*wash.avl2)/(1+exp(-25.99 + 20.82*wash.avl2)) > 0.5)
bad_wash_avl <- (exp(-25.99 + 20.82*wash.avl2)/(1+exp(-25.99 + 20.82*wash.avl2)) < 0.5)

good_wash_avl.city <- rep(0,6)
bad_wash_avl.city <- rep(0,6)
names(good_wash_avl.city)<- cities
names(bad_wash_avl.city)<- cities
for(i in 1:6){
  good_wash_avl.city[i] <- 100*sum(good_wash_avl[data2["Metro_new"]==cities[i]])/Sample_sizes[i]
  bad_wash_avl.city[i] <- 100 - good_wash_avl.city[i]
}

# plotting the results 
windows()
barplot(rbind(good_wash_avl.city,bad_wash_avl.city),legend.text = c("good wash availability","bad wash availability"),
        legend = list(x="topleft",inset = c(0.05,0)),ylim=c(0,130),main = "Overall Wash Availabilty 2011-12")


