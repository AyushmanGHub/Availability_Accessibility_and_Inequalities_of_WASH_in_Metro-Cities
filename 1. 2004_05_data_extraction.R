# reading the data
 #data3 <- read.csv("C://Users//Ayushman//Documents//ISI class//Statistic01//metro india//2004-05.csv")
 
detach(data1)
detach(data2)
 View(data1)
data1 <- read.csv("C://Users//Ayushman//Downloads//2004-05 (2).csv")
attach(data1)
####################################################################################
##..... Dichotomising the variables for WASH availability index for(2004-05)

# water resources 
d1_water_resource <- replace(Water_avail1,Water_avail1=="Improved Source",1)
d1_water_resource <- replace(d1_water_resource,d1_water_resource!=1,0)


d1_water_resource <- type.convert(d1_water_resource,as.is = TRUE)

# water supply
d1_water_supply <- replace(water_supply,water_supply==" <1 hour",0)
d1_water_supply <- replace(d1_water_supply,d1_water_supply==">1 hour",1)
d1_water_supply <- type.convert(d1_water_supply,as.is = TRUE)

# toilet availability
d1_toilet_avail <- replace(toilet_avail,toilet_avail==" Improved Toilet",1)
d1_toilet_avail <- replace(d1_toilet_avail,d1_toilet_avail!= 1,0)
d1_toilet_avail <- type.convert(d1_toilet_avail,as.is = TRUE)

# water storage
d1_water_storage <- replace(water_storage,water_storage==" Hygienic Vessel",1)
d1_water_storage <- replace(d1_water_storage,d1_water_storage != 1,0)
d1_water_storage <- type.convert(d1_water_storage,as.is = TRUE)

# kitchen
d1_kitchen <- replace(kitchen,kitchen=="Improved Cooking Place",1)
d1_kitchen <- replace(d1_kitchen,d1_kitchen != 1,0)
d1_kitchen <- type.convert(d1_kitchen,as.is = TRUE)

# residents density
d1_rdens_avail <- replace(rdens_avail,rdens_avail==">3",0)
d1_rdens_avail <- replace(d1_rdens_avail,d1_rdens_avail!=0,1)
d1_rdens_avail <- type.convert(d1_rdens_avail,as.is = TRUE)

# overall house types
d1_ht_total <- replace(ht_total,ht_final == "Pucca",1)
d1_ht_total <- replace(d1_ht_total,d1_ht_total!= 1,0)
d1_ht_total <- type.convert(d1_ht_total,as.is = TRUE)

###########################################################################
# Dichotomising the variables for WASH accessibility index for(2004-05)

# water accessibility variables
# water access time needed
d1_wtraccfinal <- replace(wtracc_final,wtracc_final == "<30 minutes",1)
d1_wtraccfinal <- replace(d1_wtraccfinal,d1_wtraccfinal != 1,0)
d1_wtraccfinal <- type.convert(d1_wtraccfinal,as.is = TRUE)

# toilet access
d1_toilet_access <- replace(acc_toilet,acc_toilet == 99,NA)

# handwash
d1_handwash <- replace(handwash,handwash=="Hygienic",1)
d1_handwash <- replace(d1_handwash,d1_handwash!=1,0)
d1_handwash <- type.convert(d1_handwash,as.is = TRUE)

# water purify or not
d1_water_purify <- replace(water_purify,water_purify=="Always Purify",1)
d1_water_purify <- replace(d1_water_purify,d1_water_purify!=1,0)
d1_water_purify <- type.convert(d1_water_purify,as.is = TRUE)

# water pouring
d1_water_pouring <- replace(water_pouring,water_pouring == "Improved method of Pouring Drinking Water",1)
d1_water_pouring <- replace(d1_water_pouring,d1_water_pouring != 1,0)
d1_water_pouring <- type.convert(d1_water_pouring,as.is = TRUE)

# fuel used type
d1_fuel_used <- replace(fuel_used,fuel_used == "Improved fuels",1)
d1_fuel_used <- replace(d1_fuel_used,d1_fuel_used == "Unimproved fuels",0)
d1_fuel_used <- type.convert(d1_fuel_used,as.is = TRUE)

##############################################################################
# descriptive statistics of the sample data

# declaring variables for storing the data
cities = unique(METRO6)

Sample_sizes = rep(0,6)
names(Sample_sizes) <- cities

poor = rep(0,6)
names(poor) <- cities

illiterate = rep(0,6)
names(illiterate) <- cities

higher_education = rep(0,6)
names(higher_education) <- cities

secondary_education = rep(0,6)
names(secondary_education) <- cities

primary_education = rep(0,6)
names(primary_education) <- cities

general = rep(0,6)
names(general) <- cities

muslims = rep(0,6)
names(muslims) <- cities

others = rep(0,6)
names(others) <- cities

obc = rep(0,6)
names(obc) <- cities

sc = rep(0,6)
names(sc) <- cities

st = rep(0,6)
names(st) <- cities

primary_occupation = rep(0,6)
names(primary_occupation) <- cities

secondary_occupation = rep(0,6)
names(secondary_occupation) <- cities

tertiay_occupation = rep(0,6)
names(tertiay_occupation) <- cities


#################################################################################
# for background-Variables
# assigning the values to the variables
for(i in 1:6){
  Sample_sizes[i] = sum(data1["METRO6"]==cities[i])
  
  
  poor[i] = sum(data1[data1["METRO6"]==cities[i],"BPL"]=="Poor")
  
  illiterate[i] = sum(data1[data1["METRO6"]==cities[i],"hheducation"]=="Illiterate")
  higher_education[i] = sum(data1[data1["METRO6"]==cities[i],"hheducation"]=="Higher")
  secondary_education[i] = sum(data1[data1["METRO6"]==cities[i],"hheducation"]=="Secondary")
  primary_education[i] = sum(data1[data1["METRO6"]==cities[i],"hheducation"]=="Primary")
  
  general[i] = sum(data1[data1["METRO6"]==cities[i],"SOCIAL_CAT"]=="General")
  muslims[i] = sum(data1[data1["METRO6"]==cities[i],"SOCIAL_CAT"]=="MUSLIMS")
  others[i] = sum(data1[data1["METRO6"]==cities[i],"SOCIAL_CAT"]=="SIKHS, JAIN, CHRISTIANS")
  obc[i] = sum(data1[data1["METRO6"]==cities[i],"SOCIAL_CAT"]=="OBC")
  sc[i] = sum(data1[data1["METRO6"]==cities[i],"SOCIAL_CAT"]=="SC")
  st[i] = sum(data1[data1["METRO6"]==cities[i],"SOCIAL_CAT"]=="ST")
  
  primary_occupation[i] = sum(data1[data1["METRO6"]==cities[i],"Occu_headhh"]=="Primary",na.rm = TRUE)
  secondary_occupation[i] = sum(data1[data1["METRO6"]==cities[i],"Occu_headhh"]=="Secondary",na.rm = TRUE)
  tertiay_occupation[i] = sum(data1[data1["METRO6"]==cities[i],"Occu_headhh"]=="Tertiary",na.rm = TRUE)
  
}
Sample_sizes


non_poor = Sample_sizes - poor
unemployed = Sample_sizes - primary_occupation - secondary_occupation - tertiay_occupation

# storing the descriptive data in a dataframe for future reference
data_2004_sample <- data.frame(cities,Sample_sizes,poor,non_poor,illiterate,higher_education,
                        secondary_education,primary_education,general,muslims,others,
                        obc,sc,st,primary_occupation,secondary_occupation,tertiay_occupation,
                        unemployed)


df <- data.frame(t(data_2004_sample[-1]))
colnames(df) <- data_2004_sample[, 1]
data_2004_sample <- df
rm(df)
View(data_2004_sample)
write.table(data_2004_sample,"C:\\Users\\Ayushman\\Documents\\Metro-India\\2004-05backgroundvariables.csv",
            append=FALSE,quote=FALSE,sep=",",dec=".",row.names=TRUE,col.names=NA)


########################################################################################
# wash availability variables city wise data
water_source_1 <- rep(0,6)
time_water_supply_1 <- rep(0,6)
toilet_facility_1 <- rep(0,6)
handwash_1 <- rep(0,6)
water_strorage_1 <- rep(0,6)
housing_space_1 <- rep(0,6)
kitchen_1 <- rep(0,6)
house_1 <- rep(0,6)

for(i in 1:6){
  water_source_1[i] <- sum(d1_water_resource[data1["METRO6"]==cities[i]])*100/Sample_sizes[i]
  time_water_supply_1[i] <- sum(d1_water_supply[data1["METRO6"]==cities[i]])*100/Sample_sizes[i] 
  toilet_facility_1[i] <- sum(d1_toilet_avail[data1["METRO6"]==cities[i]])*100 /Sample_sizes[i]
  handwash_1[i] <- sum(d1_handwash[data1["METRO6"]==cities[i]])*100/Sample_sizes[i]
  water_strorage_1[i] <- sum(d1_water_storage[data1["METRO6"]==cities[i]])*100/Sample_sizes[i]
  housing_space_1[i] <- sum(d1_rdens_avail[data1["METRO6"]==cities[i]])*100 /Sample_sizes[i]
  kitchen_1[i] <- sum(d1_kitchen[data1["METRO6"]==cities[i]])*100/Sample_sizes[i]
  house_1[i] <- sum(d1_ht_total[data1["METRO6"]==cities[i]])*100/Sample_sizes[i]
}

water_source_0 <- 100 - water_source_1
time_water_supply_0 <- 100 - time_water_supply_1
toilet_facility_0 <- 100 - toilet_facility_1
handwash_0 <- 100 - handwash_1 
water_strorage_0 <- 100 - water_strorage_1 
housing_space_0 <- 100 - housing_space_1
kitchen_0 <- 100 - kitchen_1
house_0 <- 100 - house_1

wash_aval_2004 <- data.frame(cities,water_source_1,water_source_0,time_water_supply_1,
                             time_water_supply_0,toilet_facility_1,toilet_facility_0,handwash_1,
                             handwash_0,water_strorage_1,water_strorage_0,housing_space_1,
                             housing_space_0,kitchen_1,kitchen_0,house_1,house_0)
                             

df <- data.frame(t(wash_aval_2004[-1]))
colnames(df) <- wash_aval_2004[, 1]
wash_aval_2004 <- df
rm(df)

View(wash_aval_2004)
write.table(wash_aval_2004,"C:\\Users\\Ayushman\\Documents\\Metro-India\\wash_aval_2004.csv",
            append=FALSE,quote=FALSE,sep=",",dec=".",row.names=TRUE,col.names=NA)
#######################################################################################
# wash accessibility variables city wise data
time_water_access_1 <- rep(0,6)
toilet_access_1 <- rep(0,6)
handwash_acc_1 <- rep(0,6)
water_purity_1 <- rep(0,6)
water_pouring_1 <- rep(0,6) 
fuel_used_1 <- rep(0,6)

for(i in 1:6){
  time_water_access_1[i] <- sum(d1_wtraccfinal[data1["METRO6"]==cities[i]])*100/Sample_sizes[i]
  toilet_access_1[i] <- sum(d1_toilet_access[data1["METRO6"]==cities[i]],na.rm = TRUE)*100/sum(acc_toilet != 99) 
  handwash_acc_1[i] <- sum(d1_handwash[data1["METRO6"]==cities[i]])*100 /Sample_sizes[i]
  water_purity_1[i] <- sum(d1_water_purify[data1["METRO6"]==cities[i]])*100/Sample_sizes[i]
  water_pouring_1[i] <- sum(d1_water_pouring[data1["METRO6"]==cities[i]])*100/Sample_sizes[i]
  fuel_used_1[i] <- sum(d1_fuel_used[data1["METRO6"]==cities[i]])*100 /Sample_sizes[i]
}

time_water_access_0 <- 100 - time_water_access_1
toilet_access_0 <- 100 - toilet_access_1
handwash_acc_0 <- 100 - handwash_acc_1
water_purity_0 <- 100 - water_purity_1 
water_pouring_0 <- 100 - water_pouring_1 
fuel_used_0 <- 100 - fuel_used_1

wash_acc_2004 <- data.frame(cities,time_water_access_1,time_water_access_0,toilet_access_1,
                            toilet_access_0,handwash_acc_1,handwash_acc_0,water_purity_1,
                            water_purity_0,water_pouring_1,water_pouring_0,fuel_used_1,fuel_used_0)

df <- data.frame(t(wash_acc_2004[-1]))
colnames(df) <- wash_acc_2004[, 1]
wash_acc_2004 <- df
rm(df)

View(wash_acc_2004)
write.table(wash_acc_2004_05,"C:\\Users\\Ayushman\\Documents\\Metro-India\\wash_acc_2004.csv",
            append=FALSE,quote=FALSE,sep=",",dec=".",row.names=TRUE,col.names=NA)
######################################################################################  

# creating a dataframe of variables used in wash accessibility and wash availability 
acc_var1 <- data.frame(d1_wtraccfinal,d1_toilet_access,d1_handwash,d1_water_purify
             ,d1_water_pouring,d1_fuel_used)
acc_var1 <- na.omit(acc_var1)
avai_var1 <- data.frame(d1_water_resource,d1_water_supply,d1_toilet_avail,d1_water_storage,
                       d1_kitchen,d1_rdens_avail,d1_ht_total)

mean_avai1 = rep(0,7)
sd_avai1 = rep(0,7)
mean_acc1 = rep(0,6)
sd_acc1 = rep(0,6)

for(i in 1:6){
  mean_acc1[i] = sum(acc_var1[i])/sum(Sample_sizes)
  sd_acc1[i] = sqrt(sum(acc_var1[i]^2)/sum(Sample_sizes) - mean_acc1[i]^2)
}

for(i in 1:7){
  mean_avai1[i] = sum(avai_var1[i])/sum(Sample_sizes)
  sd_avai1[i] = sqrt(sum(avai_var1[i]^2)/sum(Sample_sizes) - mean_avai1[i]^2)
}


# 6x6 covariance matrix for the wash accessibility
# 7x7 covariance matrix for the wash availability

cov_mat_acc1 <- matrix(rep(0,6*6),nrow = 6)
cov_mat_avai1 <- matrix(rep(0,7*7),nrow = 7)

for(i in 1:6){
  for(j in 1:6){
    cov_mat_acc1[i,j]<-cov((acc_var1[i]-mean_acc1[i])/sd_acc1[i],(acc_var1[j]-mean_acc1[j])/sd_acc1[j],
                           method = "pearson")
  }
}

for(i in 1:7){
  for(j in 1:7){
    cov_mat_avai1[i,j]<-cov((avai_var1[i]-mean_avai1[i])/sd_avai1[i],(avai_var1[j]-mean_avai1[j])/sd_avai1[j],
                           method = "pearson")
  }
}
cov_mat_acc1

#####################################################################################
# getting the eigenvalues and the first eigen vector
eigen(cov_mat_acc1)
acc_point1 <- eigen(cov_mat_acc1)$values
facc_matrix1 <- -(cbind(eigen(cov_mat_acc1)$vectors[,1]))

eigen(cov_mat_avai1)
avai_point1 <- eigen(cov_mat_avai1)$values
favai_matrix1 <- -(cbind(eigen(cov_mat_avai1)$vectors[,1]))

# code for plotting the eigenvalues
par(mfrow=c(1,2))
plot(acc_point1,ylab = "Eigenvalues",xlab = "Number",main = "Screeplot for Water accessibility
     2004-05",pch = 19)
lines(1:length(acc_point1),acc_point1,col = "blue",lwd = 1.5)
#windows()
plot(avai_point1,ylab = "Eigenvalues",xlab = "Number",main = "Screeplot for Water availabilty
     2004-05",pch = 19)
lines(1:length(avai_point1),avai_point1,col = "blue",lwd = 1.5)

###############################################################################

# calculating the wash scores
# availability
avl_score1 <- as.matrix(avai_var1)%*%(favai_matrix1)
wash.avl <- avl_score1

# accessibility
acc_score1 <- as.matrix(acc_var1)%*%(facc_matrix1)
wash.acc <- acc_score1

# ignoring missing values for wash accessibility
index <- acc_toilet != 99
wash.avl <- wash.avl[index]

# removing households for which complete information about Wash is unavailable
detach(data1)
data1 <- data1[data1["acc_toilet"] != 99,]
attach(data1)
#city-wise availability scores
wash.avl.city = rep(0,6)
names(wash.avl.city) <- cities

for(i in 1:6){
  wash.avl.city[i] <- sum(avl_score1[data1["METRO6"]==cities[i]])/sum(data1["METRO6"]==cities[i])
}
# city-wise accessibility scores
wash.acc.city = rep(0,6)
names(wash.acc.city) <- cities
for(i in 1:6){
  wash.acc.city[i] <- sum(wash.acc[data1["METRO6"]==cities[i]])/sum(data1["METRO6"]==cities[i])
}

# overall score is average of accessibility and availability score
score <- (wash.acc.city + wash.avl.city)/2
score

# city wise scores
score.city <- rep(0,6)
names(score.city) <- cities
for(i in 1:6){
  score.city[i] <- sum(score[data1["METRO6"]==cities[i]],na.rm = TRUE)/sum(data1["METRO6"]==cities[i])
}

# score has 1 negative value , we remove it
score <- score[score > 0 ]
############################################################################################
# calculating the inequalities
library(ineq)

# Gini
gini <- rep(0,6)
for(i in 1:6){
  gini[i] <- ineq(score[data1["METRO6"] == cities[i]],type = "Gini")
}
gini

# Theil
theil <- rep(0,6)
for(i in 1:6){
  theil[i] <- ineq(score[data1["METRO6"] == cities[i]],type = "Theil")
}

# Atkinson
atkinson <- rep(0,6)



for(i in 1:6){
  atkinson[i] <- ineq(score[data1["METRO6"] == cities[i]],type = "Atkinson")
}

# for total population
gini[7] <- ineq(score,type = "Gini")
gini
theil[7] <- ineq(score,type = "Theil")
theil
atkinson[7] <- ineq(score,type = "Atkinson")
atkinson
cities_names <- c(cities,c("Total"))


Inequalities_2004 <- data.frame(cities_names,gini,theil,atkinson)
View(Inequalities_2004)


######################
Wash_scores_2004 <- data.frame(cities,wash.avl.city,wash.acc.city,
                               score_city = (wash.avl.city + wash.acc.city)/2)
View(Wash_scores_2004)




# codes for the plots
# Wash between poor and non-poor
score_poor = rep(0,6)
score_non_poor <- rep(0,6)
for(i in 1:6){
  score_poor[i] <- sum(score[data1["METRO6"]==cities[i] & data1["BPL"]=="Poor"],na.rm = TRUE)/sum(poor[i])
  score_non_poor[i] <- sum(score[data1["METRO6"]==cities[i] & data1["BPL"]!="Poor"],na.rm = TRUE)/sum(non_poor[i])
}
  #windows()
  barplot(cbind(score_poor,score_non_poor),names.arg = c("poor","non-poor"),
        main = "Average Wash scores of poor and non-poor 2004-05",beside = TRUE,legend.text = cities,
        col = c("red","blue","green","yellow","magenta","grey"),ylim = c(0,2.5),
        args.legend = list(x="topleft",inset = c(0.1,0)))

# wash scores between education levels
score_h_ed <- rep(0,6)
score_p_ed <- rep(0,6)
score_s_ed <- rep(0,6)
score_illi <- rep(0,6)

for(i in 1:6){
  score_h_ed[i] <- sum(score[data1["METRO6"]==cities[i] & data1["hheducation"]=="Higher"],na.rm = TRUE)/sum(data1["METRO6"]==cities[i] & data1["hheducation"]=="Higher")
  score_p_ed[i] <- sum(score[data1["METRO6"]==cities[i] & data1["hheducation"]=="Primary"],na.rm = TRUE)/sum(data1["METRO6"]==cities[i]& data1["hheducation"]=="Primary")
  score_s_ed[i] <- sum(score[data1["METRO6"]==cities[i] & data1["hheducation"]=="Secondary"],na.rm = TRUE)/sum(data1["METRO6"]==cities[i]& data1["hheducation"]=="Secondary")
  score_illi[i] <- sum(score[data1["METRO6"]==cities[i] & data1["hheducation"]=="Illiterate"],na.rm = TRUE)/sum(data1["METRO6"]==cities[i]& data1["hheducation"]=="Illiterate")  
}
  
#windows()
barplot(cbind(score_illi,score_p_ed,score_s_ed,score_h_ed),names.arg = c("Illiterate","Primary",
        "Secondary","Higher"),main = "Average Wash scores and education levels 2004-05"
        ,ylim = c(0,2.5),legend.text = cities,beside = TRUE,width = 1,col = c("red","blue","green","yellow"
        ,"magenta","grey"),args.legend = list(x="topleft",inset = c(0.05,0)))  

# average wash scores city-wise
#windows()
barplot(score.city,beside = T,space = 1,width = 0.05,ylim = c(0,2),main = "Average Wash scores of cities 2004-05",
        xlab = "Cities",ylab = "Average Wash Scores")

# average wash accessibility scores
#windows()
barplot(wash.acc.city,beside = T,ylim = c(0,2.5),main = "Wash Accessibility scores 2004-05",
        xlab = "Cities",ylab = "Wash Accessibility scores")

# average wash availability scores
#windows()
barplot(wash.avl.city,beside = T,ylim = c(0,2.5),main = "Wash Availability scores 2004-05",
        xlab = "Cities",ylab = "Wash Availability scores")
  
 