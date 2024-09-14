# reading the data
#data2 <- read.csv("C://Users//Ayushman//Documents//Metro-India//2011-12.csv")
View(data2)
#attach(data2)
################################################################################
# Dichotomising the variables for WASH availability index for(2011-12)

# water resources
d2_water_resource <- replace(Water_avail1,Water_avail1=="Improved Source",1)
d2_water_resource <- replace(d2_water_resource,d2_water_resource!=1,0)
d2_water_resource <- type.convert(d2_water_resource,as.is = TRUE)

# water supply
d2_water_supply <- replace(water_supply,water_supply=="< 1 Hour supply",0)
d2_water_supply <- replace(d2_water_supply,d2_water_supply=="> 1 Hour Supply",1)
# converting to numeric
d2_water_supply <- type.convert(d2_water_supply,as.is = TRUE)

# toilet availability
d2_toilet_avail <- replace(toilet_avail,toilet_avail==" Improved Toilet",1)
d2_toilet_avail <- replace(d2_toilet_avail,d2_toilet_avail!= 1,0)
d2_toilet_avail <- type.convert(d2_toilet_avail,as.is = TRUE)

# water storage
d2_water_storage <- replace(water_storage,water_storage=="Improved Method of Storage",1)
d2_water_storage <- replace(d2_water_storage,d2_water_storage != 1,0)
d2_water_storage <- type.convert(d2_water_storage,as.is = TRUE)

# kitchen
d2_kitchen <- replace(Kitchen,Kitchen=="Improved Cooking Place",1)
d2_kitchen <- replace(d2_kitchen,d2_kitchen != 1,0)
d2_kitchen <- type.convert(d2_kitchen,as.is = TRUE)

# residents density
d2_rdens_avail <- replace(rdens_avail,rdens_avail=="> 3",0)
d2_rdens_avail <- replace(d2_rdens_avail,d2_rdens_avail!=0,1)
d2_rdens_avail <- type.convert(d2_rdens_avail,as.is = TRUE)

# house type
d2_ht_total <- replace(HT_FINAL,HT_FINAL == "Pucca",1)
d2_ht_total <- replace(d2_ht_total,d2_ht_total!= 1,0)
d2_ht_total <- type.convert(d2_ht_total,as.is = TRUE)

#######################################################################################

# Dichotomising the variables for WASH accessibility index for(2004-05)

# water accessibility
d2_wtraccfinal <- replace(water_access,water_access ==99,0)
d2_wtraccfinal <- type.convert(d2_wtraccfinal,as.is = TRUE)

# toilet access
d2_toilet_access <- replace(t_accfinal,t_accfinal=="Not Accessed",0)
d2_toilet_access <- replace(d2_toilet_access,d2_toilet_access=="Accessed",1)
d2_toilet_access <- type.convert(d2_toilet_access,as.is = TRUE)

# handwash
d2_handwash <- replace(handwash,handwash=="Hygienic",1)
d2_handwash <- replace(d2_handwash,d2_handwash!=1,0)
d2_handwash <- type.convert(d2_handwash,as.is = TRUE)

# water purify or not
d2_water_purify <- replace(Purify_water,Purify_water=="Always Purify",1)
d2_water_purify <- replace(d2_water_purify,d2_water_purify!=1,0)
d2_water_purify <- type.convert(d2_water_purify,as.is = TRUE)

# water pouring
d2_water_pouring <- replace(water_pouring,water_pouring == "Improved method of Pouring Drinking Water",1)
d2_water_pouring <- replace(d2_water_pouring,d2_water_pouring != 1,0)
d2_water_pouring <- type.convert(d2_water_pouring,as.is = TRUE)

# fuels used
d2_fuel_used <- replace(fuel_used,fuel_used == "Improved fuels",1)
d2_fuel_used <- replace(d2_fuel_used,d2_fuel_used == "Unimproved fuels",0)
d2_fuel_used <- type.convert(d2_fuel_used,as.is = TRUE)
######################################################################################

# descriptive statistics of the sample data for 2011-12

# declaring variables for storing the data
cities = unique(Metro_new)

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

# assigning the values to the variables

for(i in 1:6){
  Sample_sizes[i] = sum(data2["Metro_new"]==cities[i])
  
  poor[i] = sum(data2[data2["Metro_new"]==cities[i],"BPL"]=="Poor")
  
  illiterate[i] = sum(data2[data2["Metro_new"]==cities[i],"HH_Education"]=="Illiterate")
  higher_education[i] = sum(data2[data2["Metro_new"]==cities[i],"HH_Education"]=="Higher")
  secondary_education[i] = sum(data2[data2["Metro_new"]==cities[i],"HH_Education"]=="Secondary")
  primary_education[i] = sum(data2[data2["Metro_new"]==cities[i],"HH_Education"]=="Primary")
  
  general[i] = sum(data2[data2["Metro_new"]==cities[i],"Soci_cat"]=="General")
  muslims[i] = sum(data2[data2["Metro_new"]==cities[i],"Soci_cat"]=="MUSLIM")
  others[i] = sum(data2[data2["Metro_new"]==cities[i],"Soci_cat"]=="Christian,Sikh,jain")
  obc[i] = sum(data2[data2["Metro_new"]==cities[i],"Soci_cat"]=="OBC")
  sc[i] = sum(data2[data2["Metro_new"]==cities[i],"Soci_cat"]=="SC")
  st[i] = sum(data2[data2["Metro_new"]==cities[i],"Soci_cat"]=="ST")
  
  primary_occupation[i] = sum(data2[data2["Metro_new"]==cities[i],"Occu_headhh"]=="Primary")
  secondary_occupation[i] = sum(data2[data2["Metro_new"]==cities[i],"Occu_headhh"]=="Secondary")
  tertiay_occupation[i] = sum(data2[data2["Metro_new"]==cities[i],"Occu_headhh"]=="Tertiary")
  
}


non_poor = Sample_sizes - poor
unemployed = Sample_sizes - primary_occupation - secondary_occupation - tertiay_occupation

data_2011_sample <- data.frame(cities,Sample_sizes,poor,non_poor,illiterate,higher_education,
                               secondary_education,primary_education,general,muslims,others,
                               obc,sc,st,primary_occupation,secondary_occupation,tertiay_occupation,
                               unemployed)

df <- data.frame(t(data_2011_sample[-1]))
colnames(df) <- data_2011_sample[, 1]
data_2011_sample <- df
rm(df)
View(data_2011_sample)
write.table(data_2011_sample,"C:\\Users\\Ayushman\\Documents\\Metro-India\\2011-12backgroundvar.csv",
            append=FALSE,quote=FALSE,sep=",",dec=".",row.names=TRUE,col.names=NA)

############################################################################################
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
  water_source_1[i] <- sum(d2_water_resource[data2["Metro_new"]==cities[i]])*100/Sample_sizes[i]
  time_water_supply_1[i] <- sum(d2_water_supply[data2["Metro_new"]==cities[i]])*100/Sample_sizes[i] 
  toilet_facility_1[i] <- sum(d2_toilet_avail[data2["Metro_new"]==cities[i]])*100 /Sample_sizes[i]
  handwash_1[i] <- sum(d2_handwash[data2["Metro_new"]==cities[i]])*100/Sample_sizes[i]
  water_strorage_1[i] <- sum(d2_water_storage[data2["Metro_new"]==cities[i]])*100/Sample_sizes[i]
  housing_space_1[i] <- sum(d2_rdens_avail[data2["Metro_new"]==cities[i]])*100 /Sample_sizes[i]
  kitchen_1[i] <- sum(d2_kitchen[data2["Metro_new"]==cities[i]])*100/Sample_sizes[i]
  house_1[i] <- sum(d2_ht_total[data2["Metro_new"]==cities[i]])*100/Sample_sizes[i]
}

water_source_0 <- 100 - water_source_1
time_water_supply_0 <- 100 - time_water_supply_1
toilet_facility_0 <- 100 - toilet_facility_1
handwash_0 <- 100 - handwash_1 
water_strorage_0 <- 100 - water_strorage_1 
housing_space_0 <- 100 - housing_space_1
kitchen_0 <- 100 - kitchen_1
house_0 <- 100 - house_1

wash_aval_2011 <- data.frame(cities,water_source_1,water_source_0,time_water_supply_1,
                             time_water_supply_0,toilet_facility_1,toilet_facility_0,handwash_1,
                             handwash_0,water_strorage_1,water_strorage_0,housing_space_1,
                             housing_space_0,kitchen_1,kitchen_0,house_1,house_0)


df <- data.frame(t(wash_aval_2011[-1]))
colnames(df) <- wash_aval_2011[, 1]
wash_aval_2011 <- df
rm(df)

View(wash_aval_2011)
write.table(wash_aval_2011,"C:\\Users\\Ayushman\\Documents\\Metro-India\\wash_aval_2011.csv",
            append=FALSE,quote=FALSE,sep=",",dec=".",row.names=TRUE,col.names=NA)
#####################################################

# wash accessibility variables city wise data
time_water_access_1 <- rep(0,6)
toilet_access_1 <- rep(0,6)
handwash_acc_1 <- rep(0,6)
water_purity_1 <- rep(0,6)
water_pouring_1 <- rep(0,6) 
fuel_used_1 <- rep(0,6)

for(i in 1:6){
  time_water_access_1[i] <- sum(d2_wtraccfinal[data2["Metro_new"]==cities[i]])*100/Sample_sizes[i]
  toilet_access_1[i] <- sum(d2_toilet_access[data2["Metro_new"]==cities[i]],na.rm = TRUE)*100/sum(acc_toilet != 99) 
  handwash_acc_1[i] <- sum(d2_handwash[data2["Metro_new"]==cities[i]])*100 /Sample_sizes[i]
  water_purity_1[i] <- sum(d2_water_purify[data2["Metro_new"]==cities[i]])*100/Sample_sizes[i]
  water_pouring_1[i] <- sum(d2_water_pouring[data2["Metro_new"]==cities[i]])*100/Sample_sizes[i]
  fuel_used_1[i] <- sum(d2_fuel_used[data2["Metro_new"]==cities[i]])*100 /Sample_sizes[i]
}

time_water_access_0 <- 100 - time_water_access_1
toilet_access_0 <- 100 - toilet_access_1
handwash_acc_0 <- 100 - handwash_acc_1
water_purity_0 <- 100 - water_purity_1 
water_pouring_0 <- 100 - water_pouring_1 
fuel_used_0 <- 100 - fuel_used_1

wash_acc_2011 <- data.frame(cities,time_water_access_1,time_water_access_0,toilet_access_1,
                            toilet_access_0,handwash_acc_1,handwash_acc_0,water_purity_1,
                            water_purity_0,water_pouring_1,water_pouring_0,fuel_used_1,fuel_used_0)

df <- data.frame(t(wash_acc_2011[-1]))
colnames(df) <- wash_acc_2011[, 1]
wash_acc_2011 <- df
rm(df)

View(wash_acc_2011)
write.table(wash_acc_2011,"C:\\Users\\Ayushman\\Documents\\Metro-India\\wash_acc_2011.csv",
            append=FALSE,quote=FALSE,sep=",",dec=".",row.names=TRUE,col.names=NA)
#######################################################################################
# creating a dataframe of variables used in wash accessibility and wash availability 

acc_var2 <- data.frame(d2_wtraccfinal,d2_toilet_access,d2_handwash,d2_water_purify
                      ,d2_water_pouring,d2_fuel_used)

avai_var2 <- data.frame(d2_water_resource,d2_water_supply,d2_toilet_avail,d2_water_storage,
                        d2_kitchen,d2_rdens_avail,d2_ht_total)

mean_acc2 = rep(0,6)
sd_acc2 = rep(0,6)
mean_avai2 = rep(0,7)
sd_avai2 = rep(0,7)
for(i in 1:6){
  mean_acc2[i] = sum(acc_var2[i],na.rm = TRUE)/sum(Sample_sizes)
  sd_acc2[i] = sqrt(sum(acc_var2[i]^2,na.rm = TRUE)/sum(Sample_sizes) - mean_acc2[i]^2)
}

for(i in 1:7){
  mean_avai2[i] = sum(avai_var2[i],na.rm = TRUE)/sum(Sample_sizes)
  sd_avai2[i] = sqrt(sum(avai_var2[i]^2,na.rm = TRUE)/sum(Sample_sizes) - mean_avai2[i]^2)
}
# 6x6 covariance matrix for the wash accessibility
# 7x7 covariance matrix for the wash availabilty
cov_mat_acc2 <- matrix(rep(0,6*6),nrow = 6)
cov_mat_avai2 <- matrix(rep(0,7*7),nrow = 7)
for(i in 1:6){
  for(j in 1:6){
    cov_mat_acc2[i,j]<-cov((acc_var2[i]-mean_acc2[i])/sd_acc2[i],(acc_var2[j]-mean_acc2[j])/sd_acc2[j])
  }
}

for(i in 1:7){
  for(j in 1:7){
    cov_mat_avai2[i,j] <- cov((avai_var2[i]-mean_avai2[i])/sd_avai2[i],(avai_var2[j]-mean_avai2[j])/sd_avai2[j])
  }
}

# getting the eigen values
eigen(cov_mat_acc2)
acc_point2 <- eigen(cov_mat_acc2)$values
facc_matrix2 <- -(cbind(eigen(cov_mat_acc2)$vectors[,1]))

eigen(cov_mat_avai2)
avai_point2 <- eigen(cov_mat_avai2)$values
favai_matrix2 <- -(cbind(eigen(cov_mat_avai2)$vectors[,1]))
###################################################
# code for plotting the eigenvalues
#windows()
plot(acc_point2,ylab = "Eigenvalues",xlab = "Number",main = "Screeplot for Water accessibility
     2011-12",pch = 19)
lines(1:length(acc_point2),acc_point2,col = "blue",lwd = 1.5)
#windows()
plot(avai_point2,ylab = "Eigenvalues",xlab = "Number",main = "Screeplot for Water availabilty
     2011-12",pch = 19)
lines(1:length(avai_point2),avai_point2,col = "blue",lwd = 1.5)

###################################################
# calculating the wash scores

avl_score2 <- as.matrix(avai_var2)%*%(favai_matrix2)
wash.avl2 <- avl_score2

acc_score2 <- as.matrix(acc_var2)%*%(facc_matrix2)
wash.acc2 <- acc_score2

#city-wise availability scores
wash.avl.city = rep(0,6)
names(wash.avl.city) <- cities

for(i in 1:6){
  wash.avl.city[i] <- sum(avl_score2[data2["Metro_new"]==cities[i]])/sum(data2["Metro_new"]==cities[i])
}
# city-wise accessibility scores
wash.acc.city = rep(0,6)
names(wash.acc.city) <- cities
for(i in 1:6){
  wash.acc.city[i] <- sum(wash.acc2[data2["Metro_new"]==cities[i]])/sum(data2["Metro_new"]==cities[i])
}

# overall score is average of accessibility and availability score
score2 <- 0.5*(wash.acc2 + wash.avl2)

# city wise scores
score2.city <- rep(0,6)
names(score2.city) <- cities
for(i in 1:6){
  score2.city[i] <- sum(score2[data2["Metro_new"]==cities[i]],na.rm = TRUE)/sum(data2["Metro_new"]==cities[i])
}

# calculating the inequalities
library(ineq)

# Gini
gini <- rep(0,6)
for(i in 1:6){
  gini[i] <- ineq(score2[data2["Metro_new"] == cities[i]],type = "Gini")
}

# Theil
theil <- rep(0,6)
for(i in 1:6){
  theil[i] <- ineq(score2[data2["Metro_new"] == cities[i]],type = "Theil")
}

# Atkinson
atkinson <- rep(0,6)
for(i in 1:6){
  atkinson[i] <- ineq(score2[data2["Metro_new"] == cities[i]],type = "Atkinson")
}

# for total population
gini[7] <- ineq(score2,type = "Gini")
theil[7] <- ineq(score2,type = "Theil")
atkinson[7] <- ineq(score2,type = "Atkinson")
cities_names <- c(cities,c("Total"))
Inequalities_2011 <- data.frame(cities_names,gini,theil,atkinson)
View(Inequalities_2011)

Wash_scores_2011 <- data.frame(cities,wash.avl.city,wash.acc.city,
                               score2.city)
################################################################
# codes for the plots
# Wash between poor and non-poor
score_poor <- rep(0,6)
score_non_poor <- rep(0,6)
for(i in 1:6){
  score_poor[i] <- sum(score2[data2["Metro_new"]==cities[i] & data2["BPL"]=="Poor"],na.rm = TRUE)/sum(poor[i])
  score_non_poor[i] <- sum(score2[data2["Metro_new"]==cities[i] & data2["BPL"]!="Poor"],na.rm = TRUE)/sum(non_poor[i])
}
#windows()
barplot(cbind(score_poor,score_non_poor),names.arg = c("poor","non-poor"),
        main = "Average Wash scores of poor and non-poor 2011-12",beside = TRUE,legend.text = cities,
        col = c("red","blue","green","yellow","magenta","grey"),ylim = c(0,2.5),
        args.legend = list(x="topleft",inset = c(0.1,0)))

# wash scores between education levels
score_h_ed <- rep(0,6)
score_p_ed <- rep(0,6)
score_s_ed <- rep(0,6)
score_illi <- rep(0,6)

for(i in 1:6){
  score_h_ed[i] <- sum(score2[data2["Metro_new"]==cities[i] & data2["HH_Education"]=="Higher"])/sum(data2["Metro_new"]==cities[i] & data2["HH_Education"]=="Higher")
  score_p_ed[i] <- sum(score2[data2["Metro_new"]==cities[i] & data2["HH_Education"]=="Primary"])/sum(data2["Metro_new"]==cities[i]& data2["HH_Education"]=="Primary")
  score_s_ed[i] <- sum(score2[data2["Metro_new"]==cities[i] & data2["HH_Education"]=="Secondary"],na.rm = TRUE)/sum(data2["Metro_new"]==cities[i]& data2["HH_Education"]=="Secondary")
  score_illi[i] <- sum(score2[data2["Metro_new"]==cities[i] & data2["HH_Education"]=="Illiterate"])/sum(data2["Metro_new"]==cities[i]& data2["HH_Education"]=="Illiterate")  
}

#windows()
barplot(cbind(score_illi,score_p_ed,score_s_ed,score_h_ed),names.arg = c("Illiterate","Primary",
                                                                         "Secondary","Higher"),
        main = "Average Wash scores and education levels 2011-12"
        ,ylim = c(0,3),legend.text = cities,beside = TRUE,width = 1,
        col = c("red","blue","green","yellow","magenta","grey"),args.legend = list(x="topleft",
                                                                                   inset = c(0.05,0)))  

# average wash scores city-wise
#windows()
barplot(score2.city,beside = TRUE,space = 1,width = 0.05,ylim = c(0,2),main = "Average Wash scores of cities 2011-12",
        xlab = "Cities",ylab = "Average Wash Scores")


# average wash accessibility scores
#windows()
barplot(wash.acc.city,beside = T,ylim = c(0,2.5),main = "Wash Accessibility scores 2011-12",
        xlab = "Cities",ylab = "Wash Accessibility scores")

# average wash availability scores
#windows()
barplot(wash.avl.city,beside = T,ylim = c(0,2.5),main = "Wash Availability scores 2011-12",
        xlab = "Cities",ylab = "Wash Availability scores")















