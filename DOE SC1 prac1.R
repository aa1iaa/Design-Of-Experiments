#Q1
#H0 : u1 = u2 = u3  vs H1 : atleast 1 is not equal
y <- c(22,42,44,52,45,37,52,33,8,47,43,32,16,24,19,18,34,39)
AFactor <- c("i1","i1","i1","i1","i1","i1", "i2", "i2", "i2", "i2", "i2", "i2", "i3", "i3", "i3", "i3", "i3", "i3")
AFactor1 <- c(rep("i1",6),rep("i2",6),rep("i3",6))
DF <- data.frame(y,AFactor)
result <- aov(y~AFactor)
summary(result)  
TukeyHSD(result)
plot(result)
#plot1 shows slight deviation it can still be considered to be linear
#plot2 shows majority of  observations in a straight line i.e.they are normally distributed
#plot3 shows the line deviating thus it is hetroscedastic
#plot4 shows no outliers outside the cook's distance line, the factor i2 has a higher mean


#Q2)
#H0 : u1 = u2 = u3 vs H1 : atleast 1 is not the same
lh <- c(4.5, 5, 5.5, 6.75, 6.5, 6.5, 10.5, 9.5, 9.75, 8.75, 6.5, 6.25)
time <- c(rep("35", 4), rep("40", 4), rep("45", 4))
result2 <- aov(lh~time)
result2
summary(result2)
qf(0.95,2,9) #Ftab
TukeyHSD(result2)
plot(result2)




#plot1 shows no deviation, thus can be considered to be linear
#plot2 shows a few observations deviationg greatly from the straight line i.e. they are not normally distributed
#plot3 shows the line deviating thus it is hetroscedastic
#plot4 shows no outliers outside the cook's distance line


#Q3) 
#H0 : u1 = u2 = u3 = u4 vs H1 : atleast 1 is not the same


tstr <- c(3129, 3000, 2865, 2890, 3200, 3300, 2975, 3150, 2800, 2900, 2985, 3050, 2600, 2700, 2600, 2765)
mt <- c(rep("mt1",4), rep("mt2",4), rep("mt3",4), rep("mt4",4))
result3 <- aov(tstr~mt)
result3
qf(0.95, 3, 12)
summary(result3)
TukeyHSD(result3)
plot(result3)
#plot1 shows slight deviation but can still be considered to be linear
#plot2 shows majority of  observations in a straight line i.e.they are normally distributed
#plot3 shows the line only slightly deviating thus it is homoscedastic
#plot4 shows no outliers outside the cook's distance line and the mean of of mt2 is slightly higher compared to others

