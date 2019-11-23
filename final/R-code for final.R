library(readr)
epwt9 <- read_csv("~/Grad School CG/R/epwt9.0.csv")
View(epwt9)
attach(epwt9)
library(ggplot2)
library(data.table)
data<-read.csv("EPWTv4.csv",header=TRUE)
head(epwt9,10)
summary(epwt9)

# Clearing data in set using data.table#
d<-data.table(data)
d<-d[Year >= 1983 & Year <= 2016 & (Quality=="A" | Quality=="B"), .(Country,Id,Year,Quality,X,c)]






library(readxl)
productivity <- read_excel("~/Grad School CG/R/productivity.xlsx")
View(productivity)
x<-productivity$Year
y1<-productivity$`Hourly compensation`
y2<-productivity$Productivity
plot(x,y1)
par(new=TRUE)
plot(x,y2)



#BEA data
library(readxl)


library(readxl)
H_w_data <- read_excel("~/Grad School CG/R/H-w data.xlsx")
View(H_w_data)
attach(H_w_data)

plot(Year, `Finance, insurance, real estate, rental, and leasing`,type="l",col=4,xlim=c(1997,2016),ylim=c(0,5000)) # hierarchical)
lines(Year, Manufacturing, col='green')
lines(Year,Construction)
lines(Year, `Professional and business services`)
lines(Year, `Educational services, health care, and social assistance`)
lines(Year, Government)
legend(.07,110,c("Individual","Population","Hierarchical"),col=c(1,2,4),pch=c(20,20,20),bty="n")
}

-----------------------------------------------------------------------------------------------------

####Updata data with all variables needed
library(readxl)
Final_EPWT6_0 <- read_excel("~/Grad School CG/R/Final EPWT6.0.xlsx", 
                            sheet = "EPWT OECD")
View(Final_EPWT6_0)
attach(Final_EPWT6_0)
data <- Final_EPWT6_0
library(ggplot2)
library(data.table)
head(data,10)
summary(data)

#plot against

mod2 <-plot(data$pi, log(data$pistar),pch=20,xlab="Profit Share",ylab="Viability Parameter")
mod3 <-plot(data$pi,log(data$pistari))xlim=c(0,1),ylim=c(0,1))

# Add vertical CI lines
abline(mod2,col="red",lwd=2)
abline(v=mod4[2],lty=2)

# Clearing data in set using data.table#
d<-data.table(data)
d<-d[ Year >= 2014 & Year <= 2014] 

lm(d$pi,log(d$pistar)

mod4 <-plot(d$pi,log(d$pistar))


 stop(call. = TRUE)


# Load the package that contains the full dataset.

library(car)
library(corrplot) # We'll use corrplot later on in this example too.
library(visreg) # This library will allow us to show multivariate graphs.
library(rgl)
library(knitr)
library(scatterplot3d)




set.seed(1)

# Center predictors.

profitshare.x = scale( d$pi, center=TRUE, scale=FALSE)
productivity.x = scale(xppp2011, center=TRUE, scale=FALSE)
fertility.x = scale(Fertility, center=TRUE, scale=FALSE)

# bind these new variables into newdata and display a summary.
new.x.vars = cbind(profitshare.x, productivity.x, fertility.x)
newdestdata = cbind(d, new.x.vars)
names(newestdata)[64:66] = c("profitshare.x", "productivity.x", "fertility.x" )
summary(newestdata)
view(newestdata)












--------------------------------------------------------------
# Load the package that contains the full dataset.

library(car)
library(corrplot) # We'll use corrplot later on in this example too.
library(visreg) # This library will allow us to show multivariate graphs.
library(rgl)
library(knitr)
library(scatterplot3d)




set.seed(1)

# Center predictors.

profitshare.c = scale( data$pi, center=TRUE, scale=FALSE)
productivity.c = scale(xppp2011, center=TRUE, scale=FALSE)
fertility.c = scale(Fertility, center=TRUE, scale=FALSE)

# bind these new variables into newdata and display a summary.
new.c.vars = cbind(profitshare.c, productivity.c, fertility.c)
newdata = cbind(data, new.c.vars)
names(newdata)[64:66] = c("profitshare.c", "productivity.c", "fertility.c" )
summary(newdata)
view(newdata)


#create variable pi i
shareeach.c = scale(data$pi, center = TRUE, scale = FALSE)
countryshare.c = scale(data$Country , center = TRUE, scale = FALSE)
#bind this together
pii.c = cbind(shareeach.c,countryshare.c)
newerdata = cbind(newdata, pii.c)
names(newerdata)[67:68] = c("shareeach.c, countryshare.c")
summary(newerdata)


data$Country


New.pii = 

# fit a linear model and run a summary of its results.
mod1 = lm(data$Country ~ profitshare.c + productivity.c + fertility.c, data=newdata)
summary(mod1)


# Plot matrix of all variables.
plot(d$country, pch=16, col="blue", main="Matrix Scatterplot of Income, Education, Women and Prestige")

# fit a linear model and run a summary of its results.
mod1 <- lm(vars ~ data$pi + 'pi' + Fertility, data )
summary(mod1)
View(mod1)
# Multiple Linear Regression 
fit <- lm(y ~ x1 + x2 + x3, data=mydata)
summary(fit) # show results



