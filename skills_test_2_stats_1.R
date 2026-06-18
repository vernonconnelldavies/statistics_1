library(dplyr)
library(tidyverse)
library(openintro)
library(ggfortify)
library(broom)

#firstly I have copied and pasted the skills test questions description below

#This skills test makes us of an included dataset. You are required to read in the 
#supplied dataset (named skills_test2.csv) and conduct appropriate regression analysis on it.
#The comments in the submitted R script should justify the choice for the methods used 
#and what conclusions should be drawn.

house_prices<-read.csv("https://raw.githubusercontent.com/vernonconnelldavies/statistics_1/refs/heads/main/skills_test2.csv")
#I uploaded the dataset to my Github page for convenience of the person running this script.
house_prices

#so we have the dataset available now in our script, its fairly intuitive, 
#that our output factor will be 'price,' that is we will input 'size,' 'location,' 'bedrooms,'
#'bathrooms,' 'garden' and 'condition.'
#'We will use backwards elimination to find an accurate linear model, using the course notes 
#'from Statistics 1 as our main guide.

summary(house_prices$price)

ggplot(house_prices,aes(x=price))+geom_histogram()
#the graph output shows a normal distribution for house prices skewed towards the left

#we can see the median and the mean sit on around 275k from the summary

#we can start with a simple model of 'size' versus 'price,' it makes sense that a house would 
#generally cost more the larger it is

lmx<-lm(size~price, data=house_prices)
lmx
cor(house_prices$size,house_prices$price)

#we can see from the output we have a nice positive correlation value of 0.92

ggplot(house_prices,aes(x=size,y=price))+geom_point()

#our graph similarly shows a straight line of thick width, this gives us further
#confidence of a correlation

#these two parameters are both numeric, we should try now with a binary parameter, 'location'

#for the purposes of this exercise we will take 'city' as '1' and 'country' as '0' for our
#house price analysis


#after some trial and error and help from the following Stack Over Flow thread, 
#'city' was changed to the number 1 and 'country' was changed to the number 0
#https://stackoverflow.com/questions/30298773/rhow-to-replace-string-to-integer

house_prices$location[which(house_prices$location=='city')]<-1
house_prices$location[which(house_prices$location=='country')]<-0

#next we have 'garden' size, which we will change to 'small'=1, 'medium'=2 and 'large'=3 
#respectively

house_prices$garden[which(house_prices$garden=='small')]<-1
house_prices$garden[which(house_prices$garden=='medium')]<-2
house_prices$garden[which(house_prices$garden=='large')]<-3

#for 'condition,' we will use 'work needed'=1, 'good'=2 and 'excellent'=3

house_prices$condition[which(house_prices$condition=='work needed')]<-1
house_prices$condition[which(house_prices$condition=='good')]<-2
house_prices$condition[which(house_prices$condition=='excellent')]<-3

house_prices_1<-select(house_prices,condition,price)%>%
  mutate(condition=as.numeric(condition)) #this code makes sure our string is numeric so R 
#can know some idea of its value, for example that 'good' is better than 'work needed' 
#but not better than 'excellent'

#Now we will test our data to see if there is a correlation between condition and price,
#which there should be

lmy<-lm(condition~price, data=house_prices_1)
lmy

cor(house_prices_1$condition,house_prices_1$price)

#this result is surprising, we would expect better condition to result in a better price
#we would expect a correlation between these 2 parameters

house_prices_2<-select(house_prices,location,price)%>%
  mutate(location=as.numeric(location)) #this code makes sure our string is numeric as earlier

cor(house_prices_2$location,house_prices_2$price)

#there is a linear correlation as we would expect at 0.3, city prices are generally higher
#than country prices


#now to build the linear regression model via backward elimination

house_prices_ready<-mutate(house_prices,location=as.numeric(location),
                           garden=as.numeric(garden), condition=as.numeric(condition))
#this code just does what we already did, making strings numeric so R can get some idea
#of the value of each data entry that is categorical. Not the most efficient, so sorry 
#for that.



#excluding nothing
lm0<-lm(price~size+location+bedrooms+bathrooms+garden+condition,data=house_prices_ready)


#using the 'glace' tool
glance(lm0)



#now we exclude each of the parameters in turn

#excluding size
lm1<-lm(price~location+bedrooms+bathrooms+garden+condition,data=house_prices_ready)
#excluding location
lm2<-lm(price~size+bedrooms+bathrooms+garden+condition,data=house_prices_ready)
#excluding bedrooms
lm3<-lm(price~size+location+bathrooms+garden+condition,data=house_prices_ready)
#excluding bathrooms
lm4<-lm(price~size+location+bedrooms+garden+condition,data=house_prices_ready)
#excluding garden
lm5<-lm(price~size+location+bedrooms+bathrooms+condition,data=house_prices_ready)
#excluding condition
lm6<-lm(price~size+location+bedrooms+bathrooms+garden,data=house_prices_ready)


#I copied the next block line of code directly from the notes.

# first we make a list of all the models
all_models <- list(lm0,lm1, lm2, lm3, lm4, lm5, lm6)
# then we lool through them and extract the equation and adjusted R^2
all_models %>%
  map_dfr(function(lm_x) {
    # create a tibble with the bits we want
    output <- tibble(
      call = as.character(lm_x$call)[2], # the equation
      adj.r.squared = glance(lm_x)$adj.r.squared
    )
  }) %>%
  # find the largest value
  arrange(desc(adj.r.squared))

#so if we take out size, condition or location we end up with a lower adjusted r squared value
#we can take out bedrooms, bathrooms and garden without any effect.
#Therefore size, condition and location are significant parameters. Bedrooms, 
#bathroom and garden are insignificant.

tidy(lm0)

#from our final tidy output we can see the p values for size, location and condition are exceptionally
#small meaning they are the most statistically significant.

#after receiving feedback from Paula, 
#we are after the model that has the fewest possible parameters to explain the data.
#This is called the parsimonious model.
#A good guess is 'size,' 'location' and 'condition' as these parameters have very low
#P values in our output above

lm7<-lm(price~size+location+condition,data=house_prices_ready)

#now back to our code which outputs the adjusted R squared value

# first we make a list of all the models
all_models <- list(lm0,lm1,lm2,lm3,lm4,lm5,lm6,lm7)
# then we lool through them and extract the equation and adjusted R^2
all_models %>%
  map_dfr(function(lm_x) {
    # create a tibble with the bits we want
    output <- tibble(
      call = as.character(lm_x$call)[2], # the equation
      adj.r.squared = glance(lm_x)$adj.r.squared
    )
  }) %>%
  # find the largest value
  arrange(desc(adj.r.squared))

#and the adjusted r squared value is also 0.996, looks like we have a winner.
#lm7 has only 3 parameters and yet still has the same high adjusted R squared value.
#we will now see if we can reduce further just for completeness sake.

#removed size
lm8<-lm(price~location+condition,data=house_prices_ready)
#remove condition
lm9<-lm(price~size+location,data=house_prices_ready)
#remove location
lm10<-lm(price~size+condition,data=house_prices_ready)

# first we make a list of all the models
all_models <- list(lm0,lm1,lm2,lm3,lm4,lm5,lm6,lm7,lm8,lm9,lm10)
# then we lool through them and extract the equation and adjusted R^2
all_models %>%
  map_dfr(function(lm_x) {
    # create a tibble with the bits we want
    output <- tibble(
      call = as.character(lm_x$call)[2], # the equation
      adj.r.squared = glance(lm_x)$adj.r.squared
    )
  }) %>%
  # find the largest value
  arrange(desc(adj.r.squared))

#From our output we can see the adjusted R squared value gets smaller in lm8, lm9 
#and lm10 so we know we can't take away one of these 3 key parameters without 
#having the adjusted R squared value affected.

#so our final result is lm7. 

#finally we will move onto predicting, firstly we would expect a large house in the city 
#in excellent condition would command a price well above the median and well above the mean

#so our condition=3, location=1 (city)...
summary(house_prices_ready$size) #we use this code to get the maximum for 'size'

#for 'size' we will take the max value (in the dataset) of 684.
#now we have our 3 values for our predictive model. What price would these 3 values give?

summary(lm7)

plot(lm7)
#this produces a normal looking QQ plot (a straight line)

#the following code was taken from statology.org webpage on linear regression predicting code
#https://www.statology.org/r-lm-predict/

newdata1=data.frame(condition=3,location=1,size=684) #condition='excellent,' location='city'
predict(lm7,newdata1)

#so our equation is price=-32000+648*size+50000*location+9999*condition

#and our final predictive output is 491k which is realistic because the highest value of 
#our price dataset is 471k from our 'summary(house_prices$price)' code from earlier.
#We would expect a very large price for these values of parameters.
#we can check this result on our hand calculator (with our equation above), 
#the answer is 481k. The discrepancy between 491k (our R output) plus the 481k (hand calculator)
#can reasonably be put down to rounding error, it is approximately 2% outwith the 
#output generated in R value.

#another test we will do is enter 3 values of location, size and condition where we
#already know the price, for example our first data entry from the original dataset.
#so size=134, location=1, condition=2

newdata2=data.frame(condition=2,location=1,size=134)
predict(lm7,newdata2)

#and our output is 125k which is about 10k off the price value entered into our dataset,
#this is less than 10% and seems fairly realistic for our predictive model


#for the next phase of this assignment we will take the predicted tool a bit further
#and predict values for every row in our original dataset, this will give us a predicted 
#price value and an actual price value which we can then plot the result.

house_prices_predicted<-mutate(house_prices_ready,predicted_price=(-32000)+648*size+50000*location+9999*condition)
house_prices_predicted #we can see we now have our new column with predicted price

ggplot(house_prices_predicted,aes(x=price,y=predicted_price))+geom_point()

#and we can see a strong linear graph going through the origin, we can safely assume if given a new set of data we could 
#predict with good accuracy a new price