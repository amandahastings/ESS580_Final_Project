# Data wrangling {-}



```r
library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways

# knitr::opts_knit$set(root.dir='..')
```



```r
####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('dataDataWrangle',full.names=T)
```


```r
#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')
```

```
## Warning: One or more parsing issues, see `problems()` for details
```

```
## Rows: 2103 Columns: 3
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## dbl  (2): NDWI (NIR/SWIR1) (Landsat 4/5/7/8 SR) at Polygon 1, 1984-01-01 to ...
## date (1): DateTime
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')
```

```
## Warning: One or more parsing issues, see `problems()` for details
```

```
## Rows: 2103 Columns: 3
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## dbl  (2): NDSI (Landsat 4/5/7/8 SR) at Polygon 1, 1984-01-01 to 2019-08-15, ...
## date (1): DateTime
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')
```

```
## Warning: One or more parsing issues, see `problems()` for details
```

```
## Rows: 2102 Columns: 3
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## dbl  (2): NDVI (Landsat 4/5/7/8 SR) at Polygon 1, 1984-01-01 to 2019-08-15, ...
## date (1): DateTime
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))
```



## Question 1


1) What is the correlation between NDVI and NDMI? 
Here, I want you to convert the full_long dataset in to a wide dataset using the function "spread."
Then, make a plot that shows the correlation as a function of if the site was burned or not (x axis should be ndmi).
You should exclude winter months and focus on summer months.



```r
# Convert from long to wide data with spread() 
# Add month and year columns to wide data
full_wide1 <- full_long %>%
  spread(key='data', value='value') %>% 
  mutate(month=month(DateTime)) %>% 
  mutate(year=year(DateTime))
```



```r
# Plot ndvi as response and ndmi as predictor, distinguishing between burn or unburned sites, limit to only summer months 
summer_wide <- full_wide1 %>%
  filter(month %in% c(6,7,8))
ggplot(summer_wide, aes(x=ndmi, y=ndvi, color=site))+
  geom_point(shape=1)+
  labs(x="NDMI", y="NDVI")+
  theme_few()+ 
  scale_color_manual(name="Site",labels=c("Burned","Unburned"), values= c("#274a12","#babf28"))+ 
  xlim(-0.6,0.7)+ 
  ylim(0.05, 0.6)
```

<img src="02-DataWrangle_files/figure-html/plot ndvi vs ndmi-1.png" width="672" />



```r
# Plot ndvi vs ndmi with facet wrap over site type (burned or unburned)
ggplot(summer_wide, aes(x=ndmi, y=ndvi))+
  geom_point(shape=1)+
  labs(x="NDMI", y="NDVI")+
  theme_few()+
  facet_wrap('site')
```

```
## Warning: Removed 11 rows containing missing values (geom_point).
```

<img src="02-DataWrangle_files/figure-html/plot ndvi vs ndmi, facet wrap site-1.png" width="672" />



```r
# Test correlation between ndvi and ndmi
cor.test(summer_wide$ndmi, summer_wide$ndvi, method='pearson')
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  summer_wide$ndmi and summer_wide$ndvi
## t = 37.356, df = 946, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.7449419 0.7965158
## sample estimates:
##       cor 
## 0.7719964
```

```r
# Fit lm model for ndvi by ndmi
LMFit1 <- lm(ndvi~ndmi, data =summer_wide)
summary(LMFit1)
```

```
## 
## Call:
## lm(formula = ndvi ~ ndmi, data = summer_wide)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.42148 -0.03375  0.01529  0.04675  0.14172 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.404394   0.002479  163.12   <2e-16 ***
## ndmi        0.908772   0.024327   37.36   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.07008 on 946 degrees of freedom
##   (11 observations deleted due to missingness)
## Multiple R-squared:  0.596,	Adjusted R-squared:  0.5956 
## F-statistic:  1395 on 1 and 946 DF,  p-value: < 2.2e-16
```



```r
# LMFit1 diagnostic plots 
par(mfrow=c(1,2))
plot(LMFit1, which= c(1:2))
```



**1. Based upon a test of correlation, we have evidence of a positive linear association between summer NDMI and summer NDVI, with a p-value < 2.2e-16 (less than 0.05). For every 1 unit increase in summer NDMI, there is a 0.908772 increase in summer NDVI (p-value < 2.2e-16).** 



#### Bonus Question 1

```r
# Convert from long to wide with pivot_wider() 
full_wide2 <- full_long %>%
  pivot_wider(names_from = 'data', values_from = 'value')
```


## Question 2 


2) What is the correlation between average NDSI (normalized snow index) for January - April and average NDVI for June-August?
In other words, does the previous year's snow cover influence vegetation growth for the following summer?



```r
# Summarize data by average ndvi for summer months
summer_ndvi<- full_wide1 %>%
  group_by(site, year, month) %>%
  filter(month %in% c(6, 7, 8)) %>%
  summarize(mean_ndvi = mean(ndvi)) %>%
  filter(!is.na(mean_ndvi))

# Summarize data by average ndsi over winter months 
winter_ndsi <- full_wide1 %>%
  group_by(site, year, month) %>%
  filter(month %in% c(1, 2, 3, 4)) %>%
  summarize(mean_ndsi = mean(ndsi)) %>%
  filter(!is.na(mean_ndsi))
```



```r
# Join average summer ndvi and average winter ndsi by year and site
# Add burnperiod column to distinguish pre- and post-fire years
wide_averages <- inner_join(winter_ndsi, summer_ndvi, by= c('site', 'year')) %>%
  mutate(burnperiod = as.factor(ifelse(year < 2002,"prefire", "postfire"))) 
```

  

```r
# Plot and evaluate relationship between ndvi and ndsi
ggplot(wide_averages, aes(x=mean_ndsi, y=mean_ndvi))+
  geom_point(shape=1, color = "#22AA99")+
  geom_smooth(method=lm, color="#22AA99", size=0.1, se=FALSE)+
  theme_few()+ 
  labs(x="Average Winter NDSI", y="Average Summer NDVI")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

<img src="02-DataWrangle_files/figure-html/unnamed-chunk-5-1.png" width="672" />



```r
# Test correlation between ndvi and ndsi overall
cor.test(wide_averages$mean_ndsi, wide_averages$mean_ndvi)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  wide_averages$mean_ndsi and wide_averages$mean_ndvi
## t = 3.7226, df = 728, p-value = 0.0002124
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.06475296 0.20718259
## sample estimates:
##      cor 
## 0.136674
```

```r
# Fit lm model for ndvi by ndsi overall
LMFit2 <- lm(mean_ndvi~mean_ndsi, data = wide_averages)
summary(LMFit2)
```

```
## 
## Call:
## lm(formula = mean_ndvi ~ mean_ndsi, data = wide_averages)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.27031 -0.07904  0.03542  0.07462  0.19958 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 0.376857   0.003837  98.209  < 2e-16 ***
## mean_ndsi   0.042658   0.011459   3.723 0.000212 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.102 on 728 degrees of freedom
## Multiple R-squared:  0.01868,	Adjusted R-squared:  0.01733 
## F-statistic: 13.86 on 1 and 728 DF,  p-value: 0.0002124
```



```r
# LMFit2 diagnostics
par(mfrow=c(1,2))
plot(LMFit2, which= c(1:2))
```



**2. The p-value from a test of correlation for average summer NDVI and average winter NDSI is 0.0002124 and less than 0.05. We have evidence of a positive linear association between average summer NDVI and average winter NDSI. For every 1 unit increase in average winter NDSI, there is a 0.042658 increase in average summer NDVI.** 



## Question 3


3) How is the snow effect from question 2 different between pre- and post-burn and burned and unburned? 


#### *NDVI-NDSI: Pre- and postfire figure and analyses* 


```r
# Plot and compare ndvi-ndsi relationship between pre- and post-burn periods
ggplot(wide_averages, aes(x=mean_ndsi, y=mean_ndvi, color=burnperiod))+
  geom_point(shape=1)+
  theme_few()+
  geom_smooth(method=lm, size=0.1, se=FALSE)+
  labs(x="Average Winter NDSI", y="Average Summer NDVI")+
  scale_color_manual(name="",labels=c("Pre-fire","Post-fire"), values= c("#2f94b5","#b5982f"))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

<img src="02-DataWrangle_files/figure-html/unnamed-chunk-7-1.png" width="672" />



```r
# Fit lm model to evaluate ndvi-ndsi correlation pre- and post-fire 
LMFit3 <- lm(mean_ndvi~mean_ndsi*burnperiod, data= wide_averages)
summary(LMFit3)
```



```r
# LMFit3 diagnostic plots
par(mfrow=c(1,2))
plot(LMFit3, which= c(1:2))
```



```r
# Create separate data frames for prefire and postfire 
prefire <- wide_averages %>% 
  filter(burnperiod %in% 'prefire')
  
postfire <- wide_averages %>% 
  filter(burnperiod %in% 'postfire')
```



```r
# Test correlation between ndvi and ndsi prefire and postfire 
cor.test(prefire$mean_ndsi,prefire$mean_ndvi)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  prefire$mean_ndsi and prefire$mean_ndvi
## t = 1.3684, df = 328, p-value = 0.1721
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.03289123  0.18182497
## sample estimates:
##        cor 
## 0.07534012
```

```r
cor.test(postfire$mean_ndsi,postfire$mean_ndvi)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  postfire$mean_ndsi and postfire$mean_ndvi
## t = 2.5716, df = 398, p-value = 0.01048
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.03017306 0.22310155
## sample estimates:
##       cor 
## 0.1278465
```



**3a. In pre-fire years, we do not have evidence of an association between average summer NDVI and average winter NDSI (p-value=0.1721). However, in post-fire years, we have evidence of an association between average summer NDVI and average winter NDSI (p-value=0.01048).** 



#### *NDVI-NDSI: Burned and unburned figure and analyses*


```r
# Plot and compare ndvi-ndsi relationship across burned versus unburned sites
ggplot(wide_averages, aes(x=mean_ndsi, y=mean_ndvi, color=site))+
  geom_point(shape=1)+
  theme_few()+ 
  geom_smooth(method=lm, size=0.1, se=FALSE)+
  labs(x="Average Winter NDSI", y="Average Summer NDVI")+ 
  scale_color_manual(name="Site",labels=c("Burned","Unburned"), values= c("#292423","#a4a823"))
```

```
## `geom_smooth()` using formula 'y ~ x'
```

<img src="02-DataWrangle_files/figure-html/unnamed-chunk-11-1.png" width="672" />



```r
# Fit lm model to evaluate ndvi-ndsi correlation between burned and unburned sites
LMFit4 <- lm(mean_ndvi~mean_ndsi*site, data=wide_averages)
summary(LMFit4)
```



```r
# LMFit4 diagnostics
par(mfrow=c(1,2))
plot(LMFit4, which= c(1:2))
```



```r
# Create separate data frames for burned and unburned sites
burned <- wide_averages %>% 
  # filter(burnperiod %in% 'postfire') %>%
  filter(site %in% 'burned')
  
unburned <- wide_averages %>% 
  # filter(burnperiod %in% 'postfire') %>%
  filter(site %in% 'unburned')
```



```r
# Test correlation between ndvi and ndsi in burned and unburned sites separately 
cor.test(burned$mean_ndsi, burned$mean_ndvi)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  burned$mean_ndsi and burned$mean_ndvi
## t = 1.3356, df = 370, p-value = 0.1825
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.03264288  0.16974960
## sample estimates:
##        cor 
## 0.06926608
```

```r
cor.test(unburned$mean_ndsi,unburned$mean_ndvi)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  unburned$mean_ndsi and unburned$mean_ndvi
## t = 0.44175, df = 356, p-value = 0.6589
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.08043935  0.12674928
## sample estimates:
##        cor 
## 0.02340629
```



**3b. When analyzing all years included in the dataset, we do not have evidence of an association between average summer NDVI and average winter NDSI within the unburned area (p-value= 0.6589) and burned area (p-value=0.1825). When analyzing only years postfire, we still do not have evidence of an association between average summer NDVI and average winter NDSI in the unburned (p-value=0.905) and burned areas (p-value=0.3226).**



## Question 4


4) What month is the greenest month on average? 



```r
# Calculate maximum, monthly mean ndvi over time
ndvi_stats<- wide_averages %>% 
  group_by(month.y) %>% 
  summarize(max_ndvi=max(mean_ndvi))
ndvi_stats
```

```
## # A tibble: 3 x 2
##   month.y max_ndvi
##     <dbl>    <dbl>
## 1       6    0.480
## 2       7    0.501
## 3       8    0.561
```



**4. August is the greenest month on average.** 



## Question 5 


5) What month is the snowiest on average?



```r
# Calculate maximum, monthly mean ndsi over time
ndsi_stats <- wide_averages %>% 
  group_by(month.x) %>% 
  summarize(max_ndsi=max(mean_ndsi))
ndsi_stats
```

```
## # A tibble: 4 x 2
##   month.x max_ndsi
##     <dbl>    <dbl>
## 1       1    0.689
## 2       2    0.711
## 3       3    0.655
## 4       4    0.605
```



**5. February is the snowiest month on average.** 



#### Bonus Question 2: Use Climate Engine to pull the same data for the assignment, but updated with 2020/2021 data.

Updated Climate Engine with years 2020 and 2021 is stored in data2. 


