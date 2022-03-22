# Key programming concepts: webscraping, iteration, functions {-}





# Simple web scraping

R can read html using either rvest, xml, or xml2 packages. Here we are going to navigate to the Center for Snow and Avalance Studies  [Website](https://snowstudies.org/archived-data/) and read a table in. This table contains links to data we want to programmatically download for three sites. We don't know much about these sites, but they contain incredibly rich snow, temperature, and precip data. 


## Reading an html 

### Extract CSV links from webpage


```r
site_url <- 'https://snowstudies.org/archived-data/'

#Read the web url
webpage <- read_html(site_url)

#See if we can extract tables and get the data that way
tables <- webpage %>%
  html_nodes('table') %>%
  magrittr::extract2(3) %>%
  html_table(fill = TRUE)
#That didn't work, so let's try a different approach

#Extract only weblinks and then the URLs!
links <- webpage %>%
  html_nodes('a') %>%
  .[grepl('24hr',.)] %>%
  html_attr('href')
```


## Data Download

### Download data in a for loop


```r
#Grab only the name of the file by splitting out on forward slashes
splits <- str_split_fixed(links,'/',8)

#Keep only the 8th column
dataset <- splits[,8] 

#generate a file list for where the data goes
file_names <- paste0('dataFunctions/',dataset)

for(i in 1:3){
  download.file(links[i],destfile=file_names[i])
}

downloaded <- file.exists(file_names)

evaluate <- !all(downloaded)
```


### Download data in a map


```r
#Map version of the same for loop (downloading 3 files)
if(evaluate == T){
  map2(links[1:3],file_names[1:3],download.file)
}else{print('data already downloaded')}
```

```
## [[1]]
## [1] 0
## 
## [[2]]
## [1] 0
## 
## [[3]]
## [1] 0
```


## Data read-in 

### Read in just the snow data as a loop


```r
#Pattern matching to only keep certain files
snow_files <- file_names %>%
  .[!grepl('SG_24',.)] %>%
  .[!grepl('PTSP',.)]

#empty_data <- list()

# snow_data <- for(i in 1:length(snow_files)){
#   empty_data[[i]] <- read_csv(snow_files[i]) %>%
#     select(Year,DOY,Sno_Height_M)
# }

#snow_data_full <- do.call('rbind',empty_data)

#summary(snow_data_full)
```


### Read in the data as a map function


```r
our_snow_reader <- function(file){
  name = str_split_fixed(file,'/',2)[,2] %>%
    gsub('_24hr.csv','',.)
  df <- read_csv(file) %>%
    select(Year,DOY,Sno_Height_M) %>%
    mutate(site = name)
}

snow_data_full <- map_dfr(snow_files,our_snow_reader)
```

```
## Rows: 6211 Columns: 52
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## dbl (52): ArrayID, Year, DOY, Hour, LoAir_Min_C, LoAir_Min_Time, LoAir_Max_C...
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```
## Rows: 6575 Columns: 48
```

```
## -- Column specification --------------------------------------------------------
## Delimiter: ","
## dbl (48): ArrayID, Year, DOY, Hour, LoAir_Min_C, LoAir_Min_Time, LoAir_Max_C...
```

```
## 
## i Use `spec()` to retrieve the full column specification for this data.
## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
summary(snow_data_full)
```

```
##       Year           DOY         Sno_Height_M        site          
##  Min.   :2003   Min.   :  1.0   Min.   :-3.523   Length:12786      
##  1st Qu.:2008   1st Qu.: 92.0   1st Qu.: 0.350   Class :character  
##  Median :2012   Median :183.0   Median : 0.978   Mode  :character  
##  Mean   :2012   Mean   :183.1   Mean   : 0.981                     
##  3rd Qu.:2016   3rd Qu.:274.0   3rd Qu.: 1.520                     
##  Max.   :2021   Max.   :366.0   Max.   : 2.905                     
##                                 NA's   :4554
```


### Plot snow data


```r
snow_yearly <- snow_data_full %>%
  group_by(Year,site) %>%
  summarize(mean_height = mean(Sno_Height_M,na.rm=T))
```

```
## `summarise()` has grouped output by 'Year'. You can override using the `.groups` argument.
```

```r
ggplot(snow_yearly,aes(x=Year,y=mean_height,color=site)) + 
  geom_point() +
  ggthemes::theme_few() + 
  ggthemes::scale_color_few()
```

```
## Warning: Removed 2 rows containing missing values (geom_point).
```

<img src="03-Functions_files/figure-html/unnamed-chunk-6-1.png" width="672" />



# Assignment:


1. Extract the meteorological data URLs. Here we want you to use the `rvest` package to get the URLs for the `SASP forcing` and `SBSP_forcing` meteorological datasets.


### Extract meteorological data URLS from website


```r
# Extract data URLs for SASP forcing and SBSP forcing datasets

site_url2 <- 'https://snowstudies.org/archived-data/'

webpage2 <- read_html(site_url2)

links2 <- webpage2 %>% 
  html_nodes('a') %>% 
  .[grepl('forcing',.)] %>% 
  html_attr('href')

links2
```

```
## [1] "https://snowstudies.org/wp-content/uploads/2022/02/SBB_SASP_Forcing_Data.txt"
## [2] "https://snowstudies.org/wp-content/uploads/2022/02/SBB_SBSP_Forcing_Data.txt"
```



2. Download the meteorological data. Use the `download_file` and `str_split_fixed` commands to download the data and save it in your data folder. You can use a for loop or a map function. 


### Download data and save it within data folder 


```r
# Download the data and save within data folder 

splits2 <- str_split_fixed(links2,'/',8)

dataset2 <- splits2[,8]

filenames2 <- paste0('dataFunctions/',dataset2)

# Download data in a for loop 

# for (i in 1:length(filenames2)) {
#   download.file(links2[i], destfile = filenames2[i])
# }

# Utilize map2() to download data 

map2(links2,filenames2,download.file)
```

```
## [[1]]
## [1] 0
## 
## [[2]]
## [1] 0
```



3. Write a custom function to read in the data and append a site column to the data. 


### Write function to read in data and append site column to the data


```r
# This code grabs the variable names from the metadata pdf file

library(pdftools)
```

```
## Using poppler version 21.04.0
```

```r
headers <- pdf_text('https://snowstudies.org/wp-content/uploads/2022/02/Serially-Complete-Metadata-text08.pdf') %>%
  readr::read_lines(.) %>%
  trimws(.) %>%
  str_split_fixed(.,'\\.',2) %>%
  .[,2] %>%
  .[1:26] %>%
  str_trim(side = "left")
headers
```

```
##  [1] "year"                                                       
##  [2] "month"                                                      
##  [3] "day"                                                        
##  [4] "hour"                                                       
##  [5] "minute"                                                     
##  [6] "second"                                                     
##  [7] "precip [kg m-2 s-1]"                                        
##  [8] "sw down [W m-2]"                                            
##  [9] "lw down [W m-2]"                                            
## [10] "air temp [K]"                                               
## [11] "windspeed [m s-1]"                                          
## [12] "relative humidity [%]"                                      
## [13] "pressure [Pa]"                                              
## [14] "specific humidity [g g-1]"                                  
## [15] "calculated dewpoint temperature [K]"                        
## [16] "precip, WMO-corrected [kg m-2 s-1]"                         
## [17] "air temp, corrected with Kent et al. (1993) [K]"            
## [18] "air temp, corrected with Anderson and Baumgartner (1998)[K]"
## [19] "air temp, corrected with Nakamura and Mahrt (2005) [K]"     
## [20] "air temp, corrected with Huwald et al. (2009) [K]"          
## [21] "qc code precip"                                             
## [22] "qc code sw down"                                            
## [23] "qc code lw down"                                            
## [24] "qc code air temp"                                           
## [25] "qc code wind speed"                                         
## [26] "qc code relhum"
```

```r
# Write a function to read in the data and append site column 

forcing_files <- filenames2  
#   .[!grepl('24hr',.)]
# forcing_files


file <- forcing_files[1]


forcefile_reader <- function(file) {
  name2 = str_split_fixed(file, '_', 3)[, 2] 
    df <- read.csv(file, header = FALSE, sep = '') %>%
      select(V1, V2, V3, V7, V10) %>%
      rename(
        year = 1,
        month = 2,
        day = 3,
        precip = 4,
        airtemp = 5
      ) %>%
      mutate(site = name2)
    
}
```



4. Use the `map` function to read in both meteorological files. Display a summary of your tibble.


### Read in meterological data files with map function 


```r
# Use map function to read in meteorological files 

forcing_data_full <- map_dfr(forcing_files, forcefile_reader)

# # Display summary as tibble 

library(tibble)
forcing_tibble <-as_tibble(forcing_data_full)
summary(forcing_tibble)
```

```
##       year          month             day            precip         
##  Min.   :2003   Min.   : 1.000   Min.   : 1.00   Min.   :0.000e+00  
##  1st Qu.:2005   1st Qu.: 3.000   1st Qu.: 8.00   1st Qu.:0.000e+00  
##  Median :2007   Median : 6.000   Median :16.00   Median :0.000e+00  
##  Mean   :2007   Mean   : 6.472   Mean   :15.76   Mean   :3.838e-05  
##  3rd Qu.:2009   3rd Qu.: 9.000   3rd Qu.:23.00   3rd Qu.:0.000e+00  
##  Max.   :2011   Max.   :12.000   Max.   :31.00   Max.   :6.111e-03  
##     airtemp          site          
##  Min.   :242.1   Length:138336     
##  1st Qu.:265.8   Class :character  
##  Median :272.6   Mode  :character  
##  Mean   :272.6                     
##  3rd Qu.:279.7                     
##  Max.   :295.8
```



5. Make a line plot of mean temp by year by site (using the `air temp [K]` variable). Is there anything suspicious in the plot? Adjust your filtering if needed.


### Make line plot of mean temperature by year by site 


```r
library(ggthemes)

# Create data frame with mean air temperature by year by site 

temp_yearly <- forcing_data_full %>% 
  filter(!year %in% 2003) %>% 
  group_by(year,site) %>% 
  summarize(mean_yrtemp = mean(airtemp, na.rm=T))
```

```
## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.
```

```r
# Make line plot of mean temperature by year by site 

ggplot(temp_yearly, aes(x=year, y=mean_yrtemp, color=site))+
  geom_line(size=.75)+ 
  theme_few()+ 
  scale_color_manual(name="Site", values= c("#58A3EB","#58E8EB"))+ 
  labs(x='Year', y='Average Air Temperature (K)')
```

<img src="03-Functions_files/figure-html/unnamed-chunk-11-1.png" width="672" />


I filtered out 2003 given there were only records for two months out of the year. 


6. Write a function that makes line plots of monthly average temperature at each site for a given year. Use a for loop to make these plots for 2005 to 2010. Are monthly average temperatures at the Senator Beck Study Plot ever warmer than the Snow Angel Study Plot?
Hint: https://ggplot2.tidyverse.org/reference/print.ggplot.html


### Create function to make line plots of monthly average temperature at each site per year


```r
# Write function to make line plots of monthly average temps per site per year

line_plotter <- function(df, year) {
  temp_monthly <- df %>%
    group_by(year, month, site) %>%
    summarize(mean_motemp = mean(airtemp, na.rm = T)) %>%
    filter(i == year)
  
  print(
    ggplot(temp_monthly, aes(
      x = month, y = mean_motemp, color = site
    )) +
      geom_line(size = .75) +
      theme_few() +
      scale_color_manual(name = "Site", values = c("#762448", "#B1D374")) +
      labs(x="Month",y = 'Average Air Temperature (K)', title = i)+
      scale_x_continuous(
        breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
        labels = c(
          'January',
          'February',
          'March',
          'April',
          'May',
          'June',
          'July',
          'August',
          'September',
          'October',
          'November',
          'December'
        )
      ) +
      theme(
        axis.text.x = element_text(
          color = "black",
          size = 8,
          angle = 30,
          vjust = .8,
          hjust = .8
        )
      ))
}

# Use for loop to plot years 2005 to 2010

yrs = c(2005:2010)

for (i in yrs){
  line_plotter(forcing_data_full,year)
}
```

```
## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.
```

<img src="03-Functions_files/figure-html/unnamed-chunk-12-1.png" width="672" />

```
## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.
```

<img src="03-Functions_files/figure-html/unnamed-chunk-12-2.png" width="672" />

```
## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.
```

<img src="03-Functions_files/figure-html/unnamed-chunk-12-3.png" width="672" />

```
## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.
```

<img src="03-Functions_files/figure-html/unnamed-chunk-12-4.png" width="672" />

```
## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.
```

<img src="03-Functions_files/figure-html/unnamed-chunk-12-5.png" width="672" /><img src="03-Functions_files/figure-html/unnamed-chunk-12-6.png" width="672" />


Bonus: Make a plot of average daily precipitation by day of year (averaged across all available years).  


### Create a plot of average daily precip by day of year


```r
# Create plot with mean daily precip by day of year using ggplot 

# Add date column 
# Use lubridate:: y day for day of year 
precip_daily <- forcing_data_full %>%
  group_by(month, day, year, site) %>%
  summarize(mean_precip = mean(precip, na.rm = T)) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "/"))) %>%
  mutate(yday = yday(date)) %>%
  pivot_wider(names_from = site, values_from = mean_precip) %>%
  dplyr::select(-SBSP)%>% 
  group_by(yday)%>% 
  summarize(mean_dy_precip = mean(SASP))
```

```
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
```

```r
ggplot(precip_daily, aes(x = yday, y = mean_dy_precip)) +
  geom_point(shape = 1) +
  theme_few() +
  labs(x = 'Day of Year', y = expression('Average Precipitation'~ ('kg'*m^2*s^1)))+
  scale_x_continuous(breaks=c(1,90,180,270,360))
```

<img src="03-Functions_files/figure-html/unnamed-chunk-13-1.png" width="672" />

```r
# Create plot with mean daily precip by day of year using dygraphs
# This is not averaged across available years 

library(dygraphs)
library(xts)
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```
## 
## Attaching package: 'xts'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     first, last
```

```r
library(lubridate)

precip_daily2 <- forcing_data_full %>%
  group_by(month, day, year, site) %>%
  summarize(mean_precip = mean(precip, na.rm = T)) %>%
  mutate(date = as.Date(paste(year, month, day, sep = "/"))) %>%
  pivot_wider(names_from = site, values_from = mean_precip) %>%
  ungroup() %>%
  select(-SBSP, -month, -day, -year)
```

```
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
```

```r
precip_xts2 <- xts(precip_daily2 %>%
                     select(SASP), order.by = precip_daily2$date)

dygraph(precip_xts2, ylab = "Average Daily Precipitation") %>%
  dyOptions(fillGraph = TRUE, axisLabelFontSize=10)
```

```{=html}
<div id="htmlwidget-fe3ad449b8faf69ec6c3" style="width:672px;height:480px;" class="dygraphs html-widget"></div>
<script type="application/json" data-for="htmlwidget-fe3ad449b8faf69ec6c3">{"x":{"attrs":{"ylabel":"Average Daily Precipitation","labels":["day","SASP"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60,"drawAxis":true},"y":{"drawAxis":true}},"stackedGraph":false,"fillGraph":true,"fillAlpha":0.15,"stepPlot":false,"drawPoints":false,"pointSize":1,"drawGapEdgePoints":false,"connectSeparatedPoints":false,"strokeWidth":1,"strokeBorderColor":"white","colorValue":0.5,"colorSaturation":1,"includeZero":false,"drawAxesAtZero":false,"logscale":false,"axisTickSize":3,"axisLineColor":"black","axisLineWidth":0.3,"axisLabelColor":"black","axisLabelFontSize":10,"axisLabelWidth":60,"drawGrid":true,"gridLineWidth":0.3,"rightGap":5,"digitsAfterDecimal":2,"labelsKMB":false,"labelsKMG2":false,"labelsUTC":false,"maxNumberWidth":6,"animatedZooms":false,"mobileDisableYTouch":true,"disableZoom":false},"scale":"daily","annotations":[],"shadings":[],"events":[],"format":"date","data":[["2003-11-10T00:00:00.000Z","2003-11-11T00:00:00.000Z","2003-11-12T00:00:00.000Z","2003-11-13T00:00:00.000Z","2003-11-14T00:00:00.000Z","2003-11-15T00:00:00.000Z","2003-11-16T00:00:00.000Z","2003-11-17T00:00:00.000Z","2003-11-18T00:00:00.000Z","2003-11-19T00:00:00.000Z","2003-11-20T00:00:00.000Z","2003-11-21T00:00:00.000Z","2003-11-22T00:00:00.000Z","2003-11-23T00:00:00.000Z","2003-11-24T00:00:00.000Z","2003-11-25T00:00:00.000Z","2003-11-26T00:00:00.000Z","2003-11-27T00:00:00.000Z","2003-11-28T00:00:00.000Z","2003-11-29T00:00:00.000Z","2003-11-30T00:00:00.000Z","2003-12-01T00:00:00.000Z","2003-12-02T00:00:00.000Z","2003-12-03T00:00:00.000Z","2003-12-04T00:00:00.000Z","2003-12-05T00:00:00.000Z","2003-12-06T00:00:00.000Z","2003-12-07T00:00:00.000Z","2003-12-08T00:00:00.000Z","2003-12-09T00:00:00.000Z","2003-12-10T00:00:00.000Z","2003-12-11T00:00:00.000Z","2003-12-12T00:00:00.000Z","2003-12-13T00:00:00.000Z","2003-12-14T00:00:00.000Z","2003-12-15T00:00:00.000Z","2003-12-16T00:00:00.000Z","2003-12-17T00:00:00.000Z","2003-12-18T00:00:00.000Z","2003-12-19T00:00:00.000Z","2003-12-20T00:00:00.000Z","2003-12-21T00:00:00.000Z","2003-12-22T00:00:00.000Z","2003-12-23T00:00:00.000Z","2003-12-24T00:00:00.000Z","2003-12-25T00:00:00.000Z","2003-12-26T00:00:00.000Z","2003-12-27T00:00:00.000Z","2003-12-28T00:00:00.000Z","2003-12-29T00:00:00.000Z","2003-12-30T00:00:00.000Z","2003-12-31T00:00:00.000Z","2004-01-01T00:00:00.000Z","2004-01-02T00:00:00.000Z","2004-01-03T00:00:00.000Z","2004-01-04T00:00:00.000Z","2004-01-05T00:00:00.000Z","2004-01-06T00:00:00.000Z","2004-01-07T00:00:00.000Z","2004-01-08T00:00:00.000Z","2004-01-09T00:00:00.000Z","2004-01-10T00:00:00.000Z","2004-01-11T00:00:00.000Z","2004-01-12T00:00:00.000Z","2004-01-13T00:00:00.000Z","2004-01-14T00:00:00.000Z","2004-01-15T00:00:00.000Z","2004-01-16T00:00:00.000Z","2004-01-17T00:00:00.000Z","2004-01-18T00:00:00.000Z","2004-01-19T00:00:00.000Z","2004-01-20T00:00:00.000Z","2004-01-21T00:00:00.000Z","2004-01-22T00:00:00.000Z","2004-01-23T00:00:00.000Z","2004-01-24T00:00:00.000Z","2004-01-25T00:00:00.000Z","2004-01-26T00:00:00.000Z","2004-01-27T00:00:00.000Z","2004-01-28T00:00:00.000Z","2004-01-29T00:00:00.000Z","2004-01-30T00:00:00.000Z","2004-01-31T00:00:00.000Z","2004-02-01T00:00:00.000Z","2004-02-02T00:00:00.000Z","2004-02-03T00:00:00.000Z","2004-02-04T00:00:00.000Z","2004-02-05T00:00:00.000Z","2004-02-06T00:00:00.000Z","2004-02-07T00:00:00.000Z","2004-02-08T00:00:00.000Z","2004-02-09T00:00:00.000Z","2004-02-10T00:00:00.000Z","2004-02-11T00:00:00.000Z","2004-02-12T00:00:00.000Z","2004-02-13T00:00:00.000Z","2004-02-14T00:00:00.000Z","2004-02-15T00:00:00.000Z","2004-02-16T00:00:00.000Z","2004-02-17T00:00:00.000Z","2004-02-18T00:00:00.000Z","2004-02-19T00:00:00.000Z","2004-02-20T00:00:00.000Z","2004-02-21T00:00:00.000Z","2004-02-22T00:00:00.000Z","2004-02-23T00:00:00.000Z","2004-02-24T00:00:00.000Z","2004-02-25T00:00:00.000Z","2004-02-26T00:00:00.000Z","2004-02-27T00:00:00.000Z","2004-02-28T00:00:00.000Z","2004-02-29T00:00:00.000Z","2004-03-01T00:00:00.000Z","2004-03-02T00:00:00.000Z","2004-03-03T00:00:00.000Z","2004-03-04T00:00:00.000Z","2004-03-05T00:00:00.000Z","2004-03-06T00:00:00.000Z","2004-03-07T00:00:00.000Z","2004-03-08T00:00:00.000Z","2004-03-09T00:00:00.000Z","2004-03-10T00:00:00.000Z","2004-03-11T00:00:00.000Z","2004-03-12T00:00:00.000Z","2004-03-13T00:00:00.000Z","2004-03-14T00:00:00.000Z","2004-03-15T00:00:00.000Z","2004-03-16T00:00:00.000Z","2004-03-17T00:00:00.000Z","2004-03-18T00:00:00.000Z","2004-03-19T00:00:00.000Z","2004-03-20T00:00:00.000Z","2004-03-21T00:00:00.000Z","2004-03-22T00:00:00.000Z","2004-03-23T00:00:00.000Z","2004-03-24T00:00:00.000Z","2004-03-25T00:00:00.000Z","2004-03-26T00:00:00.000Z","2004-03-27T00:00:00.000Z","2004-03-28T00:00:00.000Z","2004-03-29T00:00:00.000Z","2004-03-30T00:00:00.000Z","2004-03-31T00:00:00.000Z","2004-04-01T00:00:00.000Z","2004-04-02T00:00:00.000Z","2004-04-03T00:00:00.000Z","2004-04-04T00:00:00.000Z","2004-04-05T00:00:00.000Z","2004-04-06T00:00:00.000Z","2004-04-07T00:00:00.000Z","2004-04-08T00:00:00.000Z","2004-04-09T00:00:00.000Z","2004-04-10T00:00:00.000Z","2004-04-11T00:00:00.000Z","2004-04-12T00:00:00.000Z","2004-04-13T00:00:00.000Z","2004-04-14T00:00:00.000Z","2004-04-15T00:00:00.000Z","2004-04-16T00:00:00.000Z","2004-04-17T00:00:00.000Z","2004-04-18T00:00:00.000Z","2004-04-19T00:00:00.000Z","2004-04-20T00:00:00.000Z","2004-04-21T00:00:00.000Z","2004-04-22T00:00:00.000Z","2004-04-23T00:00:00.000Z","2004-04-24T00:00:00.000Z","2004-04-25T00:00:00.000Z","2004-04-26T00:00:00.000Z","2004-04-27T00:00:00.000Z","2004-04-28T00:00:00.000Z","2004-04-29T00:00:00.000Z","2004-04-30T00:00:00.000Z","2004-05-01T00:00:00.000Z","2004-05-02T00:00:00.000Z","2004-05-03T00:00:00.000Z","2004-05-04T00:00:00.000Z","2004-05-05T00:00:00.000Z","2004-05-06T00:00:00.000Z","2004-05-07T00:00:00.000Z","2004-05-08T00:00:00.000Z","2004-05-09T00:00:00.000Z","2004-05-10T00:00:00.000Z","2004-05-11T00:00:00.000Z","2004-05-12T00:00:00.000Z","2004-05-13T00:00:00.000Z","2004-05-14T00:00:00.000Z","2004-05-15T00:00:00.000Z","2004-05-16T00:00:00.000Z","2004-05-17T00:00:00.000Z","2004-05-18T00:00:00.000Z","2004-05-19T00:00:00.000Z","2004-05-20T00:00:00.000Z","2004-05-21T00:00:00.000Z","2004-05-22T00:00:00.000Z","2004-05-23T00:00:00.000Z","2004-05-24T00:00:00.000Z","2004-05-25T00:00:00.000Z","2004-05-26T00:00:00.000Z","2004-05-27T00:00:00.000Z","2004-05-28T00:00:00.000Z","2004-05-29T00:00:00.000Z","2004-05-30T00:00:00.000Z","2004-05-31T00:00:00.000Z","2004-06-01T00:00:00.000Z","2004-06-02T00:00:00.000Z","2004-06-03T00:00:00.000Z","2004-06-04T00:00:00.000Z","2004-06-05T00:00:00.000Z","2004-06-06T00:00:00.000Z","2004-06-07T00:00:00.000Z","2004-06-08T00:00:00.000Z","2004-06-09T00:00:00.000Z","2004-06-10T00:00:00.000Z","2004-06-11T00:00:00.000Z","2004-06-12T00:00:00.000Z","2004-06-13T00:00:00.000Z","2004-06-14T00:00:00.000Z","2004-06-15T00:00:00.000Z","2004-06-16T00:00:00.000Z","2004-06-17T00:00:00.000Z","2004-06-18T00:00:00.000Z","2004-06-19T00:00:00.000Z","2004-06-20T00:00:00.000Z","2004-06-21T00:00:00.000Z","2004-06-22T00:00:00.000Z","2004-06-23T00:00:00.000Z","2004-06-24T00:00:00.000Z","2004-06-25T00:00:00.000Z","2004-06-26T00:00:00.000Z","2004-06-27T00:00:00.000Z","2004-06-28T00:00:00.000Z","2004-06-29T00:00:00.000Z","2004-06-30T00:00:00.000Z","2004-07-01T00:00:00.000Z","2004-07-02T00:00:00.000Z","2004-07-03T00:00:00.000Z","2004-07-04T00:00:00.000Z","2004-07-05T00:00:00.000Z","2004-07-06T00:00:00.000Z","2004-07-07T00:00:00.000Z","2004-07-08T00:00:00.000Z","2004-07-09T00:00:00.000Z","2004-07-10T00:00:00.000Z","2004-07-11T00:00:00.000Z","2004-07-12T00:00:00.000Z","2004-07-13T00:00:00.000Z","2004-07-14T00:00:00.000Z","2004-07-15T00:00:00.000Z","2004-07-16T00:00:00.000Z","2004-07-17T00:00:00.000Z","2004-07-18T00:00:00.000Z","2004-07-19T00:00:00.000Z","2004-07-20T00:00:00.000Z","2004-07-21T00:00:00.000Z","2004-07-22T00:00:00.000Z","2004-07-23T00:00:00.000Z","2004-07-24T00:00:00.000Z","2004-07-25T00:00:00.000Z","2004-07-26T00:00:00.000Z","2004-07-27T00:00:00.000Z","2004-07-28T00:00:00.000Z","2004-07-29T00:00:00.000Z","2004-07-30T00:00:00.000Z","2004-07-31T00:00:00.000Z","2004-08-01T00:00:00.000Z","2004-08-02T00:00:00.000Z","2004-08-03T00:00:00.000Z","2004-08-04T00:00:00.000Z","2004-08-05T00:00:00.000Z","2004-08-06T00:00:00.000Z","2004-08-07T00:00:00.000Z","2004-08-08T00:00:00.000Z","2004-08-09T00:00:00.000Z","2004-08-10T00:00:00.000Z","2004-08-11T00:00:00.000Z","2004-08-12T00:00:00.000Z","2004-08-13T00:00:00.000Z","2004-08-14T00:00:00.000Z","2004-08-15T00:00:00.000Z","2004-08-16T00:00:00.000Z","2004-08-17T00:00:00.000Z","2004-08-18T00:00:00.000Z","2004-08-19T00:00:00.000Z","2004-08-20T00:00:00.000Z","2004-08-21T00:00:00.000Z","2004-08-22T00:00:00.000Z","2004-08-23T00:00:00.000Z","2004-08-24T00:00:00.000Z","2004-08-25T00:00:00.000Z","2004-08-26T00:00:00.000Z","2004-08-27T00:00:00.000Z","2004-08-28T00:00:00.000Z","2004-08-29T00:00:00.000Z","2004-08-30T00:00:00.000Z","2004-08-31T00:00:00.000Z","2004-09-01T00:00:00.000Z","2004-09-02T00:00:00.000Z","2004-09-03T00:00:00.000Z","2004-09-04T00:00:00.000Z","2004-09-05T00:00:00.000Z","2004-09-06T00:00:00.000Z","2004-09-07T00:00:00.000Z","2004-09-08T00:00:00.000Z","2004-09-09T00:00:00.000Z","2004-09-10T00:00:00.000Z","2004-09-11T00:00:00.000Z","2004-09-12T00:00:00.000Z","2004-09-13T00:00:00.000Z","2004-09-14T00:00:00.000Z","2004-09-15T00:00:00.000Z","2004-09-16T00:00:00.000Z","2004-09-17T00:00:00.000Z","2004-09-18T00:00:00.000Z","2004-09-19T00:00:00.000Z","2004-09-20T00:00:00.000Z","2004-09-21T00:00:00.000Z","2004-09-22T00:00:00.000Z","2004-09-23T00:00:00.000Z","2004-09-24T00:00:00.000Z","2004-09-25T00:00:00.000Z","2004-09-26T00:00:00.000Z","2004-09-27T00:00:00.000Z","2004-09-28T00:00:00.000Z","2004-09-29T00:00:00.000Z","2004-09-30T00:00:00.000Z","2004-10-01T00:00:00.000Z","2004-10-02T00:00:00.000Z","2004-10-03T00:00:00.000Z","2004-10-04T00:00:00.000Z","2004-10-05T00:00:00.000Z","2004-10-06T00:00:00.000Z","2004-10-07T00:00:00.000Z","2004-10-08T00:00:00.000Z","2004-10-09T00:00:00.000Z","2004-10-10T00:00:00.000Z","2004-10-11T00:00:00.000Z","2004-10-12T00:00:00.000Z","2004-10-13T00:00:00.000Z","2004-10-14T00:00:00.000Z","2004-10-15T00:00:00.000Z","2004-10-16T00:00:00.000Z","2004-10-17T00:00:00.000Z","2004-10-18T00:00:00.000Z","2004-10-19T00:00:00.000Z","2004-10-20T00:00:00.000Z","2004-10-21T00:00:00.000Z","2004-10-22T00:00:00.000Z","2004-10-23T00:00:00.000Z","2004-10-24T00:00:00.000Z","2004-10-25T00:00:00.000Z","2004-10-26T00:00:00.000Z","2004-10-27T00:00:00.000Z","2004-10-28T00:00:00.000Z","2004-10-29T00:00:00.000Z","2004-10-30T00:00:00.000Z","2004-10-31T00:00:00.000Z","2004-11-01T00:00:00.000Z","2004-11-02T00:00:00.000Z","2004-11-03T00:00:00.000Z","2004-11-04T00:00:00.000Z","2004-11-05T00:00:00.000Z","2004-11-06T00:00:00.000Z","2004-11-07T00:00:00.000Z","2004-11-08T00:00:00.000Z","2004-11-09T00:00:00.000Z","2004-11-10T00:00:00.000Z","2004-11-11T00:00:00.000Z","2004-11-12T00:00:00.000Z","2004-11-13T00:00:00.000Z","2004-11-14T00:00:00.000Z","2004-11-15T00:00:00.000Z","2004-11-16T00:00:00.000Z","2004-11-17T00:00:00.000Z","2004-11-18T00:00:00.000Z","2004-11-19T00:00:00.000Z","2004-11-20T00:00:00.000Z","2004-11-21T00:00:00.000Z","2004-11-22T00:00:00.000Z","2004-11-23T00:00:00.000Z","2004-11-24T00:00:00.000Z","2004-11-25T00:00:00.000Z","2004-11-26T00:00:00.000Z","2004-11-27T00:00:00.000Z","2004-11-28T00:00:00.000Z","2004-11-29T00:00:00.000Z","2004-11-30T00:00:00.000Z","2004-12-01T00:00:00.000Z","2004-12-02T00:00:00.000Z","2004-12-03T00:00:00.000Z","2004-12-04T00:00:00.000Z","2004-12-05T00:00:00.000Z","2004-12-06T00:00:00.000Z","2004-12-07T00:00:00.000Z","2004-12-08T00:00:00.000Z","2004-12-09T00:00:00.000Z","2004-12-10T00:00:00.000Z","2004-12-11T00:00:00.000Z","2004-12-12T00:00:00.000Z","2004-12-13T00:00:00.000Z","2004-12-14T00:00:00.000Z","2004-12-15T00:00:00.000Z","2004-12-16T00:00:00.000Z","2004-12-17T00:00:00.000Z","2004-12-18T00:00:00.000Z","2004-12-19T00:00:00.000Z","2004-12-20T00:00:00.000Z","2004-12-21T00:00:00.000Z","2004-12-22T00:00:00.000Z","2004-12-23T00:00:00.000Z","2004-12-24T00:00:00.000Z","2004-12-25T00:00:00.000Z","2004-12-26T00:00:00.000Z","2004-12-27T00:00:00.000Z","2004-12-28T00:00:00.000Z","2004-12-29T00:00:00.000Z","2004-12-30T00:00:00.000Z","2004-12-31T00:00:00.000Z","2005-01-01T00:00:00.000Z","2005-01-02T00:00:00.000Z","2005-01-03T00:00:00.000Z","2005-01-04T00:00:00.000Z","2005-01-05T00:00:00.000Z","2005-01-06T00:00:00.000Z","2005-01-07T00:00:00.000Z","2005-01-08T00:00:00.000Z","2005-01-09T00:00:00.000Z","2005-01-10T00:00:00.000Z","2005-01-11T00:00:00.000Z","2005-01-12T00:00:00.000Z","2005-01-13T00:00:00.000Z","2005-01-14T00:00:00.000Z","2005-01-15T00:00:00.000Z","2005-01-16T00:00:00.000Z","2005-01-17T00:00:00.000Z","2005-01-18T00:00:00.000Z","2005-01-19T00:00:00.000Z","2005-01-20T00:00:00.000Z","2005-01-21T00:00:00.000Z","2005-01-22T00:00:00.000Z","2005-01-23T00:00:00.000Z","2005-01-24T00:00:00.000Z","2005-01-25T00:00:00.000Z","2005-01-26T00:00:00.000Z","2005-01-27T00:00:00.000Z","2005-01-28T00:00:00.000Z","2005-01-29T00:00:00.000Z","2005-01-30T00:00:00.000Z","2005-01-31T00:00:00.000Z","2005-02-01T00:00:00.000Z","2005-02-02T00:00:00.000Z","2005-02-03T00:00:00.000Z","2005-02-04T00:00:00.000Z","2005-02-05T00:00:00.000Z","2005-02-06T00:00:00.000Z","2005-02-07T00:00:00.000Z","2005-02-08T00:00:00.000Z","2005-02-09T00:00:00.000Z","2005-02-10T00:00:00.000Z","2005-02-11T00:00:00.000Z","2005-02-12T00:00:00.000Z","2005-02-13T00:00:00.000Z","2005-02-14T00:00:00.000Z","2005-02-15T00:00:00.000Z","2005-02-16T00:00:00.000Z","2005-02-17T00:00:00.000Z","2005-02-18T00:00:00.000Z","2005-02-19T00:00:00.000Z","2005-02-20T00:00:00.000Z","2005-02-21T00:00:00.000Z","2005-02-22T00:00:00.000Z","2005-02-23T00:00:00.000Z","2005-02-24T00:00:00.000Z","2005-02-25T00:00:00.000Z","2005-02-26T00:00:00.000Z","2005-02-27T00:00:00.000Z","2005-02-28T00:00:00.000Z","2005-03-01T00:00:00.000Z","2005-03-02T00:00:00.000Z","2005-03-03T00:00:00.000Z","2005-03-04T00:00:00.000Z","2005-03-05T00:00:00.000Z","2005-03-06T00:00:00.000Z","2005-03-07T00:00:00.000Z","2005-03-08T00:00:00.000Z","2005-03-09T00:00:00.000Z","2005-03-10T00:00:00.000Z","2005-03-11T00:00:00.000Z","2005-03-12T00:00:00.000Z","2005-03-13T00:00:00.000Z","2005-03-14T00:00:00.000Z","2005-03-15T00:00:00.000Z","2005-03-16T00:00:00.000Z","2005-03-17T00:00:00.000Z","2005-03-18T00:00:00.000Z","2005-03-19T00:00:00.000Z","2005-03-20T00:00:00.000Z","2005-03-21T00:00:00.000Z","2005-03-22T00:00:00.000Z","2005-03-23T00:00:00.000Z","2005-03-24T00:00:00.000Z","2005-03-25T00:00:00.000Z","2005-03-26T00:00:00.000Z","2005-03-27T00:00:00.000Z","2005-03-28T00:00:00.000Z","2005-03-29T00:00:00.000Z","2005-03-30T00:00:00.000Z","2005-03-31T00:00:00.000Z","2005-04-01T00:00:00.000Z","2005-04-02T00:00:00.000Z","2005-04-03T00:00:00.000Z","2005-04-04T00:00:00.000Z","2005-04-05T00:00:00.000Z","2005-04-06T00:00:00.000Z","2005-04-07T00:00:00.000Z","2005-04-08T00:00:00.000Z","2005-04-09T00:00:00.000Z","2005-04-10T00:00:00.000Z","2005-04-11T00:00:00.000Z","2005-04-12T00:00:00.000Z","2005-04-13T00:00:00.000Z","2005-04-14T00:00:00.000Z","2005-04-15T00:00:00.000Z","2005-04-16T00:00:00.000Z","2005-04-17T00:00:00.000Z","2005-04-18T00:00:00.000Z","2005-04-19T00:00:00.000Z","2005-04-20T00:00:00.000Z","2005-04-21T00:00:00.000Z","2005-04-22T00:00:00.000Z","2005-04-23T00:00:00.000Z","2005-04-24T00:00:00.000Z","2005-04-25T00:00:00.000Z","2005-04-26T00:00:00.000Z","2005-04-27T00:00:00.000Z","2005-04-28T00:00:00.000Z","2005-04-29T00:00:00.000Z","2005-04-30T00:00:00.000Z","2005-05-01T00:00:00.000Z","2005-05-02T00:00:00.000Z","2005-05-03T00:00:00.000Z","2005-05-04T00:00:00.000Z","2005-05-05T00:00:00.000Z","2005-05-06T00:00:00.000Z","2005-05-07T00:00:00.000Z","2005-05-08T00:00:00.000Z","2005-05-09T00:00:00.000Z","2005-05-10T00:00:00.000Z","2005-05-11T00:00:00.000Z","2005-05-12T00:00:00.000Z","2005-05-13T00:00:00.000Z","2005-05-14T00:00:00.000Z","2005-05-15T00:00:00.000Z","2005-05-16T00:00:00.000Z","2005-05-17T00:00:00.000Z","2005-05-18T00:00:00.000Z","2005-05-19T00:00:00.000Z","2005-05-20T00:00:00.000Z","2005-05-21T00:00:00.000Z","2005-05-22T00:00:00.000Z","2005-05-23T00:00:00.000Z","2005-05-24T00:00:00.000Z","2005-05-25T00:00:00.000Z","2005-05-26T00:00:00.000Z","2005-05-27T00:00:00.000Z","2005-05-28T00:00:00.000Z","2005-05-29T00:00:00.000Z","2005-05-30T00:00:00.000Z","2005-05-31T00:00:00.000Z","2005-06-01T00:00:00.000Z","2005-06-02T00:00:00.000Z","2005-06-03T00:00:00.000Z","2005-06-04T00:00:00.000Z","2005-06-05T00:00:00.000Z","2005-06-06T00:00:00.000Z","2005-06-07T00:00:00.000Z","2005-06-08T00:00:00.000Z","2005-06-09T00:00:00.000Z","2005-06-10T00:00:00.000Z","2005-06-11T00:00:00.000Z","2005-06-12T00:00:00.000Z","2005-06-13T00:00:00.000Z","2005-06-14T00:00:00.000Z","2005-06-15T00:00:00.000Z","2005-06-16T00:00:00.000Z","2005-06-17T00:00:00.000Z","2005-06-18T00:00:00.000Z","2005-06-19T00:00:00.000Z","2005-06-20T00:00:00.000Z","2005-06-21T00:00:00.000Z","2005-06-22T00:00:00.000Z","2005-06-23T00:00:00.000Z","2005-06-24T00:00:00.000Z","2005-06-25T00:00:00.000Z","2005-06-26T00:00:00.000Z","2005-06-27T00:00:00.000Z","2005-06-28T00:00:00.000Z","2005-06-29T00:00:00.000Z","2005-06-30T00:00:00.000Z","2005-07-01T00:00:00.000Z","2005-07-02T00:00:00.000Z","2005-07-03T00:00:00.000Z","2005-07-04T00:00:00.000Z","2005-07-05T00:00:00.000Z","2005-07-06T00:00:00.000Z","2005-07-07T00:00:00.000Z","2005-07-08T00:00:00.000Z","2005-07-09T00:00:00.000Z","2005-07-10T00:00:00.000Z","2005-07-11T00:00:00.000Z","2005-07-12T00:00:00.000Z","2005-07-13T00:00:00.000Z","2005-07-14T00:00:00.000Z","2005-07-15T00:00:00.000Z","2005-07-16T00:00:00.000Z","2005-07-17T00:00:00.000Z","2005-07-18T00:00:00.000Z","2005-07-19T00:00:00.000Z","2005-07-20T00:00:00.000Z","2005-07-21T00:00:00.000Z","2005-07-22T00:00:00.000Z","2005-07-23T00:00:00.000Z","2005-07-24T00:00:00.000Z","2005-07-25T00:00:00.000Z","2005-07-26T00:00:00.000Z","2005-07-27T00:00:00.000Z","2005-07-28T00:00:00.000Z","2005-07-29T00:00:00.000Z","2005-07-30T00:00:00.000Z","2005-07-31T00:00:00.000Z","2005-08-01T00:00:00.000Z","2005-08-02T00:00:00.000Z","2005-08-03T00:00:00.000Z","2005-08-04T00:00:00.000Z","2005-08-05T00:00:00.000Z","2005-08-06T00:00:00.000Z","2005-08-07T00:00:00.000Z","2005-08-08T00:00:00.000Z","2005-08-09T00:00:00.000Z","2005-08-10T00:00:00.000Z","2005-08-11T00:00:00.000Z","2005-08-12T00:00:00.000Z","2005-08-13T00:00:00.000Z","2005-08-14T00:00:00.000Z","2005-08-15T00:00:00.000Z","2005-08-16T00:00:00.000Z","2005-08-17T00:00:00.000Z","2005-08-18T00:00:00.000Z","2005-08-19T00:00:00.000Z","2005-08-20T00:00:00.000Z","2005-08-21T00:00:00.000Z","2005-08-22T00:00:00.000Z","2005-08-23T00:00:00.000Z","2005-08-24T00:00:00.000Z","2005-08-25T00:00:00.000Z","2005-08-26T00:00:00.000Z","2005-08-27T00:00:00.000Z","2005-08-28T00:00:00.000Z","2005-08-29T00:00:00.000Z","2005-08-30T00:00:00.000Z","2005-08-31T00:00:00.000Z","2005-09-01T00:00:00.000Z","2005-09-02T00:00:00.000Z","2005-09-03T00:00:00.000Z","2005-09-04T00:00:00.000Z","2005-09-05T00:00:00.000Z","2005-09-06T00:00:00.000Z","2005-09-07T00:00:00.000Z","2005-09-08T00:00:00.000Z","2005-09-09T00:00:00.000Z","2005-09-10T00:00:00.000Z","2005-09-11T00:00:00.000Z","2005-09-12T00:00:00.000Z","2005-09-13T00:00:00.000Z","2005-09-14T00:00:00.000Z","2005-09-15T00:00:00.000Z","2005-09-16T00:00:00.000Z","2005-09-17T00:00:00.000Z","2005-09-18T00:00:00.000Z","2005-09-19T00:00:00.000Z","2005-09-20T00:00:00.000Z","2005-09-21T00:00:00.000Z","2005-09-22T00:00:00.000Z","2005-09-23T00:00:00.000Z","2005-09-24T00:00:00.000Z","2005-09-25T00:00:00.000Z","2005-09-26T00:00:00.000Z","2005-09-27T00:00:00.000Z","2005-09-28T00:00:00.000Z","2005-09-29T00:00:00.000Z","2005-09-30T00:00:00.000Z","2005-10-01T00:00:00.000Z","2005-10-02T00:00:00.000Z","2005-10-03T00:00:00.000Z","2005-10-04T00:00:00.000Z","2005-10-05T00:00:00.000Z","2005-10-06T00:00:00.000Z","2005-10-07T00:00:00.000Z","2005-10-08T00:00:00.000Z","2005-10-09T00:00:00.000Z","2005-10-10T00:00:00.000Z","2005-10-11T00:00:00.000Z","2005-10-12T00:00:00.000Z","2005-10-13T00:00:00.000Z","2005-10-14T00:00:00.000Z","2005-10-15T00:00:00.000Z","2005-10-16T00:00:00.000Z","2005-10-17T00:00:00.000Z","2005-10-18T00:00:00.000Z","2005-10-19T00:00:00.000Z","2005-10-20T00:00:00.000Z","2005-10-21T00:00:00.000Z","2005-10-22T00:00:00.000Z","2005-10-23T00:00:00.000Z","2005-10-24T00:00:00.000Z","2005-10-25T00:00:00.000Z","2005-10-26T00:00:00.000Z","2005-10-27T00:00:00.000Z","2005-10-28T00:00:00.000Z","2005-10-29T00:00:00.000Z","2005-10-30T00:00:00.000Z","2005-10-31T00:00:00.000Z","2005-11-01T00:00:00.000Z","2005-11-02T00:00:00.000Z","2005-11-03T00:00:00.000Z","2005-11-04T00:00:00.000Z","2005-11-05T00:00:00.000Z","2005-11-06T00:00:00.000Z","2005-11-07T00:00:00.000Z","2005-11-08T00:00:00.000Z","2005-11-09T00:00:00.000Z","2005-11-10T00:00:00.000Z","2005-11-11T00:00:00.000Z","2005-11-12T00:00:00.000Z","2005-11-13T00:00:00.000Z","2005-11-14T00:00:00.000Z","2005-11-15T00:00:00.000Z","2005-11-16T00:00:00.000Z","2005-11-17T00:00:00.000Z","2005-11-18T00:00:00.000Z","2005-11-19T00:00:00.000Z","2005-11-20T00:00:00.000Z","2005-11-21T00:00:00.000Z","2005-11-22T00:00:00.000Z","2005-11-23T00:00:00.000Z","2005-11-24T00:00:00.000Z","2005-11-25T00:00:00.000Z","2005-11-26T00:00:00.000Z","2005-11-27T00:00:00.000Z","2005-11-28T00:00:00.000Z","2005-11-29T00:00:00.000Z","2005-11-30T00:00:00.000Z","2005-12-01T00:00:00.000Z","2005-12-02T00:00:00.000Z","2005-12-03T00:00:00.000Z","2005-12-04T00:00:00.000Z","2005-12-05T00:00:00.000Z","2005-12-06T00:00:00.000Z","2005-12-07T00:00:00.000Z","2005-12-08T00:00:00.000Z","2005-12-09T00:00:00.000Z","2005-12-10T00:00:00.000Z","2005-12-11T00:00:00.000Z","2005-12-12T00:00:00.000Z","2005-12-13T00:00:00.000Z","2005-12-14T00:00:00.000Z","2005-12-15T00:00:00.000Z","2005-12-16T00:00:00.000Z","2005-12-17T00:00:00.000Z","2005-12-18T00:00:00.000Z","2005-12-19T00:00:00.000Z","2005-12-20T00:00:00.000Z","2005-12-21T00:00:00.000Z","2005-12-22T00:00:00.000Z","2005-12-23T00:00:00.000Z","2005-12-24T00:00:00.000Z","2005-12-25T00:00:00.000Z","2005-12-26T00:00:00.000Z","2005-12-27T00:00:00.000Z","2005-12-28T00:00:00.000Z","2005-12-29T00:00:00.000Z","2005-12-30T00:00:00.000Z","2005-12-31T00:00:00.000Z","2006-01-01T00:00:00.000Z","2006-01-02T00:00:00.000Z","2006-01-03T00:00:00.000Z","2006-01-04T00:00:00.000Z","2006-01-05T00:00:00.000Z","2006-01-06T00:00:00.000Z","2006-01-07T00:00:00.000Z","2006-01-08T00:00:00.000Z","2006-01-09T00:00:00.000Z","2006-01-10T00:00:00.000Z","2006-01-11T00:00:00.000Z","2006-01-12T00:00:00.000Z","2006-01-13T00:00:00.000Z","2006-01-14T00:00:00.000Z","2006-01-15T00:00:00.000Z","2006-01-16T00:00:00.000Z","2006-01-17T00:00:00.000Z","2006-01-18T00:00:00.000Z","2006-01-19T00:00:00.000Z","2006-01-20T00:00:00.000Z","2006-01-21T00:00:00.000Z","2006-01-22T00:00:00.000Z","2006-01-23T00:00:00.000Z","2006-01-24T00:00:00.000Z","2006-01-25T00:00:00.000Z","2006-01-26T00:00:00.000Z","2006-01-27T00:00:00.000Z","2006-01-28T00:00:00.000Z","2006-01-29T00:00:00.000Z","2006-01-30T00:00:00.000Z","2006-01-31T00:00:00.000Z","2006-02-01T00:00:00.000Z","2006-02-02T00:00:00.000Z","2006-02-03T00:00:00.000Z","2006-02-04T00:00:00.000Z","2006-02-05T00:00:00.000Z","2006-02-06T00:00:00.000Z","2006-02-07T00:00:00.000Z","2006-02-08T00:00:00.000Z","2006-02-09T00:00:00.000Z","2006-02-10T00:00:00.000Z","2006-02-11T00:00:00.000Z","2006-02-12T00:00:00.000Z","2006-02-13T00:00:00.000Z","2006-02-14T00:00:00.000Z","2006-02-15T00:00:00.000Z","2006-02-16T00:00:00.000Z","2006-02-17T00:00:00.000Z","2006-02-18T00:00:00.000Z","2006-02-19T00:00:00.000Z","2006-02-20T00:00:00.000Z","2006-02-21T00:00:00.000Z","2006-02-22T00:00:00.000Z","2006-02-23T00:00:00.000Z","2006-02-24T00:00:00.000Z","2006-02-25T00:00:00.000Z","2006-02-26T00:00:00.000Z","2006-02-27T00:00:00.000Z","2006-02-28T00:00:00.000Z","2006-03-01T00:00:00.000Z","2006-03-02T00:00:00.000Z","2006-03-03T00:00:00.000Z","2006-03-04T00:00:00.000Z","2006-03-05T00:00:00.000Z","2006-03-06T00:00:00.000Z","2006-03-07T00:00:00.000Z","2006-03-08T00:00:00.000Z","2006-03-09T00:00:00.000Z","2006-03-10T00:00:00.000Z","2006-03-11T00:00:00.000Z","2006-03-12T00:00:00.000Z","2006-03-13T00:00:00.000Z","2006-03-14T00:00:00.000Z","2006-03-15T00:00:00.000Z","2006-03-16T00:00:00.000Z","2006-03-17T00:00:00.000Z","2006-03-18T00:00:00.000Z","2006-03-19T00:00:00.000Z","2006-03-20T00:00:00.000Z","2006-03-21T00:00:00.000Z","2006-03-22T00:00:00.000Z","2006-03-23T00:00:00.000Z","2006-03-24T00:00:00.000Z","2006-03-25T00:00:00.000Z","2006-03-26T00:00:00.000Z","2006-03-27T00:00:00.000Z","2006-03-28T00:00:00.000Z","2006-03-29T00:00:00.000Z","2006-03-30T00:00:00.000Z","2006-03-31T00:00:00.000Z","2006-04-01T00:00:00.000Z","2006-04-02T00:00:00.000Z","2006-04-03T00:00:00.000Z","2006-04-04T00:00:00.000Z","2006-04-05T00:00:00.000Z","2006-04-06T00:00:00.000Z","2006-04-07T00:00:00.000Z","2006-04-08T00:00:00.000Z","2006-04-09T00:00:00.000Z","2006-04-10T00:00:00.000Z","2006-04-11T00:00:00.000Z","2006-04-12T00:00:00.000Z","2006-04-13T00:00:00.000Z","2006-04-14T00:00:00.000Z","2006-04-15T00:00:00.000Z","2006-04-16T00:00:00.000Z","2006-04-17T00:00:00.000Z","2006-04-18T00:00:00.000Z","2006-04-19T00:00:00.000Z","2006-04-20T00:00:00.000Z","2006-04-21T00:00:00.000Z","2006-04-22T00:00:00.000Z","2006-04-23T00:00:00.000Z","2006-04-24T00:00:00.000Z","2006-04-25T00:00:00.000Z","2006-04-26T00:00:00.000Z","2006-04-27T00:00:00.000Z","2006-04-28T00:00:00.000Z","2006-04-29T00:00:00.000Z","2006-04-30T00:00:00.000Z","2006-05-01T00:00:00.000Z","2006-05-02T00:00:00.000Z","2006-05-03T00:00:00.000Z","2006-05-04T00:00:00.000Z","2006-05-05T00:00:00.000Z","2006-05-06T00:00:00.000Z","2006-05-07T00:00:00.000Z","2006-05-08T00:00:00.000Z","2006-05-09T00:00:00.000Z","2006-05-10T00:00:00.000Z","2006-05-11T00:00:00.000Z","2006-05-12T00:00:00.000Z","2006-05-13T00:00:00.000Z","2006-05-14T00:00:00.000Z","2006-05-15T00:00:00.000Z","2006-05-16T00:00:00.000Z","2006-05-17T00:00:00.000Z","2006-05-18T00:00:00.000Z","2006-05-19T00:00:00.000Z","2006-05-20T00:00:00.000Z","2006-05-21T00:00:00.000Z","2006-05-22T00:00:00.000Z","2006-05-23T00:00:00.000Z","2006-05-24T00:00:00.000Z","2006-05-25T00:00:00.000Z","2006-05-26T00:00:00.000Z","2006-05-27T00:00:00.000Z","2006-05-28T00:00:00.000Z","2006-05-29T00:00:00.000Z","2006-05-30T00:00:00.000Z","2006-05-31T00:00:00.000Z","2006-06-01T00:00:00.000Z","2006-06-02T00:00:00.000Z","2006-06-03T00:00:00.000Z","2006-06-04T00:00:00.000Z","2006-06-05T00:00:00.000Z","2006-06-06T00:00:00.000Z","2006-06-07T00:00:00.000Z","2006-06-08T00:00:00.000Z","2006-06-09T00:00:00.000Z","2006-06-10T00:00:00.000Z","2006-06-11T00:00:00.000Z","2006-06-12T00:00:00.000Z","2006-06-13T00:00:00.000Z","2006-06-14T00:00:00.000Z","2006-06-15T00:00:00.000Z","2006-06-16T00:00:00.000Z","2006-06-17T00:00:00.000Z","2006-06-18T00:00:00.000Z","2006-06-19T00:00:00.000Z","2006-06-20T00:00:00.000Z","2006-06-21T00:00:00.000Z","2006-06-22T00:00:00.000Z","2006-06-23T00:00:00.000Z","2006-06-24T00:00:00.000Z","2006-06-25T00:00:00.000Z","2006-06-26T00:00:00.000Z","2006-06-27T00:00:00.000Z","2006-06-28T00:00:00.000Z","2006-06-29T00:00:00.000Z","2006-06-30T00:00:00.000Z","2006-07-01T00:00:00.000Z","2006-07-02T00:00:00.000Z","2006-07-03T00:00:00.000Z","2006-07-04T00:00:00.000Z","2006-07-05T00:00:00.000Z","2006-07-06T00:00:00.000Z","2006-07-07T00:00:00.000Z","2006-07-08T00:00:00.000Z","2006-07-09T00:00:00.000Z","2006-07-10T00:00:00.000Z","2006-07-11T00:00:00.000Z","2006-07-12T00:00:00.000Z","2006-07-13T00:00:00.000Z","2006-07-14T00:00:00.000Z","2006-07-15T00:00:00.000Z","2006-07-16T00:00:00.000Z","2006-07-17T00:00:00.000Z","2006-07-18T00:00:00.000Z","2006-07-19T00:00:00.000Z","2006-07-20T00:00:00.000Z","2006-07-21T00:00:00.000Z","2006-07-22T00:00:00.000Z","2006-07-23T00:00:00.000Z","2006-07-24T00:00:00.000Z","2006-07-25T00:00:00.000Z","2006-07-26T00:00:00.000Z","2006-07-27T00:00:00.000Z","2006-07-28T00:00:00.000Z","2006-07-29T00:00:00.000Z","2006-07-30T00:00:00.000Z","2006-07-31T00:00:00.000Z","2006-08-01T00:00:00.000Z","2006-08-02T00:00:00.000Z","2006-08-03T00:00:00.000Z","2006-08-04T00:00:00.000Z","2006-08-05T00:00:00.000Z","2006-08-06T00:00:00.000Z","2006-08-07T00:00:00.000Z","2006-08-08T00:00:00.000Z","2006-08-09T00:00:00.000Z","2006-08-10T00:00:00.000Z","2006-08-11T00:00:00.000Z","2006-08-12T00:00:00.000Z","2006-08-13T00:00:00.000Z","2006-08-14T00:00:00.000Z","2006-08-15T00:00:00.000Z","2006-08-16T00:00:00.000Z","2006-08-17T00:00:00.000Z","2006-08-18T00:00:00.000Z","2006-08-19T00:00:00.000Z","2006-08-20T00:00:00.000Z","2006-08-21T00:00:00.000Z","2006-08-22T00:00:00.000Z","2006-08-23T00:00:00.000Z","2006-08-24T00:00:00.000Z","2006-08-25T00:00:00.000Z","2006-08-26T00:00:00.000Z","2006-08-27T00:00:00.000Z","2006-08-28T00:00:00.000Z","2006-08-29T00:00:00.000Z","2006-08-30T00:00:00.000Z","2006-08-31T00:00:00.000Z","2006-09-01T00:00:00.000Z","2006-09-02T00:00:00.000Z","2006-09-03T00:00:00.000Z","2006-09-04T00:00:00.000Z","2006-09-05T00:00:00.000Z","2006-09-06T00:00:00.000Z","2006-09-07T00:00:00.000Z","2006-09-08T00:00:00.000Z","2006-09-09T00:00:00.000Z","2006-09-10T00:00:00.000Z","2006-09-11T00:00:00.000Z","2006-09-12T00:00:00.000Z","2006-09-13T00:00:00.000Z","2006-09-14T00:00:00.000Z","2006-09-15T00:00:00.000Z","2006-09-16T00:00:00.000Z","2006-09-17T00:00:00.000Z","2006-09-18T00:00:00.000Z","2006-09-19T00:00:00.000Z","2006-09-20T00:00:00.000Z","2006-09-21T00:00:00.000Z","2006-09-22T00:00:00.000Z","2006-09-23T00:00:00.000Z","2006-09-24T00:00:00.000Z","2006-09-25T00:00:00.000Z","2006-09-26T00:00:00.000Z","2006-09-27T00:00:00.000Z","2006-09-28T00:00:00.000Z","2006-09-29T00:00:00.000Z","2006-09-30T00:00:00.000Z","2006-10-01T00:00:00.000Z","2006-10-02T00:00:00.000Z","2006-10-03T00:00:00.000Z","2006-10-04T00:00:00.000Z","2006-10-05T00:00:00.000Z","2006-10-06T00:00:00.000Z","2006-10-07T00:00:00.000Z","2006-10-08T00:00:00.000Z","2006-10-09T00:00:00.000Z","2006-10-10T00:00:00.000Z","2006-10-11T00:00:00.000Z","2006-10-12T00:00:00.000Z","2006-10-13T00:00:00.000Z","2006-10-14T00:00:00.000Z","2006-10-15T00:00:00.000Z","2006-10-16T00:00:00.000Z","2006-10-17T00:00:00.000Z","2006-10-18T00:00:00.000Z","2006-10-19T00:00:00.000Z","2006-10-20T00:00:00.000Z","2006-10-21T00:00:00.000Z","2006-10-22T00:00:00.000Z","2006-10-23T00:00:00.000Z","2006-10-24T00:00:00.000Z","2006-10-25T00:00:00.000Z","2006-10-26T00:00:00.000Z","2006-10-27T00:00:00.000Z","2006-10-28T00:00:00.000Z","2006-10-29T00:00:00.000Z","2006-10-30T00:00:00.000Z","2006-10-31T00:00:00.000Z","2006-11-01T00:00:00.000Z","2006-11-02T00:00:00.000Z","2006-11-03T00:00:00.000Z","2006-11-04T00:00:00.000Z","2006-11-05T00:00:00.000Z","2006-11-06T00:00:00.000Z","2006-11-07T00:00:00.000Z","2006-11-08T00:00:00.000Z","2006-11-09T00:00:00.000Z","2006-11-10T00:00:00.000Z","2006-11-11T00:00:00.000Z","2006-11-12T00:00:00.000Z","2006-11-13T00:00:00.000Z","2006-11-14T00:00:00.000Z","2006-11-15T00:00:00.000Z","2006-11-16T00:00:00.000Z","2006-11-17T00:00:00.000Z","2006-11-18T00:00:00.000Z","2006-11-19T00:00:00.000Z","2006-11-20T00:00:00.000Z","2006-11-21T00:00:00.000Z","2006-11-22T00:00:00.000Z","2006-11-23T00:00:00.000Z","2006-11-24T00:00:00.000Z","2006-11-25T00:00:00.000Z","2006-11-26T00:00:00.000Z","2006-11-27T00:00:00.000Z","2006-11-28T00:00:00.000Z","2006-11-29T00:00:00.000Z","2006-11-30T00:00:00.000Z","2006-12-01T00:00:00.000Z","2006-12-02T00:00:00.000Z","2006-12-03T00:00:00.000Z","2006-12-04T00:00:00.000Z","2006-12-05T00:00:00.000Z","2006-12-06T00:00:00.000Z","2006-12-07T00:00:00.000Z","2006-12-08T00:00:00.000Z","2006-12-09T00:00:00.000Z","2006-12-10T00:00:00.000Z","2006-12-11T00:00:00.000Z","2006-12-12T00:00:00.000Z","2006-12-13T00:00:00.000Z","2006-12-14T00:00:00.000Z","2006-12-15T00:00:00.000Z","2006-12-16T00:00:00.000Z","2006-12-17T00:00:00.000Z","2006-12-18T00:00:00.000Z","2006-12-19T00:00:00.000Z","2006-12-20T00:00:00.000Z","2006-12-21T00:00:00.000Z","2006-12-22T00:00:00.000Z","2006-12-23T00:00:00.000Z","2006-12-24T00:00:00.000Z","2006-12-25T00:00:00.000Z","2006-12-26T00:00:00.000Z","2006-12-27T00:00:00.000Z","2006-12-28T00:00:00.000Z","2006-12-29T00:00:00.000Z","2006-12-30T00:00:00.000Z","2006-12-31T00:00:00.000Z","2007-01-01T00:00:00.000Z","2007-01-02T00:00:00.000Z","2007-01-03T00:00:00.000Z","2007-01-04T00:00:00.000Z","2007-01-05T00:00:00.000Z","2007-01-06T00:00:00.000Z","2007-01-07T00:00:00.000Z","2007-01-08T00:00:00.000Z","2007-01-09T00:00:00.000Z","2007-01-10T00:00:00.000Z","2007-01-11T00:00:00.000Z","2007-01-12T00:00:00.000Z","2007-01-13T00:00:00.000Z","2007-01-14T00:00:00.000Z","2007-01-15T00:00:00.000Z","2007-01-16T00:00:00.000Z","2007-01-17T00:00:00.000Z","2007-01-18T00:00:00.000Z","2007-01-19T00:00:00.000Z","2007-01-20T00:00:00.000Z","2007-01-21T00:00:00.000Z","2007-01-22T00:00:00.000Z","2007-01-23T00:00:00.000Z","2007-01-24T00:00:00.000Z","2007-01-25T00:00:00.000Z","2007-01-26T00:00:00.000Z","2007-01-27T00:00:00.000Z","2007-01-28T00:00:00.000Z","2007-01-29T00:00:00.000Z","2007-01-30T00:00:00.000Z","2007-01-31T00:00:00.000Z","2007-02-01T00:00:00.000Z","2007-02-02T00:00:00.000Z","2007-02-03T00:00:00.000Z","2007-02-04T00:00:00.000Z","2007-02-05T00:00:00.000Z","2007-02-06T00:00:00.000Z","2007-02-07T00:00:00.000Z","2007-02-08T00:00:00.000Z","2007-02-09T00:00:00.000Z","2007-02-10T00:00:00.000Z","2007-02-11T00:00:00.000Z","2007-02-12T00:00:00.000Z","2007-02-13T00:00:00.000Z","2007-02-14T00:00:00.000Z","2007-02-15T00:00:00.000Z","2007-02-16T00:00:00.000Z","2007-02-17T00:00:00.000Z","2007-02-18T00:00:00.000Z","2007-02-19T00:00:00.000Z","2007-02-20T00:00:00.000Z","2007-02-21T00:00:00.000Z","2007-02-22T00:00:00.000Z","2007-02-23T00:00:00.000Z","2007-02-24T00:00:00.000Z","2007-02-25T00:00:00.000Z","2007-02-26T00:00:00.000Z","2007-02-27T00:00:00.000Z","2007-02-28T00:00:00.000Z","2007-03-01T00:00:00.000Z","2007-03-02T00:00:00.000Z","2007-03-03T00:00:00.000Z","2007-03-04T00:00:00.000Z","2007-03-05T00:00:00.000Z","2007-03-06T00:00:00.000Z","2007-03-07T00:00:00.000Z","2007-03-08T00:00:00.000Z","2007-03-09T00:00:00.000Z","2007-03-10T00:00:00.000Z","2007-03-11T00:00:00.000Z","2007-03-12T00:00:00.000Z","2007-03-13T00:00:00.000Z","2007-03-14T00:00:00.000Z","2007-03-15T00:00:00.000Z","2007-03-16T00:00:00.000Z","2007-03-17T00:00:00.000Z","2007-03-18T00:00:00.000Z","2007-03-19T00:00:00.000Z","2007-03-20T00:00:00.000Z","2007-03-21T00:00:00.000Z","2007-03-22T00:00:00.000Z","2007-03-23T00:00:00.000Z","2007-03-24T00:00:00.000Z","2007-03-25T00:00:00.000Z","2007-03-26T00:00:00.000Z","2007-03-27T00:00:00.000Z","2007-03-28T00:00:00.000Z","2007-03-29T00:00:00.000Z","2007-03-30T00:00:00.000Z","2007-03-31T00:00:00.000Z","2007-04-01T00:00:00.000Z","2007-04-02T00:00:00.000Z","2007-04-03T00:00:00.000Z","2007-04-04T00:00:00.000Z","2007-04-05T00:00:00.000Z","2007-04-06T00:00:00.000Z","2007-04-07T00:00:00.000Z","2007-04-08T00:00:00.000Z","2007-04-09T00:00:00.000Z","2007-04-10T00:00:00.000Z","2007-04-11T00:00:00.000Z","2007-04-12T00:00:00.000Z","2007-04-13T00:00:00.000Z","2007-04-14T00:00:00.000Z","2007-04-15T00:00:00.000Z","2007-04-16T00:00:00.000Z","2007-04-17T00:00:00.000Z","2007-04-18T00:00:00.000Z","2007-04-19T00:00:00.000Z","2007-04-20T00:00:00.000Z","2007-04-21T00:00:00.000Z","2007-04-22T00:00:00.000Z","2007-04-23T00:00:00.000Z","2007-04-24T00:00:00.000Z","2007-04-25T00:00:00.000Z","2007-04-26T00:00:00.000Z","2007-04-27T00:00:00.000Z","2007-04-28T00:00:00.000Z","2007-04-29T00:00:00.000Z","2007-04-30T00:00:00.000Z","2007-05-01T00:00:00.000Z","2007-05-02T00:00:00.000Z","2007-05-03T00:00:00.000Z","2007-05-04T00:00:00.000Z","2007-05-05T00:00:00.000Z","2007-05-06T00:00:00.000Z","2007-05-07T00:00:00.000Z","2007-05-08T00:00:00.000Z","2007-05-09T00:00:00.000Z","2007-05-10T00:00:00.000Z","2007-05-11T00:00:00.000Z","2007-05-12T00:00:00.000Z","2007-05-13T00:00:00.000Z","2007-05-14T00:00:00.000Z","2007-05-15T00:00:00.000Z","2007-05-16T00:00:00.000Z","2007-05-17T00:00:00.000Z","2007-05-18T00:00:00.000Z","2007-05-19T00:00:00.000Z","2007-05-20T00:00:00.000Z","2007-05-21T00:00:00.000Z","2007-05-22T00:00:00.000Z","2007-05-23T00:00:00.000Z","2007-05-24T00:00:00.000Z","2007-05-25T00:00:00.000Z","2007-05-26T00:00:00.000Z","2007-05-27T00:00:00.000Z","2007-05-28T00:00:00.000Z","2007-05-29T00:00:00.000Z","2007-05-30T00:00:00.000Z","2007-05-31T00:00:00.000Z","2007-06-01T00:00:00.000Z","2007-06-02T00:00:00.000Z","2007-06-03T00:00:00.000Z","2007-06-04T00:00:00.000Z","2007-06-05T00:00:00.000Z","2007-06-06T00:00:00.000Z","2007-06-07T00:00:00.000Z","2007-06-08T00:00:00.000Z","2007-06-09T00:00:00.000Z","2007-06-10T00:00:00.000Z","2007-06-11T00:00:00.000Z","2007-06-12T00:00:00.000Z","2007-06-13T00:00:00.000Z","2007-06-14T00:00:00.000Z","2007-06-15T00:00:00.000Z","2007-06-16T00:00:00.000Z","2007-06-17T00:00:00.000Z","2007-06-18T00:00:00.000Z","2007-06-19T00:00:00.000Z","2007-06-20T00:00:00.000Z","2007-06-21T00:00:00.000Z","2007-06-22T00:00:00.000Z","2007-06-23T00:00:00.000Z","2007-06-24T00:00:00.000Z","2007-06-25T00:00:00.000Z","2007-06-26T00:00:00.000Z","2007-06-27T00:00:00.000Z","2007-06-28T00:00:00.000Z","2007-06-29T00:00:00.000Z","2007-06-30T00:00:00.000Z","2007-07-01T00:00:00.000Z","2007-07-02T00:00:00.000Z","2007-07-03T00:00:00.000Z","2007-07-04T00:00:00.000Z","2007-07-05T00:00:00.000Z","2007-07-06T00:00:00.000Z","2007-07-07T00:00:00.000Z","2007-07-08T00:00:00.000Z","2007-07-09T00:00:00.000Z","2007-07-10T00:00:00.000Z","2007-07-11T00:00:00.000Z","2007-07-12T00:00:00.000Z","2007-07-13T00:00:00.000Z","2007-07-14T00:00:00.000Z","2007-07-15T00:00:00.000Z","2007-07-16T00:00:00.000Z","2007-07-17T00:00:00.000Z","2007-07-18T00:00:00.000Z","2007-07-19T00:00:00.000Z","2007-07-20T00:00:00.000Z","2007-07-21T00:00:00.000Z","2007-07-22T00:00:00.000Z","2007-07-23T00:00:00.000Z","2007-07-24T00:00:00.000Z","2007-07-25T00:00:00.000Z","2007-07-26T00:00:00.000Z","2007-07-27T00:00:00.000Z","2007-07-28T00:00:00.000Z","2007-07-29T00:00:00.000Z","2007-07-30T00:00:00.000Z","2007-07-31T00:00:00.000Z","2007-08-01T00:00:00.000Z","2007-08-02T00:00:00.000Z","2007-08-03T00:00:00.000Z","2007-08-04T00:00:00.000Z","2007-08-05T00:00:00.000Z","2007-08-06T00:00:00.000Z","2007-08-07T00:00:00.000Z","2007-08-08T00:00:00.000Z","2007-08-09T00:00:00.000Z","2007-08-10T00:00:00.000Z","2007-08-11T00:00:00.000Z","2007-08-12T00:00:00.000Z","2007-08-13T00:00:00.000Z","2007-08-14T00:00:00.000Z","2007-08-15T00:00:00.000Z","2007-08-16T00:00:00.000Z","2007-08-17T00:00:00.000Z","2007-08-18T00:00:00.000Z","2007-08-19T00:00:00.000Z","2007-08-20T00:00:00.000Z","2007-08-21T00:00:00.000Z","2007-08-22T00:00:00.000Z","2007-08-23T00:00:00.000Z","2007-08-24T00:00:00.000Z","2007-08-25T00:00:00.000Z","2007-08-26T00:00:00.000Z","2007-08-27T00:00:00.000Z","2007-08-28T00:00:00.000Z","2007-08-29T00:00:00.000Z","2007-08-30T00:00:00.000Z","2007-08-31T00:00:00.000Z","2007-09-01T00:00:00.000Z","2007-09-02T00:00:00.000Z","2007-09-03T00:00:00.000Z","2007-09-04T00:00:00.000Z","2007-09-05T00:00:00.000Z","2007-09-06T00:00:00.000Z","2007-09-07T00:00:00.000Z","2007-09-08T00:00:00.000Z","2007-09-09T00:00:00.000Z","2007-09-10T00:00:00.000Z","2007-09-11T00:00:00.000Z","2007-09-12T00:00:00.000Z","2007-09-13T00:00:00.000Z","2007-09-14T00:00:00.000Z","2007-09-15T00:00:00.000Z","2007-09-16T00:00:00.000Z","2007-09-17T00:00:00.000Z","2007-09-18T00:00:00.000Z","2007-09-19T00:00:00.000Z","2007-09-20T00:00:00.000Z","2007-09-21T00:00:00.000Z","2007-09-22T00:00:00.000Z","2007-09-23T00:00:00.000Z","2007-09-24T00:00:00.000Z","2007-09-25T00:00:00.000Z","2007-09-26T00:00:00.000Z","2007-09-27T00:00:00.000Z","2007-09-28T00:00:00.000Z","2007-09-29T00:00:00.000Z","2007-09-30T00:00:00.000Z","2007-10-01T00:00:00.000Z","2007-10-02T00:00:00.000Z","2007-10-03T00:00:00.000Z","2007-10-04T00:00:00.000Z","2007-10-05T00:00:00.000Z","2007-10-06T00:00:00.000Z","2007-10-07T00:00:00.000Z","2007-10-08T00:00:00.000Z","2007-10-09T00:00:00.000Z","2007-10-10T00:00:00.000Z","2007-10-11T00:00:00.000Z","2007-10-12T00:00:00.000Z","2007-10-13T00:00:00.000Z","2007-10-14T00:00:00.000Z","2007-10-15T00:00:00.000Z","2007-10-16T00:00:00.000Z","2007-10-17T00:00:00.000Z","2007-10-18T00:00:00.000Z","2007-10-19T00:00:00.000Z","2007-10-20T00:00:00.000Z","2007-10-21T00:00:00.000Z","2007-10-22T00:00:00.000Z","2007-10-23T00:00:00.000Z","2007-10-24T00:00:00.000Z","2007-10-25T00:00:00.000Z","2007-10-26T00:00:00.000Z","2007-10-27T00:00:00.000Z","2007-10-28T00:00:00.000Z","2007-10-29T00:00:00.000Z","2007-10-30T00:00:00.000Z","2007-10-31T00:00:00.000Z","2007-11-01T00:00:00.000Z","2007-11-02T00:00:00.000Z","2007-11-03T00:00:00.000Z","2007-11-04T00:00:00.000Z","2007-11-05T00:00:00.000Z","2007-11-06T00:00:00.000Z","2007-11-07T00:00:00.000Z","2007-11-08T00:00:00.000Z","2007-11-09T00:00:00.000Z","2007-11-10T00:00:00.000Z","2007-11-11T00:00:00.000Z","2007-11-12T00:00:00.000Z","2007-11-13T00:00:00.000Z","2007-11-14T00:00:00.000Z","2007-11-15T00:00:00.000Z","2007-11-16T00:00:00.000Z","2007-11-17T00:00:00.000Z","2007-11-18T00:00:00.000Z","2007-11-19T00:00:00.000Z","2007-11-20T00:00:00.000Z","2007-11-21T00:00:00.000Z","2007-11-22T00:00:00.000Z","2007-11-23T00:00:00.000Z","2007-11-24T00:00:00.000Z","2007-11-25T00:00:00.000Z","2007-11-26T00:00:00.000Z","2007-11-27T00:00:00.000Z","2007-11-28T00:00:00.000Z","2007-11-29T00:00:00.000Z","2007-11-30T00:00:00.000Z","2007-12-01T00:00:00.000Z","2007-12-02T00:00:00.000Z","2007-12-03T00:00:00.000Z","2007-12-04T00:00:00.000Z","2007-12-05T00:00:00.000Z","2007-12-06T00:00:00.000Z","2007-12-07T00:00:00.000Z","2007-12-08T00:00:00.000Z","2007-12-09T00:00:00.000Z","2007-12-10T00:00:00.000Z","2007-12-11T00:00:00.000Z","2007-12-12T00:00:00.000Z","2007-12-13T00:00:00.000Z","2007-12-14T00:00:00.000Z","2007-12-15T00:00:00.000Z","2007-12-16T00:00:00.000Z","2007-12-17T00:00:00.000Z","2007-12-18T00:00:00.000Z","2007-12-19T00:00:00.000Z","2007-12-20T00:00:00.000Z","2007-12-21T00:00:00.000Z","2007-12-22T00:00:00.000Z","2007-12-23T00:00:00.000Z","2007-12-24T00:00:00.000Z","2007-12-25T00:00:00.000Z","2007-12-26T00:00:00.000Z","2007-12-27T00:00:00.000Z","2007-12-28T00:00:00.000Z","2007-12-29T00:00:00.000Z","2007-12-30T00:00:00.000Z","2007-12-31T00:00:00.000Z","2008-01-01T00:00:00.000Z","2008-01-02T00:00:00.000Z","2008-01-03T00:00:00.000Z","2008-01-04T00:00:00.000Z","2008-01-05T00:00:00.000Z","2008-01-06T00:00:00.000Z","2008-01-07T00:00:00.000Z","2008-01-08T00:00:00.000Z","2008-01-09T00:00:00.000Z","2008-01-10T00:00:00.000Z","2008-01-11T00:00:00.000Z","2008-01-12T00:00:00.000Z","2008-01-13T00:00:00.000Z","2008-01-14T00:00:00.000Z","2008-01-15T00:00:00.000Z","2008-01-16T00:00:00.000Z","2008-01-17T00:00:00.000Z","2008-01-18T00:00:00.000Z","2008-01-19T00:00:00.000Z","2008-01-20T00:00:00.000Z","2008-01-21T00:00:00.000Z","2008-01-22T00:00:00.000Z","2008-01-23T00:00:00.000Z","2008-01-24T00:00:00.000Z","2008-01-25T00:00:00.000Z","2008-01-26T00:00:00.000Z","2008-01-27T00:00:00.000Z","2008-01-28T00:00:00.000Z","2008-01-29T00:00:00.000Z","2008-01-30T00:00:00.000Z","2008-01-31T00:00:00.000Z","2008-02-01T00:00:00.000Z","2008-02-02T00:00:00.000Z","2008-02-03T00:00:00.000Z","2008-02-04T00:00:00.000Z","2008-02-05T00:00:00.000Z","2008-02-06T00:00:00.000Z","2008-02-07T00:00:00.000Z","2008-02-08T00:00:00.000Z","2008-02-09T00:00:00.000Z","2008-02-10T00:00:00.000Z","2008-02-11T00:00:00.000Z","2008-02-12T00:00:00.000Z","2008-02-13T00:00:00.000Z","2008-02-14T00:00:00.000Z","2008-02-15T00:00:00.000Z","2008-02-16T00:00:00.000Z","2008-02-17T00:00:00.000Z","2008-02-18T00:00:00.000Z","2008-02-19T00:00:00.000Z","2008-02-20T00:00:00.000Z","2008-02-21T00:00:00.000Z","2008-02-22T00:00:00.000Z","2008-02-23T00:00:00.000Z","2008-02-24T00:00:00.000Z","2008-02-25T00:00:00.000Z","2008-02-26T00:00:00.000Z","2008-02-27T00:00:00.000Z","2008-02-28T00:00:00.000Z","2008-02-29T00:00:00.000Z","2008-03-01T00:00:00.000Z","2008-03-02T00:00:00.000Z","2008-03-03T00:00:00.000Z","2008-03-04T00:00:00.000Z","2008-03-05T00:00:00.000Z","2008-03-06T00:00:00.000Z","2008-03-07T00:00:00.000Z","2008-03-08T00:00:00.000Z","2008-03-09T00:00:00.000Z","2008-03-10T00:00:00.000Z","2008-03-11T00:00:00.000Z","2008-03-12T00:00:00.000Z","2008-03-13T00:00:00.000Z","2008-03-14T00:00:00.000Z","2008-03-15T00:00:00.000Z","2008-03-16T00:00:00.000Z","2008-03-17T00:00:00.000Z","2008-03-18T00:00:00.000Z","2008-03-19T00:00:00.000Z","2008-03-20T00:00:00.000Z","2008-03-21T00:00:00.000Z","2008-03-22T00:00:00.000Z","2008-03-23T00:00:00.000Z","2008-03-24T00:00:00.000Z","2008-03-25T00:00:00.000Z","2008-03-26T00:00:00.000Z","2008-03-27T00:00:00.000Z","2008-03-28T00:00:00.000Z","2008-03-29T00:00:00.000Z","2008-03-30T00:00:00.000Z","2008-03-31T00:00:00.000Z","2008-04-01T00:00:00.000Z","2008-04-02T00:00:00.000Z","2008-04-03T00:00:00.000Z","2008-04-04T00:00:00.000Z","2008-04-05T00:00:00.000Z","2008-04-06T00:00:00.000Z","2008-04-07T00:00:00.000Z","2008-04-08T00:00:00.000Z","2008-04-09T00:00:00.000Z","2008-04-10T00:00:00.000Z","2008-04-11T00:00:00.000Z","2008-04-12T00:00:00.000Z","2008-04-13T00:00:00.000Z","2008-04-14T00:00:00.000Z","2008-04-15T00:00:00.000Z","2008-04-16T00:00:00.000Z","2008-04-17T00:00:00.000Z","2008-04-18T00:00:00.000Z","2008-04-19T00:00:00.000Z","2008-04-20T00:00:00.000Z","2008-04-21T00:00:00.000Z","2008-04-22T00:00:00.000Z","2008-04-23T00:00:00.000Z","2008-04-24T00:00:00.000Z","2008-04-25T00:00:00.000Z","2008-04-26T00:00:00.000Z","2008-04-27T00:00:00.000Z","2008-04-28T00:00:00.000Z","2008-04-29T00:00:00.000Z","2008-04-30T00:00:00.000Z","2008-05-01T00:00:00.000Z","2008-05-02T00:00:00.000Z","2008-05-03T00:00:00.000Z","2008-05-04T00:00:00.000Z","2008-05-05T00:00:00.000Z","2008-05-06T00:00:00.000Z","2008-05-07T00:00:00.000Z","2008-05-08T00:00:00.000Z","2008-05-09T00:00:00.000Z","2008-05-10T00:00:00.000Z","2008-05-11T00:00:00.000Z","2008-05-12T00:00:00.000Z","2008-05-13T00:00:00.000Z","2008-05-14T00:00:00.000Z","2008-05-15T00:00:00.000Z","2008-05-16T00:00:00.000Z","2008-05-17T00:00:00.000Z","2008-05-18T00:00:00.000Z","2008-05-19T00:00:00.000Z","2008-05-20T00:00:00.000Z","2008-05-21T00:00:00.000Z","2008-05-22T00:00:00.000Z","2008-05-23T00:00:00.000Z","2008-05-24T00:00:00.000Z","2008-05-25T00:00:00.000Z","2008-05-26T00:00:00.000Z","2008-05-27T00:00:00.000Z","2008-05-28T00:00:00.000Z","2008-05-29T00:00:00.000Z","2008-05-30T00:00:00.000Z","2008-05-31T00:00:00.000Z","2008-06-01T00:00:00.000Z","2008-06-02T00:00:00.000Z","2008-06-03T00:00:00.000Z","2008-06-04T00:00:00.000Z","2008-06-05T00:00:00.000Z","2008-06-06T00:00:00.000Z","2008-06-07T00:00:00.000Z","2008-06-08T00:00:00.000Z","2008-06-09T00:00:00.000Z","2008-06-10T00:00:00.000Z","2008-06-11T00:00:00.000Z","2008-06-12T00:00:00.000Z","2008-06-13T00:00:00.000Z","2008-06-14T00:00:00.000Z","2008-06-15T00:00:00.000Z","2008-06-16T00:00:00.000Z","2008-06-17T00:00:00.000Z","2008-06-18T00:00:00.000Z","2008-06-19T00:00:00.000Z","2008-06-20T00:00:00.000Z","2008-06-21T00:00:00.000Z","2008-06-22T00:00:00.000Z","2008-06-23T00:00:00.000Z","2008-06-24T00:00:00.000Z","2008-06-25T00:00:00.000Z","2008-06-26T00:00:00.000Z","2008-06-27T00:00:00.000Z","2008-06-28T00:00:00.000Z","2008-06-29T00:00:00.000Z","2008-06-30T00:00:00.000Z","2008-07-01T00:00:00.000Z","2008-07-02T00:00:00.000Z","2008-07-03T00:00:00.000Z","2008-07-04T00:00:00.000Z","2008-07-05T00:00:00.000Z","2008-07-06T00:00:00.000Z","2008-07-07T00:00:00.000Z","2008-07-08T00:00:00.000Z","2008-07-09T00:00:00.000Z","2008-07-10T00:00:00.000Z","2008-07-11T00:00:00.000Z","2008-07-12T00:00:00.000Z","2008-07-13T00:00:00.000Z","2008-07-14T00:00:00.000Z","2008-07-15T00:00:00.000Z","2008-07-16T00:00:00.000Z","2008-07-17T00:00:00.000Z","2008-07-18T00:00:00.000Z","2008-07-19T00:00:00.000Z","2008-07-20T00:00:00.000Z","2008-07-21T00:00:00.000Z","2008-07-22T00:00:00.000Z","2008-07-23T00:00:00.000Z","2008-07-24T00:00:00.000Z","2008-07-25T00:00:00.000Z","2008-07-26T00:00:00.000Z","2008-07-27T00:00:00.000Z","2008-07-28T00:00:00.000Z","2008-07-29T00:00:00.000Z","2008-07-30T00:00:00.000Z","2008-07-31T00:00:00.000Z","2008-08-01T00:00:00.000Z","2008-08-02T00:00:00.000Z","2008-08-03T00:00:00.000Z","2008-08-04T00:00:00.000Z","2008-08-05T00:00:00.000Z","2008-08-06T00:00:00.000Z","2008-08-07T00:00:00.000Z","2008-08-08T00:00:00.000Z","2008-08-09T00:00:00.000Z","2008-08-10T00:00:00.000Z","2008-08-11T00:00:00.000Z","2008-08-12T00:00:00.000Z","2008-08-13T00:00:00.000Z","2008-08-14T00:00:00.000Z","2008-08-15T00:00:00.000Z","2008-08-16T00:00:00.000Z","2008-08-17T00:00:00.000Z","2008-08-18T00:00:00.000Z","2008-08-19T00:00:00.000Z","2008-08-20T00:00:00.000Z","2008-08-21T00:00:00.000Z","2008-08-22T00:00:00.000Z","2008-08-23T00:00:00.000Z","2008-08-24T00:00:00.000Z","2008-08-25T00:00:00.000Z","2008-08-26T00:00:00.000Z","2008-08-27T00:00:00.000Z","2008-08-28T00:00:00.000Z","2008-08-29T00:00:00.000Z","2008-08-30T00:00:00.000Z","2008-08-31T00:00:00.000Z","2008-09-01T00:00:00.000Z","2008-09-02T00:00:00.000Z","2008-09-03T00:00:00.000Z","2008-09-04T00:00:00.000Z","2008-09-05T00:00:00.000Z","2008-09-06T00:00:00.000Z","2008-09-07T00:00:00.000Z","2008-09-08T00:00:00.000Z","2008-09-09T00:00:00.000Z","2008-09-10T00:00:00.000Z","2008-09-11T00:00:00.000Z","2008-09-12T00:00:00.000Z","2008-09-13T00:00:00.000Z","2008-09-14T00:00:00.000Z","2008-09-15T00:00:00.000Z","2008-09-16T00:00:00.000Z","2008-09-17T00:00:00.000Z","2008-09-18T00:00:00.000Z","2008-09-19T00:00:00.000Z","2008-09-20T00:00:00.000Z","2008-09-21T00:00:00.000Z","2008-09-22T00:00:00.000Z","2008-09-23T00:00:00.000Z","2008-09-24T00:00:00.000Z","2008-09-25T00:00:00.000Z","2008-09-26T00:00:00.000Z","2008-09-27T00:00:00.000Z","2008-09-28T00:00:00.000Z","2008-09-29T00:00:00.000Z","2008-09-30T00:00:00.000Z","2008-10-01T00:00:00.000Z","2008-10-02T00:00:00.000Z","2008-10-03T00:00:00.000Z","2008-10-04T00:00:00.000Z","2008-10-05T00:00:00.000Z","2008-10-06T00:00:00.000Z","2008-10-07T00:00:00.000Z","2008-10-08T00:00:00.000Z","2008-10-09T00:00:00.000Z","2008-10-10T00:00:00.000Z","2008-10-11T00:00:00.000Z","2008-10-12T00:00:00.000Z","2008-10-13T00:00:00.000Z","2008-10-14T00:00:00.000Z","2008-10-15T00:00:00.000Z","2008-10-16T00:00:00.000Z","2008-10-17T00:00:00.000Z","2008-10-18T00:00:00.000Z","2008-10-19T00:00:00.000Z","2008-10-20T00:00:00.000Z","2008-10-21T00:00:00.000Z","2008-10-22T00:00:00.000Z","2008-10-23T00:00:00.000Z","2008-10-24T00:00:00.000Z","2008-10-25T00:00:00.000Z","2008-10-26T00:00:00.000Z","2008-10-27T00:00:00.000Z","2008-10-28T00:00:00.000Z","2008-10-29T00:00:00.000Z","2008-10-30T00:00:00.000Z","2008-10-31T00:00:00.000Z","2008-11-01T00:00:00.000Z","2008-11-02T00:00:00.000Z","2008-11-03T00:00:00.000Z","2008-11-04T00:00:00.000Z","2008-11-05T00:00:00.000Z","2008-11-06T00:00:00.000Z","2008-11-07T00:00:00.000Z","2008-11-08T00:00:00.000Z","2008-11-09T00:00:00.000Z","2008-11-10T00:00:00.000Z","2008-11-11T00:00:00.000Z","2008-11-12T00:00:00.000Z","2008-11-13T00:00:00.000Z","2008-11-14T00:00:00.000Z","2008-11-15T00:00:00.000Z","2008-11-16T00:00:00.000Z","2008-11-17T00:00:00.000Z","2008-11-18T00:00:00.000Z","2008-11-19T00:00:00.000Z","2008-11-20T00:00:00.000Z","2008-11-21T00:00:00.000Z","2008-11-22T00:00:00.000Z","2008-11-23T00:00:00.000Z","2008-11-24T00:00:00.000Z","2008-11-25T00:00:00.000Z","2008-11-26T00:00:00.000Z","2008-11-27T00:00:00.000Z","2008-11-28T00:00:00.000Z","2008-11-29T00:00:00.000Z","2008-11-30T00:00:00.000Z","2008-12-01T00:00:00.000Z","2008-12-02T00:00:00.000Z","2008-12-03T00:00:00.000Z","2008-12-04T00:00:00.000Z","2008-12-05T00:00:00.000Z","2008-12-06T00:00:00.000Z","2008-12-07T00:00:00.000Z","2008-12-08T00:00:00.000Z","2008-12-09T00:00:00.000Z","2008-12-10T00:00:00.000Z","2008-12-11T00:00:00.000Z","2008-12-12T00:00:00.000Z","2008-12-13T00:00:00.000Z","2008-12-14T00:00:00.000Z","2008-12-15T00:00:00.000Z","2008-12-16T00:00:00.000Z","2008-12-17T00:00:00.000Z","2008-12-18T00:00:00.000Z","2008-12-19T00:00:00.000Z","2008-12-20T00:00:00.000Z","2008-12-21T00:00:00.000Z","2008-12-22T00:00:00.000Z","2008-12-23T00:00:00.000Z","2008-12-24T00:00:00.000Z","2008-12-25T00:00:00.000Z","2008-12-26T00:00:00.000Z","2008-12-27T00:00:00.000Z","2008-12-28T00:00:00.000Z","2008-12-29T00:00:00.000Z","2008-12-30T00:00:00.000Z","2008-12-31T00:00:00.000Z","2009-01-01T00:00:00.000Z","2009-01-02T00:00:00.000Z","2009-01-03T00:00:00.000Z","2009-01-04T00:00:00.000Z","2009-01-05T00:00:00.000Z","2009-01-06T00:00:00.000Z","2009-01-07T00:00:00.000Z","2009-01-08T00:00:00.000Z","2009-01-09T00:00:00.000Z","2009-01-10T00:00:00.000Z","2009-01-11T00:00:00.000Z","2009-01-12T00:00:00.000Z","2009-01-13T00:00:00.000Z","2009-01-14T00:00:00.000Z","2009-01-15T00:00:00.000Z","2009-01-16T00:00:00.000Z","2009-01-17T00:00:00.000Z","2009-01-18T00:00:00.000Z","2009-01-19T00:00:00.000Z","2009-01-20T00:00:00.000Z","2009-01-21T00:00:00.000Z","2009-01-22T00:00:00.000Z","2009-01-23T00:00:00.000Z","2009-01-24T00:00:00.000Z","2009-01-25T00:00:00.000Z","2009-01-26T00:00:00.000Z","2009-01-27T00:00:00.000Z","2009-01-28T00:00:00.000Z","2009-01-29T00:00:00.000Z","2009-01-30T00:00:00.000Z","2009-01-31T00:00:00.000Z","2009-02-01T00:00:00.000Z","2009-02-02T00:00:00.000Z","2009-02-03T00:00:00.000Z","2009-02-04T00:00:00.000Z","2009-02-05T00:00:00.000Z","2009-02-06T00:00:00.000Z","2009-02-07T00:00:00.000Z","2009-02-08T00:00:00.000Z","2009-02-09T00:00:00.000Z","2009-02-10T00:00:00.000Z","2009-02-11T00:00:00.000Z","2009-02-12T00:00:00.000Z","2009-02-13T00:00:00.000Z","2009-02-14T00:00:00.000Z","2009-02-15T00:00:00.000Z","2009-02-16T00:00:00.000Z","2009-02-17T00:00:00.000Z","2009-02-18T00:00:00.000Z","2009-02-19T00:00:00.000Z","2009-02-20T00:00:00.000Z","2009-02-21T00:00:00.000Z","2009-02-22T00:00:00.000Z","2009-02-23T00:00:00.000Z","2009-02-24T00:00:00.000Z","2009-02-25T00:00:00.000Z","2009-02-26T00:00:00.000Z","2009-02-27T00:00:00.000Z","2009-02-28T00:00:00.000Z","2009-03-01T00:00:00.000Z","2009-03-02T00:00:00.000Z","2009-03-03T00:00:00.000Z","2009-03-04T00:00:00.000Z","2009-03-05T00:00:00.000Z","2009-03-06T00:00:00.000Z","2009-03-07T00:00:00.000Z","2009-03-08T00:00:00.000Z","2009-03-09T00:00:00.000Z","2009-03-10T00:00:00.000Z","2009-03-11T00:00:00.000Z","2009-03-12T00:00:00.000Z","2009-03-13T00:00:00.000Z","2009-03-14T00:00:00.000Z","2009-03-15T00:00:00.000Z","2009-03-16T00:00:00.000Z","2009-03-17T00:00:00.000Z","2009-03-18T00:00:00.000Z","2009-03-19T00:00:00.000Z","2009-03-20T00:00:00.000Z","2009-03-21T00:00:00.000Z","2009-03-22T00:00:00.000Z","2009-03-23T00:00:00.000Z","2009-03-24T00:00:00.000Z","2009-03-25T00:00:00.000Z","2009-03-26T00:00:00.000Z","2009-03-27T00:00:00.000Z","2009-03-28T00:00:00.000Z","2009-03-29T00:00:00.000Z","2009-03-30T00:00:00.000Z","2009-03-31T00:00:00.000Z","2009-04-01T00:00:00.000Z","2009-04-02T00:00:00.000Z","2009-04-03T00:00:00.000Z","2009-04-04T00:00:00.000Z","2009-04-05T00:00:00.000Z","2009-04-06T00:00:00.000Z","2009-04-07T00:00:00.000Z","2009-04-08T00:00:00.000Z","2009-04-09T00:00:00.000Z","2009-04-10T00:00:00.000Z","2009-04-11T00:00:00.000Z","2009-04-12T00:00:00.000Z","2009-04-13T00:00:00.000Z","2009-04-14T00:00:00.000Z","2009-04-15T00:00:00.000Z","2009-04-16T00:00:00.000Z","2009-04-17T00:00:00.000Z","2009-04-18T00:00:00.000Z","2009-04-19T00:00:00.000Z","2009-04-20T00:00:00.000Z","2009-04-21T00:00:00.000Z","2009-04-22T00:00:00.000Z","2009-04-23T00:00:00.000Z","2009-04-24T00:00:00.000Z","2009-04-25T00:00:00.000Z","2009-04-26T00:00:00.000Z","2009-04-27T00:00:00.000Z","2009-04-28T00:00:00.000Z","2009-04-29T00:00:00.000Z","2009-04-30T00:00:00.000Z","2009-05-01T00:00:00.000Z","2009-05-02T00:00:00.000Z","2009-05-03T00:00:00.000Z","2009-05-04T00:00:00.000Z","2009-05-05T00:00:00.000Z","2009-05-06T00:00:00.000Z","2009-05-07T00:00:00.000Z","2009-05-08T00:00:00.000Z","2009-05-09T00:00:00.000Z","2009-05-10T00:00:00.000Z","2009-05-11T00:00:00.000Z","2009-05-12T00:00:00.000Z","2009-05-13T00:00:00.000Z","2009-05-14T00:00:00.000Z","2009-05-15T00:00:00.000Z","2009-05-16T00:00:00.000Z","2009-05-17T00:00:00.000Z","2009-05-18T00:00:00.000Z","2009-05-19T00:00:00.000Z","2009-05-20T00:00:00.000Z","2009-05-21T00:00:00.000Z","2009-05-22T00:00:00.000Z","2009-05-23T00:00:00.000Z","2009-05-24T00:00:00.000Z","2009-05-25T00:00:00.000Z","2009-05-26T00:00:00.000Z","2009-05-27T00:00:00.000Z","2009-05-28T00:00:00.000Z","2009-05-29T00:00:00.000Z","2009-05-30T00:00:00.000Z","2009-05-31T00:00:00.000Z","2009-06-01T00:00:00.000Z","2009-06-02T00:00:00.000Z","2009-06-03T00:00:00.000Z","2009-06-04T00:00:00.000Z","2009-06-05T00:00:00.000Z","2009-06-06T00:00:00.000Z","2009-06-07T00:00:00.000Z","2009-06-08T00:00:00.000Z","2009-06-09T00:00:00.000Z","2009-06-10T00:00:00.000Z","2009-06-11T00:00:00.000Z","2009-06-12T00:00:00.000Z","2009-06-13T00:00:00.000Z","2009-06-14T00:00:00.000Z","2009-06-15T00:00:00.000Z","2009-06-16T00:00:00.000Z","2009-06-17T00:00:00.000Z","2009-06-18T00:00:00.000Z","2009-06-19T00:00:00.000Z","2009-06-20T00:00:00.000Z","2009-06-21T00:00:00.000Z","2009-06-22T00:00:00.000Z","2009-06-23T00:00:00.000Z","2009-06-24T00:00:00.000Z","2009-06-25T00:00:00.000Z","2009-06-26T00:00:00.000Z","2009-06-27T00:00:00.000Z","2009-06-28T00:00:00.000Z","2009-06-29T00:00:00.000Z","2009-06-30T00:00:00.000Z","2009-07-01T00:00:00.000Z","2009-07-02T00:00:00.000Z","2009-07-03T00:00:00.000Z","2009-07-04T00:00:00.000Z","2009-07-05T00:00:00.000Z","2009-07-06T00:00:00.000Z","2009-07-07T00:00:00.000Z","2009-07-08T00:00:00.000Z","2009-07-09T00:00:00.000Z","2009-07-10T00:00:00.000Z","2009-07-11T00:00:00.000Z","2009-07-12T00:00:00.000Z","2009-07-13T00:00:00.000Z","2009-07-14T00:00:00.000Z","2009-07-15T00:00:00.000Z","2009-07-16T00:00:00.000Z","2009-07-17T00:00:00.000Z","2009-07-18T00:00:00.000Z","2009-07-19T00:00:00.000Z","2009-07-20T00:00:00.000Z","2009-07-21T00:00:00.000Z","2009-07-22T00:00:00.000Z","2009-07-23T00:00:00.000Z","2009-07-24T00:00:00.000Z","2009-07-25T00:00:00.000Z","2009-07-26T00:00:00.000Z","2009-07-27T00:00:00.000Z","2009-07-28T00:00:00.000Z","2009-07-29T00:00:00.000Z","2009-07-30T00:00:00.000Z","2009-07-31T00:00:00.000Z","2009-08-01T00:00:00.000Z","2009-08-02T00:00:00.000Z","2009-08-03T00:00:00.000Z","2009-08-04T00:00:00.000Z","2009-08-05T00:00:00.000Z","2009-08-06T00:00:00.000Z","2009-08-07T00:00:00.000Z","2009-08-08T00:00:00.000Z","2009-08-09T00:00:00.000Z","2009-08-10T00:00:00.000Z","2009-08-11T00:00:00.000Z","2009-08-12T00:00:00.000Z","2009-08-13T00:00:00.000Z","2009-08-14T00:00:00.000Z","2009-08-15T00:00:00.000Z","2009-08-16T00:00:00.000Z","2009-08-17T00:00:00.000Z","2009-08-18T00:00:00.000Z","2009-08-19T00:00:00.000Z","2009-08-20T00:00:00.000Z","2009-08-21T00:00:00.000Z","2009-08-22T00:00:00.000Z","2009-08-23T00:00:00.000Z","2009-08-24T00:00:00.000Z","2009-08-25T00:00:00.000Z","2009-08-26T00:00:00.000Z","2009-08-27T00:00:00.000Z","2009-08-28T00:00:00.000Z","2009-08-29T00:00:00.000Z","2009-08-30T00:00:00.000Z","2009-08-31T00:00:00.000Z","2009-09-01T00:00:00.000Z","2009-09-02T00:00:00.000Z","2009-09-03T00:00:00.000Z","2009-09-04T00:00:00.000Z","2009-09-05T00:00:00.000Z","2009-09-06T00:00:00.000Z","2009-09-07T00:00:00.000Z","2009-09-08T00:00:00.000Z","2009-09-09T00:00:00.000Z","2009-09-10T00:00:00.000Z","2009-09-11T00:00:00.000Z","2009-09-12T00:00:00.000Z","2009-09-13T00:00:00.000Z","2009-09-14T00:00:00.000Z","2009-09-15T00:00:00.000Z","2009-09-16T00:00:00.000Z","2009-09-17T00:00:00.000Z","2009-09-18T00:00:00.000Z","2009-09-19T00:00:00.000Z","2009-09-20T00:00:00.000Z","2009-09-21T00:00:00.000Z","2009-09-22T00:00:00.000Z","2009-09-23T00:00:00.000Z","2009-09-24T00:00:00.000Z","2009-09-25T00:00:00.000Z","2009-09-26T00:00:00.000Z","2009-09-27T00:00:00.000Z","2009-09-28T00:00:00.000Z","2009-09-29T00:00:00.000Z","2009-09-30T00:00:00.000Z","2009-10-01T00:00:00.000Z","2009-10-02T00:00:00.000Z","2009-10-03T00:00:00.000Z","2009-10-04T00:00:00.000Z","2009-10-05T00:00:00.000Z","2009-10-06T00:00:00.000Z","2009-10-07T00:00:00.000Z","2009-10-08T00:00:00.000Z","2009-10-09T00:00:00.000Z","2009-10-10T00:00:00.000Z","2009-10-11T00:00:00.000Z","2009-10-12T00:00:00.000Z","2009-10-13T00:00:00.000Z","2009-10-14T00:00:00.000Z","2009-10-15T00:00:00.000Z","2009-10-16T00:00:00.000Z","2009-10-17T00:00:00.000Z","2009-10-18T00:00:00.000Z","2009-10-19T00:00:00.000Z","2009-10-20T00:00:00.000Z","2009-10-21T00:00:00.000Z","2009-10-22T00:00:00.000Z","2009-10-23T00:00:00.000Z","2009-10-24T00:00:00.000Z","2009-10-25T00:00:00.000Z","2009-10-26T00:00:00.000Z","2009-10-27T00:00:00.000Z","2009-10-28T00:00:00.000Z","2009-10-29T00:00:00.000Z","2009-10-30T00:00:00.000Z","2009-10-31T00:00:00.000Z","2009-11-01T00:00:00.000Z","2009-11-02T00:00:00.000Z","2009-11-03T00:00:00.000Z","2009-11-04T00:00:00.000Z","2009-11-05T00:00:00.000Z","2009-11-06T00:00:00.000Z","2009-11-07T00:00:00.000Z","2009-11-08T00:00:00.000Z","2009-11-09T00:00:00.000Z","2009-11-10T00:00:00.000Z","2009-11-11T00:00:00.000Z","2009-11-12T00:00:00.000Z","2009-11-13T00:00:00.000Z","2009-11-14T00:00:00.000Z","2009-11-15T00:00:00.000Z","2009-11-16T00:00:00.000Z","2009-11-17T00:00:00.000Z","2009-11-18T00:00:00.000Z","2009-11-19T00:00:00.000Z","2009-11-20T00:00:00.000Z","2009-11-21T00:00:00.000Z","2009-11-22T00:00:00.000Z","2009-11-23T00:00:00.000Z","2009-11-24T00:00:00.000Z","2009-11-25T00:00:00.000Z","2009-11-26T00:00:00.000Z","2009-11-27T00:00:00.000Z","2009-11-28T00:00:00.000Z","2009-11-29T00:00:00.000Z","2009-11-30T00:00:00.000Z","2009-12-01T00:00:00.000Z","2009-12-02T00:00:00.000Z","2009-12-03T00:00:00.000Z","2009-12-04T00:00:00.000Z","2009-12-05T00:00:00.000Z","2009-12-06T00:00:00.000Z","2009-12-07T00:00:00.000Z","2009-12-08T00:00:00.000Z","2009-12-09T00:00:00.000Z","2009-12-10T00:00:00.000Z","2009-12-11T00:00:00.000Z","2009-12-12T00:00:00.000Z","2009-12-13T00:00:00.000Z","2009-12-14T00:00:00.000Z","2009-12-15T00:00:00.000Z","2009-12-16T00:00:00.000Z","2009-12-17T00:00:00.000Z","2009-12-18T00:00:00.000Z","2009-12-19T00:00:00.000Z","2009-12-20T00:00:00.000Z","2009-12-21T00:00:00.000Z","2009-12-22T00:00:00.000Z","2009-12-23T00:00:00.000Z","2009-12-24T00:00:00.000Z","2009-12-25T00:00:00.000Z","2009-12-26T00:00:00.000Z","2009-12-27T00:00:00.000Z","2009-12-28T00:00:00.000Z","2009-12-29T00:00:00.000Z","2009-12-30T00:00:00.000Z","2009-12-31T00:00:00.000Z","2010-01-01T00:00:00.000Z","2010-01-02T00:00:00.000Z","2010-01-03T00:00:00.000Z","2010-01-04T00:00:00.000Z","2010-01-05T00:00:00.000Z","2010-01-06T00:00:00.000Z","2010-01-07T00:00:00.000Z","2010-01-08T00:00:00.000Z","2010-01-09T00:00:00.000Z","2010-01-10T00:00:00.000Z","2010-01-11T00:00:00.000Z","2010-01-12T00:00:00.000Z","2010-01-13T00:00:00.000Z","2010-01-14T00:00:00.000Z","2010-01-15T00:00:00.000Z","2010-01-16T00:00:00.000Z","2010-01-17T00:00:00.000Z","2010-01-18T00:00:00.000Z","2010-01-19T00:00:00.000Z","2010-01-20T00:00:00.000Z","2010-01-21T00:00:00.000Z","2010-01-22T00:00:00.000Z","2010-01-23T00:00:00.000Z","2010-01-24T00:00:00.000Z","2010-01-25T00:00:00.000Z","2010-01-26T00:00:00.000Z","2010-01-27T00:00:00.000Z","2010-01-28T00:00:00.000Z","2010-01-29T00:00:00.000Z","2010-01-30T00:00:00.000Z","2010-01-31T00:00:00.000Z","2010-02-01T00:00:00.000Z","2010-02-02T00:00:00.000Z","2010-02-03T00:00:00.000Z","2010-02-04T00:00:00.000Z","2010-02-05T00:00:00.000Z","2010-02-06T00:00:00.000Z","2010-02-07T00:00:00.000Z","2010-02-08T00:00:00.000Z","2010-02-09T00:00:00.000Z","2010-02-10T00:00:00.000Z","2010-02-11T00:00:00.000Z","2010-02-12T00:00:00.000Z","2010-02-13T00:00:00.000Z","2010-02-14T00:00:00.000Z","2010-02-15T00:00:00.000Z","2010-02-16T00:00:00.000Z","2010-02-17T00:00:00.000Z","2010-02-18T00:00:00.000Z","2010-02-19T00:00:00.000Z","2010-02-20T00:00:00.000Z","2010-02-21T00:00:00.000Z","2010-02-22T00:00:00.000Z","2010-02-23T00:00:00.000Z","2010-02-24T00:00:00.000Z","2010-02-25T00:00:00.000Z","2010-02-26T00:00:00.000Z","2010-02-27T00:00:00.000Z","2010-02-28T00:00:00.000Z","2010-03-01T00:00:00.000Z","2010-03-02T00:00:00.000Z","2010-03-03T00:00:00.000Z","2010-03-04T00:00:00.000Z","2010-03-05T00:00:00.000Z","2010-03-06T00:00:00.000Z","2010-03-07T00:00:00.000Z","2010-03-08T00:00:00.000Z","2010-03-09T00:00:00.000Z","2010-03-10T00:00:00.000Z","2010-03-11T00:00:00.000Z","2010-03-12T00:00:00.000Z","2010-03-13T00:00:00.000Z","2010-03-14T00:00:00.000Z","2010-03-15T00:00:00.000Z","2010-03-16T00:00:00.000Z","2010-03-17T00:00:00.000Z","2010-03-18T00:00:00.000Z","2010-03-19T00:00:00.000Z","2010-03-20T00:00:00.000Z","2010-03-21T00:00:00.000Z","2010-03-22T00:00:00.000Z","2010-03-23T00:00:00.000Z","2010-03-24T00:00:00.000Z","2010-03-25T00:00:00.000Z","2010-03-26T00:00:00.000Z","2010-03-27T00:00:00.000Z","2010-03-28T00:00:00.000Z","2010-03-29T00:00:00.000Z","2010-03-30T00:00:00.000Z","2010-03-31T00:00:00.000Z","2010-04-01T00:00:00.000Z","2010-04-02T00:00:00.000Z","2010-04-03T00:00:00.000Z","2010-04-04T00:00:00.000Z","2010-04-05T00:00:00.000Z","2010-04-06T00:00:00.000Z","2010-04-07T00:00:00.000Z","2010-04-08T00:00:00.000Z","2010-04-09T00:00:00.000Z","2010-04-10T00:00:00.000Z","2010-04-11T00:00:00.000Z","2010-04-12T00:00:00.000Z","2010-04-13T00:00:00.000Z","2010-04-14T00:00:00.000Z","2010-04-15T00:00:00.000Z","2010-04-16T00:00:00.000Z","2010-04-17T00:00:00.000Z","2010-04-18T00:00:00.000Z","2010-04-19T00:00:00.000Z","2010-04-20T00:00:00.000Z","2010-04-21T00:00:00.000Z","2010-04-22T00:00:00.000Z","2010-04-23T00:00:00.000Z","2010-04-24T00:00:00.000Z","2010-04-25T00:00:00.000Z","2010-04-26T00:00:00.000Z","2010-04-27T00:00:00.000Z","2010-04-28T00:00:00.000Z","2010-04-29T00:00:00.000Z","2010-04-30T00:00:00.000Z","2010-05-01T00:00:00.000Z","2010-05-02T00:00:00.000Z","2010-05-03T00:00:00.000Z","2010-05-04T00:00:00.000Z","2010-05-05T00:00:00.000Z","2010-05-06T00:00:00.000Z","2010-05-07T00:00:00.000Z","2010-05-08T00:00:00.000Z","2010-05-09T00:00:00.000Z","2010-05-10T00:00:00.000Z","2010-05-11T00:00:00.000Z","2010-05-12T00:00:00.000Z","2010-05-13T00:00:00.000Z","2010-05-14T00:00:00.000Z","2010-05-15T00:00:00.000Z","2010-05-16T00:00:00.000Z","2010-05-17T00:00:00.000Z","2010-05-18T00:00:00.000Z","2010-05-19T00:00:00.000Z","2010-05-20T00:00:00.000Z","2010-05-21T00:00:00.000Z","2010-05-22T00:00:00.000Z","2010-05-23T00:00:00.000Z","2010-05-24T00:00:00.000Z","2010-05-25T00:00:00.000Z","2010-05-26T00:00:00.000Z","2010-05-27T00:00:00.000Z","2010-05-28T00:00:00.000Z","2010-05-29T00:00:00.000Z","2010-05-30T00:00:00.000Z","2010-05-31T00:00:00.000Z","2010-06-01T00:00:00.000Z","2010-06-02T00:00:00.000Z","2010-06-03T00:00:00.000Z","2010-06-04T00:00:00.000Z","2010-06-05T00:00:00.000Z","2010-06-06T00:00:00.000Z","2010-06-07T00:00:00.000Z","2010-06-08T00:00:00.000Z","2010-06-09T00:00:00.000Z","2010-06-10T00:00:00.000Z","2010-06-11T00:00:00.000Z","2010-06-12T00:00:00.000Z","2010-06-13T00:00:00.000Z","2010-06-14T00:00:00.000Z","2010-06-15T00:00:00.000Z","2010-06-16T00:00:00.000Z","2010-06-17T00:00:00.000Z","2010-06-18T00:00:00.000Z","2010-06-19T00:00:00.000Z","2010-06-20T00:00:00.000Z","2010-06-21T00:00:00.000Z","2010-06-22T00:00:00.000Z","2010-06-23T00:00:00.000Z","2010-06-24T00:00:00.000Z","2010-06-25T00:00:00.000Z","2010-06-26T00:00:00.000Z","2010-06-27T00:00:00.000Z","2010-06-28T00:00:00.000Z","2010-06-29T00:00:00.000Z","2010-06-30T00:00:00.000Z","2010-07-01T00:00:00.000Z","2010-07-02T00:00:00.000Z","2010-07-03T00:00:00.000Z","2010-07-04T00:00:00.000Z","2010-07-05T00:00:00.000Z","2010-07-06T00:00:00.000Z","2010-07-07T00:00:00.000Z","2010-07-08T00:00:00.000Z","2010-07-09T00:00:00.000Z","2010-07-10T00:00:00.000Z","2010-07-11T00:00:00.000Z","2010-07-12T00:00:00.000Z","2010-07-13T00:00:00.000Z","2010-07-14T00:00:00.000Z","2010-07-15T00:00:00.000Z","2010-07-16T00:00:00.000Z","2010-07-17T00:00:00.000Z","2010-07-18T00:00:00.000Z","2010-07-19T00:00:00.000Z","2010-07-20T00:00:00.000Z","2010-07-21T00:00:00.000Z","2010-07-22T00:00:00.000Z","2010-07-23T00:00:00.000Z","2010-07-24T00:00:00.000Z","2010-07-25T00:00:00.000Z","2010-07-26T00:00:00.000Z","2010-07-27T00:00:00.000Z","2010-07-28T00:00:00.000Z","2010-07-29T00:00:00.000Z","2010-07-30T00:00:00.000Z","2010-07-31T00:00:00.000Z","2010-08-01T00:00:00.000Z","2010-08-02T00:00:00.000Z","2010-08-03T00:00:00.000Z","2010-08-04T00:00:00.000Z","2010-08-05T00:00:00.000Z","2010-08-06T00:00:00.000Z","2010-08-07T00:00:00.000Z","2010-08-08T00:00:00.000Z","2010-08-09T00:00:00.000Z","2010-08-10T00:00:00.000Z","2010-08-11T00:00:00.000Z","2010-08-12T00:00:00.000Z","2010-08-13T00:00:00.000Z","2010-08-14T00:00:00.000Z","2010-08-15T00:00:00.000Z","2010-08-16T00:00:00.000Z","2010-08-17T00:00:00.000Z","2010-08-18T00:00:00.000Z","2010-08-19T00:00:00.000Z","2010-08-20T00:00:00.000Z","2010-08-21T00:00:00.000Z","2010-08-22T00:00:00.000Z","2010-08-23T00:00:00.000Z","2010-08-24T00:00:00.000Z","2010-08-25T00:00:00.000Z","2010-08-26T00:00:00.000Z","2010-08-27T00:00:00.000Z","2010-08-28T00:00:00.000Z","2010-08-29T00:00:00.000Z","2010-08-30T00:00:00.000Z","2010-08-31T00:00:00.000Z","2010-09-01T00:00:00.000Z","2010-09-02T00:00:00.000Z","2010-09-03T00:00:00.000Z","2010-09-04T00:00:00.000Z","2010-09-05T00:00:00.000Z","2010-09-06T00:00:00.000Z","2010-09-07T00:00:00.000Z","2010-09-08T00:00:00.000Z","2010-09-09T00:00:00.000Z","2010-09-10T00:00:00.000Z","2010-09-11T00:00:00.000Z","2010-09-12T00:00:00.000Z","2010-09-13T00:00:00.000Z","2010-09-14T00:00:00.000Z","2010-09-15T00:00:00.000Z","2010-09-16T00:00:00.000Z","2010-09-17T00:00:00.000Z","2010-09-18T00:00:00.000Z","2010-09-19T00:00:00.000Z","2010-09-20T00:00:00.000Z","2010-09-21T00:00:00.000Z","2010-09-22T00:00:00.000Z","2010-09-23T00:00:00.000Z","2010-09-24T00:00:00.000Z","2010-09-25T00:00:00.000Z","2010-09-26T00:00:00.000Z","2010-09-27T00:00:00.000Z","2010-09-28T00:00:00.000Z","2010-09-29T00:00:00.000Z","2010-09-30T00:00:00.000Z","2010-10-01T00:00:00.000Z","2010-10-02T00:00:00.000Z","2010-10-03T00:00:00.000Z","2010-10-04T00:00:00.000Z","2010-10-05T00:00:00.000Z","2010-10-06T00:00:00.000Z","2010-10-07T00:00:00.000Z","2010-10-08T00:00:00.000Z","2010-10-09T00:00:00.000Z","2010-10-10T00:00:00.000Z","2010-10-11T00:00:00.000Z","2010-10-12T00:00:00.000Z","2010-10-13T00:00:00.000Z","2010-10-14T00:00:00.000Z","2010-10-15T00:00:00.000Z","2010-10-16T00:00:00.000Z","2010-10-17T00:00:00.000Z","2010-10-18T00:00:00.000Z","2010-10-19T00:00:00.000Z","2010-10-20T00:00:00.000Z","2010-10-21T00:00:00.000Z","2010-10-22T00:00:00.000Z","2010-10-23T00:00:00.000Z","2010-10-24T00:00:00.000Z","2010-10-25T00:00:00.000Z","2010-10-26T00:00:00.000Z","2010-10-27T00:00:00.000Z","2010-10-28T00:00:00.000Z","2010-10-29T00:00:00.000Z","2010-10-30T00:00:00.000Z","2010-10-31T00:00:00.000Z","2010-11-01T00:00:00.000Z","2010-11-02T00:00:00.000Z","2010-11-03T00:00:00.000Z","2010-11-04T00:00:00.000Z","2010-11-05T00:00:00.000Z","2010-11-06T00:00:00.000Z","2010-11-07T00:00:00.000Z","2010-11-08T00:00:00.000Z","2010-11-09T00:00:00.000Z","2010-11-10T00:00:00.000Z","2010-11-11T00:00:00.000Z","2010-11-12T00:00:00.000Z","2010-11-13T00:00:00.000Z","2010-11-14T00:00:00.000Z","2010-11-15T00:00:00.000Z","2010-11-16T00:00:00.000Z","2010-11-17T00:00:00.000Z","2010-11-18T00:00:00.000Z","2010-11-19T00:00:00.000Z","2010-11-20T00:00:00.000Z","2010-11-21T00:00:00.000Z","2010-11-22T00:00:00.000Z","2010-11-23T00:00:00.000Z","2010-11-24T00:00:00.000Z","2010-11-25T00:00:00.000Z","2010-11-26T00:00:00.000Z","2010-11-27T00:00:00.000Z","2010-11-28T00:00:00.000Z","2010-11-29T00:00:00.000Z","2010-11-30T00:00:00.000Z","2010-12-01T00:00:00.000Z","2010-12-02T00:00:00.000Z","2010-12-03T00:00:00.000Z","2010-12-04T00:00:00.000Z","2010-12-05T00:00:00.000Z","2010-12-06T00:00:00.000Z","2010-12-07T00:00:00.000Z","2010-12-08T00:00:00.000Z","2010-12-09T00:00:00.000Z","2010-12-10T00:00:00.000Z","2010-12-11T00:00:00.000Z","2010-12-12T00:00:00.000Z","2010-12-13T00:00:00.000Z","2010-12-14T00:00:00.000Z","2010-12-15T00:00:00.000Z","2010-12-16T00:00:00.000Z","2010-12-17T00:00:00.000Z","2010-12-18T00:00:00.000Z","2010-12-19T00:00:00.000Z","2010-12-20T00:00:00.000Z","2010-12-21T00:00:00.000Z","2010-12-22T00:00:00.000Z","2010-12-23T00:00:00.000Z","2010-12-24T00:00:00.000Z","2010-12-25T00:00:00.000Z","2010-12-26T00:00:00.000Z","2010-12-27T00:00:00.000Z","2010-12-28T00:00:00.000Z","2010-12-29T00:00:00.000Z","2010-12-30T00:00:00.000Z","2010-12-31T00:00:00.000Z","2011-01-01T00:00:00.000Z","2011-01-02T00:00:00.000Z","2011-01-03T00:00:00.000Z","2011-01-04T00:00:00.000Z","2011-01-05T00:00:00.000Z","2011-01-06T00:00:00.000Z","2011-01-07T00:00:00.000Z","2011-01-08T00:00:00.000Z","2011-01-09T00:00:00.000Z","2011-01-10T00:00:00.000Z","2011-01-11T00:00:00.000Z","2011-01-12T00:00:00.000Z","2011-01-13T00:00:00.000Z","2011-01-14T00:00:00.000Z","2011-01-15T00:00:00.000Z","2011-01-16T00:00:00.000Z","2011-01-17T00:00:00.000Z","2011-01-18T00:00:00.000Z","2011-01-19T00:00:00.000Z","2011-01-20T00:00:00.000Z","2011-01-21T00:00:00.000Z","2011-01-22T00:00:00.000Z","2011-01-23T00:00:00.000Z","2011-01-24T00:00:00.000Z","2011-01-25T00:00:00.000Z","2011-01-26T00:00:00.000Z","2011-01-27T00:00:00.000Z","2011-01-28T00:00:00.000Z","2011-01-29T00:00:00.000Z","2011-01-30T00:00:00.000Z","2011-01-31T00:00:00.000Z","2011-02-01T00:00:00.000Z","2011-02-02T00:00:00.000Z","2011-02-03T00:00:00.000Z","2011-02-04T00:00:00.000Z","2011-02-05T00:00:00.000Z","2011-02-06T00:00:00.000Z","2011-02-07T00:00:00.000Z","2011-02-08T00:00:00.000Z","2011-02-09T00:00:00.000Z","2011-02-10T00:00:00.000Z","2011-02-11T00:00:00.000Z","2011-02-12T00:00:00.000Z","2011-02-13T00:00:00.000Z","2011-02-14T00:00:00.000Z","2011-02-15T00:00:00.000Z","2011-02-16T00:00:00.000Z","2011-02-17T00:00:00.000Z","2011-02-18T00:00:00.000Z","2011-02-19T00:00:00.000Z","2011-02-20T00:00:00.000Z","2011-02-21T00:00:00.000Z","2011-02-22T00:00:00.000Z","2011-02-23T00:00:00.000Z","2011-02-24T00:00:00.000Z","2011-02-25T00:00:00.000Z","2011-02-26T00:00:00.000Z","2011-02-27T00:00:00.000Z","2011-02-28T00:00:00.000Z","2011-03-01T00:00:00.000Z","2011-03-02T00:00:00.000Z","2011-03-03T00:00:00.000Z","2011-03-04T00:00:00.000Z","2011-03-05T00:00:00.000Z","2011-03-06T00:00:00.000Z","2011-03-07T00:00:00.000Z","2011-03-08T00:00:00.000Z","2011-03-09T00:00:00.000Z","2011-03-10T00:00:00.000Z","2011-03-11T00:00:00.000Z","2011-03-12T00:00:00.000Z","2011-03-13T00:00:00.000Z","2011-03-14T00:00:00.000Z","2011-03-15T00:00:00.000Z","2011-03-16T00:00:00.000Z","2011-03-17T00:00:00.000Z","2011-03-18T00:00:00.000Z","2011-03-19T00:00:00.000Z","2011-03-20T00:00:00.000Z","2011-03-21T00:00:00.000Z","2011-03-22T00:00:00.000Z","2011-03-23T00:00:00.000Z","2011-03-24T00:00:00.000Z","2011-03-25T00:00:00.000Z","2011-03-26T00:00:00.000Z","2011-03-27T00:00:00.000Z","2011-03-28T00:00:00.000Z","2011-03-29T00:00:00.000Z","2011-03-30T00:00:00.000Z","2011-03-31T00:00:00.000Z","2011-04-01T00:00:00.000Z","2011-04-02T00:00:00.000Z","2011-04-03T00:00:00.000Z","2011-04-04T00:00:00.000Z","2011-04-05T00:00:00.000Z","2011-04-06T00:00:00.000Z","2011-04-07T00:00:00.000Z","2011-04-08T00:00:00.000Z","2011-04-09T00:00:00.000Z","2011-04-10T00:00:00.000Z","2011-04-11T00:00:00.000Z","2011-04-12T00:00:00.000Z","2011-04-13T00:00:00.000Z","2011-04-14T00:00:00.000Z","2011-04-15T00:00:00.000Z","2011-04-16T00:00:00.000Z","2011-04-17T00:00:00.000Z","2011-04-18T00:00:00.000Z","2011-04-19T00:00:00.000Z","2011-04-20T00:00:00.000Z","2011-04-21T00:00:00.000Z","2011-04-22T00:00:00.000Z","2011-04-23T00:00:00.000Z","2011-04-24T00:00:00.000Z","2011-04-25T00:00:00.000Z","2011-04-26T00:00:00.000Z","2011-04-27T00:00:00.000Z","2011-04-28T00:00:00.000Z","2011-04-29T00:00:00.000Z","2011-04-30T00:00:00.000Z","2011-05-01T00:00:00.000Z","2011-05-02T00:00:00.000Z","2011-05-03T00:00:00.000Z","2011-05-04T00:00:00.000Z","2011-05-05T00:00:00.000Z","2011-05-06T00:00:00.000Z","2011-05-07T00:00:00.000Z","2011-05-08T00:00:00.000Z","2011-05-09T00:00:00.000Z","2011-05-10T00:00:00.000Z","2011-05-11T00:00:00.000Z","2011-05-12T00:00:00.000Z","2011-05-13T00:00:00.000Z","2011-05-14T00:00:00.000Z","2011-05-15T00:00:00.000Z","2011-05-16T00:00:00.000Z","2011-05-17T00:00:00.000Z","2011-05-18T00:00:00.000Z","2011-05-19T00:00:00.000Z","2011-05-20T00:00:00.000Z","2011-05-21T00:00:00.000Z","2011-05-22T00:00:00.000Z","2011-05-23T00:00:00.000Z","2011-05-24T00:00:00.000Z","2011-05-25T00:00:00.000Z","2011-05-26T00:00:00.000Z","2011-05-27T00:00:00.000Z","2011-05-28T00:00:00.000Z","2011-05-29T00:00:00.000Z","2011-05-30T00:00:00.000Z","2011-05-31T00:00:00.000Z","2011-06-01T00:00:00.000Z","2011-06-02T00:00:00.000Z","2011-06-03T00:00:00.000Z","2011-06-04T00:00:00.000Z","2011-06-05T00:00:00.000Z","2011-06-06T00:00:00.000Z","2011-06-07T00:00:00.000Z","2011-06-08T00:00:00.000Z","2011-06-09T00:00:00.000Z","2011-06-10T00:00:00.000Z","2011-06-11T00:00:00.000Z","2011-06-12T00:00:00.000Z","2011-06-13T00:00:00.000Z","2011-06-14T00:00:00.000Z","2011-06-15T00:00:00.000Z","2011-06-16T00:00:00.000Z","2011-06-17T00:00:00.000Z","2011-06-18T00:00:00.000Z","2011-06-19T00:00:00.000Z","2011-06-20T00:00:00.000Z","2011-06-21T00:00:00.000Z","2011-06-22T00:00:00.000Z","2011-06-23T00:00:00.000Z","2011-06-24T00:00:00.000Z","2011-06-25T00:00:00.000Z","2011-06-26T00:00:00.000Z","2011-06-27T00:00:00.000Z","2011-06-28T00:00:00.000Z","2011-06-29T00:00:00.000Z","2011-06-30T00:00:00.000Z","2011-07-01T00:00:00.000Z","2011-07-02T00:00:00.000Z","2011-07-03T00:00:00.000Z","2011-07-04T00:00:00.000Z","2011-07-05T00:00:00.000Z","2011-07-06T00:00:00.000Z","2011-07-07T00:00:00.000Z","2011-07-08T00:00:00.000Z","2011-07-09T00:00:00.000Z","2011-07-10T00:00:00.000Z","2011-07-11T00:00:00.000Z","2011-07-12T00:00:00.000Z","2011-07-13T00:00:00.000Z","2011-07-14T00:00:00.000Z","2011-07-15T00:00:00.000Z","2011-07-16T00:00:00.000Z","2011-07-17T00:00:00.000Z","2011-07-18T00:00:00.000Z","2011-07-19T00:00:00.000Z","2011-07-20T00:00:00.000Z","2011-07-21T00:00:00.000Z","2011-07-22T00:00:00.000Z","2011-07-23T00:00:00.000Z","2011-07-24T00:00:00.000Z","2011-07-25T00:00:00.000Z","2011-07-26T00:00:00.000Z","2011-07-27T00:00:00.000Z","2011-07-28T00:00:00.000Z","2011-07-29T00:00:00.000Z","2011-07-30T00:00:00.000Z","2011-07-31T00:00:00.000Z","2011-08-01T00:00:00.000Z","2011-08-02T00:00:00.000Z","2011-08-03T00:00:00.000Z","2011-08-04T00:00:00.000Z","2011-08-05T00:00:00.000Z","2011-08-06T00:00:00.000Z","2011-08-07T00:00:00.000Z","2011-08-08T00:00:00.000Z","2011-08-09T00:00:00.000Z","2011-08-10T00:00:00.000Z","2011-08-11T00:00:00.000Z","2011-08-12T00:00:00.000Z","2011-08-13T00:00:00.000Z","2011-08-14T00:00:00.000Z","2011-08-15T00:00:00.000Z","2011-08-16T00:00:00.000Z","2011-08-17T00:00:00.000Z","2011-08-18T00:00:00.000Z","2011-08-19T00:00:00.000Z","2011-08-20T00:00:00.000Z","2011-08-21T00:00:00.000Z","2011-08-22T00:00:00.000Z","2011-08-23T00:00:00.000Z","2011-08-24T00:00:00.000Z","2011-08-25T00:00:00.000Z","2011-08-26T00:00:00.000Z","2011-08-27T00:00:00.000Z","2011-08-28T00:00:00.000Z","2011-08-29T00:00:00.000Z","2011-08-30T00:00:00.000Z","2011-08-31T00:00:00.000Z","2011-09-01T00:00:00.000Z","2011-09-02T00:00:00.000Z","2011-09-03T00:00:00.000Z","2011-09-04T00:00:00.000Z","2011-09-05T00:00:00.000Z","2011-09-06T00:00:00.000Z","2011-09-07T00:00:00.000Z","2011-09-08T00:00:00.000Z","2011-09-09T00:00:00.000Z","2011-09-10T00:00:00.000Z","2011-09-11T00:00:00.000Z","2011-09-12T00:00:00.000Z","2011-09-13T00:00:00.000Z","2011-09-14T00:00:00.000Z","2011-09-15T00:00:00.000Z","2011-09-16T00:00:00.000Z","2011-09-17T00:00:00.000Z","2011-09-18T00:00:00.000Z","2011-09-19T00:00:00.000Z","2011-09-20T00:00:00.000Z","2011-09-21T00:00:00.000Z","2011-09-22T00:00:00.000Z","2011-09-23T00:00:00.000Z","2011-09-24T00:00:00.000Z","2011-09-25T00:00:00.000Z","2011-09-26T00:00:00.000Z","2011-09-27T00:00:00.000Z","2011-09-28T00:00:00.000Z","2011-09-29T00:00:00.000Z","2011-09-30T00:00:00.000Z","2011-10-01T00:00:00.000Z"],[0.000181160434782609,0,4.62966666666667e-05,0.000150464166666667,2.31483333333333e-05,0,4.62966666666667e-05,0.000243057083333333,2.31483333333333e-05,3.47225e-05,0,0,0.000150464166666667,0,0,0,0.000162038333333333,0,0,0,0,0,0,0,0,0,0,2.31483333333333e-05,0.000254631666666667,1.15741666666667e-05,0,0,0,0,0,0.00013889,0,0,0,0,0,0.000162038333333333,6.9445e-05,0,0,5.78708333333333e-05,0.000324074166666667,9.25933333333333e-05,3.47225e-05,0,2.31483333333333e-05,0,9.25933333333333e-05,0.000254631666666667,0.000162038333333333,0,0,0,5.78708333333333e-05,0,0,0,0,0,0,0,0,0,0,0,0,4.62966666666667e-05,0,0,0,0,0.00013889,2.31483333333333e-05,0,0,0,0,0.0001041675,1.15741666666667e-05,1.15741666666667e-05,4.62966666666667e-05,0.000115741666666667,8.10191666666667e-05,1.15741666666667e-05,3.47225e-05,6.9445e-05,0,0,3.47225e-05,0,0,0,0,0,0,0,0.000162037916666667,1.15741666666667e-05,0.00018518625,0.000150464166666667,2.31483333333333e-05,3.47225e-05,1.15741666666667e-05,0,0.000370374583333333,0.000231483333333333,0.000185186666666667,5.78708333333333e-05,0,0,6.9445e-05,4.62966666666667e-05,0,0,0,0,0,0,0,0,0,8.10191666666667e-05,0,1.15741666666667e-05,0,0,0,0,0,5.78708333333333e-05,3.47225e-05,0,0,0.0002430575,4.62966666666667e-05,0,0,0,0,9.25929166666667e-05,0.000266206666666667,9.25929166666667e-05,3.47225e-05,2.31483333333333e-05,0.000104168333333333,0.000138889166666667,0.000231482916666667,0,1.15741666666667e-05,1.15741666666667e-05,0,2.31483333333333e-05,0,0,0,3.47225e-05,0,0,0.0001041675,0.000150462916666667,0.000104165833333333,1.15741666666667e-05,2.31483333333333e-05,0,0,0,0.000127315833333333,0.000208335,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,1.15741666666667e-05,0.000127314583333333,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4.62966666666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,2.9398e-05,2.9398e-05,0,5.8796e-05,0,0,0,0,0,0,0,0,0,0,2.9398e-05,0,0,0,0,2.9398e-05,2.9398e-05,0,5.8796e-05,0,0.00026458,5.8796e-05,2.9398e-05,2.9398e-05,0.00026458,0,5.8796e-05,8.8194e-05,0,0,0,0,0,2.9398e-05,0,0,0,0,0,5.8796e-05,0,0,0,0,0,0,0,2.9398e-05,5.8796e-05,0,0,2.9398e-05,0,2.9398e-05,0,0,0,0,0,0,0,0,0,0,0,5.8796e-05,0.00020579,0.00035278,0,0,0,0,2.9398e-05,0.00011759,0,0,0,0,0,0,0,0.00011759,0.00058796,0.00014699,8.8194e-05,5.8796e-05,2.9398e-05,0,5.8796e-05,0,2.9398e-05,2.9398e-05,0.00014699,0,2.44983333333333e-06,0,0,0,6.94445833333333e-05,2.31483333333333e-05,1.15741666666667e-05,0,0,0,2.31483333333333e-05,2.31483333333333e-05,0.00011574125,0,0,0,3.47225e-05,3.47225e-05,5.63461666666667e-05,5.8796e-05,5.8796e-05,2.9398e-05,4.4097e-05,0,0.000138889583333333,6.9445e-05,0.000254631666666667,0.000509260833333333,0,0,9.25929166666667e-05,3.47225e-05,0,0,0,0,0,0,1.15741666666667e-05,0.00019676125,1.15741666666667e-05,1.15741666666667e-05,4.62966666666667e-05,8.10191666666667e-05,1.15741666666667e-05,0,0,0,0,0,0.000219907916666667,0.000289353333333333,0,0,0,0,9.25933333333333e-05,0.000150464166666667,0.00013889,0,0,0,0,0,0,2.31483333333333e-05,1.15741666666667e-05,0,8.10191666666667e-05,0.000150464166666667,1.15741666666667e-05,0,0,0,0,2.31483333333333e-05,1.15741666666667e-05,0,0,0,1.15741666666667e-05,0,2.31483333333333e-05,2.31483333333333e-05,0,0,0,0,0,0.000567130833333333,3.47225e-05,1.15741666666667e-05,3.47225e-05,0,9.25933333333333e-05,0.000439815833333333,5.78708333333333e-05,0,4.62966666666667e-05,0.000254631666666667,9.25933333333333e-05,0.000277779583333333,0.00040509375,6.94445833333333e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,4.62966666666667e-05,4.62966666666667e-05,4.62966666666667e-05,3.907225e-05,3.47225e-05,0.000173612083333333,0,1.15741666666667e-05,0,0,0,1.15741666666667e-05,0.000185186666666667,4.62966666666667e-05,1.15741666666667e-05,0,3.47225e-05,0.000243057083333333,8.101875e-05,0,0,8.10191666666667e-05,0,3.47225e-05,0.000127315833333333,0.0001041675,1.15741666666667e-05,5.78708333333333e-05,2.31483333333333e-05,0,0,5.78708333333333e-05,1.15741666666667e-05,0,8.10191666666667e-05,1.15741666666667e-05,1.15741666666667e-05,0,1.15741666666667e-05,0,0,1.15741666666667e-05,0,0,0,0,0,1.15741666666667e-05,0,2.31483333333333e-05,3.47225e-05,6.9445e-05,0,0.00018518625,0.0001041675,1.15741666666667e-05,0.00032407625,5.78708333333333e-05,9.25933333333333e-05,1.15741666666667e-05,0,0,6.9445e-05,0.000300927916666667,5.78708333333333e-05,0,0,0,0.000127314583333333,0.000150463333333333,0,0,9.25929166666667e-05,0,0.000173612083333333,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0.000277777916666667,5.78704166666667e-05,0,0.000104167083333333,0.000138889583333333,0.000127314583333333,0,5.78708333333333e-05,4.62958333333333e-05,1.15741666666667e-05,5.78704166666667e-05,1.15741666666667e-05,4.62966666666667e-05,2.31483333333333e-05,0,0,0,4.62966666666667e-05,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2.31483333333333e-05,0,0,0,9.25925e-05,0,0,0,0,0,0,4.62966666666667e-05,0.00010416625,5.78708333333333e-05,0,0,0,0,0,0,0,0,0,0,0.000219909166666667,1.15741666666667e-05,5.78704166666667e-05,0,0,5.78704166666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5.78704166666667e-05,0,0.00030092875,2.31483333333333e-05,0,0,0,1.15741666666667e-05,9.25925e-05,0,4.62966666666667e-05,3.47225e-05,0.000127315416666667,4.62966666666667e-05,0.00015046375,2.31483333333333e-05,0,0.0001041675,1.15741666666667e-05,0.00015046375,0,3.47225e-05,0,1.15741666666667e-05,6.94445833333333e-05,0,0,1.15741666666667e-05,4.62958333333333e-05,2.31483333333333e-05,0,2.31483333333333e-05,0,0.000104168333333333,0,3.47225e-05,0,0,0,0,0,1.15741666666667e-05,8.101875e-05,0.000138889583333333,3.47225e-05,1.15741666666667e-05,0,3.47225e-05,0.000127314166666667,0,0,0,0,1.15741666666667e-05,0,1.15741666666667e-05,0,0,0,0,8.101875e-05,0.00018518625,0,0,0,0,8.101875e-05,0.0001041675,0.000312500833333333,0,0,1.15741666666667e-05,0,0.00011574125,0,0,0,0.000115740833333333,0.000335649166666667,3.47225e-05,0,0,0,0,0,1.15741666666667e-05,0,0.000162036666666667,0.000185185416666667,0,0,0,0,0,0,1.15741666666667e-05,0,1.15741666666667e-05,0.0001041675,9.25933333333333e-05,2.31483333333333e-05,0,1.15741666666667e-05,3.47225e-05,0,0,0,0,0,0,0,0.000150463333333333,0.00013889,0,0.000185185833333333,4.62966666666667e-05,0,1.15741666666667e-05,0,0,0,0,0,0,0,0,4.62966666666667e-05,9.25933333333333e-05,2.31483333333333e-05,5.78708333333333e-05,0.0001041675,3.47225e-05,6.9445e-05,0.000266205,5.78708333333333e-05,0,4.62966666666667e-05,0.000127315416666667,0,0,0,0,0,0.0001041675,5.78708333333333e-05,0,0,1.15741666666667e-05,0.000115741666666667,0.000196760833333333,2.31483333333333e-05,0,1.15741666666667e-05,9.25933333333333e-05,0,0,4.62966666666667e-05,8.10191666666667e-05,1.15741666666667e-05,5.78708333333333e-05,4.62966666666667e-05,4.62966666666667e-05,0.000231482916666667,1.15741666666667e-05,3.47225e-05,1.15741666666667e-05,0,1.15741666666667e-05,0,8.10191666666667e-05,3.47225e-05,0,0,6.9445e-05,0,0,6.9445e-05,3.47225e-05,0,0,0.00013889,3.47225e-05,0,0,0,0,0.00011574125,0.0001041675,4.62966666666667e-05,0.00013889,0.000231483333333333,0,1.15741666666667e-05,2.31483333333333e-05,0.000474537083333333,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,0,3.47225e-05,4.62966666666667e-05,1.15741666666667e-05,3.47225e-05,0,1.15741666666667e-05,2.31483333333333e-05,0,0,0,0,0,0,5.78708333333333e-05,0,1.15741666666667e-05,0,2.31483333333333e-05,0,0,0,0.000324075,5.78708333333333e-05,0.00024305625,0.000127315833333333,0.000138889166666667,0.0001041675,1.15741666666667e-05,2.31483333333333e-05,2.31483333333333e-05,8.10191666666667e-05,0.000254631666666667,6.9445e-05,5.78708333333333e-05,2.31483333333333e-05,4.62966666666667e-05,1.15741666666667e-05,0,0,4.62966666666667e-05,0,0.000127315833333333,0.000532407916666667,0.0001041675,0,0.000138889583333333,2.31483333333333e-05,0,1.15741666666667e-05,0.000254630833333333,0.00025463125,9.25929166666667e-05,0,0,0,2.31483333333333e-05,0,0,2.31483333333333e-05,0.00015046375,0,0,0,0,0,0,0,0,1.15741666666667e-05,1.15741666666667e-05,0,0,2.31483333333333e-05,0,0,0,4.62966666666667e-05,0,0,0,1.15741666666667e-05,0,3.47225e-05,0.000138889583333333,0,0,0,0,0,0,0,0,0,0,1.15741666666667e-05,0,0.000150462916666667,0,0,0,0,0,2.31483333333333e-05,0,0,0,0,0,0,0,0,0,0,0.00015046375,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1.15741666666667e-05,3.47220833333333e-05,1.15741666666667e-05,0,0,0.000150465,1.15741666666667e-05,0,8.101875e-05,0.000196757083333333,0.000138889166666667,4.62966666666667e-05,0.000173611666666667,0,0.000127315833333333,1.15741666666667e-05,0,0,0,0,2.31483333333333e-05,3.47225e-05,0,1.15741666666667e-05,0,0.0001620375,0,1.15741666666667e-05,0,0.000115742083333333,0,2.31483333333333e-05,0,6.94441666666667e-05,1.15741666666667e-05,0.00047454,9.25929166666667e-05,2.31483333333333e-05,0,0.000127315416666667,0,0,2.31483333333333e-05,0,0,4.62966666666667e-05,1.15741666666667e-05,6.94445833333333e-05,2.31483333333333e-05,0,0.000196760833333333,1.15741666666667e-05,6.9445e-05,0,1.15741666666667e-05,1.15741666666667e-05,0,1.15741666666667e-05,1.15741666666667e-05,2.31483333333333e-05,5.78708333333333e-05,0.000173611666666667,0,0,0,0,0,1.15741666666667e-05,0,0,0,0,1.15741666666667e-05,2.31483333333333e-05,0.000162038333333333,9.25925e-05,4.62966666666667e-05,0,0,3.47220833333333e-05,0.000138888333333333,0.000150463333333333,0,0,0,0,0.000162037083333333,0.000196760416666667,0.000289352083333333,0,0,0,0,0,0,0,0,0,0,8.102e-05,5.78704166666667e-05,0.00019676,0.00048610875,0.00013889,4.62966666666667e-05,8.10191666666667e-05,0.000115741666666667,0,0,0,0.0002199075,3.47225e-05,0,0.000254629583333333,4.62966666666667e-05,0,3.47220833333333e-05,8.10191666666667e-05,0,0,0,0.000115739166666667,9.25933333333333e-05,0,0,0,0,0,0,0,0,0,1.15741666666667e-05,0,0,0,0,0,1.15741666666667e-05,0.00033564875,8.10191666666667e-05,0.000219907916666667,0,0,0,0,0,0,0,0,0,3.47225e-05,3.47225e-05,1.15741666666667e-05,0.000150464166666667,0.000196760833333333,4.62966666666667e-05,0,0,0,0,0,0,0,0,0,0,4.629625e-05,0.000196760416666667,1.15741666666667e-05,1.15741666666667e-05,0,0,0,0.000196760833333333,6.9445e-05,0.000266204583333333,0.0001041675,0,0,0,3.47225e-05,0,0,1.15741666666667e-05,4.62966666666667e-05,1.15741666666667e-05,0,0,0,0,0,1.15741666666667e-05,0.000115741666666667,0,0,0,0,0,0,0.0004282425,0.000127315833333333,5.78708333333333e-05,0,0,0,0,0,2.31483333333333e-05,1.15741666666667e-05,0,0,0,0,0,0,0,0,4.62966666666667e-05,0.000254631666666667,4.62966666666667e-05,0,0,0,0,0,0,0,0,0,0.000277779583333333,8.10191666666667e-05,9.25929166666667e-05,9.25933333333333e-05,1.15741666666667e-05,6.9445e-05,0,1.15741666666667e-05,8.10191666666667e-05,1.15741666666667e-05,0,0,9.25933333333333e-05,9.25933333333333e-05,0,5.78708333333333e-05,5.78708333333333e-05,0.000185186666666667,5.78708333333333e-05,9.25933333333333e-05,0,1.15741666666667e-05,0,0,0,6.9445e-05,0,8.10183333333333e-05,0,0,0,0,0,0,0,0,0,0,0.00015046375,8.10191666666667e-05,4.62966666666667e-05,4.62966666666667e-05,0,0,4.62966666666667e-05,0.000115741666666667,6.94445833333333e-05,0.000138889583333333,0,0,0,0,0,0,0,0.0001041675,0.000451386666666667,4.62966666666667e-05,0.000138889166666667,0,0.000208334583333333,4.62966666666667e-05,0,9.25929166666667e-05,5.78708333333333e-05,0,2.31483333333333e-05,0,0,0.00017361125,3.47225e-05,0.000150464166666667,0.000254630833333333,0,0,0,0,0,0,0.000185185416666667,0.000162037083333333,0,0.000115741666666667,0.000162038333333333,0.0001041675,1.15741666666667e-05,0,0,0,0,0,0,1.15741666666667e-05,0,4.62966666666667e-05,1.15741666666667e-05,0,1.15741666666667e-05,8.101875e-05,1.15741666666667e-05,0.00018518625,8.10191666666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,1.15741666666667e-05,0,0,0,0,2.31483333333333e-05,0.000150464166666667,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1.15741666666667e-05,0,0,0,0,0,0,6.94441666666667e-05,0,0,0,0,0,1.15741666666667e-05,0.000115740833333333,0,0,0,0,1.15741666666667e-05,3.96221666666667e-05,5.787e-05,9.25933333333333e-05,8.10191666666667e-05,6.9445e-05,0.000173610833333333,1.15741666666667e-05,0.0001041675,1.15741666666667e-05,0.0001967575,8.101875e-05,0,0,2.31483333333333e-05,0,1.15741666666667e-05,9.25933333333333e-05,3.47225e-05,0.000173609166666667,5.78704166666667e-05,0.000104168333333333,0,0,0,2.31483333333333e-05,0,2.31483333333333e-05,0.000115739166666667,0,0.000138889166666667,0,1.15741666666667e-05,1.15741666666667e-05,0,0,0,0,1.15741666666667e-05,0,1.15741666666667e-05,0.000127314166666667,0,3.47225e-05,0,8.101875e-05,0,1.15741666666667e-05,2.31483333333333e-05,6.9445e-05,1.15741666666667e-05,2.31483333333333e-05,0,0,0,1.15741666666667e-05,1.15741666666667e-05,1.15741666666667e-05,0,0,1.15741666666667e-05,8.101875e-05,0.000300926666666667,0,0,5.78704166666667e-05,0,8.101875e-05,0.000590277916666667,0,0,0,0,0,3.47225e-05,0,4.62966666666667e-05,8.10183333333333e-05,0,3.47225e-05,0.00018518625,0.000115741666666667,0,0,0,0,0,0,0.000138889166666667,0.00011574125,0,2.31483333333333e-05,0.000266204583333333,4.62966666666667e-05,1.15741666666667e-05,1.15741666666667e-05,0.0002314825,1.15741666666667e-05,0,0,0,0,0,0,0,0,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.47225e-05,0,0.00015046375,0,0,0,0,0,0,0.000405095,0.00054398375,4.62966666666667e-05,1.15741666666667e-05,0,2.31483333333333e-05,0.0001041675,0.000439816666666667,0.000162038333333333,0,0.00025463125,6.9445e-05,0,0,1.15741666666667e-05,0,0,0,1.15741666666667e-05,2.31483333333333e-05,1.15741666666667e-05,8.10191666666667e-05,2.31483333333333e-05,5.78708333333333e-05,1.15741666666667e-05,0.000150464166666667,1.15741666666667e-05,5.78708333333333e-05,0,1.15741666666667e-05,5.78708333333333e-05,0.0001041675,1.15741666666667e-05,0,0,4.62966666666667e-05,0.000405094583333333,0.0002430575,0.000185186666666667,4.62966666666667e-05,0.000208335,1.15741666666667e-05,0.000115741666666667,1.15741666666667e-05,0,0,1.15741666666667e-05,2.31483333333333e-05,1.15741666666667e-05,4.62966666666667e-05,1.15741666666667e-05,0,0,0,0,0.00015046375,2.31483333333333e-05,0,0.0002199075,0.000243057083333333,9.25933333333333e-05,0.000150464166666667,1.15741666666667e-05,0.000127315833333333,8.10191666666667e-05,0.000196760833333333,8.10191666666667e-05,1.15741666666667e-05,0.0001041675,8.10191666666667e-05,9.25929166666667e-05,0,0,0,0,0,0.000474537916666667,6.9445e-05,4.62966666666667e-05,5.78708333333333e-05,1.15741666666667e-05,0,2.31483333333333e-05,8.10191666666667e-05,5.78708333333333e-05,6.9445e-05,0.000115741666666667,0.000208334166666667,5.78708333333333e-05,0,0,0,0,8.10191666666667e-05,1.15741666666667e-05,5.78708333333333e-05,5.78708333333333e-05,0,0,3.47225e-05,0,2.31483333333333e-05,0,4.62966666666667e-05,2.31483333333333e-05,5.78708333333333e-05,1.15741666666667e-05,2.31483333333333e-05,4.62966666666667e-05,1.15741666666667e-05,0,4.62966666666667e-05,1.15741666666667e-05,0,0,0,0,0,0,0,4.62966666666667e-05,0.00011574125,0.00015046375,0,0,0.00013889,1.15741666666667e-05,8.10191666666667e-05,2.31483333333333e-05,0.00013889,0,0.00018518625,0.00025463125,0.000312502083333333,0,0,0,1.15741666666667e-05,2.31483333333333e-05,2.31483333333333e-05,0,0,0,0,0,0,1.15741666666667e-05,0,0,0,0,0,0,0.0001736125,4.62966666666667e-05,0,0,0,0,1.15741666666667e-05,0.00017361125,1.15741666666667e-05,0.000185185416666667,0,1.15741666666667e-05,0.000266204583333333,3.47225e-05,8.10191666666667e-05,0,0,0,0,0,1.15741666666667e-05,2.31483333333333e-05,0.00015046375,6.9445e-05,0,0,0,0,0,0,0,0,0,0,3.47225e-05,0.00015046375,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1.15741666666667e-05,4.629625e-05,1.15741666666667e-05,0,0,0,0,0,0,1.15741666666667e-05,1.15741666666667e-05,8.101875e-05,8.10191666666667e-05,2.31483333333333e-05,2.31483333333333e-05,5.78704166666667e-05,0.000138890833333333,1.15741666666667e-05,8.101875e-05,0,0.000127315,0.000127316666666667,0,0,0,0,0,0,0,0,0,0.000104166666666667,5.78708333333333e-05,9.25929166666667e-05,0.0001273125,4.62966666666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.47225e-05,1.15741666666667e-05,0,0,0,3.47220833333333e-05,0.000358797083333333,0,0,0,0,0,0,0,0,2.31483333333333e-05,0.000173611666666667,0.00013889,6.9445e-05,0,0,0,1.15741666666667e-05,5.787e-05,1.15741666666667e-05,0,0,1.15741666666667e-05,0,0,0,0,1.15741666666667e-05,0,5.78708333333333e-05,1.15741666666667e-05,0,0,0,4.62966666666667e-05,0.0001041675,0.000104167083333333,3.47225e-05,0,0,0,0,0.00047453875,3.47225e-05,0,2.31483333333333e-05,1.15741666666667e-05,0,0,0,0,1.15741666666667e-05,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,0,4.62966666666667e-05,3.47225e-05,0.000138889583333333,0.000358797083333333,1.15741666666667e-05,0,0,0,5.78708333333333e-05,0.000115741666666667,0,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0001041675,8.10191666666667e-05,0.000150464166666667,0.000231483333333333,3.47225e-05,2.31483333333333e-05,0.0001041675,1.15741666666667e-05,0,0,0,0.000312500416666667,3.47225e-05,0,0,0,0.000196760833333333,0.000138889583333333,6.9445e-05,0.00015046375,0.000127315833333333,0.0003472225,0,0,0,0.000185186666666667,8.10191666666667e-05,5.78708333333333e-05,0.000300928333333333,0.000150464166666667,4.62966666666667e-05,0,0,1.15741666666667e-05,0,0,0,6.9445e-05,6.9445e-05,3.47225e-05,9.25933333333333e-05,2.31483333333333e-05,0,6.9445e-05,0,1.15741666666667e-05,9.25933333333333e-05,0,0,0,0,0,0,0,0,0,0,0.000127315833333333,6.9445e-05,5.78708333333333e-05,0.000231482083333333,2.31483333333333e-05,5.78708333333333e-05,3.47225e-05,1.15741666666667e-05,0,0,0,0,0,0,0,0,8.10183333333333e-05,0.000173612083333333,0.000127315833333333,1.15741666666667e-05,2.31483333333333e-05,3.47225e-05,2.31483333333333e-05,1.15741666666667e-05,0,5.78708333333333e-05,9.25933333333333e-05,0,0,0,0,0.00033565,2.31483333333333e-05,0,0,0,0,0,0,0,0,0,0,8.10191666666667e-05,1.15741666666667e-05,6.9445e-05,0.000150463333333333,1.15741666666667e-05,0,0,3.47225e-05,0,0,0,0,0,0,0,3.47225e-05,0.000231482916666667,8.10191666666667e-05,9.25933333333333e-05,0.00011574125,1.15741666666667e-05,1.15741666666667e-05,1.15741666666667e-05,0.000196760833333333,8.10191666666667e-05,0.0001736125,0,0.000150464166666667,0.00013889,2.31483333333333e-05,0,0,0,0.000104167083333333,3.47225e-05,0.0002893525,0.0002430575,0,3.47225e-05,0.000231483333333333,0.0001041675,0.000127315833333333,4.62966666666667e-05,0,0,0,0,0,0,0,3.47225e-05,3.47225e-05,0,0,0,0,0.00019676,0.000127315833333333,9.25933333333333e-05,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,2.31483333333333e-05,1.15741666666667e-05,3.47225e-05,4.62966666666667e-05,6.9445e-05,0.000115741666666667,6.9445e-05,4.62966666666667e-05,0,1.15741666666667e-05,1.15741666666667e-05,2.31483333333333e-05,5.78708333333333e-05,0,4.629625e-05,3.47225e-05,0,1.15741666666667e-05,0,0,0,2.31483333333333e-05,0.0001620375,4.62966666666667e-05,0,0.00013888875,1.15741666666667e-05,1.15741666666667e-05,0,1.15741666666667e-05,0,0,9.25925e-05,0,0,0.000115739166666667,2.31483333333333e-05,0.000312500833333333,0.000127315,1.15741666666667e-05,0,0,0,0,3.47225e-05,1.15741666666667e-05,0,0,1.15741666666667e-05,0,0,0,0,4.62958333333333e-05,0.00020833375,1.15741666666667e-05,0,0,0,0,0,0,5.787e-05,0.000104166666666667,0,0,0,8.10191666666667e-05,0.00019676,0,2.31483333333333e-05,6.94445833333333e-05,8.101875e-05,1.15741666666667e-05,0,1.15741666666667e-05,0,0,0,1.15741666666667e-05,1.15741666666667e-05,0,0,0,0,0,4.62966666666667e-05,9.25929166666667e-05,0,0,0,0,0,0,0,0,5.78704166666667e-05,0.000127315833333333,0,0,0,0,0,3.47225e-05,0,0,4.629625e-05,1.15741666666667e-05,2.31483333333333e-05,1.15741666666667e-05,3.47225e-05,0,2.31483333333333e-05,4.62966666666667e-05,0.0001041675,0,6.94445833333333e-05,0.000127315,3.47225e-05,4.62966666666667e-05,8.101875e-05,2.31483333333333e-05,5.78704166666667e-05,5.78704166666667e-05,0.000104167083333333,0,0,0,0,0,0,0,0,0,9.25929166666667e-05,0,0,2.31483333333333e-05,0.000208335,0.000127315833333333,0,1.15741666666667e-05,0,0,0,0,0,0.000243054583333333,1.15741666666667e-05,0,0,0,0,1.15741666666667e-05,9.25929166666667e-05,0.000115741666666667,0,0,8.10191666666667e-05,9.25920833333333e-05,0,4.62966666666667e-05,0.000104166666666667,5.78708333333333e-05,2.31483333333333e-05,0,0,0,0,0,0,0,0,0,0,0,0,2.31483333333333e-05,0.0002199075,0.0001736125,6.9445e-05,0,0,0,0,0,0,0,4.62966666666667e-05,0,0,0,0,0,0,0,0,3.47225e-05,0,0,1.15741666666667e-05,6.9445e-05,0.000243056666666667,0.0004166675,3.47225e-05,1.15741666666667e-05,0,5.78708333333333e-05,0.000266204166666667,1.15741666666667e-05,0,0,0,0,0,0,0,8.10191666666667e-05,3.47225e-05,1.15741666666667e-05,1.15741666666667e-05,0,0,0,6.9445e-05,0.0001736125,4.62966666666667e-05,0,3.47225e-05,0,0,0,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,1.15741666666667e-05,3.47225e-05,0.0001041675,0.000162038333333333,0.00030092625,0.00027777875,0.00011574125,9.25933333333333e-05,0,1.15741666666667e-05,6.9445e-05,1.15741666666667e-05,0,1.15741666666667e-05,1.15741666666667e-05,0,0,0,2.31483333333333e-05,0,3.47225e-05,9.25933333333333e-05,2.31483333333333e-05,1.15741666666667e-05,1.15741666666667e-05,2.31483333333333e-05,3.47225e-05,0.000115741666666667,3.47225e-05,2.31483333333333e-05,1.15741666666667e-05,0,3.47225e-05,0.000231482916666667,0.000196760833333333,0.000208335,3.47225e-05,0,1.15741666666667e-05,2.31483333333333e-05,0,0,2.31483333333333e-05,1.15741666666667e-05,0,0,1.15741666666667e-05,2.31483333333333e-05,0,0.00013889,0.000162038333333333,1.15741666666667e-05,0.00015046375,4.62966666666667e-05,1.15741666666667e-05,2.31483333333333e-05,0.000127315833333333,0,0,0,0,0.0002893525,2.31483333333333e-05,1.15741666666667e-05,0,5.78708333333333e-05,5.78708333333333e-05,0,0.000405094583333333,9.25929166666667e-05,0,0,0,9.25933333333333e-05,0.000231483333333333,6.9445e-05,1.15741666666667e-05,0,1.15741666666667e-05,0.000115741666666667,3.47225e-05,0,0,0,0,0,4.62966666666667e-05,1.15741666666667e-05,0,9.25925e-05,4.62966666666667e-05,0,0,0,6.9445e-05,0.000162037083333333,0.000219909166666667,9.25933333333333e-05,0,0,1.15741666666667e-05,1.15741666666667e-05,0.00015046375,0.0001041675,2.31483333333333e-05,9.25933333333333e-05,0,0,0,1.15741666666667e-05,0,0,0,0,0,3.47225e-05,3.47225e-05,4.62966666666667e-05,1.15741666666667e-05,0,0,9.25933333333333e-05,0.000196760833333333,0,0,0,0,5.78708333333333e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3.47225e-05,0.000150462916666667,2.31483333333333e-05,2.31483333333333e-05,0,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,2.31483333333333e-05,0,0,0,1.15741666666667e-05,0,0.000162036666666667,1.15741666666667e-05,1.15741666666667e-05,0,0,5.78708333333333e-05,4.62966666666667e-05,0,6.94441666666667e-05,0,1.15741666666667e-05,0,0,0,0,0,0,0,0.00016203875,0.000370371666666667,4.62966666666667e-05,0,2.31483333333333e-05,0,0,0.000173610833333333,8.101875e-05,1.15741666666667e-05,5.78704166666667e-05,4.62966666666667e-05,5.78708333333333e-05,0.000115741666666667,0.000173611666666667,3.47225e-05,2.31483333333333e-05,1.15741666666667e-05,2.31483333333333e-05,1.15741666666667e-05,1.15741666666667e-05,1.15741666666667e-05,0.00019676,2.31483333333333e-05,0,0,0,0.000138889166666667,1.15741666666667e-05,3.47225e-05,0.000115741666666667,0,0,2.31483333333333e-05,8.10183333333333e-05,3.47225e-05,0,2.31483333333333e-05,0,3.47225e-05,4.62966666666667e-05,6.9445e-05,0,0,0,0,0,0,0,1.15741666666667e-05,8.10191666666667e-05,3.47225e-05,0,0,0,0,0,0,0,0,0,0,1.15741666666667e-05,0,0.000208334583333333,9.25925e-05,0,0,0,0,0,0,0,0,0,0,6.94445833333333e-05,8.10183333333333e-05,0.00019676,9.25933333333333e-05,0.0001041675,0,0,0,0,0,0,0,0,0,0.000127314583333333,1.15741666666667e-05,0,5.78708333333333e-05,4.62966666666667e-05,0.000266205416666667,5.78708333333333e-05,0.00025463,0,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,0,2.31483333333333e-05,0.0001041675,2.31483333333333e-05,6.9445e-05,1.15741666666667e-05,0,1.15741666666667e-05,0.0002430575,5.78708333333333e-05,2.31483333333333e-05,0,0,1.15741666666667e-05,0.000219909166666667,3.47225e-05,0,0.0001041675,0,0,0,6.9445e-05,6.9445e-05,1.15741666666667e-05,3.47225e-05,0,0,0,0,5.78708333333333e-05,6.9445e-05,0,4.62966666666667e-05,3.47225e-05,0.00013889,0,0,0,9.25933333333333e-05,4.62966666666667e-05,2.31483333333333e-05,0.000185186666666667,0.000162038333333333,0.000185186666666667,0.000231483333333333,0.000115741666666667,0.00033564875,0,0,1.15741666666667e-05,1.15741666666667e-05,0,0.00028935375,0.000231483333333333,2.31483333333333e-05,0,0,0,0,0,0,0,0,0.000150464166666667,2.31483333333333e-05,0,0,0,5.78708333333333e-05,0,0,0.000162037916666667,9.25929166666667e-05,0.000185186666666667,1.15741666666667e-05,0,0.000138889166666667,0,3.47225e-05,0,1.15741666666667e-05,0,0,0,0,0.000173612083333333,4.62966666666667e-05,1.15741666666667e-05,2.31483333333333e-05,1.15741666666667e-05,0.000324075,4.62966666666667e-05,5.78708333333333e-05,0.000162037916666667,0,0,0,0,0,0,0,2.31483333333333e-05,4.62966666666667e-05,0,0.000381945833333333,8.10191666666667e-05,0,1.15741666666667e-05,0,1.15741666666667e-05,0.0001736125,9.25933333333333e-05,1.15741666666667e-05,0,0,0,0,0.00013889,0,0.000162037916666667,0.000115741666666667,0.0001041675,0,0,0,0,5.78708333333333e-05,0,0,0,6.9445e-05,2.31483333333333e-05,0,0,0.00011574125,9.25933333333333e-05,0,9.25929166666667e-05,5.78708333333333e-05,8.10191666666667e-05,6.9445e-05,3.47225e-05,4.629625e-05,2.31483333333333e-05,1.15741666666667e-05,0,0,0.00015046375,4.62966666666667e-05,0,8.10191666666667e-05,5.78708333333333e-05,0.000115741666666667,0.000208335,0.000150464166666667,1.15741666666667e-05,0,0,0.000266205416666667,0,0,0,5.78708333333333e-05,0.000196758333333333,0,1.15741666666667e-05,0,0.000162037916666667,0.0002430575,0.00027777875,0.00041666875,2.31483333333333e-05,0,0,0,0.00018518625,0,3.47225e-05,0,0,0,0,0,0.0001736125,5.78708333333333e-05,0.00059028,0.000127315833333333,0,0,0,0,5.78708333333333e-05,0.00030092625,0.000185185833333333,0.000173612083333333,1.15741666666667e-05,0,1.15741666666667e-05,0.00019676125,1.15741666666667e-05,1.15741666666667e-05,0,0,0,5.78708333333333e-05,1.15741666666667e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.000162037916666667,0,0,1.15741666666667e-05,0,0,0,0,0,1.15741666666667e-05,3.47225e-05,0,0,5.787e-05,2.31483333333333e-05,2.31483333333333e-05,9.25941666666667e-05,6.94458333333333e-05,2.31483333333333e-05,0.0001041675,8.102e-05,4.62966666666667e-05,8.10191666666667e-05,0,0,0,0,5.78704166666667e-05,0.00016203875,5.78708333333333e-05,0,0,0,0,0,8.101875e-05,0.000231482083333333,1.15741666666667e-05,0,0,3.47225e-05,0,2.31483333333333e-05,6.94445833333333e-05,0,3.47225e-05,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5.78708333333333e-05,0.00012731625,4.62966666666667e-05,2.31483333333333e-05,0,2.31483333333333e-05,2.31483333333333e-05,2.31483333333333e-05,2.31483333333333e-05,2.31483333333333e-05,2.31483333333333e-05,0,0.000104167083333333,0,1.15741666666667e-05,0,0,0,5.78704166666667e-05,0,0,3.47225e-05,1.15741666666667e-05,3.47225e-05,6.94445833333333e-05,1.15741666666667e-05,8.101875e-05,6.9445e-05,9.25929166666667e-05,0.000289352916666667,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],"fixedtz":false,"tzone":"UTC"},"evals":[],"jsHooks":[]}</script>
```



Bonus #2: Use a function and for loop to create yearly plots of precipitation by day of year.


### Create yearly plots of precip by day of year 

#### Function using ggplot 


```r
# Write a function to create yearly plots for precip by day of year with ggplot

precip_plotter2 <- function(df, year) {
  precip_daily4 <- df %>%
    group_by(month, day, year, site) %>%
    summarize(mean_precip = mean(precip, na.rm = T)) %>%
    mutate(date = as.Date(paste(year, month, day, sep = "/"))) %>%
    mutate(yday = yday(date)) %>%
    pivot_wider(names_from = site, values_from = mean_precip) %>%
    select(-SBSP) %>%
    filter(year == i)
  
  print(
    ggplot(precip_daily4, aes(x = yday, y = SASP)) +
      geom_point(shape = 1) +
      theme_few() +
      labs(
        title = i,
        x = 'Day of Year',
        y = expression('Average Precipitation' ~ ('kg' *
                                                    m ^ 2 * s ^ 1))
      ) +
      scale_x_continuous(breaks = c(1, 90, 180, 270, 360))
  )
}

# Use for loop to plot years 2005 to 2010

yrs = c(2005:2010)

for (i in yrs){
  precip_plotter2(forcing_data_full,year)
}
```

```
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
```

<img src="03-Functions_files/figure-html/ggplot, yearly precip plot-1.png" width="672" />

```
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
```

<img src="03-Functions_files/figure-html/ggplot, yearly precip plot-2.png" width="672" />

```
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
```

<img src="03-Functions_files/figure-html/ggplot, yearly precip plot-3.png" width="672" />

```
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
```

<img src="03-Functions_files/figure-html/ggplot, yearly precip plot-4.png" width="672" />

```
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
```

<img src="03-Functions_files/figure-html/ggplot, yearly precip plot-5.png" width="672" /><img src="03-Functions_files/figure-html/ggplot, yearly precip plot-6.png" width="672" />


#### Function using dygraphs

This will print the plots in the console/inline and the document will knit. However, the dygraphs plots do not show up in the final document. 



```r
# Write function to create yearly plots of precipitation by day of year with dygraphs

dy_precip_plotter <- function(df, year) {
  precip_daily3 <- df %>%
    group_by(month, day, year, site) %>%
    summarize(mean_precip = mean(precip, na.rm = T)) %>%
    mutate(date = as.Date(paste(year, month, day, sep = "/"))) %>%
    pivot_wider(names_from = site, values_from = mean_precip) %>%
    filter(year == i) %>%
    ungroup() %>%
    select(-SBSP,-month,-day,-year)
  
  precip_xts3 <- xts(precip_daily3 %>%
                      select(SASP), order.by = precip_daily3$date)
  
  print(
    dygraph(precip_xts3, main = i, ylab = "Average Daily Precipitation") %>%
      dyOptions(fillGraph = TRUE, axisLabelFontSize=10)
  )
}

# Use for loop to plot years 2005 to 2010

yrs = c(2005:2010)
 
for (i in yrs){
  dy_precip_plotter(forcing_data_full,year)
}
```

```
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
## `summarise()` has grouped output by 'month', 'day', 'year'. You can override using the `.groups` argument.
```

