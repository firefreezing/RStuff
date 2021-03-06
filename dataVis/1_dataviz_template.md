---
title: "**Data Visualization Plots Gallery**"
author: "the data viz team"
date: "2018-09-26"
project_name: "QMD"
output:
  html_document:
    keep_md: true
    theme: "cosmo"
    toc: true
    toc_float: 
      collapsed: false
---


```r
# Load packages
library(pacman)
source("pkg_list.r")

# Load self-defined functions
source("fct_list.r")

p_load(plotly)
p_load(forcats)

# Global knitr options
opts_chunk$set(fig.align = "center", echo = T, warning = F)
```

# **Overview**

## **To cover & discuss:**

+ file management
    - naming convention: easy for group work
    - file structure: easy to add/reorganize new additions during the task
    - try the TFS server (Norberto has some tutorial for the HCIA project, test space)
    
+ data management
    - store data 
    - data format (categorical data - *forcats* pkg)
    
    The car::recode can be replaced by dplyr::recode, and may be further replaced by the *forcats* package
    
# **I. Exploratory Data Analysis**

## **a. Static visualization**

Payment measures and value of care displays – provider data. This data set includes provider data for the payment measures and value of care displays associated with a 30-day episode of care for heart attack, heart failure, and pneumonia patients.



```r
# Read in data
my_col <- cols(
 `Denominator` = "n",
 `Payment` = "n",
 `Lower estimate` = "n",
 `Higher estimate` = "n",
 `Measure start date` = col_date('%m/%d/%Y'),
 `Measure End Date` = col_date('%m/%d/%Y'),
 .default = "c"
)

dat <- read_csv("data/Payment_and_value_of_care_Hospital.csv", 
                col_types = my_col, na = "Not Available") %>%
  set_names(str_replace_all(tolower(names(.)), "\\s+", "_"))

dat_clean <- dat %>%
  select(provider_id, hospital_name, address, city, 
         state, zip_code, county_name,
         payment_measure_name, payment_measure_id,
         payment_category, 
         denominator, payment, 
         lower_estimate,
         higher_estimate)
```




```r
myplot_1 <- ggplot(dat_clean %>% filter(!is.na(payment)), 
       aes(x = payment_measure_id, y = payment)) +
  geom_boxplot(outlier.shape = 8, outlier.color = "red",
               fill = "steelblue", color = "steelblue") +
  stat_summary(geom = "crossbar", width = 1, 
               fatten = 0, color="white", 
               fun.data = function(x){ return(c(y = median(x), 
                                                ymin = median(x), 
                                                ymax = median(x))) }) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0)) + 
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ",")) +
  scale_x_discrete(name = "", 
                   labels = c("PAYM_30_AMI" = "AMI",
                              "PAYM_30_HF" = "Heart Failure",
                              "PAYM_30_PN" = "Pneumonia")) +
  labs(title = "Hospital Payment for Patients during a 30-day Episode of Care")
```


```r
myplot_1
```

<img src="1_dataviz_template_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

Tips:

+ Use `scales::dollar_format` to customize the format of payment
+ can relabel the category names using the `scale_x/y_discrete/continuous` function, rather than relabel them in the raw data
+ trick: You can use continuous positions even with a discrete position scale - this allows you (e.g.) to place labels between bars in a bar chart. Continuous positions are numeric values starting at one for the first level, and increasing by one for each level (i.e. the labels are placed at integer positions). This is what allows jittering to work.



```r
myplot_2 <- ggplot(data = dat_clean %>% filter(!is.na(denominator)), 
       aes(denominator)) +
  geom_histogram(binwidth = 30, fill = "steelblue") + 
  facet_grid(payment_measure_id ~ ., scale = "fixed",
             labeller = as_labeller(c("PAYM_30_AMI" = "AMI",
                                  "PAYM_30_HF" = "Heart Failure",
                                  "PAYM_30_PN" = "Pneumonia"))) +  
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0),
       panel.margin = unit(0.5, "in")) +
  scale_x_continuous(name = "Number of Patients", labels = scales::comma) + 
  ggtitle("Distribution of Hospital Patient Volume, by Care")
```


```r
myplot_2
```

<img src="1_dataviz_template_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />


```r
dat_clean %>%
  filter(!is.na(denominator)) %>%
  dplyr::group_by(payment_measure_id) %>%
  dplyr::summarise(n = n(),
            min = min(denominator),
            q_10 = quantile(denominator, .1) %>% round(),
            q_25 = quantile(denominator, .25) %>% round(),
            median = median(denominator) %>% round(),
            q_75 = quantile(denominator, .75) %>% round(),
            q_90 = quantile(denominator, .9) %>% round(),
            max = max(denominator)) %>%
  pander()
```


-----------------------------------------------------------------------------
 payment_measure_id    n     min   q_10   q_25   median   q_75   q_90   max  
-------------------- ------ ----- ------ ------ -------- ------ ------ ------
    PAYM_30_AMI       2341   25     36     66     138     253    408    1447 

     PAYM_30_HF       3549   25     38     67     157     343    566    2871 

     PAYM_30_PN       3984   25     45     76     148     284    452    2113 
-----------------------------------------------------------------------------



```r
myplot_3 <- ggplot(data = dat_clean %>% filter(!is.na(denominator) & !is.na(payment)),
       aes(denominator, payment)) +
  geom_point(alpha = 0.5, color = "steelblue") + 
  facet_grid(payment_measure_id ~ ., scale = "fixed",
             labeller = as_labeller(c("PAYM_30_AMI" = "AMI",
                                  "PAYM_30_HF" = "Heart Failure",
                                  "PAYM_30_PN" = "Pneumonia"))) +  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0),
        panel.margin = unit(0.5, "in")) + 
  scale_x_continuous(label = scales::comma, name = "Number of Patients") +
  scale_y_continuous(name = "Payment",
                     label = scales::dollar_format(prefix = "$", big.mark = ","))
```


```r
myplot_3
```

<img src="1_dataviz_template_files/figure-html/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

#### Spaghetti Plots


```r
p_load(nlme)
data(Oxboys)

ggplot(Oxboys, aes(age, height, group = Subject)) +
  geom_line()
```

<img src="1_dataviz_template_files/figure-html/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />


```r
dat <- data.frame(
  state = c("AZ", "VA", "MD", "DC", "NJ", "MA", "CA", "WA"),
  val_1 = c(1:8),
  val_2 = c(8:1),
  label = sample(c("Yes","No"), size = 8, replace = T)
)

dat_spgt <- dat %>%
  gather(key = key, value = value, val_1:val_2) %>%
  mutate(key = case_when(.$key == "val_1" ~ 0,
                         .$key == "val_2" ~ 1))

plot_spgt <- ggplot(dat_spgt, aes(key, value, group = state, color = label)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0, 1),
                     labels = c("Year 1", "Year 2")) +
  scale_color_manual(values = c("Yes" = "red", "No" = "blue"),
                     labels = c("Yes" = "Y", "No" = "N")) +
  labs(x = "year", y = "measure rate", color = "condition", 
       title = "Spaghetti Plot Demo") +
  theme_minimal()

plot_spgt
```

<img src="1_dataviz_template_files/figure-html/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />


```r
ggplotly(plot_spgt)
```



### Tree Map


```r
p_load(treemap)

data("GNI2014")

treemap(GNI2014,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value",
       format.legend = list(scientific = FALSE, big.mark = " "))
```

<img src="1_dataviz_template_files/figure-html/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

The treeplot is too crowed. Let's collpase the countries with small population together. For example, only keep the top 10 countries in each continent. 


```r
GNI2014 <- GNI2014 %>%
  mutate(id = 1:dim(GNI2014)[1])

GNI2014_clean_top <- GNI2014 %>%
  group_by(continent) %>%
  top_n(10, population)

GNI2014_clean_other <- GNI2014 %>%
  filter(!id %in% GNI2014_clean_top$id) %>%
  group_by(continent) %>%
  summarise(iso3 = "OTH",
            country = "Others",
            population = sum(population),
            GNI = median(GNI),
            id = NA)

GNI2014_clean <- bind_rows(GNI2014_clean_top, GNI2014_clean_other)
```


```r
treemap(GNI2014_clean,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value",
       overlap.labels = 1,
       format.legend = list(scientific = FALSE, big.mark = " "))
```

<img src="1_dataviz_template_files/figure-html/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

Another package to draw treemap:


```r
p_load(portfolio)

map.market(id = GNI2014_clean$iso3, 
           area = GNI2014_clean$population, 
           group = GNI2014_clean$continent, 
           color = GNI2014_clean$GNI, 
           lab = c(T, T),
           main = "Treemap")
```

<img src="1_dataviz_template_files/figure-html/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />



#### **Map**


```r
dat_state <- dat_clean %>%
  filter(!is.na(denominator) & !is.na(payment)) %>%
  group_by(state, payment_measure_id) %>%
  dplyr::summarise(pmt = sum(denominator*payment)/sum(denominator))
```


```r
# test <- state_hexmap(dat_state %>% 
#                filter(payment_measure_id == "PAYM_30_AMI") %>%
#                select(-payment_measure_id),
#              varname = "Payment per \n Patient",
#              this_title = "Average Per Patient Payment for 30-day Care for 
#              AMI, by State")
# 
# test
```


```r
# ggplotly(test)
```

```r
# map_usa(dat_state %>% 
#                filter(payment_measure_id == "PAYM_30_AMI") %>%
#                select(-payment_measure_id),
#              varname = "Payment per \n Patient",
#              this_title = "Average Per Patient Payment for 30-day Care for 
#              AMI, by State")
```


```r
dat_city <- dat_clean %>%
  filter(!is.na(denominator) & !is.na(payment)) %>%
  group_by(city, payment_measure_id) %>%
  dplyr::summarise(state = first(state),
                   pmt = sum(denominator*payment)/sum(denominator))
```


```r
# city_leaflet(dat_city %>% 
#                filter(payment_measure_id == "PAYM_30_AMI") %>%
#                select(-payment_measure_id))
```


## **b. Interactive visualization**


```r
margin <- list(l = 150, r = 20, b = 50,
               t = 50, pad = 4)
```



```r
ggplotly(myplot_1) %>%
    layout(autosize = T, width = 800, height = 600, margin = margin)
```


```r
ggplotly(myplot_2) %>%
    layout(autosize = T, width = 800, height = 600, margin = margin)
```


```r
ggplotly(myplot_3) %>%
    layout(autosize = T, width = 800, height = 600, margin = margin)
```

# **II. Visualizing Statistical Analysis Output**

## **a. Static visualization**

## **b. Interactive visualization**

# **III. Formatting and Aesthetics**

## **a. Color palette**

## **b. Annotation**


```r
data("presidential")
presidential

data("economics")
economics

unemp <- ggplot(dat = economics, aes(date, unemploy)) +
  geom_line() +
  labs(x = "", y = "No. unemployed (1000s)") +
  theme_bw()

unemp

presidential <- presidential[-(1:3), ]

yrng <- range(economics$unemploy)
xrng <- range(economics$date)

unemp + geom_rect(data = presidential, aes(NULL, NULL, xmin = start, 
                                           xmax = end, fill = party))

# try this:
caption <- paste(strwrap("unemployment rates in the us have varied a lot over the years", 40),
                 collapse = "\n")

unemp + geom_text(aes(x, y, label = caption),
                  data = XXX, hjust = 1, vjust = 1, size = 4)
```


+ continuous scale

+ discrete scale
    
## **c. Texture**

See [this post](http://stackoverflow.com/questions/2895319/how-to-add-texture-to-fill-colors-in-ggplot2)


# **Reference**

+ _Forcats_ package for handling categorical variables. [https://blog.rstudio.org/2016/08/31/forcats-0-1-0/](https://blog.rstudio.org/2016/08/31/forcats-0-1-0/)

+ Instructions for Analyzing Data from CAHPS Surveys (2012). Document No. 2015. Updated 4/2/12. Available at: [https://cahpsdatabase.ahrq.gov/files/CGGuidance/Instructions%20for%20Analyzing%20CAHPS%20Surveys.pdf](https://cahpsdatabase.ahrq.gov/files/CGGuidance/Instructions%20for%20Analyzing%20CAHPS%20Surveys.pdf]) [Accessed October 12 2016]. 

+ Measure Testing Task Force Report - National Quality Forum (January 2011). Available at: [http://www.qualityforum.org/Publications/2011/01/Measure_Testing_Task_Force.aspx](http://www.qualityforum.org/Publications/2011/01/Measure_Testing_Task_Force.aspx) [Accessed October 12 2016].

+ Morris, C. N. (1983) Parametric Empirical Bayes Inference: Theory and Applications. Journal of the American Statistical Association, 78:381, 47-55

