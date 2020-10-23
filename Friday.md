Project 2
================
Jackie Steffan
10/16/2020

``` r
#load in required packages
library(tidyverse)
library(corrplot)
library(ggplot2)
library(GGally)
library(tree)
library(caret)
library(gbm)
library(rpart)
library(rmarkdown)
library(knitr)
```

# Introduction

This data set looks at articles that were published on the website
Mashable. The articles included were published over a 2 year period. The
goal of the data set was to look at the frequency of sharing and the
popularity of each article. According to the data documentation, the
variables url and timedelta are non-predicitve, so I chose not to use
them.  
This analysis is intended to predict the number of shares an article
will have based on other factors such as word count, number of pictures,
etc. I am using 2 tree methods that you will see later in this document.
One is a non-ensemble regression tree and the other is a bosoted tree.

# Data

In this section I am reading in the data described above.  
I then separate the data into a training data set that contains 70% of
the data and a testing data set that contains the remaining 30% of the
data.

``` r
#read in data
popData <- read_csv("./OnlineNewsPopularity.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   url = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
#combine boolean varaibles to make them easier to work with using mutate
popData <- popData %>% mutate(weekday = ifelse(weekday_is_monday == 1, "Monday", 
                                    ifelse(weekday_is_tuesday == 1, "Tuesday",
                                           ifelse(weekday_is_wednesday==1, "Wednesday",
                                                  ifelse(weekday_is_thursday==1, "Thursday",
                                                         ifelse(weekday_is_friday==1, "Friday",
                                                                ifelse(weekday_is_saturday==1, "Saturday", "Sunday"))))))) %>%
  mutate(channel_type = ifelse(data_channel_is_lifestyle == 1, "Lifestyle",
                               ifelse(data_channel_is_entertainment ==1, "Entertainment",
                                      ifelse(data_channel_is_bus ==1, "Business",
                                             ifelse(data_channel_is_socmed ==1, "Social Media",
                                                    ifelse(data_channel_is_tech == 1, "Tech",
                                                           ifelse(data_channel_is_world ==1, "World", "Other"))))))) %>%
                                    select(-starts_with("weekday_is"), -starts_with("data_channel"))

#filter to select appropriate weekday
weekdayDat <- filter(popData, weekday == params$weekday)

#set seed for reproducibility
set.seed(5)
#split into training and test sets
train <- sample(1:nrow(weekdayDat), size = nrow(weekdayDat)*0.7)
test <- dplyr::setdiff(1:nrow(weekdayDat), train)
DayTrain <- weekdayDat[train, ]
DayTest <- weekdayDat[test, ]
```

# Summarization

Here I am creating summaries for all the predictive variables that are
numeric, the summaries included are min, max, median, mean, and the
first and 3rd quantiles. I am also including a correlation plot that
shows the correlation between our response variable, shares, and the
other predictive numeric variables. Lastly I included a pairs plot of
the response variable and some other predictive variables, which shows
what the 2 variables look like plotted against one another.

``` r
#summary of numeric variables
summary(select(DayTrain,-c("url", "weekday", "timedelta", "channel_type", "is_weekend")))
```

    ##  n_tokens_title  n_tokens_content n_unique_tokens  n_non_stop_words n_non_stop_unique_tokens
    ##  Min.   : 3.00   Min.   :   0.0   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000          
    ##  1st Qu.: 9.00   1st Qu.: 240.0   1st Qu.:0.4752   1st Qu.:1.0000   1st Qu.:0.6294          
    ##  Median :10.00   Median : 403.0   Median :0.5448   Median :1.0000   Median :0.6960          
    ##  Mean   :10.38   Mean   : 526.6   Mean   :0.5353   Mean   :0.9704   Mean   :0.6784          
    ##  3rd Qu.:12.00   3rd Qu.: 691.0   3rd Qu.:0.6136   3rd Qu.:1.0000   3rd Qu.:0.7610          
    ##  Max.   :23.00   Max.   :7413.0   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000          
    ##    num_hrefs      num_self_hrefs      num_imgs         num_videos     average_token_length  num_keywords   
    ##  Min.   :  0.00   Min.   : 0.000   Min.   :  0.000   Min.   : 0.000   Min.   :0.000        Min.   : 1.000  
    ##  1st Qu.:  4.00   1st Qu.: 1.000   1st Qu.:  1.000   1st Qu.: 0.000   1st Qu.:4.475        1st Qu.: 6.000  
    ##  Median :  7.00   Median : 2.000   Median :  1.000   Median : 0.000   Median :4.662        Median : 7.000  
    ##  Mean   : 10.74   Mean   : 3.033   Mean   :  4.264   Mean   : 1.256   Mean   :4.550        Mean   : 7.206  
    ##  3rd Qu.: 13.00   3rd Qu.: 4.000   3rd Qu.:  3.000   3rd Qu.: 1.000   3rd Qu.:4.857        3rd Qu.: 9.000  
    ##  Max.   :186.00   Max.   :51.000   Max.   :108.000   Max.   :91.000   Max.   :6.486        Max.   :10.000  
    ##    kw_min_min       kw_max_min         kw_avg_min        kw_min_max       kw_max_max       kw_avg_max    
    ##  Min.   : -1.00   Min.   :     0.0   Min.   :   -1.0   Min.   :     0   Min.   : 28000   Min.   :  5911  
    ##  1st Qu.: -1.00   1st Qu.:   448.2   1st Qu.:  139.7   1st Qu.:     0   1st Qu.:843300   1st Qu.:174386  
    ##  Median : -1.00   Median :   663.5   Median :  234.8   Median :  1500   Median :843300   Median :245753  
    ##  Mean   : 26.04   Mean   :  1114.2   Mean   :  312.4   Mean   : 13865   Mean   :754862   Mean   :260610  
    ##  3rd Qu.:  4.00   3rd Qu.:  1000.0   3rd Qu.:  352.9   3rd Qu.:  7700   3rd Qu.:843300   3rd Qu.:331472  
    ##  Max.   :217.00   Max.   :158900.0   Max.   :39979.0   Max.   :843300   Max.   :843300   Max.   :843300  
    ##    kw_min_avg     kw_max_avg       kw_avg_avg      self_reference_min_shares self_reference_max_shares
    ##  Min.   :   0   Min.   :  2195   Min.   :  776.1   Min.   :     0.0          Min.   :     0.0         
    ##  1st Qu.:   0   1st Qu.:  3569   1st Qu.: 2374.2   1st Qu.:   629.2          1st Qu.:   995.8         
    ##  Median :1057   Median :  4373   Median : 2853.8   Median :  1200.0          Median :  2700.0         
    ##  Mean   :1125   Mean   :  5670   Mean   : 3147.6   Mean   :  4152.9          Mean   : 11232.7         
    ##  3rd Qu.:2051   3rd Qu.:  6030   3rd Qu.: 3597.5   3rd Qu.:  2700.0          3rd Qu.:  7600.0         
    ##  Max.   :3584   Max.   :171030   Max.   :37607.5   Max.   :690400.0          Max.   :843300.0         
    ##  self_reference_avg_sharess     LDA_00            LDA_01            LDA_02            LDA_03       
    ##  Min.   :     0             Min.   :0.01818   Min.   :0.01818   Min.   :0.01818   Min.   :0.01818  
    ##  1st Qu.:   935             1st Qu.:0.02504   1st Qu.:0.02501   1st Qu.:0.02857   1st Qu.:0.02540  
    ##  Median :  2169             Median :0.03337   Median :0.03334   Median :0.04006   Median :0.04000  
    ##  Mean   :  6760             Mean   :0.17925   Mean   :0.13864   Mean   :0.23004   Mean   :0.22773  
    ##  3rd Qu.:  5100             3rd Qu.:0.23368   3rd Qu.:0.15005   3rd Qu.:0.37678   3rd Qu.:0.37874  
    ##  Max.   :690400             Max.   :0.92699   Max.   :0.91940   Max.   :0.92000   Max.   :0.92554  
    ##      LDA_04        global_subjectivity global_sentiment_polarity global_rate_positive_words
    ##  Min.   :0.01819   Min.   :0.0000      Min.   :-0.36425          Min.   :0.00000           
    ##  1st Qu.:0.02857   1st Qu.:0.3967      1st Qu.: 0.05608          1st Qu.:0.02802           
    ##  Median :0.04001   Median :0.4553      Median : 0.11522          Median :0.03874           
    ##  Mean   :0.22435   Mean   :0.4444      Mean   : 0.11738          Mean   :0.03915           
    ##  3rd Qu.:0.37033   3rd Qu.:0.5093      3rd Qu.: 0.17550          3rd Qu.:0.04973           
    ##  Max.   :0.92653   Max.   :0.8987      Max.   : 0.60000          Max.   :0.13699           
    ##  global_rate_negative_words rate_positive_words rate_negative_words avg_positive_polarity
    ##  Min.   :0.000000           Min.   :0.0000      Min.   :0.0000      Min.   :0.0000       
    ##  1st Qu.:0.009767           1st Qu.:0.5968      1st Qu.:0.1921      1st Qu.:0.3049       
    ##  Median :0.015246           Median :0.7078      Median :0.2857      Median :0.3571       
    ##  Mean   :0.016768           Mean   :0.6781      Mean   :0.2924      Mean   :0.3529       
    ##  3rd Qu.:0.021973           3rd Qu.:0.8000      3rd Qu.:0.3889      3rd Qu.:0.4107       
    ##  Max.   :0.136929           Max.   :1.0000      Max.   :1.0000      Max.   :1.0000       
    ##  min_positive_polarity max_positive_polarity avg_negative_polarity min_negative_polarity
    ##  Min.   :0.0000        Min.   :0.0000        Min.   :-1.0000       Min.   :-1.0000      
    ##  1st Qu.:0.0500        1st Qu.:0.6000        1st Qu.:-0.3312       1st Qu.:-0.7000      
    ##  Median :0.1000        Median :0.8000        Median :-0.2571       Median :-0.5000      
    ##  Mean   :0.0965        Mean   :0.7472        Mean   :-0.2618       Mean   :-0.5208      
    ##  3rd Qu.:0.1000        3rd Qu.:1.0000        3rd Qu.:-0.1875       3rd Qu.:-0.3000      
    ##  Max.   :1.0000        Max.   :1.0000        Max.   : 0.0000       Max.   : 0.0000      
    ##  max_negative_polarity title_subjectivity title_sentiment_polarity abs_title_subjectivity
    ##  Min.   :-1.0000       Min.   :0.0000     Min.   :-1.00000         Min.   :0.0000        
    ##  1st Qu.:-0.1250       1st Qu.:0.0000     1st Qu.: 0.00000         1st Qu.:0.1667        
    ##  Median :-0.1000       Median :0.1250     Median : 0.00000         Median :0.5000        
    ##  Mean   :-0.1105       Mean   :0.2825     Mean   : 0.06756         Mean   :0.3464        
    ##  3rd Qu.:-0.0500       3rd Qu.:0.5000     3rd Qu.: 0.13636         3rd Qu.:0.5000        
    ##  Max.   : 0.0000       Max.   :1.0000     Max.   : 1.00000         Max.   :0.5000        
    ##  abs_title_sentiment_polarity     shares      
    ##  Min.   :0.0000               Min.   :    22  
    ##  1st Qu.:0.0000               1st Qu.:   966  
    ##  Median :0.0000               Median :  1400  
    ##  Mean   :0.1537               Mean   :  3193  
    ##  3rd Qu.:0.2500               3rd Qu.:  2700  
    ##  Max.   :1.0000               Max.   :210300

``` r
#correlation of selected variables
cors <- cor(select(DayTrain, -url, -timedelta, -weekday, -channel_type, -is_weekend))
#correlation plot showing just correlation with the response variable shares
corrplot(cors["shares",,drop=FALSE], type = "upper", tl.pos = "lt",cl.pos = "n")
```

![](Friday_files/figure-gfm/summary-1.png)<!-- -->

``` r
#pairs data with selected variables
pairs(select(DayTrain, shares, num_hrefs, self_reference_min_shares, kw_max_min, self_reference_avg_sharess, kw_avg_avg, kw_avg_min, average_token_length, global_sentiment_polarity))
```

![](Friday_files/figure-gfm/summary-2.png)<!-- -->

``` r
pairs(select(DayTrain, shares,num_videos, n_non_stop_unique_tokens, LDA_00, LDA_03, LDA_01, LDA_04, min_negative_polarity, n_unique_tokens))
```

![](Friday_files/figure-gfm/summary-3.png)<!-- -->

# Models

In this section I am creating 2 tree models. The first is a non-ensemble
based tree selected using leave one out cross-validation. I am also
using cp between 0 and 0.2 for my tuning parameter. The final model is
printed below.  
The second model is a boosted tree selected using repeated cross
validation and the default tuning parameters. The final model is also
printed in the output.  
Lastly, you will find a comparison of RMSE, when deciding which model is
the “better” model you should select the one that has the lower RMSE.

``` r
#select relevant datapoints
treeDat <- select(DayTrain,-c("url", "weekday", "is_weekend", "timedelta"))
#build regression tree using rpart and train using LOOCV and tuning parameter cp
classTree <- train(shares~., data= select(DayTrain,-url, -weekday, -is_weekend, -timedelta), 
                   method= "rpart",
                   trControl = trainControl(method = "LOOCV"),
                   tuneGrid = data.frame(cp=0:0.2),
                   metric = "MAE")
#print results of tree
classTree$results
```

    ##   cp     RMSE    Rsquared      MAE
    ## 1  0 8464.278 0.002225897 3359.156

``` r
#predict the number of shares on the test dataset using the above tree
pred<- predict(classTree, newdata = select(DayTest, -c("url", starts_with("weekday_is"), "is_weekend")))
#calculate RMSE
trRMSE<- sqrt(mean((pred-DayTest$shares)^2))


#boosted model
#build boosted tree using gbm and train with repeated cross validation and default tuning parameters.
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:3)*50, 
                        shrinkage = c(0.1, 0.2),
                        n.minobsinnode = c(10,20))
boostTree <- train(shares ~ ., data = treeDat, method = "gbm",
                   trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5),
                   preProcess = c("center", "scale"), verbose = FALSE,
                   tuneGrid = gbmGrid)
#print results of boosted tree
boostTree$results
```

    ##    shrinkage interaction.depth n.minobsinnode n.trees     RMSE   Rsquared      MAE   RMSESD RsquaredSD
    ## 1        0.1                 1             10      50 7146.156 0.02160367 2799.769 2040.166 0.02198485
    ## 4        0.1                 1             20      50 7092.739 0.02870299 2785.664 2056.466 0.02818113
    ## 19       0.2                 1             10      50 7182.034 0.02208384 2817.839 2020.358 0.01993103
    ## 22       0.2                 1             20      50 7114.888 0.03063034 2806.858 2048.047 0.02886201
    ## 7        0.1                 5             10      50 7164.084 0.03160075 2821.215 2013.268 0.02928066
    ## 10       0.1                 5             20      50 7109.977 0.03595286 2798.416 2037.299 0.03144419
    ## 25       0.2                 5             10      50 7293.108 0.02901891 2928.534 1979.995 0.02615419
    ## 28       0.2                 5             20      50 7235.497 0.02878823 2917.988 1976.004 0.02529029
    ## 13       0.1                 9             10      50 7213.223 0.03012596 2865.452 1975.204 0.02336097
    ## 16       0.1                 9             20      50 7132.935 0.03549677 2827.987 2027.254 0.02794967
    ## 31       0.2                 9             10      50 7452.215 0.02220217 3071.693 1970.618 0.02103229
    ## 34       0.2                 9             20      50 7334.435 0.02428670 3055.296 1963.958 0.02218800
    ## 2        0.1                 1             10     100 7164.098 0.02369650 2808.368 2023.538 0.02195091
    ## 5        0.1                 1             20     100 7109.734 0.02995099 2798.583 2051.708 0.02967213
    ## 20       0.2                 1             10     100 7177.023 0.02506903 2822.073 2017.375 0.02227209
    ## 23       0.2                 1             20     100 7119.741 0.03076181 2809.169 2052.108 0.02724301
    ## 8        0.1                 5             10     100 7211.936 0.03037801 2849.669 2000.485 0.02750468
    ## 11       0.1                 5             20     100 7155.112 0.03575650 2846.752 2029.734 0.03294993
    ## 26       0.2                 5             10     100 7422.464 0.02598556 3055.311 1970.888 0.02398568
    ## 29       0.2                 5             20     100 7336.084 0.02703197 3052.927 1953.873 0.02265321
    ## 14       0.1                 9             10     100 7287.852 0.02952782 2941.783 1955.292 0.02616573
    ## 17       0.1                 9             20     100 7199.242 0.03442464 2922.865 2008.833 0.03049024
    ## 32       0.2                 9             10     100 7622.324 0.02009435 3281.841 1921.696 0.01952308
    ## 35       0.2                 9             20     100 7435.257 0.02562554 3245.714 1908.491 0.02141688
    ## 3        0.1                 1             10     150 7156.274 0.02555911 2806.997 2020.066 0.02305074
    ## 6        0.1                 1             20     150 7111.953 0.03050459 2798.881 2047.885 0.02902493
    ## 21       0.2                 1             10     150 7181.152 0.02631921 2818.705 2014.371 0.02367809
    ## 24       0.2                 1             20     150 7131.926 0.03098057 2828.723 2051.150 0.02761725
    ## 9        0.1                 5             10     150 7258.050 0.02938607 2893.301 1988.248 0.02684480
    ## 12       0.1                 5             20     150 7205.806 0.03261806 2894.143 2013.625 0.03005627
    ## 27       0.2                 5             10     150 7515.949 0.02597359 3162.197 1951.722 0.02681543
    ## 30       0.2                 5             20     150 7407.091 0.02741796 3168.471 1954.742 0.02299847
    ## 15       0.1                 9             10     150 7351.699 0.02892271 3012.994 1943.409 0.02660225
    ## 18       0.1                 9             20     150 7253.003 0.03380510 2994.860 1995.858 0.03139831
    ## 33       0.2                 9             10     150 7720.905 0.01918270 3416.807 1915.660 0.01987281
    ## 36       0.2                 9             20     150 7548.997 0.02405816 3395.974 1899.889 0.02308376
    ##       MAESD
    ## 1  231.5107
    ## 4  230.1524
    ## 19 222.2774
    ## 22 225.1710
    ## 7  223.6830
    ## 10 228.4382
    ## 25 245.5185
    ## 28 241.4170
    ## 13 209.6822
    ## 16 236.4428
    ## 31 263.5037
    ## 34 250.3155
    ## 2  223.1580
    ## 5  230.4237
    ## 20 226.6154
    ## 23 230.5201
    ## 8  226.0058
    ## 11 242.3692
    ## 26 245.5291
    ## 29 248.7107
    ## 14 224.9985
    ## 17 242.8526
    ## 32 277.4598
    ## 35 254.6977
    ## 3  222.5902
    ## 6  226.3731
    ## 21 224.9236
    ## 24 233.5146
    ## 9  219.4649
    ## 12 234.1377
    ## 27 241.9382
    ## 30 265.9638
    ## 15 221.5135
    ## 18 244.4685
    ## 33 277.7956
    ## 36 256.3138

``` r
#print best tree
boostTree$bestTune
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode
    ## 4      50                 1       0.1             20

``` r
#predict number of shares on the test data set using the boosted tree
boostPred <- predict(boostTree, newdata = select(DayTest, -c("url", starts_with("weekday_is"), "is_weekend")), n.trees = 5000)
#calculate RMSE
boostRMSE<- sqrt(mean((boostPred-DayTest$shares)^2))


#print RMSE for both models and compare results.
c(tree = trRMSE, boost = boostRMSE)
```

    ##      tree     boost 
    ## 10235.729  9475.092

# Linear Regression Model

This part of the code was added by Hannah Park as part II of the
project. Here, a linear regression model is fit on the training data set
using the same predictor variables as those used in the two tree models.
Then, predictions of the model are made on the test data set, and the
test MSE from the linear regression model is compared to those of the
tree models.

``` r
# Fit a linear regression
linReg <- train(shares ~ .,
                data = select(DayTrain,-url, -weekday, -is_weekend, -timedelta),
                method = "lm",
                trControl = trainControl(method = "cv", number = 10))
summary(linReg)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -19883  -2078  -1077     48 203195 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -3.679e+02  1.685e+03  -0.218  0.82713    
    ## n_tokens_title               -1.422e+01  5.703e+01  -0.249  0.80310    
    ## n_tokens_content              1.419e+00  4.614e-01   3.076  0.00211 ** 
    ## n_unique_tokens               8.089e+03  3.846e+03   2.103  0.03552 *  
    ## n_non_stop_words             -1.706e+03  3.301e+03  -0.517  0.60532    
    ## n_non_stop_unique_tokens     -4.039e+03  3.280e+03  -1.231  0.21826    
    ## num_hrefs                     1.790e+00  1.273e+01   0.141  0.88821    
    ## num_self_hrefs               -4.920e+01  4.490e+01  -1.096  0.27325    
    ## num_imgs                     -1.574e+01  1.789e+01  -0.880  0.37901    
    ## num_videos                   -7.192e+01  3.199e+01  -2.248  0.02463 *  
    ## average_token_length         -1.060e+03  4.930e+02  -2.151  0.03157 *  
    ## num_keywords                  3.083e+01  7.463e+01   0.413  0.67952    
    ## kw_min_min                    4.651e+00  3.375e+00   1.378  0.16829    
    ## kw_max_min                    2.069e-02  1.114e-01   0.186  0.85269    
    ## kw_avg_min                   -6.272e-01  5.168e-01  -1.214  0.22489    
    ## kw_min_max                   -2.103e-03  2.361e-03  -0.890  0.37332    
    ## kw_max_max                    3.406e-04  1.233e-03   0.276  0.78234    
    ## kw_avg_max                   -7.396e-04  1.676e-03  -0.441  0.65913    
    ## kw_min_avg                   -3.308e-01  1.535e-01  -2.155  0.03125 *  
    ## kw_max_avg                   -1.440e-01  5.661e-02  -2.544  0.01099 *  
    ## kw_avg_avg                    1.325e+00  2.993e-01   4.426 9.85e-06 ***
    ## self_reference_min_shares     1.469e-02  1.382e-02   1.063  0.28782    
    ## self_reference_max_shares     1.623e-03  7.025e-03   0.231  0.81733    
    ## self_reference_avg_sharess   -4.015e-03  1.859e-02  -0.216  0.82905    
    ## LDA_00                        8.450e+02  9.251e+02   0.913  0.36109    
    ## LDA_01                        8.693e+02  1.021e+03   0.851  0.39467    
    ## LDA_02                       -1.179e+02  9.150e+02  -0.129  0.89745    
    ## LDA_03                        4.822e+02  9.742e+02   0.495  0.62064    
    ## LDA_04                               NA         NA      NA       NA    
    ## global_subjectivity           2.238e+03  1.700e+03   1.316  0.18824    
    ## global_sentiment_polarity    -4.755e+03  3.257e+03  -1.460  0.14442    
    ## global_rate_positive_words   -1.772e+04  1.406e+04  -1.260  0.20766    
    ## global_rate_negative_words    1.487e+04  2.627e+04   0.566  0.57139    
    ## rate_positive_words           4.127e+03  2.140e+03   1.929  0.05383 .  
    ## rate_negative_words                  NA         NA      NA       NA    
    ## avg_positive_polarity         3.553e+03  2.710e+03   1.311  0.18996    
    ## min_positive_polarity        -4.960e+03  2.220e+03  -2.234  0.02551 *  
    ## max_positive_polarity         5.209e+01  8.641e+02   0.060  0.95193    
    ## avg_negative_polarity         1.087e+03  2.523e+03   0.431  0.66669    
    ## min_negative_polarity        -9.615e+02  9.189e+02  -1.046  0.29544    
    ## max_negative_polarity         8.709e+02  2.078e+03   0.419  0.67514    
    ## title_subjectivity            4.532e+02  5.585e+02   0.811  0.41713    
    ## title_sentiment_polarity      7.001e+02  5.090e+02   1.376  0.16905    
    ## abs_title_subjectivity       -1.147e+02  7.380e+02  -0.155  0.87644    
    ## abs_title_sentiment_polarity -7.506e+02  8.211e+02  -0.914  0.36070    
    ## channel_typeEntertainment     2.043e+02  7.338e+02   0.278  0.78076    
    ## channel_typeLifestyle         3.450e+02  7.470e+02   0.462  0.64419    
    ## channel_typeOther             1.870e+03  7.703e+02   2.427  0.01525 *  
    ## `channel_typeSocial Media`    1.591e+03  6.190e+02   2.571  0.01018 *  
    ## channel_typeTech              8.910e+02  6.615e+02   1.347  0.17807    
    ## channel_typeWorld             8.634e+02  7.004e+02   1.233  0.21773    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7308 on 3941 degrees of freedom
    ## Multiple R-squared:  0.04686,    Adjusted R-squared:  0.03526 
    ## F-statistic: 4.037 on 48 and 3941 DF,  p-value: < 2.2e-16

``` r
# Make predictions using the Test set
lmPred <- predict(linReg, DayTest)
DayTest$predictions <- lmPred
kable(head(select(DayTest, shares, predictions)))
```

| shares | predictions |
| -----: | ----------: |
|   3200 |    5085.655 |
|   1100 |    2134.585 |
|   4400 |    1824.191 |
|   1800 |    1960.072 |
|    924 |    2768.178 |
|   1900 |    2552.387 |

``` r
# Compare test RMSE of lm model to those of the two tree models
lmRMSE <- sqrt(mean((lmPred - DayTest$shares)^2))
kable(c("Regression Tree" = trRMSE, 
        "Boosted Tree" = boostRMSE, 
        "Linear Regression" = lmRMSE),
      col.names = "RMSE")
```

|                   |      RMSE |
| ----------------- | --------: |
| Regression Tree   | 10235.729 |
| Boosted Tree      |  9475.092 |
| Linear Regression |  9506.218 |
