urTestStratFcn <- function(series,growth=0,sigLev1=.05,sigLev2=.05){
  
  #Description: this function implements an enhanced version of the unit root testing 
  #strategy from Elder and Kennedy (2001). It implements testing following the logic of the paper
  #but uses augmented, rather than standard, Dickey-Fuller Test
  #specifications. It uses the augmented dickey fuller tests from the urca package.
  #See the readme file.
  
  #required library
  library(urca)
  
  #browser()
  
  #remove any NA values from series
  series <- series[!is.na(series)]
  series <- series[!is.nan(series)]
  
  #exit function if there are too few observations
  if(length(series) < 10){
    stop("too few observations to run testing strategy")
  }
  
  #map case descriptions
  caseDesc <- list(case1 = "Unit Root, Drift",
                   case2 = "No Unit Root, Deterministic Time Trend (Trend Stationary)",
                   case3 = "Unit Root, No Drift",
                   case4 = "No Unit Root, No Determinisitc Time Trend (Stationary)")

  
  #map critical value positions in urca adf function output
  if(sigLev1==.01){
    resultCol <- 1
  } else if(sigLev1==.05){
    resultCol <- 2
  } else if(sigLev1==.10){
    resultCol <- 3
  } else{
    stop("invalid significance level 1 argument")
  }
  
  #set maximum ADF test lags using the Schwert Rule
  kmax <- floor(12*(length(series)/100)**(1/4))
  
  #ADF Test 1: "trend" type
  df1 <- ur.df(series, type = "trend", lags = kmax, selectlags = "AIC")
  #ADF Test 1: "trend" type
  df2 <- ur.df(series, type = "drift", lags = kmax, selectlags = "AIC")
  #drift term regression
  driftFit <- lm(diff(series)~1)
  
  #reject the null hypothesis of gamma=0? (H0==TRUE implies unit root)
  result1 <- abs(df1@teststat[1]) > abs(df1@cval[1,resultCol])
  #reject the null hypothesis of gamma=0? (H0==TRUE implies unit root)
  result2 <- abs(df2@teststat[2]) > abs(df2@cval[1,resultCol])
  #reject the null hypothesis that drift=0?
  result3 <- summary(driftFit)$coefficients[4] < sigLev2
  #reject the null hypothesis that the time trend coefficient is equal to zero?
  result4 <- df1@testreg$coefficients[3,4] < sigLev2
  
  if(growth==1){
  #this is the logic for when the user can confirm that the series grows over time
    if(result1==FALSE){
      #conclude that their is a unit root w/ drift (but no time trend) by using test statistic from
      #df1. However, reported results are from df2 where time trend is restricted to be zero.
      result_case <- "case1"
    } else{
      #conclude that there is no unit root, but there is a deterministic time trend
      result_case <- "case2"
    }
  } else if(growth==2){
    #this is the logic for when the user can confirm that the series does not over time
    if(result2==FALSE){
      result_case <- "case3"
    } else{
      result_case <- "case4"
    }
  } else{
    #this is the logic for when the user does not know whether the series exhibits growth over time
    if(result1==FALSE){
      if(result3==FALSE){
        result_case <- "case3"
      } else{
        result_case <- "case1"
      }
    } else{
      if(result4==FALSE){
        #fail to reject null hyp of no time trend, so conclude that time trend was actually the
        #wrong specification. Need to double check that rejecting unit root was correct decision by
        #checking result 2.
        if(result2==FALSE){
          #Unit root is rejected in df1 spec, but not in df2 spec. So, conservatively conclude that 
          #there is a unit root. Next, need to determine if there is a drift term by checking
          #result3
          if(result3==FALSE){
            result_case <- "case3"
          } else{
            result_case <- "case1"
          }
        } else{
          result_case <- "case4"
        }
      } else{
        result_case <- "case2"
      }
    }
  }
  
  #record results and return as data frame
  outList <- list()
  outList$result <- caseDesc[result_case][[1]]
  
  if(result_case=="case1" || result_case=="case3" || result_case=="case4"){
    outList <- within(outList,{
      ADF_regSpec <- df2@model
      ADF_lags <- df2@lags
      lag1_testStat <- df2@teststat[1]
      lag1_coeff <- df2@testreg$coefficients[2,1]
      lag1_critVal.01 <- df2@cval[1,1]
      lag1_critVal.05 <- df2@cval[1,2]
      lag1_critVal.1 <- df2@cval[1,3]
      drift_coeff <- df2@testreg$coefficients[1]
      drift_pval <- df2@testreg$coefficients[1]
      time_coeff <- NA
      time_pval <- NA
      })
  } else if(result_case=="case2"){
    outList <- within(outList,{
      ADF_regSpec <- df1@model
      ADF_lags <- df1@lags
      lag1_testStat <- df1@teststat[1]
      lag1_coeff <- df1@testreg$coefficients[2,1]
      lag1_critVal.01 <- df1@cval[1,1]
      lag1_critVal.05 <- df1@cval[1,2]
      lag1_critVal.1 <- df1@cval[1,3]
      drift_coeff <- df1@testreg$coefficients[1]
      drift_pval <- df1@testreg$coefficients[13]
      time_coeff <- df1@testreg$coefficients[3]
      time_pval <- df1@testreg$coefficients[15]
    })
  }
 return(data.frame(outList)) 
}
