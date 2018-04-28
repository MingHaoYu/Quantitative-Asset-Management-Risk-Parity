#403-1 Quantitative Asset Management Hw2
#Author: Ming-Hao Yu
#Date: 2018/04/16

library(data.table)
library(lubridate)
library(dplyr)
setwd("C:/Users/Ming-Hao/Desktop/MFE/431-1-Quantitative Asset Management")

if(!exists("CRSP_Bonds")){
    CRSP_Bonds = fread(file="CRSP_Bonds.csv")
    CRSP_Bonds[, MCALDT:=as.Date(MCALDT, "%m/%d/%Y")]
}
if(!exists("CRSP_Stocks")){
    CRSP_Stocks = fread(file="CRSP_Stocks.csv")
    CRSP_Stocks[, date:=as.Date(date, "%Y/%m/%d")]
}
if(!exists("CRSP_Riskless")){
    CRSP_Riskless = fread(file="CRSP_Riskless.csv")
    CRSP_Riskless[, caldt:=as.Date(as.character(caldt), "%Y%m%d")]
}

PS1_Q1 <- function(CRSP_Stocks) {
    #Restrict to the common shares and securities traded in NYSE
    CRSP_Stocks[, date:=as.Date(date, "%Y/%m/%d")]
    dt = CRSP_Stocks[SHRCD %in% c(10, 11) & EXCHCD %in% c(1,2,3), ]
    
    #Process Missing return
    dt$RET = as.numeric(as.character(dt$RET))
    dt$DLRET = as.numeric(as.character(dt$DLRET))
    dt[RET %in% c(-66, -77, -88, -99), "RET"] = NA
    dt[DLRET %in% c(-66, -77, -88, -99), "DLRET"] = NA
    
    #Calculate cum-dividend return
    dt[!is.na(RET) & is.na(DLRET), Return:=RET]
    dt[is.na(RET) & !is.na(DLRET), Return:=DLRET]
    dt[!is.na(RET) & !is.na(DLRET), Return:=(1+RET)*(1+DLRET)-1]
    
    dt[, MktCap:=abs(PRC)*SHROUT]
    dt[, MktCapLag:=lag(MktCap), by=PERMNO]
    
    dt = dt[, -c("SHRCD", "EXCHCD", "DLRET", "PRC", "RET", "SHROUT", "MktCap")]
    
    #Eliminate NA
    dt = na.omit(dt)
    
    dt[, YM:=format(date, "%Y%m")]
    
    out = dt %>% group_by(YM) %>% summarise(
        #Year = format(last(date), "%Y"),
        #Month = format(last(date), "%m"),
        Number = mean(length(unique(PERMNO))),
        Year = mean(year(date)),
        Month = mean(month(date)),
        Stock_lag_MV = sum(MktCapLag/1000),
        Stock_Ew_Ret = sum(Return),
        Stock_Vw_Ret = sum(Return*MktCapLag/sum(MktCapLag))
    )
    out$Stock_Ew_Ret = out$Stock_Ew_Ret/c(out$Number[1], head(out$Number, -1))
    return(out[, seq(3,7)])
}

PS2_Q1 <- function(CRSP_Bonds) {
    dt = data.table(CRSP_Bonds)
    dt[which(is.na(CRSP_Bonds$TMTOTOUT)), TMTOTOUT:=NA]
    dt[TMRETNUA %in% c(-66, -77, -88, -99), "TMRETNUA"] = NA
    dt[, MVLag:=lag(TMTOTOUT), by=KYCRSPID]
    dt[, YM:=format(MCALDT, "%Y%m")]
    
    dt = na.omit(dt)
    out = dt %>% group_by(YM) %>% summarise(
        Number = mean(length(unique(KYCRSPID))),
        Year = mean(year(MCALDT)),
        Month = mean(month(MCALDT)),
        Bond_lag_MV = sum(MVLag),
        Bond_Ew_Ret = sum(TMRETNUA),
        Bond_Vw_Ret = sum(TMRETNUA*MVLag/sum(MVLag))
    )
    #capital_(t-1) * return_t
    out$Bond_Ew_Ret = out$Bond_Ew_Ret/c(out$Number[1], head(out$Number, -1))
    return(out[, seq(3,7)])
}

Monthly_CRSP_Stocks = PS1_Q1(CRSP_Stocks)
Monthly_CRSP_Bonds = PS2_Q1(CRSP_Bonds)
Monthly_CRSP_Riskless = CRSP_Riskless

PS2_Q2 <- function(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds, Monthly_CRSP_Riskless) {
    Rf = data.table(Monthly_CRSP_Riskless)
    Rf[, Year:=year(caldt)]
    Rf[, Month:=month(caldt)]
    Rf = Rf[, c("Year", "Month", "t30ret")]
    
    dt = merge(data.table(Monthly_CRSP_Stocks), data.table(Monthly_CRSP_Bonds), by=c("Year", "Month"))
    dt = merge(dt, Rf, by=c("Year", "Month"))
    dt = dt[order(dt$Year, dt$Month), ]
    
    dt[, Stock_Excess_Vw_Ret:=Stock_Vw_Ret-t30ret]
    dt[, Bond_Excess_Vw_Ret:=Bond_Vw_Ret-t30ret]
    dt = subset(dt, select=c("Year", "Month", "Stock_lag_MV", "Stock_Excess_Vw_Ret", "Bond_lag_MV", "Bond_Excess_Vw_Ret"))
    return(dt)
}

Monthly_CRSP_Universe = PS2_Q2(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds, Monthly_CRSP_Riskless)

PS2_Q3 <- function(Monthly_CRSP_Universe){
    dt = data.table(Monthly_CRSP_Universe)
    if(length(which(dt$Year>2010)) !=0) {dt = dt[-which(Year>2010), ]}
    dt[, Excess_Vw_Ret:=(Stock_Excess_Vw_Ret*Stock_lag_MV+Bond_Excess_Vw_Ret*Bond_lag_MV)/(Stock_lag_MV+Bond_lag_MV)]
    dt[, Excess_60_40_Ret:=Stock_Excess_Vw_Ret*0.6+Bond_Excess_Vw_Ret*0.4]
    
    StockSigma = BondSigma = rep(NA, length=length(dt$Year))
    for(i in 37:length(dt$Year)) {
        StockSigma[i] = sd(dt$Stock_Excess_Vw_Ret[(i-36):(i-1)])
        BondSigma[i] = sd(dt$Bond_Excess_Vw_Ret[(i-36):(i-1)])
    }
    dt[, Stock_inverse_sigma_hat:=1/StockSigma]
    dt[, Bond_inverse_sigma_hat:=1/BondSigma]
    dt[, Unlevered_k:=1/(Stock_inverse_sigma_hat+Bond_inverse_sigma_hat)]
    dt[, Excess_Unlevered_RP_Ret:=Unlevered_k*(Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret+Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret)]
    dt[, Levered_k:=1]
    dt[, Excess_Levered_RP_Ret:=Levered_k*(Stock_inverse_sigma_hat*Stock_Excess_Vw_Ret+Bond_inverse_sigma_hat*Bond_Excess_Vw_Ret)]
    k = sd(dt$Excess_Vw_Ret)/sd(dt$Excess_Levered_RP_Ret[37:length(dt$Excess_Levered_RP_Ret)])
    dt[, Levered_k:=k]
    dt[, Excess_Levered_RP_Ret:=Levered_k*Excess_Levered_RP_Ret]
    dt = subset(dt, select=c("Year", "Month", "Stock_Excess_Vw_Ret", "Bond_Excess_Vw_Ret", 
                             "Excess_Vw_Ret", "Excess_60_40_Ret", "Stock_inverse_sigma_hat", "Bond_inverse_sigma_hat",
                             "Unlevered_k", "Excess_Unlevered_RP_Ret", "Levered_k", "Excess_Levered_RP_Ret"))
    return(dt)
}

Port_Rets = PS2_Q3(Monthly_CRSP_Universe)

PS2_Q4 <- function(Port_Rets) {
    dt = data.table(Port_Rets)
    dt = na.omit(dt)
    
    #constrain data range from 1930~2010
    if(length(which(dt$Year>2010)) !=0) {dt = dt[-which(Year>2010), ]}
    
    out = matrix(nrow=6, ncol=6)
    d1 = dt$Stock_Excess_Vw_Ret
    d2 = dt$Bond_Excess_Vw_Ret
    d3 = dt$Excess_Vw_Ret
    d4 = dt$Excess_60_40_Ret
    d5 = dt$Excess_Unlevered_RP_Ret
    d6 = dt$Excess_Levered_RP_Ret
    
    out[, 1] = c(mean(d1), mean(d2), mean(d3), mean(d4), mean(d5), mean(d6)) * 12
    
    t1 = mean(d1) / (sd(d1)/sqrt(length(d1)))
    t2 = mean(d2) / (sd(d2)/sqrt(length(d2)))
    t3 = mean(d3) / (sd(d3)/sqrt(length(d3)))
    t4 = mean(d4) / (sd(d4)/sqrt(length(d4)))
    t5 = mean(d5) / (sd(d5)/sqrt(length(d5)))
    t6 = mean(d6) / (sd(d6)/sqrt(length(d6)))
    out[, 2] = c(t1, t2, t3, t4, t5, t6)
    
    out[, 3] = c(sd(d1), sd(d2), sd(d3), sd(d4), sd(d5), sd(d6))*sqrt(12)
    out[, 4] = out[, 1] / out[, 3]
    s1 = mean(((d1-mean(d1))/sd(d1))^3)
    s2 = mean(((d2-mean(d2))/sd(d2))^3)
    s3 = mean(((d3-mean(d3))/sd(d3))^3)
    s4 = mean(((d4-mean(d4))/sd(d4))^3)
    s5 = mean(((d5-mean(d5))/sd(d5))^3)
    s6 = mean(((d6-mean(d6))/sd(d6))^3)
    k1 = mean(((d1-mean(d1))/sd(d1))^4)-3
    k2 = mean(((d2-mean(d2))/sd(d2))^4)-3
    k3 = mean(((d3-mean(d3))/sd(d3))^4)-3
    k4 = mean(((d4-mean(d4))/sd(d4))^4)-3
    k5 = mean(((d5-mean(d5))/sd(d5))^4)-3
    k6 = mean(((d6-mean(d6))/sd(d6))^4)-3
    
    out[, 5] = c(s1, s2, s3, s4, s5, s6)
    out[, 6] = c(k1, k2, k3, k4, k5, k6)
    
    colnames(out) = c("Annualized Mean", "t-stat of Annualized Mean", "Annualized Standard Deviation", 
                      "Annualized Sharpe Ratio", "Skewness", "Excess Kurtosis")
    rownames(out) = c("CRSP Stocks", "CRSP Bonds", "Value-weighted portfolio", "60/40 portfolio", "unlevered RP", "levered RP")
    return(out)
}

Result = PS2_Q4(Port_Rets)
