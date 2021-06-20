install.packages("dplyr")
install.packages("Screeplots", "psych")

library("dplyr")

defaultW <- getOption("warn")
options(warn = -1)

creditdf <- read.csv("credit-card-data.csv", sep=",", header = TRUE, stringsAsFactors = FALSE)

#list of columns
str(creditdf)

#data type information
summary(creditdf)
# Missing Values Treatment
# Quick code to replace missing values with the mean

creditdf$MINIMUM_PAYMENTS[which(is.na(creditdf$MINIMUM_PAYMENTS))] <- (mean(creditdf$MINIMUM_PAYMENTS, na.rm=TRUE))
creditdf$CREDIT_LIMIT[which(is.na(creditdf$CREDIT_LIMIT))] <- mean(creditdf$CREDIT_LIMIT, na.rm=TRUE)

# deriving the new columns
# like monthly average purchase and cash advance amount,
# purchases by type (one-off, installments), average amount per purchase and
# cash advance transaction, limit usage (balance to credit limit ratio), payments to
# minimum payments ratio etc.

#monthly average purchase
creditdf$MONTHLY_AVG_PURCHASE <- creditdf$PURCHASES/creditdf$TENURE
#monthly cash advance
creditdf$MONTHLY_CASH_ADVANCE <- creditdf$CASH_ADVANCE/creditdf$TENURE

# Purchase by type
purchase_type <- function(credit){
  
  if(as.numeric(credit['INSTALLMENTS_PURCHASES'])>0.0 & as.numeric(credit['ONEOFF_PURCHASES'])==0.0){
    return('INSTALLMENT')
  }
  if (as.numeric(credit['ONEOFF_PURCHASES'])==0.0 & !is.na(as.numeric(credit['INSTALLMENTS_PURCHASES'])==0.0)){
    return('NONE')
  }
  if (!is.na(as.numeric(credit['ONEOFF_PURCHASES']))>0.0 & as.numeric(credit['INSTALLMENTS_PURCHASES'])>0.0){
    return('BOTH_ONEOFF_INSTALLMENT')
  }
  if (!is.na(as.numeric(credit['ONEOFF_PURCHASES']))>0.0 &as.numeric(credit['INSTALLMENTS_PURCHASES'])==0.0){
    return('ONE_OFF')
  }
  
}

creditdf['PURCHASE_TYPE'] = apply(creditdf,1,purchase_type)
as.data.frame(table(creditdf$PURCHASE_TYPE))
#Limit Usage
creditdf$LIMIT_USAGE <- creditdf$BALANCE/creditdf$CREDIT_LIMIT

#Min Payment Ratio
creditdf$MINIMUM_PAYMENT_RATIO <- creditdf$PAYMENTS/creditdf$MINIMUM_PAYMENTS

Num_Vars <- c(
  "BALANCE",
  "BALANCE_FREQUENCY",
  "PURCHASES",
  "MONTHLY_AVG_PURCHASE",
  "ONEOFF_PURCHASES",
  "INSTALLMENTS_PURCHASES",
  "CASH_ADVANCE",
  "MONTHLY_CASH_ADVANCE",
  "PURCHASES_FREQUENCY",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "CASH_ADVANCE_FREQUENCY",
  "CASH_ADVANCE_TRX",
  "PURCHASES_TRX",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PAYMENTS",
  "MINIMUM_PAYMENTS",
  "MINIMUM_PAYMENT_RATIO",
  "PRC_FULL_PAYMENT",
  "TENURE")

Step_nums <- creditdf[Num_Vars]
corrm<- cor(Step_nums)    
View(corrm)

write.csv(corrm, "Correlation_matrix.csv")

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE)### SCREE PLOT


eigen(corrm)$values

eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))

library(psych)
FA<-fa(r=corrm, 7, rotate="varimax", fm="ml")  
#SORTING THE LOADINGS
FA_SORT<-fa.sort(FA)      
FA_SORT$loadings

Loadings<-data.frame(FA_SORT$loadings[1:ncol(Step_nums),])
write.csv(Loadings, "loadings2.csv")

# standardizing the data
segment_prepared <-creditdf[Num_Vars]

segment_prepared = scale(segment_prepared)
write.csv(segment_prepared, "standardized data.csv")


#building clusters using k-means clustering 
cluster_three <- kmeans(segment_prepared,3)
cluster_four <- kmeans(segment_prepared,4)
cluster_five <- kmeans(segment_prepared,5)
cluster_six <- kmeans(segment_prepared,6)

credit_new<-cbind(creditdf,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(credit_new)

# Profiling

Num_Vars2 <- c(
  "MONTHLY_AVG_PURCHASE",
  "MONTHLY_CASH_ADVANCE",
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "MINIMUM_PAYMENT_RATIO",
  "BALANCE",
  "TENURE"
)

install.packages("tables")
library(tables)
tt <-cbind(tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
                     factor(km_clust_6)~Heading()*length*All(creditdf[1]),
                   data=credit_new),tabular(1+factor(km_clust_3)+factor(km_clust_4)+factor(km_clust_5)+
                                              factor(km_clust_6)~Heading()*mean*All(creditdf[Num_Vars2]),
                                            data=credit_new))

tt2 <- as.data.frame.matrix(tt)
View(tt2)

rownames(tt2)<-c(
  "ALL",
  "KM3_1",
  "KM3_2",
  "KM3_3",
  "KM4_1",
  "KM4_2",
  "KM4_3",
  "KM4_4",
  "KM5_1",
  "KM5_2",
  "KM5_3",
  "KM5_4",
  "KM5_5",
  "KM6_1",
  "KM6_2",
  "KM6_3",
  "KM6_4",
  "KM6_5",
  "KM6_6")


colnames(tt2)<-c(
  "SEGMENT_SIZE",
  "MONTHLY_AVG_PURCHASE",
  "MONTHLY_CASH_ADVANCE",
  "CASH_ADVANCE",
  "CASH_ADVANCE_TRX",
  "CASH_ADVANCE_FREQUENCY",
  "ONEOFF_PURCHASES",
  "ONEOFF_PURCHASES_FREQUENCY",
  "PAYMENTS",
  "CREDIT_LIMIT",
  "LIMIT_USAGE",
  "PURCHASES_INSTALLMENTS_FREQUENCY",
  "PURCHASES_FREQUENCY",
  "INSTALLMENTS_PURCHASES",
  "PURCHASES_TRX",
  "MINIMUM_PAYMENTS",
  "MINIMUM_PAYMENT_RATIO",
  "BALANCE",
  "TENURE"
)


cluster_profiling2 <- t(tt2)

write.csv(cluster_profiling2,'cluster_profiling2.csv')

