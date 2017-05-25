library(sparklyr)
library(dplyr)
library(DBI)

### Establish a connection with locl spark
sc=spark_connect(master="local")

load("req_month.rda")
load("month_request.rda")
load("reqNoSR.rda")
load("date_request.rda")
load("request_data.rda")

RequestType=copy_to(dest=sc,
                       df=request_data,
                       name="RequestType")

req_cnt=copy_to(dest=sc,
                    df=date_request,
                    name="req_cnt")



reqNoSR=copy_to(dest=sc,
                df=reqNoSR,
                name="reqNoSR")


req_month=copy_to(dest=sc,
                      df=req_month,
                      name="req_month")
month_req=copy_to(dest=sc,
                  df=month_request,
                  name="month_request")

dbListTables(sc)

