library(sparklyr)
library(dplyr)
load("sparkdataset.RData")


sc=spark_connect(master="local")
RequestTypeGeo=copy_to(dest=sc,
                       df=RequestTypeGeo,
                       name="RequestTypeGeo")
APCRequestTypeCnt=copy_to(dest=sc,
                          df=APCRequestTypeCount,
                          name="APCRequestTypeCount")
CDRequestTypeCnt=copy_to(dest=sc,
                         df=CDRequestTypeCount,
                         name="CDRequestTypeCount")

PPRequestTypeCnt=copy_to(dest=sc,
                         df=PPRequestTypeCount,
                         name="PPRequestTypeCount")

SourceRequestTypeCnt=copy_to(dest=sc,
                             df=SourceRequestTypeCount,
                             name="SourceRequestTypeCount")


NCRequestTypeCnt=copy_to(dest=sc,
                             df=NCRequestTypeCount,
                             name="NCRequestTypeCount")
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

