

RequestType=copy_to(dest=sc,
                    df=request_data,
                    name="RequestType")

req_cnt=copy_to(dest=sc,
                df=date_request,
                name="req_cnt")

reqNoSR=copy_to(dest=sc,
                df=reqNoSR,
                name="reqNoSR")

month_request=copy_to(dest=sc,
                      df=month_request,
                      name="month_request")

req_month=copy_to(dest=sc,
                  df=req_month,
                  name="req_month")
