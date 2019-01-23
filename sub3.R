library(data.table)
setwd("/Users/mark/Documents/AV-Capillary/")
train<-fread("train.csv")
train[,dtOrderDate:=as.Date(paste0("20",substr(OrderDate,7,8),"-",substr(OrderDate,4,5),"-",substr(OrderDate,1,2)))]

test<-fread("test_nFNPSyV.csv")
ss<-fread("sample_submission_qfCnaKZ.csv")

user_products<-train[,.(user_product_orders=.N,user_product_qty=sum(Quantity),user_product_last_ordered=max(dtOrderDate))
                     ,.(UserId,productid)]
sub_user<-user_products[order(UserId,-user_product_last_ordered,-user_product_orders,-user_product_qty)]
sub_user[,sub_user_rank:=.I]
sub_user[,min_rank:=min(sub_user_rank),UserId]
sub_user[,user_rank:=sub_user_rank-min_rank+1]
sub_user<-sub_user[user_rank<=10]
## now get overall item popularity...for most recent month
sub_product<-train[dtOrderDate>="2018-10-21",.(product_orders=.N),productid][order(-product_orders)]
sub_product[,product_rank:=.I+10]

final_sub<-sub_user[,.(UserId,productid,user_rank)]
for(pRank in 11:25){
  final_sub[,items:=.N,UserId]
  print(final_sub[,.(.N,stage="before"),items][order(items)])
  new_items<-final_sub[items<10 & user_rank==1 & !(UserId %in% final_sub[productid==sub_product[product_rank==pRank,productid]])
                       ,.(UserId,productid=sub_product[product_rank==pRank,productid],user_rank=pRank,items=0L)]
  final_sub<-rbind(final_sub,new_items)
  final_sub[,items:=.N,UserId]
  print(final_sub[,.(.N,stage="after"),items][order(items)])
}
final_sub<-final_sub[UserId %in% test$UserId][order(UserId,user_rank)]
submission_format<-final_sub[,.(product_list=paste(productid,collapse = ", ")),UserId]
submission_format[,product_list:=paste0("[",product_list,"]")]
submission_format[1:2]
fwrite(submission_format,"sub3_updatedDate.csv")
