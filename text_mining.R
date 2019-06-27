setwd('D:/my_computer/Est ML 2018_complete/CSDS')
#read_csv1('bag_words_bbc_bs_new.csv')
bag_words_bbc_bs_new <- read_csv("D:/my_computer/Est ML 2018_complete/CSDS/bag_words_bbc_bs_new.csv")
bag_words_bbc_bs_new<-bag_words_bbc_bs_new%>%
      mutate(news_class=news_class%>%as.factor)
banco_de_dados<-bag_words_bbc_bs_new[,-1] %>% as.data.frame()

banco_de_dados$news_class<-ifelse(banco_de_dados$news_class==2,1,-1) %>% as.factor

library(fastAdaboost)

set.seed(123)
training_index<-sample(1:nrow(banco_de_dados),round(0.7*nrow(banco_de_dados)))
training_data<-banco_de_dados[training_index,]
test_data<-banco_de_dados[-training_index,]



library(fastAdaboost)

#model<-adaBoost_tree(news_class~.,data=training_data,n_models=100,stump = TRUE)


model<-adaboost(news_class~.,data=training_data,nIter = 10)
pred_news<-predict(model,test_data)

acc2<-sum(diag(table(pred_news$class,test_data$news_class)))/sum(table(pred_news$class,test_data$news_class))
acc2


#================
library(sboost)

mod<-sboost(training_data[-5230],training_data[5230],iterations = 1)
pred_news<-predict(mod,test_data) %>% as.factor


acc<-sum(diag(table(pred_news,test_data$news_class)))/sum(table(pred_news,test_data$news_class))
acc
#================
library(rpart)


mod_tree<-rpart(news_class~.,data=training_data)
pred_tree<-predict(mod_tree,test_data,type='class')

acc_tree<-sum(diag(table(pred_tree,test_data$news_class)))/sum(table(pred_tree,test_data$news_class))
acc_tree

#============
library(randomForest)

mod_rf<-randomForest(news_class~.,data=training_data,ntree=100)


#============



library(kernlab)

mod_svm<-ksvm(news_class~.,data=training_data,scaled=FALSE)
pred_svm<-predict(mod_svm,test_data)
acc_svm<-sum(diag(table(pred_svm,test_data$news_class)))/sum(table(pred_svm,test_data$news_class))
acc_svm



#=======ACP========

pca<-prcomp(banco_de_dados %>% select(-'news_class'))
plot2d<-pca$x[,1:2]

plot(plot2d,col=banco_de_dados$news_class)

pca$sdev


pve=(pca$sdev^2)/sum(pca$sdev^2)

plot(1:length(pve),cumsum(pve))


#===================
acc_train<-numeric(0)
acc_test<-numeric(0)
acc_mean_train<-numeric(0)
acc_mean_test<-numeric(0)
for(k in 1:10){
      acc_train<-numeric(0)
      acc_test<-numeric(0)
      for(i in 1:10){
            
            
                  fold<-dismo::kfold(1:nrow(banco_de_dados),k=10)
                  training_data<-banco_de_dados[fold!=i,]
                  test_data<-banco_de_dados[fold==i,]
                  
                  mod<-sboost(training_data[-5230],training_data[5230],iterations = 190)
                  pred_news_test<-predict(mod,test_data) %>% as.factor
                  pred_news_train<-predict(mod,training_data) %>% as.factor
                  
                  acc_train[i]<-sum(diag(table(pred_news_train,training_data$news_class)))/sum(table(pred_news_train,training_data$news_class))
                  acc_test[i]<-sum(diag(table(pred_news_test,test_data$news_class)))/sum(table(pred_news_test,test_data$news_class))
                  print(k) 
            
            
      }
      acc_mean_train[k]<-mean(acc_train)
      acc_mean_test[k]<-mean(acc_test)

}


error_train<-1-acc_train
error_test<-1-acc_test


error_mean_train<-1-acc_mean_train
error_mean_test<-1-acc_mean_test



error_train_data<-data.frame(error=error_train,
                             type_data=as.factor("train"),
                             n_model=1:500)

error_test_data<-data.frame(error=error_test,
                            type_data=as.factor('test'),
                            n_model=1:500)
#==========
error_train_data_mean<-data.frame(error=error_mean_train,
                             type_data=as.factor("train"),
                             n_model=1:200)

error_test_data_mean<-data.frame(error=error_mean_test,
                            type_data=as.factor('test'),
                            n_model=1:200)


# write.csv(error_train_data,'error_tr_backup.csv')
# write.csv(error_test_data,'error_test_backup.csv')


#write.csv(error_train_data_mean,'error_tr_backup_mean.csv')
#write.csv(error_test_data_mean,'error_test_backup_mean.csv')

error_200<-rbind(error_train_data_mean,error_test_data_mean)

ggplot(error_200)+
      geom_line(mapping = aes(x=n_model,y=error,col=type_data),lwd=0.5)+
      scale_y_continuous(limits = c(0.0,max(0.5,error_200$error)),expand=c(0,0))+
      xlab("Number of Models")+
      ylab("% Error")+
      ggtitle("AdaBoosting Error BBC News")+
      scale_color_manual(values=c("train"='#33658A',"test"='#CC6C35'),labels = c("train"="Training Error","test"="Test Error"))+
      guides(col=guide_legend(title = NULL,keywidth = 2,keyheight = 1.5))+
      theme_bw()+
      theme(legend.justification=c(0.8,0.8), legend.position=c(0.9,0.9),
            legend.text = element_text(size=13),
            legend.background = element_rect(colour = 'black',
                                             linetype = 'solid'))

# 
# set.seed(23)
# sample(1:60,6)


set.seed(42)
sample(1:60,6)
