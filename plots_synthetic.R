setwd("D:/my_computer/Est_ML_2019/adaboost_implementation/shiny/adaboost_project/adaBoosting")

library(tidyverse)

Circles_Noisy<- read_csv("circles_noise_15.csv")
Circles_Noisy$X1<-as.factor(Circles_Noisy$X1)
levels(Circles_Noisy$X1)<-c("-1","1")
colnames(Circles_Noisy)<-c("classes","x.1","x.2")
Circles_Noisy<-data.frame(x.1=Circles_Noisy$x.1,
                          x.2=Circles_Noisy$x.2,
                          classes=Circles_Noisy$classes)

ggplot(Circles_Noisy)+
      geom_point(mapping=aes(x=x.1,y=x.2,fill=classes),col='black',pch=21,size=1.4,show.legend = FALSE)+
      ggtitle(paste("Complete Dataset: Circles"))+
      scale_fill_manual(values = c("1"='#0050FF',"-1"= '#FF9000'))+
      scale_color_manual(values=c("1"='#8CB0FF',"-1"='#FFC06D'))+
      xlab("X1")+
      ylab("X2")+      
      scale_size_area()+
      theme_bw()

sample_index<-sample(1:nrow(Circles_Noisy),round(0.7*nrow(Circles_Noisy)))
training_data<-Circles_Noisy[sample_index,]
test_data<-Circles_Noisy[-sample_index,]

all_models_x<-map(1:250,~adaBoost_tree(formula=classes~.,data=training_data,
                                     n_models=.x,stump = TRUE))


plot_adaBoost(data=test_data,all_models_x[[250]])


gif4<-animate(animate_adaBoost(test_data,all_models_x),start_pause=3,duration=40,end_pause=3)
anim_save(filename = "ada_gif_presentation4.gif",gif4)


#==========
y_pred_test<-map(all_models_x,~predict_test_adaBost_tree(test_data = test_data,models_selected = .x$models,
                                                          alpha = .x$alpha))
error_test<-1-map_dbl(y_pred_test,~sum(diag(table(.x,test_data$classes)))/sum(table(.x,test_data$classes)))

y_pred_train<-map(all_models_x,~predict_test_adaBost_tree(test_data = training_data,models_selected = .x$models,
                                                           alpha = .x$alpha))
error_train<-1-map_dbl(y_pred_train,~sum(diag(table(.x,training_data$classes)))/sum(table(.x,training_data$classes)))

n_model<-1:length(error_test)

error_train<-data.frame(error=error_train,
                        type_data=as.factor("train"),
                        n_model=n_model)

error_test<-data.frame(error=error_test,
                       type_data=as.factor("test"),
                       n_model=n_model)

total_error<-rbind(error_train,error_test)


erro_plot3<-animate(plot_error_animate(total_error),end_pause=5)
anim_save(filename='error_gif_presentation3.gif',erro_plot3)



      #==========

ggplot(total_error)+
      geom_line(mapping = aes(x=n_model,y=error,col=type_data),lwd=1)+
      scale_y_continuous(limits = c(0.0,max(0.5,total_error$error)),expand=c(0,0))+
      xlab("Number of Models")+
      ylab("% Error")+
      ggtitle("AdaBoosting Error")+
      scale_color_manual(values=c("train"='#33658A',"test"='#CC6C35'),labels = c("train"="Training Error","test"="Test Error"))+
      guides(col=guide_legend(title = NULL,keywidth = 2,keyheight = 1.5))+
      theme_bw()+
      theme(legend.justification=c(0.8,0.8), legend.position=c(0.9,0.9),
            legend.text = element_text(size=13),
            legend.background = element_rect(colour = 'black',
                                             linetype = 'solid'))
