library(tidyverse)
library(caret)
library(pls)


#Read in data
pre_annual_flux<-read_csv("ars_annual_N2O.csv")

#Filter and adjust data - Morris is a special case
annual_flux<-pre_annual_flux%>%
  select(-Unit_ID, -site)%>%
  mutate(total_N2O = ifelse((town == "Morris"), total_N2O*0.80, total_N2O))%>%
  distinct()%>%
  na.omit()

#######Getting data into required formats#######

#Clean up predictor variables
flux_x<-select(annual_flux, -series, -town, -avg_N2O, -log_avg_N2O,-year, -total_N2O, -log_total_N2O)

#Response variable
flux_y<-select(annual_flux, total_N2O)

#Not everything likes tibbles
flux_xish<-as.data.frame(unclass(flux_x))

#Centering and scaling, removing rows with NA
flux_x_processed<-preProcess(flux_xish, method = c("BoxCox", "center", "scale"), na.remove = TRUE)
flux_x_pro_mat<-predict(flux_x_processed, flux_xish) #Not 100% sure what this line does

#Put the x and y back together for PLS
final_df<-cbind(flux_y, flux_x_pro_mat)

#######Modeling#######

#Fit model
plsFit<-plsr(total_N2O ~ ., validation = "LOO", data=final_df)

#Look at model
summary(plsFit)
plot(RMSEP(plsFit), legendpos = "none") 
plot(plsFit, ncomp=6, asp = 0, line=TRUE)

#Goodness of fit
RMSEP(plsFit)
R2(plsFit)


#Rank importance of predictor variables 
#?varImp
varImp(plsFit)%>%
  rownames_to_column()%>%
  ggplot(aes(x=reorder(rowname, -Overall), y=Overall))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(y = "Coefficient sum", x = "Predictors")
