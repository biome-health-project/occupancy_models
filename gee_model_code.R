unmarker<-function(sp_matrix, species_name){
  umf.sp_matrix<-unmarkedFrameOccu(y=sp_matrix, siteCovs=covs)  
  m0_5d<-occu(~1 ~1,data=umf.sp_matrix) # null model
  m1_5d <- occu(~1 ~manag,data=umf.sp_matrix) # managemt type
  m2_5d<-occu(~1 ~DistPark_Norm,data=umf.sp_matrix) #distance to park
  
  m_list<-list(m0_5d, m1_5d, m2_5d)
  names(m_list)<-c(paste0(species_name, " m0"),
                   paste0(species_name, " m1"),
                   paste0(species_name, " m2"))
  return(m_list)
}



###function which gets predictions from a chosen model
predict_unmarked<-function(model, species_name) {
  # Predicting m2
  predPark<-predict(model,type="state",newdata=dist_seq,appendData=T)
  parkReal<- (predPark$DistPark*sdPark)+meanPark #formula=(z-value*SD of original value)+Mean of original values
  predParkReal<- cbind(species_name,parkReal,predPark) #adding the real values as a collumn to the predicted dataframe  
  return(predParkReal)
}