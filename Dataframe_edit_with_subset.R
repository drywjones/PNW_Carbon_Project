# manually edit dataframes by subsets


df_ed_w_subs<-function(data,sub,sub.ids,edits=NULL){
  require(dplyr)
  ## data - dataframe that will be edited by subset
  ## sub -  name of column (or columns) to use for subsetting. If more
  ##        than one column name is entered the columns will be combined together
  ##        into one sebset column. Column entries are separated by a "_".
  ## sub.id - what values should be matched in subsetting? For sub.id with
  ##          more than one value the values will be edited separately with
  ##          a dataframe pop up for each. 
  ##          
  
  # get original order of column names to resort later:
  name.order<-names(data)
  
  # get column numbers that match values in sub
  matches<-match(sub,names(data))
  
  if(any(is.na(matches))){
    stop("One or more names in sub has no match in data")
  }
  if(length(edits)>0){

    matches.eds<- match(edits,names(data))
    if(any(is.na(matches.eds))){
      stop("One or more names in edits has no match in data")
    }else{
      not.edits<-names(data)[!(names(data)%in%edits)]
      data<-data[,c(edits,not.edits)]
    }
    
  }

  
  if(length(sub)>1){
    data<-unite(data,col="subs",all_of(sub),remove=FALSE)
  }else{
    data$subs<-data%>%pull(var=c(sub))
  }
  
  if(any(is.na(match(sub.ids,c(unique(data$subs)))))){
    stop("One or more values in sub.ids has no match in subset columns")
  }
  
  for(i in 1:length(sub.ids)){
    data.sub<-data[data$subs==sub.ids[i],]
    data.sub<-fix(data.sub)
    data[data$subs==sub.ids[i],]<-data.sub
    
  }
  
  data$subs<-NULL
  data<-data[,name.order]
  return(data)
}


# make test data
test<-data.frame(names1=c("a","b","c","a","b","c"),
                 names2=c("d","e","f","f","e","g"),
                 values=c(1,2,3,4,5,6))

# run with two columns for subsetting - and two sub.ids. two separate data entry popups 
# will appear make edits and close. When done output will be saved in new.test dataframe. 
new.test<-df_ed_w_subs(test,
                       sub=c("names1","names2"),
                       sub.ids=c("a_d","b_e"),# individual entries separated by an "_"
                       edits=c("values")# this column will show up furthest to left for edits but
                       # revert to original position for final data.
                       )


# run with one column for subsetting - and two sub.ids two separate data entry popups 
# will appear make edits and close. When done output will be saved in new.test dataframe. 
new.test2<-df_ed_w_subs(test,
                       sub=c("names1"),
                       sub.ids=c("a","b"),# individual entries are as they appear in names1 column
                       edits=c("values") # this column will show up furthest to left for edits but
                                         # revert to original position for final data.
                       )

