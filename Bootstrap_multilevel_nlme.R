BootNLME <- function(fitted.object,data) {
  
  idvar=c(names(ranef(fitted.object)))
  responses<-sub("\\~.*","",formula(fitted.object))
  responses<-responses[responses!=""]
  resp=responses[1]
  pp <- predict(fitted.object,levels=length(idvar))
  rr <- residuals(fitted)
  dd <- data.frame(data,pred=pp,res=rr)
  ## sample top-level groups with replacement
  iv1 <- levels(dd[[idvar[1]]])
  bsamp1 <- sample(iv1,size=length(iv1),replace=TRUE)
  ## sample next level, *within* top-level groups
  bsamp2 <- lapply(bsamp1,
                   function(id1) {
                     iv2 <- unique(as.character(dd[dd[[idvar[1]]]==id1,idvar[2]]))
                     sample(iv2,size=length(iv2),replace=TRUE)
                   })
  ## sample at lowest level:
  ## for loop rather than nested lapply() to reduce(?) confusion
  bsamp3 <- list()
  for (i in seq_along(bsamp2)) {
    bsamp3[[i]] <- lapply(bsamp2[[i]],
                          function(x) {
                            ddb <- dd[dd[[idvar[1]]]==bsamp1[i] &
                                        dd[[idvar[2]]]==as.character(x),]
                            ddb[[resp]] <- ddb$pred +
                              sample(ddb$res,size=nrow(ddb),replace=TRUE)
                            return(ddb)
                          })
  }
  ## flatten everything
  res <- do.call(rbind,lapply(bsamp3,
                              function(x) do.call(rbind,x)))
  if (is(data,"groupedData"))
    res <- groupedData(res,formula=formula(data))
  return(res)
}

sampfun(fm1,WBF,c("Species","ID"))