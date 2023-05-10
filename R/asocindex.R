#' Function inter_db
#'
#' @param data a data frame with interactions of recruit & canopy species in each plot of many study sites
#' @param dbcover a data frame with the cover of each canopy species per plot in each study site
#'
#' @return a data frame with interactions of recruit & canopy species in each study site and the cover of each species in columns
#' @export
#' @import dplyr
#'
#' @examples
asocindex <- function(data, dbcover) {

  options(dplyr.summarise.inform = FALSE)

  data$inter_ID<-paste(data$Study_site, data$Recruit, data$Canopy, sep="_")
  dbcover2<-droplevels(dbcover[!is.na(dbcover$Cover),])
  rmnets<-setdiff(dbcover$Study_site, dbcover2$Study_site)

  #add a warning to the user if there is some study_site that does not
  #have information about the canopy of any of its species. Those study sites will be removed

  if (!(length(rmnets==0)))
    warning("For some Study_site there is no information about the canopy of any species")


  dbcover<-dbcover[dbcover$Study_site%in%setdiff(dbcover$Study_site, rmnets),]

  data<-data[data$Study_site%in%setdiff(data$Study_site, rmnets),]

  #check if the pruning of the study sites, if required, have worked properly i.e. now all the study sites have at least one canopy species
  # with information about its cover (if not stop the process)

    if (!(length(setdiff(dbcover$Study_site, data$Study_site))==0|length(setdiff(data$Study_site, dbcover$Study_site))==0))
    stop("Study_site with complete information in the two arguments x,y does not match")

  ###############

  dbcover$Canopy_cover_abs<-with (dbcover, (Cover*Sampled_distance_or_area)/100)

  surfN<-data.frame(dbcover%>%
                      group_by(Study_site, Canopy)%>%
                      dplyr::summarise(Canopy_cover_abs = sum(Canopy_cover_abs,na.rm=TRUE)))


  site_surf<-data.frame(unique(dbcover[,c("Study_site","Plot","Sampled_distance_or_area")])%>%
                          group_by(Study_site)%>%
                          dplyr::summarise(Plot_sup = sum(Sampled_distance_or_area)))


  Canopy_all<-merge(surfN,site_surf, by="Study_site")
  Canopy_all$Canopy_cover<-with(Canopy_all, (Canopy_cover_abs*100)/Plot_sup)
  Canopy_all<-Canopy_all[,c("Study_site","Canopy","Canopy_cover")]

  Canopy_all[Canopy_all$Canopy_cover==0,"Canopy_cover"]<-"NA"

  Canopy_all$Canopy_cover<-as.numeric(Canopy_all$Canopy_cover)
  data$mynet_sp<-paste(data$Study_site, data$Canopy, sep="-")
  Canopy_all$mycover_sp<-paste(Canopy_all$Study_site,Canopy_all$Canopy, sep="-")

  Canopy_all<-Canopy_all[!is.na(Canopy_all$Canopy_cover),]


  rmnets_anyNA<-unique(data[data$mynet_sp%in%setdiff(unique(data$mynet_sp), (Canopy_all$mycover_sp)),"Study_site"])

  dbcover<-droplevels(dbcover[dbcover$Study_site%in%setdiff(dbcover$Study_site, rmnets_anyNA),])
  data<-droplevels(data[data$Study_site%in%setdiff(data$Study_site, rmnets_anyNA),])
  Canopy_all<-droplevels(Canopy_all[Canopy_all$Study_site%in%setdiff(Canopy_all$Study_site, rmnets_anyNA),])

  inter<-data.frame(data%>%
                      group_by(Study_site, Recruit, Canopy,inter_ID)%>%
                      dplyr::summarise(inter_ID=unique(inter_ID), Frequency = sum(Frequency)))



  adjlist<-list()
  coverlist<-list()
  edgelist<-list()
  net<-NULL
  NorecrOpen<-NULL
  for(z in 1:length(unique(Canopy_all$Study_site)))
  {

    net[z]<-unique(Canopy_all$Study_site)[z]
    mycovs<-Canopy_all[Canopy_all$Study_site==unique(Canopy_all$Study_site)[z],]
    myadj<-data[data$Study_site==unique(Canopy_all$Study_site)[z],]



    Freq<-data.frame(myadj%>%
                       group_by(inter_ID)%>%
                       dplyr::summarise(Freq = sum(Frequency)))

    alledge<-merge(Freq,unique(myadj[,c("Study_site","inter_ID","Canopy","Recruit")]),by="inter_ID")
    myadj<-dcast(data=alledge[,c("Recruit","Canopy","Freq")], Recruit~Canopy,value.var="Freq")
    myadj2<-as.matrix(myadj[,-1])
    colnames(myadj2)<-colnames(myadj)[-1]
    rownames(myadj2)<-myadj[,1]
    myadj<-myadj2
    myadj2<-rm
    myadj[is.na(myadj)]<-0
    #selecciono solo aquellas nodrizas de las que tenemos en ambas ( coberturas y adj mat)
    myadj<-myadj[,colnames(myadj)%in%mycovs$Canopy]
    #en "AltiplanoNorteGyp" no hay reclutas en abierto hay que añadir la columna #en la matriz a mano
    if(length(colnames(myadj)[which(colnames(myadj)=="Open")])<1){
      NorecrOpen[z]<-unique(Canopy_all$Study_site)[z]
      myadj<-cbind(myadj,rep(0,dim(myadj)[1]))
      colnames(myadj)[dim(myadj)[2]]<-"Open"
    }
    mycovs<-mycovs[mycovs$Canopy%in%colnames(myadj),]
    #y ahora las ordeno
    myadj<-myadj[,c(mycovs$Canopy[-which(mycovs$Canopy=="Open")],"Open")]
    rownames(mycovs)<-mycovs$Canopy
    mycovs<-mycovs[colnames(myadj),]


    #expando la base de datos de inetarcciones para que incluya los ceros de la matriz de interacciones

    obs_inter<-data.frame(expand.grid(dimnames(provideDimnames(myadj)))[1:2], as.vector(as.matrix(myadj)))

    colnames(obs_inter)[which(colnames(obs_inter)=="Var1")]<-"Recruit"
    colnames(obs_inter)[which(colnames(obs_inter)=="Var2")]<-"Canopy"
    colnames(obs_inter)[which(colnames(obs_inter)=="as.vector.as.matrix.myadj..")]<-"Freq"

    obs_inter$Study_site<-rep(paste(net[[z]]),dim(obs_inter)[1])
    obs_inter$inter_ID<-paste(obs_inter$Study_site,obs_inter$Recruit,obs_inter$Canopy, sep="_")

    alledge<-obs_inter[,c("inter_ID", "Freq" , "Study_site", "Canopy","Recruit")]
    #preparo database para calcular RII de casa interaccion
    openedge<-alledge[alledge$Canopy=="Open",]
    myedge<-alledge[alledge$Canopy!="Open",]

    # add Canopy_cover
    myedge<-merge(myedge,mycovs[,c("Canopy","Canopy_cover")],by="Canopy")
    #add Open cover
    myedge$Open_cover<-rep(mycovs[mycovs$Canopy=="Open","Canopy_cover"],dim(myedge)[1])

    #add number or recruits in open per recruit sp

    myedge<-merge(myedge,openedge[,c("Recruit","Freq")],by="Recruit",all.x=T)
    colnames(myedge)[which(colnames(myedge)=="Freq.x")]<-"Canopy_Freq"
    colnames(myedge)[which(colnames(myedge)=="Freq.y")]<-"Open_Freq"
    myedge[is.na(myedge$Open_Freq),"Open_Freq"]<-0

    myedge<-myedge[which(myedge$Canopy_Freq+myedge$Open_Freq>0),]
    #calculo de los RII observados para cada interaccion
    myedge$RII<-with(myedge, ((Canopy_Freq/Canopy_cover)-(Open_Freq/Open_cover))/((Canopy_Freq/Canopy_cover)+(Open_Freq/Open_cover)))

    adjlist[[z]]<-myadj
    coverlist[[z]]<-mycovs
    edgelist[[z]]<-myedge

  }

  db_inter<-data.frame(do.call("rbind",edgelist))

  db_inter$freq1<-db_inter$Canopy_Freq+db_inter$Open_Freq
  db_inter<-db_inter[db_inter$freq1>1,]   #We remove recruit species with just 1 individual

  return(db_inter)


}


