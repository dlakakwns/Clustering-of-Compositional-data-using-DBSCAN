{
  library(fpc)
  library(dbscan)
  library(writexl)
  library(readxl)
  library(ClusTorus)
  library(RColorBrewer)
  library(base)
  library(data.table)
  library(plotly)  
  library(robCompositions)
}
data <- read_excel("usg_1st.xlsx")
data=data[,c(2:6)]
adist_fun=function(data){
  data=as.matrix(data)
  adist_dm=stats::as.dist(aDist(data))
  return(adist_dm)
}
radar_df <- read_excel("usg_1st_radar.xlsx")

radar_plot=function(radar_df,cluster){
  fig <- plot_ly(
    type = 'scatterpolar',
    mode='dash',
  ) 
  
  for (i in cluster){
    fig <- fig %>%
      add_trace(
        r=as.vector(radar_df[i,2:7],mode="double"),
        theta=c('C','PF','PG','SF','SG','C'),
        name=radar_df['team'][i,1]
      )
  }
  fig <- fig %>%
    layout(
      title='Aitchison',
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,0.4)
        )
      )
    )
  fig
}
radar_plot_no_lengend=function(radar_df,cluster){
  fig <- plot_ly(
    type = 'scatterpolar',
    mode='lines',
  ) 
  
  for (i in cluster){
    fig <- fig %>%
      add_trace(
        r=as.vector(radar_df[i,2:7],mode="double"),
        theta=c('C','PF','PG','SF','SG','C'),
        name=radar_df['team'][i,1]
      )
  }
  fig <- fig %>%
    layout(
      title = '',
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,0.4)
        )
      ),showlegend=FALSE
    )
  fig
}
adist_dm=adist_fun(data)
{
  kNNdistplot(adist_dm, k =3)
  {
    eps_list=seq(0,1,0.0001)
    for (i in 1:length(eps_list)){
      adist.dbscan.dm <- dbscan::dbscan(adist_dm, eps = eps_list[i], minPts =3)
      if (length(unique(adist.dbscan.dm$cluster))==5){
        print(eps_list[i])
        print(table(adist.dbscan.dm$cluster))
      }
    }
  }
}
adist.dbscan.dm <- dbscan::dbscan(adist_dm, eps = 0.2542, minPts =3)
adist_cluster_0 = which(adist.dbscan.dm$cluster==0)
radar_plot_no_lengend(radar_df,adist_cluster_0)
adist_cluster_1 = which(adist.dbscan.dm$cluster==1)
radar_plot_no_lengend(radar_df,adist_cluster_1)
adist_cluster_2 = which(adist.dbscan.dm$cluster==2)
radar_plot_no_lengend(radar_df,adist_cluster_2)
adist_cluster_3 = which(adist.dbscan.dm$cluster==3)
radar_plot_no_lengend(radar_df,adist_cluster_3)
adist_cluster_4 = which(adist.dbscan.dm$cluster==4)
radar_plot_no_lengend(radar_df,adist_cluster_4)

adist.dbscan.dm <- dbscan::dbscan(adist_dm, eps = 0.2542, minPts =3)
adist_cluster_0 = which(adist.dbscan.dm$cluster==0)
radar_plot(radar_df,adist_cluster_0)
adist_cluster_1 = which(adist.dbscan.dm$cluster==1)
radar_plot(radar_df,adist_cluster_1)
adist_cluster_2 = which(adist.dbscan.dm$cluster==2)
radar_plot(radar_df,adist_cluster_2)
adist_cluster_3 = which(adist.dbscan.dm$cluster==3)
radar_plot(radar_df,adist_cluster_3)
adist_cluster_4 = which(adist.dbscan.dm$cluster==4)
radar_plot(radar_df,adist_cluster_4)

