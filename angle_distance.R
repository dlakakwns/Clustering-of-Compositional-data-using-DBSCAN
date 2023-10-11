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
}
data <- read_excel("usg_1st.xlsx")
data=data[,c(2:6)]
data=sqrt(data)
data=as.matrix(data)
matrix_df=data %*% t(data)
diag(matrix_df)=1
acos(matrix_df)
angle_dm=stats::as.dist(acos(matrix_df))
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

{
  kNNdistplot(angle_dm, k =3)
  {
    eps_list=seq(0,1,0.0001)
    for (i in 1:length(eps_list)){
      angle.dbscan.dm <- dbscan::dbscan(angle_dm, eps = eps_list[i], minPts =3)
      if (length(unique(angle.dbscan.dm$cluster))==5){
        print(eps_list[i])
        print(table(angle.dbscan.dm$cluster))
      }
    }
  }
}
cluster_length_list=list()


kNNdistplot(angle_dm,k=4)
angle.dbscan.dm <- dbscan::dbscan(angle_dm, eps = 0.0591, minPts =3)
angle_cluster_0 = which(angle.dbscan.dm$cluster==0)
radar_plot(radar_df,angle_cluster_0)
angle_cluster_1 = which(angle.dbscan.dm$cluster==1)
radar_plot(radar_df,angle_cluster_1)
angle_cluster_2 = which(angle.dbscan.dm$cluster==2)
radar_plot(radar_df,angle_cluster_2)
angle_cluster_3 = which(angle.dbscan.dm$cluster==3)
radar_plot(radar_df,angle_cluster_3)
angle_cluster_4 = which(angle.dbscan.dm$cluster==4)
radar_plot(radar_df,angle_cluster_4)




angle_cluster_0
angle_cluster_1
angle_cluster_2
angle_cluster_3
angle_cluster_4
#최적화 방법이 없다 / 