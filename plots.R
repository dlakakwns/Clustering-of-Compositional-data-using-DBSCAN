library(plotly)
library(dplyr)
library(compositions)
library(rgl)
library(LaplacesDemon)
library(reshape2)

center <- c(0.33, 0.33, 0.34)
center_0 <- c(0, 0, 0)
radius <- 0.2
radius_2 <- 0.3



theta <- seq(0, 0.5*pi, length.out = 100)
phi <- seq(0, 0.5*pi, length.out = 50)
coords <- expand.grid(theta = theta, phi = phi)

# 평면의 방정식에 속하는 점들 생성
n_points <- 10000000
x <- runif(n_points, 0, 1)
y <- runif(n_points, 0, 1)
z <- 1 - x - y

# 원에 속하는 점들 필터링
points_in_circle <- data.frame(x, y, z) %>%
  filter(abs((x - center[1])^2 + (y - center[2])^2 + (z - center[3])^2 - radius^2) < 0.0001)

points_in_circle_2 <- data.frame(x, y, z) %>%
  filter(abs((x - center[1])^2 + (y - center[2])^2 + (z - center[3])^2 - radius_2^2) < 0.0001)

points_in_circle_clr=clr(points_in_circle)
points_in_circle_clr_df=as.data.frame(points_in_circle_clr)

points_in_circle_2_clr=clr(points_in_circle_2)
points_in_circle_clr_2_df=as.data.frame(points_in_circle_2_clr)

points_in_circle_alr=alr(points_in_circle)
points_in_circle_alr_df=as.data.frame(points_in_circle_alr)

points_in_circle_ilr=ilr(points_in_circle)
points_in_circle_ilr_df=as.data.frame(points_in_circle_ilr)

set.seed(196)
data=data.frame(rdirichlet(100, c(1000,50,50))) ####################################data 분포 설정


fig <- plot_ly()

#############lines###########
fig <- add_trace(
  fig,
  type = "scatter3d",
  mode = "lines",
  x = c(1, 0),
  y = c(0, 0),
  z = c(0, 1),
  line = list(color = "black", width = 2)  # 선의 두께 설정
)
fig <- add_trace(
  fig,
  type = "scatter3d",
  mode = "lines",
  x = c(1, 0),
  y = c(0, 1),
  z = c(0, 0),
  line = list(color = "black", width = 2)  # 선의 두께 설정
)
fig <- add_trace(
  fig,
  type = "scatter3d",
  mode = "lines",
  x = c(0, 0),
  y = c(1, 0),
  z = c(0, 1),
  line = list(color = "black", width = 2)  # 선의 두께 설정
)


fig <- add_trace(
  fig,
  type = "scatter3d",
  mode = "markers",
  x = data[, 1],  # 데이터의 x 좌표
  y = data[, 2],  # 데이터의 y 좌표
  z = data[, 3],  # 데이터의 z 좌표
  marker = list(size = 3,color='black')  # 마커 크기 설정
)

########layout###########3
layout(fig, 
       scene = list(aspectmode = "manual",aspectratio = list(x = 1, y = 1, z = 1),
                    xaxis = list(tickfont = list(size = 15, color = 'black'),gridwidth = 3),
                    yaxis = list(tickfont = list(size = 15, color = 'black'),gridwidth = 3),
                    zaxis = list(tickfont = list(size = 15, color = 'black'),gridwidth = 3)),showlegend=FALSE)

fig
###########data##########33
# fig <- add_trace(
#   fig,
#   type = "scatter3d",
#   mode = "markers",
#   x = points_in_circle[, 1],  # 데이터의 x 좌표
#   y = points_in_circle[, 2],  # 데이터의 y 좌표
#   z = points_in_circle[, 3],  # 데이터의 z 좌표
#   marker = list(size = 3,color='black')  # 마커 크기 설정
# )
# fig <- add_trace(
#   fig,
#   type = "scatter3d",
#   mode = "markers",
#   x = points_in_circle_2[, 1],  # 데이터의 x 좌표
#   y = points_in_circle_2[, 2],  # 데이터의 y 좌표
#   z = points_in_circle_2[, 3],  # 데이터의 z 좌표
#   marker = list(size = 3,color='black')  # 마커 크기 설정
# )
#########ㅊclr###############
# 
# fig <- add_trace(
#   fig,
#   type = "scatter3d",
#   mode = "markers",
#   x = points_in_circle_clr_df[, 1],  # 데이터의 x 좌표
#   y = points_in_circle_clr_df[, 2],  # 데이터의 y 좌표
#   z = points_in_circle_clr_df[, 3],  # 데이터의 z 좌표
#   marker = list(size = 3,color='darkgreen'),
#   name = 'clr1'# 마커 크기 설정
# )
# fig <- add_trace(
#   fig,
#   type = "scatter3d",
#   mode = "markers",
#   x = points_in_circle_clr_2_df[, 1],  # 데이터의 x 좌표
#   y = points_in_circle_clr_2_df[, 2],  # 데이터의 y 좌표
#   z = points_in_circle_clr_2_df[, 3],  # 데이터의 z 좌표
#   marker = list(size = 3,color='darkgreen'),
#   name = 'clr2'# 마커 크기 설정
# )
# fig <- add_trace(
#   fig,
#   type = "scatter3d",
#   mode = "markers",
#   x = points_in_circle_alr_df[, 1],  # 데이터의 x 좌표
#   y = points_in_circle_alr_df[, 2],  # 데이터의 y 좌표
#   z = 0,  # 데이터의 z 좌표
#   marker = list(size = 3,color='darkblue') ,
#   name = 'alr'# 마커 크기 설정
# )
# fig <- add_trace(
#   fig,
#   type = "scatter3d",
#   mode = "markers",
#   x = points_in_circle_ilr_df[, 1],  # 데이터의 x 좌표
#   y = points_in_circle_ilr_df[, 2],  # 데이터의 y 좌표
#   z = 0,  # 데이터의 z 좌표
#   marker = list(size = 3,color='darkred'),
#   name = 'ilr'# 마커 크기 설정
# )
# fig <- add_trace(
#   fig,
#   type = "scatter3d",
#   mode = "markers",
#   x = 0,  # 데이터의 x 좌표
#   y = 0,  # 데이터의 y 좌표
#   z = 0,  # 데이터의 z 좌표
#   marker = list(size = 3,color='black')  # 마커 크기 설정
# )

# ########layout###########3
# layout(fig, 
#        scene = list(aspectmode = "manual",aspectratio = list(x = 1, y = 1, z = 1),
#                     xaxis = list(tickfont = list(size = 15, color = 'black'),gridwidth = 3),
#                     yaxis = list(tickfont = list(size = 15, color = 'black'),gridwidth = 3),
#                     zaxis = list(tickfont = list(size = 15, color = 'black'),gridwidth = 3)))
# 
# fig

{
  spheres3d(0,0,0,lit=FALSE,color="white")
  spheres3d(0,0,0,radius=1.0,lit=FALSE,color="black",front="lines")
  origin_x <- sqrt(points_in_circle[,1])
  origin_y <- sqrt(points_in_circle[,2])
  origin_z <- sqrt(points_in_circle[,3])
  origin_x_2 <- sqrt(points_in_circle_2[,1])
  origin_y_2 <- sqrt(points_in_circle_2[,2])
  origin_z_2 <- sqrt(points_in_circle_2[,3])
  points3d(origin_x,origin_y,origin_z,col="black",radius=0.02)
  points3d(origin_x_2,origin_y_2,origin_z_2,col="black",radius=0.02)
  points3d(sin(coords$phi),cos(coords$phi),0,col="black",radius=0.005)
  points3d(sin(coords$phi),0,cos(coords$phi),col="black",radius=0.005)
  points3d(0,sin(coords$phi),cos(coords$phi),col="black",radius=0.005)
}

dis_fun_3d=function(data){
  x1 <- data['X1']
  x2 <- data['X2']
  x3 <- data['X3']
  group_labels = c('Group 1', 'Group 2', 'Group 3')  
  
  df1 <- data.frame(x1, group_labels[1])  
  colnames(df1) <- c('x', 'Group') 
  df2 <- data.frame(x2, group_labels[2]) 
  colnames(df2) <- c('x', 'Group') 
  df3 <- data.frame(x3, group_labels[3]) 
  colnames(df3) <- c('x', 'Group') 
  df <- rbind(df1,df2,df3) 
  
  colnames(df) <- c('x', 'Group') 
  
  gg <- ggplot(data = df ) +  
    geom_density(aes(x=x, color=Group,linetype=Group)) + geom_rug(aes(x=x, color=Group,linetype=Group)) + 
    ylab("") + 
    xlab("")
  
  ggplotly(gg)%>% 
    layout(plot_bgcolor='#e5ecf6',   
           xaxis = list(   
             title='value', 
             zerolinecolor = '#ffff',   
             zerolinewidth = 2,   
             gridcolor = 'ffff',
             dash='dot'),   
           yaxis = list(   
             title='density', 
             zerolinecolor = '#ffff',   
             zerolinewidth = 2,   
             gridcolor = 'ffff',
             dash='dot')) 
  
}
dis_fun_3d(data)
