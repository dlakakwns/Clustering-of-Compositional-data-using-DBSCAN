library(LaplacesDemon)
library(plotly)

#####################################

dis_fun=function(data){
  x1 <- data['X1']
  x2 <- data['X2']
  x3 <- data['X3']
  x4 <- data['X4']
  x5 <- data['X5']
  group_labels = c('Group 1', 'Group 2', 'Group 3', 'Group 4','Group 5')  
  
  df1 <- data.frame(x1, group_labels[1])  
  colnames(df1) <- c('x', 'Group') 
  df2 <- data.frame(x2, group_labels[2]) 
  colnames(df2) <- c('x', 'Group') 
  df3 <- data.frame(x3, group_labels[3]) 
  colnames(df3) <- c('x', 'Group') 
  df4 <- data.frame(x4, group_labels[4]) 
  colnames(df4) <- c('x', 'Group') 
  df5 <- data.frame(x5, group_labels[5]) 
  colnames(df5) <- c('x', 'Group') 
  df <- rbind(df1,df2,df3,df4,df5) 
  
  colnames(df) <- c('x', 'Group') 
  
  gg <- ggplot(data = df ) +  
    geom_density(aes(x=x, color=Group)) + geom_rug(aes(x=x, color=Group)) + 
    ylab("") + 
    xlab("")
  
  ggplotly(gg)%>% 
    layout(plot_bgcolor='#e5ecf6',   
           xaxis = list(   
             title='value', 
             zerolinecolor = '#ffff',   
             zerolinewidth = 2,   
             gridcolor = 'ffff'),   
           yaxis = list(   
             title='density', 
             zerolinecolor = '#ffff',   
             zerolinewidth = 2,   
             gridcolor = 'ffff'),
           title = 'dist_plot') 
  
}
angle_fun=function(data){
  data_sqrt=sqrt(data)
  data_sqrt=as.matrix(data_sqrt)
  matrix_df=data_sqrt %*% t(data_sqrt)
  diag(matrix_df)=1
  acos(matrix_df)
  angle_dm=stats::as.dist(acos(matrix_df))
  return(angle_dm)
}
log_fun=function(data){
  a1=log(data['X1']/data['X5'])
  a2=log(data['X2']/data['X5'])
  a3=log(data['X3']/data['X5'])
  a4=log(data['X4']/data['X5'])
  log_data=data.frame(a1,a2,a3,a4)
  dis_fun=function(log_data){
    n <- nrow(log_data)
    d <- ncol(log_data)
    pdistmat <- matrix(0, ncol = n, nrow = n)
    for (i in 1:(n-1)){
      ad <- 0
      for (dd in 1:d){
        ad <- ad + ((log_data[i, dd] - log_data[(i + 1):n, dd]))^2
      }
      pdistmat[i, (i + 1):n] <- ad
      pdistmat[(i + 1):n, i] <- ad
    }
    stats::as.dist(pdistmat)
  }
  log_dm=dis_fun(log_data)
  return(log_dm)
}
log_fun_V4=function(data){
  a1=log(data['X1']/data['X4'])
  a2=log(data['X2']/data['X4'])
  a3=log(data['X3']/data['X4'])
  a4=log(data['X5']/data['X4'])
  log_data=data.frame(a1,a2,a3,a4)
  dis_fun=function(log_data){
    n <- nrow(log_data)
    d <- ncol(log_data)
    pdistmat <- matrix(0, ncol = n, nrow = n)
    for (i in 1:(n-1)){
      ad <- 0
      for (dd in 1:d){
        ad <- ad + ((log_data[i, dd] - log_data[(i + 1):n, dd]))^2
      }
      pdistmat[i, (i + 1):n] <- ad
      pdistmat[(i + 1):n, i] <- ad
    }
    stats::as.dist(pdistmat)
  }
  log_dm=dis_fun(log_data)
  return(log_dm)
}
log_fun_V3=function(data){
  a1=log(data['X1']/data['X3'])
  a2=log(data['X2']/data['X3'])
  a3=log(data['X4']/data['X3'])
  a4=log(data['X5']/data['X3'])
  log_data=data.frame(a1,a2,a3,a4)
  dis_fun=function(log_data){
    n <- nrow(log_data)
    d <- ncol(log_data)
    pdistmat <- matrix(0, ncol = n, nrow = n)
    for (i in 1:(n-1)){
      ad <- 0
      for (dd in 1:d){
        ad <- ad + ((log_data[i, dd] - log_data[(i + 1):n, dd]))^2
      }
      pdistmat[i, (i + 1):n] <- ad
      pdistmat[(i + 1):n, i] <- ad
    }
    stats::as.dist(pdistmat)
  }
  log_dm=dis_fun(log_data)
  return(log_dm)
}
log_fun_V2=function(data){
  a1=log(data['X1']/data['X2'])
  a2=log(data['X3']/data['X2'])
  a3=log(data['X4']/data['X2'])
  a4=log(data['X5']/data['X2'])
  log_data=data.frame(a1,a2,a3,a4)
  dis_fun=function(log_data){
    n <- nrow(log_data)
    d <- ncol(log_data)
    pdistmat <- matrix(0, ncol = n, nrow = n)
    for (i in 1:(n-1)){
      ad <- 0
      for (dd in 1:d){
        ad <- ad + ((log_data[i, dd] - log_data[(i + 1):n, dd]))^2
      }
      pdistmat[i, (i + 1):n] <- ad
      pdistmat[(i + 1):n, i] <- ad
    }
    stats::as.dist(pdistmat)
  }
  log_dm=dis_fun(log_data)
  return(log_dm)
}
log_fun_V1=function(data){
  a1=log(data['X2']/data['X1'])
  a2=log(data['X3']/data['X1'])
  a3=log(data['X4']/data['X1'])
  a4=log(data['X5']/data['X1'])
  log_data=data.frame(a1,a2,a3,a4)
  dis_fun=function(log_data){
    n <- nrow(log_data)
    d <- ncol(log_data)
    pdistmat <- matrix(0, ncol = n, nrow = n)
    for (i in 1:(n-1)){
      ad <- 0
      for (dd in 1:d){
        ad <- ad + ((log_data[i, dd] - log_data[(i + 1):n, dd]))^2
      }
      pdistmat[i, (i + 1):n] <- ad
      pdistmat[(i + 1):n, i] <- ad
    }
    stats::as.dist(pdistmat)
  }
  log_dm=dis_fun(log_data)
  return(log_dm)
}

set.seed(196)

#######################################

data=data.frame(rdirichlet(100, c(100,1,1,1,100)))
dis_fun(data)
angle_dm=angle_fun(data)
log_dm=log_fun(data)
log_vector=as.vector(log_dm)
ang_vector=as.vector(angle_dm)
fig <- plot_ly(x = ang_vector, y =log_vector,alpha=0.4)
fig <- fig %>% layout(title = 'rdirichlet(100, c(100,1,1,1,100))')
fig

######################################

angle_dm=angle_fun(data)
log_dm=log_fun_V4(data)
log_vector=as.vector(log_dm)
ang_vector=as.vector(angle_dm)


fig <- plot_ly(x = ang_vector, y =log_vector,alpha=0.4)
fig <- fig %>% layout(title = 'rdirichlet(100, c(100,1,1,1,100))')
fig

######################################

angle_dm=angle_fun(data)
log_dm=log_fun_V3(data)
log_vector=as.vector(log_dm)
ang_vector=as.vector(angle_dm)


fig <- plot_ly(x = ang_vector, y =log_vector,alpha=0.4)
fig <- fig %>% layout(title = 'rdirichlet(100, c(100,1,1,1,100))')
fig

######################################

angle_dm=angle_fun(data)
log_dm=log_fun_V2(data)
log_vector=as.vector(log_dm)
ang_vector=as.vector(angle_dm)


fig <- plot_ly(x = ang_vector, y =log_vector,alpha=0.4)
fig <- fig %>% layout(title = 'rdirichlet(100, c(100,1,1,1,100))')
fig

######################################

angle_dm=angle_fun(data)
log_dm=log_fun_V1(data)
log_vector=as.vector(log_dm)
ang_vector=as.vector(angle_dm)


fig <- plot_ly(x = ang_vector, y =log_vector,alpha=0.4)
fig <- fig %>% layout(title = 'rdirichlet(100, c(100,1,1,1,100))')
fig
