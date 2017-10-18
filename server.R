library(shiny)
library(readr)
library(readxl)
library(lazyeval)
library(plyr)
library(dplyr)
library(rbokeh)
library(DT)
library(reshape2)
library(rpivotTable)
options(shiny.maxRequestSize=50*1024^2)
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  filedata=reactive({
    datafile=input$file
    read_excel(datafile$datapath,col_names  =T)
  })
  
  output$columnx=renderUI({
    selectInput('colx', '待离散化列c1',c("无",colnames(filedata())),selected="年龄")
  })
  output$columny=renderUI({
    selectInput('coly', '待离散化列c2',c("无",colnames(filedata())),selected="年龄")
  })
  output$columnz=renderUI({
    selectInput('colz', '待离散化列c3',c("无",colnames(filedata())),selected="年龄")
  })
 
  findInterval_optimised=function(x,y){
    splits=as.numeric(strsplit(y,",")[[1]])
    tmp=findInterval(x,splits,left.open = T)
    tmp[tmp==0]=paste("小于等于",splits[1],sep="")
    tmp[tmp==length(splits)]=paste("大于",max(splits),sep="")
    for (i in 1:length(splits)) {
      tmp[tmp==i]=paste0("(",splits[i],",",splits[i+1], "]",collapse = "")
    }
    return(tmp)
  }
  data_categorised=reactive({
    c1=findInterval_optimised(filedata()[[input$colx]],input$xBreaks)
    c2=findInterval_optimised(filedata()[[input$coly]],input$yBreaks)
    c3=findInterval_optimised(filedata()[[input$colz]],input$zBreaks)
    cbind(c1,c2,c3,filedata())
  })
  
  filedataD <- eventReactive(input$submit, {data_categorised()})
  
  output$overview=renderRbokeh({
    miss <- function(x){sum(is.na(x))/length(x)}
    missingRatio=data.frame(列=colnames(filedataD()),缺失率=apply(filedataD(),2,miss),stringsAsFactors = F)
    figure(height  =80,ylim = c(0,1),xaxes = "bottom",tools = "",ylab ="各列缺失率",xlab ="") %>% ly_bar(data = missingRatio,y=缺失率, x=列,hover = TRUE)
  })
  
  output$column1=renderUI({
    selectInput('col1', '相关性检验列',colnames(filedataD()),multiple = T)
  })
  output$correlate=renderTable({
    coln=input$col1
    if (input$continuousOrNot==T){
    dat1=filedataD()[[coln[[1]]]]
    dat2=filedataD()[[coln[[2]]]]
    df=na.omit(data.frame(dat1,dat2,stringsAsFactors = F))
    tmp=cor.test(df[,1],df[,2],method = input$method,alternative = input$alter)
    result=data.frame(结果=c("关联度",round(tmp$estimate,digits=8),"p值",round(tmp$p.value,digits=8)))
    }
    else{
      tmp=count_(filedataD(),c(coln[[1]],coln[[2]]))
      formula=interp("x~y",x=as.name(coln[[1]]),y=as.name(coln[[2]]))
      cand=dcast(tmp,formula,value.var="n",fill=0)[,-1]
      
      resultTmp=chisq.test(cand)
      result=data.frame(结果=c("统计量χ方",resultTmp$statistic,"p值",resultTmp$p.value))
    }
    result
   
  })
  output$DataPivot=renderRpivotTable({
    filedataD() %>%  tbl_df() %>%  rpivotTable(width="20%",cols = "年龄",rows = "性别",aggregatorName = "Count",vals = "Freq",rendererName = "Stacked Bar Chart")
  })
  # output$dataDistr=renderRbokeh({
  #   g= figure(width = 1600, height  = 1000, xlab=input$col, tools = tools(), title=paste(input$uvtitle,"频度分布",sep="")) 
  #   if (dataType()=="numeric") {
  #   g %>% ly_hist(input$col, data = filedata(), breaks=as.integer(input$distrBreakNum),hover=T) %>% 
  #       theme_axis("x", major_label_orientation = input$xrotate)
  #   }
  #   else {
  #     ct=count_(filedata(),input$col)
  #     g %>% ly_bar(x=eval(as.name(input$col)), y=n, data = ct,hover=T) %>% 
  #       theme_axis("x", major_label_orientation = input$xrotate)
  #   }
  #   
  # })
  
  # output$dataDensity=renderRbokeh({
  # 
  #   g= figure(width = 1600, height  = 1000, title=paste(input$uvtitle,"密度曲线",sep=""),xlab=input$col, tools = tools())
  # 
  #   if (dataType()=="numeric") {
  #     g %>% ly_density(input$col, data = filedata(),kernel=input$densityKernel)
  #   }
  #   else {
  #     ct=count_(filedata(),input$col)
  #     g %>% ly_bar(x=eval(as.name(input$col)), y=n, data = ct)
  #   }
  # })
  
  
  # output$dataBoxplot=renderRbokeh({
  #  
  #   g= figure(width = 1600, height  = 1000, title=paste(input$uvtitle,"箱线图", sep=""), tools = tools())
  # 
  #   if (dataType()=="numeric") {
  #     if (input$boxplotOutlier=="显示"){
  #     g %>% ly_boxplot(input$col, data = filedata())
  #     }
  #     else{
  #       g %>% ly_boxplot(input$col, data = filedata(), outlier_size = NA)
  #     }
  #   }
  #   else {
  # 
  #   }
  #   
  # })
  
  # output$dataScatter=renderRbokeh({
  #   
  #   g= figure(width = 1600, height  = 1000, title=paste(input$uvtitle,"散点图",sep=""), tools = tools())
  # 
  #   if (dataType()=="numeric") {
  #       g %>% ly_points(x=1:nrow(filedata()), y=input$col, data = filedata(), hover = input$col)
  # 
  #   }
  #   else {
  # 
  #   }
  #   
  # })
  # 
  # 
  # output$mv=renderRbokeh({
  #   
  #   xdata=xvarD()
  #   ydata=yvarD()
  #   颜色=gvar1D()
  #   形状=gvar2D()
  #   
  #   cand=data.frame(xdata,ydata)
  #   candC=count(cand,xdata,ydata)
  #   ready=dcast(candC,xdata~ydata,value.var="n")
  #   cand1=chisq.test(as.numeric(as.matrix(ready[,-1])))
  #   title=paste(input$title,"（相关性P值：",cand1$p.value,"）", sep = "")
    # if (length(input$xBreaks)==0){xdata=xvarD()} else {xdata=findInterval(xvarD(),as.numeric(strsplit(input$xBreaks,",")[[1]]),left.open = T)}
    # ydata=ifelse(is.null(input$yBreaks)==T,input$yvar,findInterval(input$yvar,input$yBreaks))
    # gdata=ifelse(is.null(input$gBreaks)==T,input$gvar,findInterval(input$gvar,input$gBreaks))
    # p <- figure(width=650,height = 380, title=title,legend_location = input$legendPos,xlab = input$xlab, ylab = input$ylab, tools = tools())
    # 
    # p %>% ly_points(x=xdata, y=ydata, color=颜色,glyph=形状,data = filedata(),hover = list(xdata, ydata,颜色,形状))
    # 
    # })
  
  output$DB=renderDataTable({
    datatable(filedataD(),filter = 'top',extensions = c('Scroller'),options=list(deferRender = F,scrollY = 550,scroller = TRUE,scrollX = TRUE,fixedColumns = F))
  })
 
  # rawdata=reactive({
  #   datafile=input$file
  #   read.csv(datafile$datapath)
  # })
   
  # observe({
  #   updateSelectInput(session, "xvar",choices = rep(names(filedata()),2))
  #   })
  # observe({
  #   
  #   updateSelectInput(session, "yvar",choices = names(rawdata()))
  # })
  # observe({
  #   updateSelectInput(session, "gvar",choices = names(rawdata()))
  # })
    
  })
