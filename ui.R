library(shiny)
library(rbokeh)
library(DT)
library(rpivotTable)

shinyUI(fluidPage(
  
  titlePanel("Genowis数据智能平台"),
  
   column(width=2,fileInput("file","上传数据文件:", buttonLabel = "打开"),column(width=2,actionButton("submit","离散化完成,运行"))),
   column(width=1,uiOutput("columnx"),textInput("xBreaks","分割点（,分隔）",value = "20,50,80")),
    column(width=1,uiOutput("columny"),textInput("yBreaks","分割点（,分隔）",value = "20,50,80")),
    column(width=1,uiOutput("columnz"), textInput("zBreaks","分割点（,分隔）",value = "20,50,80")),
  column(width=4,rbokehOutput("overview",height = "100px")),
  column(width=1,uiOutput("column1"),checkboxInput("continuousOrNot","两列均为数值型")),
    column(width=1, selectInput("method","检验方法",c("pearson", "kendall", "spearman")),selectInput("alter","备择假设",c("two.sided", "less", "greater"))),
  column(width=1,tableOutput("correlate")),
 
    mainPanel(width=12,
      tabsetPanel(
        tabPanel("原始数据", dataTableOutput("DB")),
        tabPanel("数据分析",rpivotTableOutput("DataPivot"))
        # tabPanel("时间序列分析"),
        # tabPanel("回归分析"), 
        # tabPanel("分类模型"),
        # tabPanel("聚类模型"),
        # tabPanel("深度学习"),
        
        
  )
)
)
)
