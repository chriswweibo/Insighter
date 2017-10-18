library(shiny)
library(rbokeh)
library(DT)
library(rpivotTable)

shinyUI(fluidPage(
  
  titlePanel("Genowis数据智能平台"),
  column(width=2,fileInput("file","忽略红色报错，直接上传数据文件:", buttonLabel = "打开"),actionButton("submit","离散化配置完成,开始运行",icon= icon("hand-peace-o"))),
  column(width=2,uiOutput("column"),textInput("Breaks","分割点（列内,分隔；列间|分割）",value = "20,50,80|200")),
  column(width=5,rbokehOutput("overview",height = "160px")),
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
