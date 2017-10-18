library(shiny)
library(rbokeh)
library(DT)
library(rpivotTable)

shinyUI(fluidPage(
  
  titlePanel("Genowis数据智能平台DataWiz"),
  column(width=2,fileInput("file","忽略红色报错，直接上传数据文件:", buttonLabel = "打开"),actionButton("submit","配置完成,开始运行",icon= icon("hand-peace-o"))),
  column(width=1,uiOutput("column"),uiOutput("column2")),
  column(width=2,textInput("Breaks","分割点（列内,分隔；列间|分割）",value = "20,50,80|200"),
         textInput("rules","规则（值|分隔；:表示映射，映射,分割）",value = "{\"男性|男\":\"male\",\"女性|女\":\"female\"}")),
  column(width=4,rbokehOutput("overview",height = "160px")),
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
