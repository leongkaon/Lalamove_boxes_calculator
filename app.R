# function
lalamove_box_calculator = function(space,box){
        answer = NULL
        repeat{
                # 所有長闊高組合
                space1 = rep(space,time=3)
                box1 = c(box[1:3],box[c(2,3,1)],box[c(3,1,2)])
                df = cbind(space1,box1)
                
                colnames(df) = c("space","box")
                rownames(df) = 1:dim(df)[1]
                df = as.data.frame(df)
                
                # 搵商同餘數
                df <- df %>% group_by(space) %>%
                        mutate(space_remain = space %% box) %>%
                        mutate(space_quotient = floor(space / box)) %>%
                        as.data.frame()
                # 計有幾箱,由於用group_by()會錯曬,因為佢係1:3,4:6咁分開計
                # 我係用3k+1,所以初始值係零 ,搵戈行出黎,連乘prod()
                ans1 = NULL
                for (k in 0:(dim(df)[1]/3-1)){
                        t1 = prod(
                                df[(3*k+1):(3*k+3),
                                   colnames(df) == "space_quotient"]
                        )
                        t2 = rep(t1,time=3)
                        ans1 = c(ans1,t2)
                }
                # 無視餘數下可放箱數
                df$ans1 = ans1
                # 最重要係answer呢格
                answer = c(answer,max(df$ans1)) 
                # 如果無辦法擺箱,就停止運算
                if (max(df$ans1)==0){
                        break
                }
                #####################################################
                # 考慮餘數 
                # 先篩選放箱最多組合
                df = df[df$ans1 == max(ans1),]
                
                # 確定剩餘空間可唔可以放箱,用剩餘邊長 同 箱最小邊長對比
                morebox = df$space_remain >= min(box)
                # 空間不足就停止運算
                if (all(morebox)==FALSE){
                        break
                }
                # 剩餘空間
                df$space[morebox] = df$space_remain[morebox]
                space = df[1:3,1]
                
        }
        print(sum(answer))
}
#######################################################################
# Shiny Start
library(shiny)
library(dplyr)

# UI
ui <- shinyUI(fluidPage(
        
        # Application title
        titlePanel("Boxes Calculator"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        selectInput(
                                "select",
                                label = h3("Type"),
                                choices = c(
                                        "Motocycle",
                                        "SUV",
                                        "Van",
                                        "Truck1.75",
                                        "Truck3.49"
                                )
                        ),
                        ######################
                        numericInput(
                                "num1",
                                label = "Long",
                                value = 20
                        ),
                        numericInput(
                                "num2",
                                label = "Width",
                                value = 20
                        ),
                        numericInput(
                                "num3",
                                label = "Height",
                                value = 20
                        )
                ),
                
                # Maximum How Many Boxes
                mainPanel(
                        h1(textOutput("answer"))
                )
        )
))

##############################################################################################

# Server
server <- shinyServer(function(input, output) {
        
        output$answer = renderText({
                # Input
                box = c(input$num1,input$num2,input$num3)
                # Dynamic Input
                space = reactive({
                        switch(input$select,
                               "Motocycle" = c(30,60,40),
                               "SUV" = c(100,100,100),
                               "Van" = c(150,100,100),
                               "Truck1.75" = c(200,120,120),
                               "Truck3.49" = c(300,150,150)
                        )
                })
                # Don't forget space() need "()" cause space is a function
                lalamove_box_calculator(space(),box)
                
        })
        
})

# Run the application 
shinyApp(ui = ui, server = server)







