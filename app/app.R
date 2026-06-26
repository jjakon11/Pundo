library(shiny)
library(ggplot2)
library(DT)
library(magick) 

ui <- fluidPage(
  titlePanel("PUNDO (Testing Version)"),
  
  fluidRow(
    
    column(3,
           wellPanel(
             h4("1. 資料上傳"),
             radioButtons("encoding", "CSV 編碼格式", 
                          choices = c("UTF-8" = "UTF-8", "Big5 / ASCII" = "big5"), 
                          inline = TRUE),
             fileInput("file1", "上傳 CSV 檔案", accept = ".csv")
           ),
           
           wellPanel(
             h4("2. 畫布與底圖校正"),
             numericInput("max_x", "實際 X 軸最大範圍 (cm)", value = 100, min = 10),
             numericInput("max_y", "實際 Y 軸最大範圍 (cm)", value = 100, min = 10),
             fileInput("bg_img", "上傳底圖 (支援 JPG, PNG)", accept = c(".jpg", ".jpeg", ".png")),
             numericInput("img_angle", "底圖旋轉角度 (度)", value = 0, step = 90),
             
             p("微調底圖邊界："),
             fluidRow(
               column(6, numericInput("img_xmin", "左界 (X min)", value = 0)),
               column(6, numericInput("img_xmax", "右界 (X max)", value = 100))
             ),
             fluidRow(
               column(6, numericInput("img_ymin", "下界 (Y min)", value = 0)),
               column(6, numericInput("img_ymax", "上界 (Y max)", value = 100))
             )
           )
    ),
    
    column(5,
           wellPanel(
             h4("3. 新增 / 更新點位"),
             p("輸入植物資訊後，點擊「準備點擊圖面」，接著在下方圖紙上點選對應位置："),
             fluidRow(
               column(4, textInput("in_tag", "Tag (標籤)")),
               column(4, textInput("in_sp", "Species (物種)")),
               column(4, numericInput("in_dbh", "DBH", value = 10, min = 0))
             ),
             fluidRow(
               column(6, actionButton("add_btn", "準備點擊圖面", class = "btn-primary", style = "width: 100%;")),
               column(6, actionButton("delete_btn", "刪除下方表格選取資料", class = "btn-danger", style = "width: 100%;"))
             )
           ),
           
           plotOutput("plot1", click = "plot_click", height = "600px")
    ),
    
    column(4,
           wellPanel(
             h4("4. 資料呈現"),
             DTOutput("table1")
           ),
           
           br(),
           downloadButton("downloadData", "下載完成的 CSV 檔案", class = "btn-success", 
                          style = "width: 100%; padding: 15px; font-size: 18px; font-weight: bold;")
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data = data.frame(
      x1 = numeric(), y1 = numeric(), x2 = numeric(), y2 = numeric(),
      tag = character(), sp = character(), dbh = numeric(),
      x3 = numeric(), y3 = numeric(), stringsAsFactors = FALSE
    ),
    waiting_for_click = FALSE
  )
  
  observeEvent(input$file1, {
    req(input$file1)
    df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE, fileEncoding = input$encoding)
    rv$data <- df
  })
  
  observeEvent(input$in_tag, {
    req(input$in_tag)
    if (nrow(rv$data) > 0) {
      match_idx <- which(rv$data$tag == input$in_tag)
      if (length(match_idx) > 0) {
        updateTextInput(session, "in_sp", value = rv$data$sp[match_idx[1]])
        updateNumericInput(session, "in_dbh", value = as.numeric(rv$data$dbh[match_idx[1]]))
      } else {
        updateTextInput(session, "in_sp", value = "")
        updateNumericInput(session, "in_dbh", value = NA)
      }
    }
  })
  
  observeEvent(input$add_btn, {
    if (input$in_tag == "") {
      showNotification("請先輸入 Tag", type = "warning")
      return()
    }
    rv$waiting_for_click <- TRUE
    showNotification("請在圖面上點擊你要放置或更新的位置", type = "message")
  })
  
  observeEvent(input$plot_click, {
    if (rv$waiting_for_click) {
      new_x <- input$plot_click$x
      new_y <- input$plot_click$y
      
      new_row <- data.frame(
        x1 = NA, y1 = NA, x2 = NA, y2 = NA, 
        tag = input$in_tag,
        sp = input$in_sp,
        dbh = input$in_dbh,
        x3 = round(new_x, 2),
        y3 = round(new_y, 2),
        stringsAsFactors = FALSE
      )
      
      if (input$in_tag %in% rv$data$tag) {
        target_idx <- which(rv$data$tag == input$in_tag)
        rv$data$sp[target_idx] <- input$in_sp
        rv$data$dbh[target_idx] <- input$in_dbh
        rv$data$x3[target_idx] <- round(new_x, 2)
        rv$data$y3[target_idx] <- round(new_y, 2)
      } else {
        rv$data <- rbind(rv$data, new_row)
      }
      
      rv$waiting_for_click <- FALSE
    }
  })
  
  observeEvent(input$delete_btn, {
    selected_row <- input$table1_rows_selected
    if (length(selected_row) > 0) {
      rv$data <- rv$data[-selected_row, ]
    } else {
      showNotification("請先在下方表格中點選要刪除的資料列", type = "warning")
    }
  })
  
  output$plot1 <- renderPlot({
    p <- ggplot() +
      scale_x_continuous(limits = c(0, input$max_x), expand = c(0, 0)) + 
      scale_y_continuous(limits = c(0, input$max_y), expand = c(0, 0)) +
      theme_void() + 
      coord_fixed(ratio = 1, expand = FALSE) +
      labs(x = "x3 座標 (cm)", y = "y3 座標 (cm)") +
      theme(
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.margin = margin(10, 10, 10, 10)
      )
    
    if (!is.null(input$bg_img)) {
      img_magick <- magick::image_read(input$bg_img$datapath)
      
      if (input$img_angle != 0) {
        img_magick <- magick::image_rotate(img_magick, input$img_angle)
      }
      
      img_raster <- as.raster(img_magick)
      
      p <- p + annotation_raster(img_raster, 
                                 xmin = input$img_xmin, xmax = input$img_xmax, 
                                 ymin = input$img_ymin, ymax = input$img_ymax)
    }
    
    major_x <- c(0, input$max_x / 2, input$max_x)
    major_y <- c(0, input$max_y / 2, input$max_y)
    minor_x <- seq(0, input$max_x, length.out = 11)
    minor_y <- seq(0, input$max_y, length.out = 11)
    
    p <- p + 
      geom_vline(xintercept = minor_x, color = "black", linewidth = 0.2, linetype = "dotted") +
      geom_hline(yintercept = minor_y, color = "black", linewidth = 0.2, linetype = "dotted") +
      geom_vline(xintercept = major_x, color = "black", linewidth = 0.8) +
      geom_hline(yintercept = major_y, color = "black", linewidth = 0.8)
    
    if (nrow(rv$data) > 0) {
      p <- p + 
        # 將 DBH 映射至 size，並加入透明度
        geom_point(data = rv$data, aes(x = as.numeric(x3), y = as.numeric(y3), size = as.numeric(dbh)), 
                   color = "#00878A", alpha = 0.7, na.rm = TRUE) +
        geom_text(data = rv$data, aes(x = as.numeric(x3), y = as.numeric(y3), label = tag), 
                  vjust = -1.5, color = "#00878A", size = 4, na.rm = TRUE) +
        # 關閉圖例並限制大小的縮放範圍
        scale_size_continuous(range = c(2, 12), guide = "none")
    }
    
    p
  })
  
  output$table1 <- renderDT({
    datatable(rv$data, selection = 'single', options = list(pageLength = 10))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("plot_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$data, file, row.names = FALSE, fileEncoding = input$encoding)
    }
  )
}

shinyApp(ui, server)
