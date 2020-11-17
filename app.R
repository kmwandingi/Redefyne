library(shiny)
library(shinyjs)
library(plotly)
library(shinybulma)
library(ShinyPsych)
library(googleAuthR)
library(shinyWidgets)
library(echarts4r)
library(rdrop2)
library(dplyr)

drop_auth(rdstoken = "dbtoken.rds")  
drop_acc()
##deps----------
#for google auth
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/urlshortener"))

##ui----------------
ui <- fixedPage(htmlOutput("page"),
       # For Shinyjs functions
       useShinyjs(),
       #CSS
       tags$head(includeCSS(path = "www/Styles.css")),
       #js
       tags$script(src = "myscript.js")
       )

home <- function(...) {
  bulmaPage(  
    bulmaNavbar(
      bulmaNavbarBrand(
        bulmaNavbarItem(tags$li(a(img(src = 'LOGO.png',title = "Rydefine")), class = "navbar-item"),
                        href = "Home"
        ),
        bulmaNavbarBurger()
      ),
     bulmaNavbarMenu(
        bulmaNavbarItem(
          "Home"
        ),
        bulmaNavbarItem(
          "About"
        ),
        bulmaNavbarItem(
          "Team"
        )
      )
    ),
    bulmaNav(
      "Home",
      bulmaSection(
        bulmaContainer(
          bulmaColumns(
            bulmaColumn(width = 6, bulmaTitle(div(HTML(paste("<p style='text-align:center;'> Take Control Of Your Future.</p>")))), 
                        p(div(HTML(paste("<p style='text-align:centerlibrary;'> We want to ensure making the best investment decison for you is as timely and as cheap as possible. 
                          Join us to ensuire a brighter future.</p>")))),
                        br(),
                        actionButton("block_one", "Start")
                        ),
            bulmaColumn(width = 6, p(tags$li(img(src = 'tap.png'),class = "tap")))
          )
        )
       )
      ),
    bulmaNav(
      "About",
      bulmaHero(
        color = "white",
        fullheight = F,
        bulmaHeroBody(
          bulmaContainer(
            bulmaTitle("About Us"),
            br(),
            p(div(HTML(paste("<p style='text-align:center;'> We recognize the profound opportunities that exist in leveraging new techniques in the data 
            and behavioural sciences to re-think how financial and investment advice is conducted.  
            </p>")))),
            br(),
            p(div(HTML(paste("<p style='text-align:center;'> We want to empower our clients in making good financial decisions at the click of a button. 
            We will design a Behavioural Risk Survey, which shall reveal an investor's persona, risk preferences, emotional and cognitive biases and attitudes 
            toward losses and gains. This helps brings convenience, drive behaviours and improve outcomes. </p>")))),
            br(),
            p(div(HTML(paste("<p style='text-align:center;'> Our approach is to help the disfranchised participate in the financial ecosystem 
            by removing barriers to access and bringing swiftness to the process.  </p>"))))
          )
        )
      )
     )
      
   )
}

# questions---------
question_one <- function(...) {
  renderUI({ source("Questions/1.R", local = TRUE)$value })
}

question_two <- function(...) {
  renderUI({ source("Questions/2.R", local = TRUE)$value })
}

question_three <- function(...) {
  renderUI({ source("Questions/3.R", local = TRUE)$value })
}

question_four <- function(...) {
  renderUI({ source("Questions/4.R", local = TRUE)$value })
}

question_five <- function(...) {
  renderUI({ source("Questions/5.R", local = TRUE)$value })
}

question_six <- function(...) {
  renderUI({ source("Questions/6.R", local = TRUE)$value })
}

question_seven <- function(...) {
  renderUI({ source("Questions/7.R", local = TRUE)$value })
}

question_eight <- function(...) {
  renderUI({ source("Questions/8.R", local = TRUE)$value })
}

question_nine <- function(...) {
  renderUI({ source("Questions/9.R", local = TRUE)$value })
}

question_ten <- function(...) {
  renderUI({ source("Questions/10.R", local = TRUE)$value })
}

question_eleven <- function(...) {
  renderUI({ source("Questions/11.R", local = TRUE)$value })
}

question_twelve <- function(...) {
  renderUI({ source("Questions/12.R", local = TRUE)$value })
}

# render page-------
render_page <- function(...,f, title = "RyDefine") {
  
  page <- f(...)
  renderUI({
    fluidPage(page, title = title)
  })
}

# server---------
server <- function(input, output, session) {
  
##render default page
  output$page <- render_page(f = home)
  
##questions-------  
  observeEvent(input$block_one, {
    output$page <- render_page(f = question_one)
  })
  
  observeEvent(input$block_two, {
    output$page <- render_page(f = question_two)
  })
  
  observeEvent(input$block_three, {
    output$page <- render_page(f = question_three)
  })
  
  observeEvent(input$block_four, {
    output$page <- render_page(f = question_four)
  })
  
  observeEvent(input$block_five, {
    output$page <- render_page(f = question_five)
  })
  
  observeEvent(input$block_six, {
    output$page <- render_page(f = question_six)
  })
  
  observeEvent(input$block_seven, {
    output$page <- render_page(f = question_seven)
  })
  
  observeEvent(input$block_eight, {
    output$page <- render_page(f = question_eight)
  })
  
  observeEvent(input$block_nine, {
    output$page <- render_page(f = question_nine)
  })
  
  observeEvent(input$block_ten, {
    output$page <- render_page(f = question_ten)
  })
  
  observeEvent(input$block_eleven, {
    output$page <- render_page(f = question_eleven)
  })
  
  observeEvent(input$block_twelve, {
    output$page <- render_page(f = question_twelve)
  })
  
  
##end questions------
  
  observeEvent(input$block_thirteen, {
    # Create progress message   
    withProgress(message = "Saving data...", value = 0, {
      
      incProgress(.25)
      
    data.list <<- list(
      "capital" = input$question1,
      "tolerance" = input$question2,
      "delegate" = input$question3, # answer to question 3
      "faith" = input$question4, # answer to question 4
      "pick" = input$question5, # answer to question 5
      "lifestyle" = input$question6, # answer to question 6
      "initiative" = input$question7, # answer to question 7
      "presavation" = input$question8, # answer to question 8
      "debt" = input$question9, # answer to question 9
      "goal" = input$question10, # answer to question 10
      "amount" = input$question11, # answer to question 11
      "timeline" = input$question12 # answer to question 12
      )
    
    # save Data
    # saveData(data.list, location = "local", outputDir = outputDir,
    #          partId = data.list$id, suffix = "_g")
      saveData(data.list)
    
    #Call funds page
    output$page <- renderUI({
      fundpage()
    })
    })
  })
  
  observeEvent(input$pool, {
    #Call pool page
    output$page <- renderUI({
      poolpage()
    })
  })
  
  observeEvent(input$auth, {
    #Call authentication page
    output$page <- renderUI({
      authpage()
    })
  })
  
  
##fund ui-------

  fundpage <- reactive({
     bulmaPage(
            bulmaSection(
              bulmaContainer(
                bulmaColumns(
                  bulmaColumn(
                    plotlyOutput("pie")
                  ),
                  bulmaColumn(
                    uiOutput("invsc")
                  )
                )
              )
            ),
            bulmaHero(
              color = "white",
              fullheight = F,
              bulmaHeroBody(
              bulmaContainer(
                    p(div(HTML(paste("<p style='text-align:center;'> Happy with this fund? Buy in!</p>")))),
                    p(div(HTML(paste("<br>")))),
                    shiny::actionButton(inputId = "auth",label = "Buy Fund"),
                    p(div(HTML(paste("<br>")))),
                    p(div(HTML(paste("<p style='text-align:center;'> Do not have sufficients funds? Not to worry. Join our pool.</p>")))),
                    p(div(HTML(paste("<br>")))),
                    shiny::actionButton(inputId = "pool",label = "Join Pool")
                  )
                )
              )
          )
  })
  
  poolpage <- reactive({
    bulmaPage(  
      bulmaHero(
        color = "white",
        fullheight = F,
        bulmaHeroBody(
          bulmaContainer(
            bulmaColumns(
              bulmaColumn(),
              bulmaColumn(),
              bulmaColumn(plotlyOutput("pool")),
              bulmaColumn(),
              bulmaColumn()
            )
          ),
          bulmaContainer(
            bulmaColumns(
              bulmaColumn(),
              bulmaColumn(
                uiOutput("pooldesc"),
                p(div(HTML(paste("<br>"))))
              ),
            bulmaColumn()
              ),
            bulmaColumns(
              bulmaColumn(),
              bulmaColumn(knobInput(
                inputId = "knobpool",
                label = "",
                value = 400,
                min = 50,
                max = 2500,
                displayPrevious = TRUE, 
                lineCap = "round",
                fgColor = "#66C2A5",
                inputColor = "#66C2A5"
              )),
              bulmaColumn()
            ),
            p(div(HTML(paste("<br>")))),
            shiny::actionButton(inputId = "auth",label = "Join Pool")
          )
        )
      )
    )
  })
  
  authpage <- reactive({
    bulmaPage(  
      bulmaNavbar(
        bulmaNavbarBrand(
          bulmaNavbarItem(
            tags$li(a(img(src = 'LOGO.png',title = "Rydefine")),class = "navbar-item"), href = "Home"
          )
        )
      ),  
      bulmaHero(
        color = "white",
        fullheight = F,
        bulmaHeroBody(bulmaSubtitle(""))
      ),
      bulmaSection(
        bulmaContainer(),
        bulmaContainer(
          bulmaSubtitle("Log in with your with your google account"),
          bulmaColumns(
            bulmaColumn(),
            bulmaColumn(googleAuthUI("loginButton")
            ),
            bulmaColumn()
          )
        )
      )
    )
  })
  
##inputs-------------
  
  data <- reactive({
   input$slider
  })

##analytics--------
  
  output$invsc <- renderUI({
    
    invsc <- data.frame(data.list)
    invsc <- dplyr::mutate_all(invsc, function(x) as.numeric(as.character(x)))
    invsc %>% dplyr::select(-goal, everything()) -> invsc
    
    invsc.n <- rowMeans(invsc[, 2:12])
    
    if (between(invsc.n,0.99,1.4)){
      invcat <- "PP"
    } else if (between(invsc.n,1.41,1.8)){
      invcat <- "FF"
    }else if (between(invsc.n,1.81,2.25)){
      invcat <- "GR"
    } else {
      invcat <- "AG"
    }
    
    p <-inv.m[c("Index", invcat, "Description")]
    
    #drop NA rows
    p <- na.omit(p)
    
    tags$div(HTML(paste("<h3>", p$Index,"</h3>","<br>", p$Description,"<br>","<br>", sep="\n")))
    
    #colnames(p) <- c("Fund","X","Description")
    
  })
  
  output$pie <- renderPlotly({
    
    invsc <- data.frame(data.list)
    invsc <- dplyr::mutate_all(invsc, function(x) as.numeric(as.character(x)))
    invsc %>% dplyr::select(-goal, everything()) -> invsc
    
    invsc.n <- rowMeans(invsc[, 2:12])
    
    if (between(invsc.n,0.99,1.4)){
      invcat <- "PP"
    } else if (between(invsc.n,1.41,1.8)){
      invcat <- "FF"
    }else if (between(invsc.n,1.81,2.25)){
      invcat <- "GR"
    } else {
      invcat <- "AG"
    }
    
    p <-inv.m[c("Index", invcat, "Description")]
    colnames(p) <- c("Fund","X","Description")
    
    plot_ly() %>%
      add_pie(data = p, labels = ~Fund, values = ~X,
              name = "Your Spread", marker = list(colors = myPalette))%>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(x = 0.5, y=-0.15, xanchor = "center"))
    
   # pie(table(p$X), labels = p$Fund, border="white", col=myPalette)
  })
  
  output$perf <- renderPlot({
    
    invsc <- data.frame(data.list)
    invsc <- dplyr::mutate_all(invsc, function(x) as.numeric(as.character(x)))
    invsc %>% dplyr::select(-goal, everything()) -> invsc
    
    invsc.n <- rowMeans(invsc[, 2:12])
    
    if (between(invsc.n,0.99,1.4)){
      invcat <- "PP"
    } else if (between(invsc.n,1.41,1.8)){
      invcat <- "FF"
    }else if (between(invsc.n,1.81,2.25)){
      invcat <- "GR"
    } else {
      invcat <- "AG"
    } 
    
    p <-inv.m[c("Index", invcat, "Description","Perfomance.1Year","Perfomance.5Years")]
    
    #drop NA rows
    p <- na.omit(p)
    
    t <- barplot(height=t(p[c("Perfomance.1Year","Perfomance.5Years")]), beside=T, names=p$Index, border="white", 
                 col=myPalette, legend.text=c('1 Yr Yield','5 YR Yield'), axes = F)
    text(x=t, y = t(p[c("Perfomance.1Year","Perfomance.5Years")]) + 1, labels=as.character(t(p[c("Perfomance.1Year","Perfomance.5Years")])), xpd=TRUE)
    
  })
  
  output$returns <- renderPlotly({
    
    invsc <- data.frame(data.list)
    invsc <- dplyr::mutate_all(invsc, function(x) as.numeric(as.character(x)))
    invsc %>% dplyr::select(-goal, everything()) -> invsc
    
    invsc.n <- rowMeans(invsc[, 2:12])
    
    if (between(invsc.n,0.99,1.4)){
      invcat <- "PP"
    } else if (between(invsc.n,1.41,1.8)){
      invcat <- "FF"
      ggplotly(ggplot(SYGALBB_returns) +
        aes(x = date) +
        geom_line(aes(y = SYGALBB_return, colour = "#66C2A5")) +
        # geom_line(aes(y = SYGALBB_twelve_mon_ret,colour = "#FC8D62")) +
        theme_minimal() + 
        scale_colour_discrete(name  ="", labels=c("SYGALBB ret", "SYGALBB 12 mon ret")) +
        ylab("") +
        xlab("") +
        theme(legend.position = "top"))
      
    } else if (between(invsc.n,1.81,2.25)){
      invcat <- "GR"
      
      ggplotly(ggplot(ASHR40_returns) +
        aes(x = date) +
        geom_line(aes(y = ASHR40_return, colour = "#66C2A5")) +
        #geom_line(aes(y = ASHR40_twelve_mon_ret,colour = "#FC8D62")) +
        theme_minimal() + 
        scale_colour_discrete(name  ="", labels=c("ASHR40 ret", "ASHR40 12 mon ret")) +
        ylab("") +
        xlab("") +
        theme(legend.position = "top"))
      
      
    } else {
      invcat <- "AG"
      
    ggplotly(ggplot(ASHR40_returns) +
        aes(x = date) +
        geom_line(aes(y = ASHR40_return, colour = "#66C2A5")) +
        #geom_line(aes(y = ASHR40_twelve_mon_ret,colour = "#FC8D62")) +
        theme_minimal() + 
        scale_colour_discrete(name  ="", labels=c("ASHR40 ret", "ASHR40 12 mon ret")) +
        ylab("") +
        xlab("") +
        theme(legend.position = "top"))
      
    }
    
  })
  
  pool <- reactive({
    cur <- 2000
    
    add <- as.numeric(input$knobpool)
    
    pl <- data.frame(
      id = "Pool Level",
      target = 5000,
      current = cur + add,
      pool = "A pool is where respondents with similar profiles can pool their money and we will invest in funds suitable to them. 
          You are finally able to participate in finance no matter your income level."
    )
    
    return(pl)
    
  })
  
  output$pool <- renderPlotly({
                
                pool <- pool()            
    
                ggplotly(ggplot(pool) +
                      geom_col(aes(x="", y=target), fill= NA, colour = "#66C2A5") +
                      geom_col(aes(x="", y=current), fill = "#66C2A5") +
                      geom_text(aes(x="", y=target, label = paste("N$",prettyNum(target, big.mark = ","))), size = 5, colour="#66C2A5", position = position_stack(vjust = 1.05)) +
                      geom_text(aes(x="", y=current, label = paste("N$",prettyNum(current, big.mark = ","))), size = 5, colour="#66C2A5", position = position_stack(vjust = 1.07)) +
                      theme_minimal() +
                      xlab("") +
                      ylab("") +
                      theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                      theme_void()) %>%
                      layout(xaxis = list(showgrid = F, showline = F),
                             yaxis = list(showgrid = F, showline = F)) %>% 
                      config(displayModeBar = F)
    
    })
  
  output$pooldesc <- renderUI({
    
    tags$div(HTML(paste("<h3>", "Join A Pool","</h3>","<br>", pool()$pool,"<br>","<br>", sep="\n")))
  })
  
  ## Create access token and render login button
  access_token <- callModule(googleAuth, "loginButton", approval_prompt = "force")

}

# end server--------

shinyApp(ui, server)
