bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("If you had to pick one of two portfolios, which would it be?"),
                      br(), br(),
                      bulmaContainer(
                          bulmaColumn(radioButtons("question5", label = NULL, choiceNames = c("80 percent stocks/20 percent bonds", "40 percent stocks/60 percent bonds"), choiceValues= c(1,2), inline = T))
                      )
        )
    ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(),
                bulmaColumn(actionButton("block_six", "Next")),
                bulmaColumn()
            )
        )
    )
)