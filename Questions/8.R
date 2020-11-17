bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("Are you capital preservation oriented"),
                      bulmaTitle("or are you willing to put your capital at risk to build wealth?"), br(),
                      bulmaContainer(
                          bulmaColumn(radioButtons("question8", label = NULL, choiceNames = c("Capital at Risk", "Capital preservation oriented"), choiceValues= c(1,2), inline = T))
                      )
        )
    ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(),
                bulmaColumn(actionButton("block_nine", "Next")),
                bulmaColumn()
            )
        )
    )
)