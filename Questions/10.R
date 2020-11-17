bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("What are you saving for?"),
                      br(), br(),
                      bulmaContainer(
                        bulmaColumns(
                            bulmaColumn(radioButtons("question10", label = NULL, choiceNames = c("Retirement", "To build wealth"), choiceValues= c(1,2), inline = T))
                        )
                   )
              )
        ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(),
                bulmaColumn(actionButton("block_eleven", "Next")),
                bulmaColumn()
            )
        )
    )
)