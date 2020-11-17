bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("How long until your stated saving goal? (In Years)"),
                      br(), br(),
                      bulmaContainer(
                          bulmaColumn(radioButtons("question11", label = NULL, choiceNames = c("0-5", "5-10", "10-15", "15-30"), choiceValues= c(1,2,3,4), inline = T))
                      )
        )
    ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(),
                bulmaColumn(actionButton("block_twelve", "Next")),
                bulmaColumn()
            )
        )
    )
)