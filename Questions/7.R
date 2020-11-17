bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("Do you generally prefer to take initiative or to take direction?"),
                      br(), br(),
                      bulmaContainer(
                          bulmaColumn(radioButtons("question7", label = NULL, choiceNames = c("Take Initiative", "Take Direction"), choiceValues= c(1,2), inline = T))
                      )
        )
    ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(),
                bulmaColumn(actionButton("block_eight", "Next")),
                bulmaColumn()
            )
        )
    )
)