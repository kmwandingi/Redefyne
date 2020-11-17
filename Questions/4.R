bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("Do you have faith in your abilities as an investor?"),
                      br(), br(),
                      bulmaContainer(
                          bulmaColumn(radioButtons("question4", label = NULL, choiceNames = c("Yes", "No"), choiceValues= c(1,2), inline = T))
                      )
        )
    ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(),
                bulmaColumn(actionButton("block_five", "Next")),
                bulmaColumn()
            )
        )
    )
)