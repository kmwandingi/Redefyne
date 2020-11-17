bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("Would you prefer to maintain control over your investments"),
                      bulmaTitle("or prefer to delegate that responsibility to someone else?"), bulmaTitle(""), 
                      bulmaContainer(
                          bulmaColumn(radioButtons("question3", label = NULL, choiceNames = c("Maintain Control", "Delegate"), choiceValues= c(1,2), inline = T))
                      )
        )
    ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(),
                bulmaColumn(actionButton("block_four", "Next")),
                bulmaColumn()
            )
        )
    )
)