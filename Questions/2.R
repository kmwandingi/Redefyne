bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("Which is Stronger? Your tolerance for risk to build weath or the desire to preserve wealth?"),
                      bulmaTitle(""), bulmaTitle(""),
                      bulmaContainer(
                          bulmaColumn(radioButtons("question2", label = NULL, choiceNames = c("Tolerance for risk", "Preserve wealth"), choiceValues= c(1,2), inline = T))
                      )
        )
    ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(),
                bulmaColumn(actionButton("block_three", "Next")),
                bulmaColumn()
            )
        )
    )
)