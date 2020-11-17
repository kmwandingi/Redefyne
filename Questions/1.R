bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("Have you risked your own capital in the creation of your wealth?"),
                      bulmaTitle(""), bulmaTitle(""),
        bulmaContainer(
                bulmaColumn(radioButtons("question1", label = NULL, choiceNames = c("Yes","No"), choiceValues= c(1,2),inline = T))
        )
      )
    ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(actionButton("block_two", "Next"))
            )
        )
    )
)