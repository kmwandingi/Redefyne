bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("Do you believe in the concept of borrowing money to make money/operate a business"),
                      bulmaTitle("or do you prefer to limit the amount of debt you owe?"), br(),
                      bulmaContainer(
                          bulmaColumn(radioButtons("question9", label = NULL, choiceNames = c("Borrow money", "Limit debt"), choiceValues= c(1,2), inline = T))
                      )
        )
    ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(),
                bulmaColumn(actionButton("block_ten", "Next")),
                bulmaColumn()
                        )
                    )
                )
            )