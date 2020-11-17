bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("Is your wealth goal intended to continue your current lifestyle,"),
                      bulmaTitle("or are you motivated to build wealth at the expense of current lifestyle?"), br(),
                      bulmaContainer(
                          bulmaColumn(radioButtons("question6", label = NULL, choiceNames = c("Build wealth", "Continue current lifestyle"), choiceValues= c(1,2), inline = T))
                      )
        )
    ),
    bulmaSection(
        bulmaContainer(
            bulmaColumns(
                bulmaColumn(),
                bulmaColumn(actionButton("block_seven", "Next")),
                bulmaColumn()
            )
        )
    )
)