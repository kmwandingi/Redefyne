bulmaPage(  
    bulmaHero(
        color = "white",
        fullheight = F,
        br(), br(),
        bulmaHeroBody(bulmaTitle("How much do you need for your stated goal?"),
                      br(), br(),
                      bulmaContainer(
                          bulmaColumn(radioButtons("question11", label = NULL, choiceNames = c("N$ 10 000 - N$50 000",  "N$50 000 - N$100 000", "N$100 000 - N$300 000", "N$300 000 - N$1 000 000"), choiceValues= c(1,2,3,4), inline = T))
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