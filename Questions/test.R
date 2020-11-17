bulmaPage(  
  bulmaHero(
    color = "white",
    fullheight = F,
    bulmaHeroBody(bulmaTitle("If you had to pick one of two portfolios, which would it be?"),
                  bulmaContainer(
                    bulmaColumn(radioButtons("question5", label = NULL, choices = c("80 percent stocks/20 percent bonds", "40 percent stocks/60 percent bonds"), inline = T))
                  )
    )
  ),
  bulmaSection(
    bulmaContainer(
      bulmaColumns(
        bulmaColumn(),
        bulmaColumn(actionButton("block_six", "Next")),
        bulmaColumn()
      )
    )
  )
)