div(class = 'container',
    div(class = 'col-sm-2'),
    div(class = 'col-sm-8',
        br(),
        br(),
        br(),
        h1("Which is Stronger? Your tolerance for risk to build weath or the desire to preserve wealth?"),
        radioButtons("question2", label = NULL, choices = c("Tolerance for risk", "Preserve wealth")),
        actionButton("block_three", "Next"),
        br()
    )
)

