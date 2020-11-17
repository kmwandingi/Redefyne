bulmaPage(  
  bulmaNavbar(
    bulmaNavbarBrand(
      bulmaNavbarItem(
        h1(div(HTML(paste0("<font color=\"#66C2A5\"><b>","rydefine", ".","</b></font>"))))
      )
    )
  ),  
  bulmaHero(
    color = "white",
    fullheight = F,
    bulmaHeroBody(bulmaSubtitle("Log in with your with your google account"))
  ),
  bulmaSection(
    bulmaContainer(
      bulmaColumns(
        bulmaColumn(),
        bulmaColumn(googleAuthUI("loginButton")
),
        bulmaColumn()
      )
    )
  )
)
