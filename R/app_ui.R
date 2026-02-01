#' The application User-Interface
#'
#' @param request Internal parameter for {shiny}.
#'
#' @noRd
app_ui <- function(request) {
    tagList(
        # External resources (CSS, JS, etc.)
        golem_add_external_resources(),

        # Session timeout module (kept outside main layout so it doesn't affect spacing)
        mod_timeout_client_ui("session_timeout"),
        tags$style(HTML("
            .nav-tabs {
                display: flex;
                flex-wrap: nowrap;
            }

            .nav-tabs .nav-icons {
                display: flex;
                gap: 0.75rem;
                margin-left: auto;
                padding-right: 1rem;
                align-items: center;
            }

            .nav-tabs .nav-icons a {
                color: inherit;
                text-decoration: none;
            }
        ")),

        # Main application layout
        fluidPage(
            theme = app_theme(),
            shinyjs::useShinyjs(),
            tags$script(
                "data-goatcounter" = "https://threatlayers.goatcounter.com/count",
                "async src" = "//gc.zgo.at/count.js"
            ),
            bslib::navset_tab(
                id = "main_tabs",
                title = NULL,
                bslib::nav_panel(
                    title = tagList(icon("layer-group"), "Threat layers"),
                    value = "threat_layers_tab",
                    div(class = "p-3", mod_threat_layers_selection_ui("threat_layers"))
                ),
                bslib::nav_panel(
                    title = tagList(icon("map"), "Map"),
                    value = "processing_tab",
                    div(class = "p-3", mod_threat_layers_processing_ui("threat_processing"))
                ),
                bslib::nav_spacer(),
                bslib::nav_item(
                    tags$span(
                        class = "nav-icons",
                        style = "display: flex; gap: 0.75rem; align-items: center;",
                        tags$a(
                            href = "https://insileco.io/ceacws/3_threats.html",
                            target = "_blank",
                            title = "Project Report",
                            icon("book")
                        ),
                        tags$a(
                            href = "https://github.com/inSilecoInc/ceacws",
                            target = "_blank",
                            title = "GitHub Repository",
                            icon("github")
                        )
                    )
                ),
                bslib::nav_item()
            )
        )
    )
}
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
    golem::add_resource_path(
        "www",
        app_sys("app/www")
    )

    tags$head(
        golem::favicon(),
        golem::bundle_resources(
            path = app_sys("app/www"),
            app_title = "ceacws"
        )
    )
}


#' Application theme helper
#'
#' Define a central bs_theme for reuse and easier maintenance.
#'
#' @return A {bslib} theme object
#' @noRd
app_theme <- function() {
    bslib::bs_theme(
        version = 5,
        "table-striped-bg" = "#e6f6ec",
        "table-hover-color" = "#fff",
        "table-hover-bg" = "#03662a",
        "table-active-bg" = "#03662a",
        "pagination-color" = "#03662a",
        "pagination-hover-bg" = "#e6f6ec",
        "pagination-focus-color" = "#03662a",
        "accordion-button-focus-border-color" = "#e6f6ec",
        "focus-ring-color" = "#e6f6ec"
    )
}
