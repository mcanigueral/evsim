
# SIMULATION OF EV SESSIONS WITH CONFIGURATION PARAMETERS -----------------
# Packages required: evsim

#' evSessions Module: UI function
#'
#' @param id character, module ID
#'
#' @export
#'
evSessionsUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns('ui'))
}

#' evSessions Module: Server function
#'
#' @param id character, module ID
#' @param evmodel object of type `evmodel` (see this [link](https://mcanigueral.github.io/evprof/articles/evmodel.html) for more information)
#' @param sessions_day tibble with variables `time_cycle` (names corresponding to `evmodel$models$time_cycle`) and `n_sessions` (number of daily sessions per day for each time-cycle model)
#' @param profiles_ratios tibble with variables `time_cycle`, `profile` and `ratio`
#' The ratios of each time_cycle must sum 100.
#' @param charging_powers tibble with variables `power` and `ratio`
#' The powers must be in kW and the ratios between 0 and 1.
#' @param dates reactive object returning a date sequence that will set the time frame of the simulated sessions
#' @param resolution reactive object returning an integer, time resolution (in minutes) of the sessions datetime variables
#' @param seed integer, random seed of the simulation
#'
#' @export
#'
#' @importFrom dplyr tibble
#' @importFrom purrr map2_dbl map_dbl walk walk2
#' @importFrom rlang .data
#'
evSessions <- function(id, evmodel, sessions_day, profiles_ratios, charging_powers, dates, resolution, seed = 1234) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

# UI  ---------------------------------------------------------------------

      output$ui <- shiny::renderUI({
        shiny::fluidRow(
          shinydashboard::box(width = 12,
                              shiny::fluidRow(
                                shiny::column(10, shiny::h4(shiny::strong("Charging sessions"), style="margin-top: 0px; margin-bottom: 20px;")),
                                shiny::column(2, align="right", shinyWidgets::actionBttn(ns("update_ev_setup"), "Update", style = 'pill', color = 'primary', size = 'sm'))
              ),
              shiny::fluidRow(
                shinydashboard::box(title = "Number of sessions per day", width = 12, collapsible = TRUE, collapsed = TRUE,
                    do.call(
                      shinydashboard::tabBox,
                      append(
                        list(id = ns("ev_ratios"), side = "left", width = 12),
                        get_ev_models_config(sessions_day, profiles_ratios, ns)
                      )
                    )
                ),
                shinydashboard::box(title = "Charging power distribution", width = 12, collapsible = TRUE, collapsed = TRUE,
                    get_charging_powers_config(charging_powers, ns)
                )
              )
          )
        )
      })


# Server ------------------------------------------------------------------

      user_profiles_srv <- shiny::reactive({
        walk2(profiles_ratios[['time_cycle']], profiles_ratios[['profile']], ~ shiny::req(input[[paste0("ratio_", .y, "_", .x)]]/100)) # Evaluate all inputs to continue
        tibble(
          time_cycle = profiles_ratios[['time_cycle']],
          profile = profiles_ratios[['profile']],
          ratio = map2_dbl(.data$time_cycle, .data$profile, ~ input[[paste0("ratio_", .y, "_", .x)]]/100)
        )
      })

      sessions_day_srv <- shiny::reactive({
        walk(sessions_day[['time_cycle']], ~ shiny::req(input[[paste0("n_ev_sessions_", .x)]])) # Evaluate all inputs to continue
        tibble(
          time_cycle = sessions_day[['time_cycle']],
          n_sessions = map_dbl(.data$time_cycle, ~ input[[paste0("n_ev_sessions_", .x)]])
        )
      })

      power_ratios_srv <- shiny::reactive({
        walk(charging_powers[['power']], ~ shiny::req(input[[paste0("charging_", .x)]])) # Evaluate all inputs to continue
        tibble(
          power = charging_powers[['power']],
          ratio = map_dbl(.data$power, ~round(input[[paste0("charging_", .x)]]))
        )
      })

      return({
        shiny::eventReactive(input[['update_ev_setup']], ignoreNULL=FALSE, ignoreInit=FALSE, {
          set.seed(seed)
          simulate_sessions(
            evmodel, sessions_day_srv(), user_profiles_srv(), power_ratios_srv(), dates(), resolution()
          )
        })
      })
    }
  )
}



# EV Sessions support functions --------------------------------------------------------
get_ev_models_config <- function(sessions_day, profiles_ratios, ns) {
  # Map over each model (one model = one row in the df = one config tab in the UI)
  purrr::map(
    sessions_day[["time_cycle"]],
    ~ shiny::tabPanel(
      title = .x,
      shiny::fluidRow(
        shiny::column(2,
                      shiny::numericInput(ns(paste0("n_ev_sessions_", .x)),
                            label = "Number of sessions per day",
                            min = 0, max = 999999,
                            value = sessions_day[["sessions_day"]][sessions_day[["time_cycle"]] == .x],
                            step = 1)
        ),
        shiny::column(10,
               get_ev_profiles_config(.x, profiles_ratios, ns)
        )
      )
    )
  )
}

get_ev_profiles_config <- function(time_cycle, profiles_ratios, ns) {
  # Map over each user profile (one profile = one row in the df)
  ratios <- profiles_ratios[ profiles_ratios[["time_cycle"]] == time_cycle, c("profile", "ratio")]
  purrr::map(
    seq_len(nrow(ratios)),
    ~ shiny::column(
      width = 2,
      shiny::numericInput(ns(paste0("ratio_", ratios[["profile"]][.x], "_", time_cycle)),
                   label = paste(ratios[["profile"]][.x], '(%)'),
                   min = 0, max = 100,
                   value = ratios[["ratio"]][.x],
                   step = 1)
    )
  )
}

get_charging_powers_config <- function(charging_powers, ns) {
  # Map over each charging power level (one charging level = one row in the df)
  purrr::map(
    seq_len(nrow(charging_powers)),
    ~ shiny::column(
      width = round(9/nrow(charging_powers)),
      shiny::numericInput(ns(paste0("charging_", charging_powers[["power"]][.x])),
                   label = paste(charging_powers[["power"]][.x], 'kW ratio (%)'),
                   min = 0, max = 100,
                   value = charging_powers[["ratio"]][.x],
                   step = 1)
    )
  )
}




