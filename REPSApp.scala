package reps.ui

import reps.analysis.AnalysisService
import reps.io.{CsvIO, FingridAPI}
import reps.models._
import reps.services._
import reps.services.PlantMonitor._

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.annotation.tailrec
import scala.io.StdIn

/**
 * Main CLI application entry point.
 *
 * The application loop is modelled as a recursive function (functional style).
 * State is threaded through as an immutable value — no mutable global state.
 */
object REPSApp extends App {

  private val DataFile = "data/energy_readings.csv"
  private val DateFmt  = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  // ── Bootstrap ──────────────────────────────────────────────────────────────

  println(Display.header("RENEWABLE ENERGY PLANT SYSTEM (REPS)"))
  println(Display.info("Loading data from disk..."))

  val initialState: PlantState = {
    val empty = PlantMonitor.initialState(DataFile)
    PlantMonitor.loadFromFile(empty) match {
      case Right(s) =>
        println(Display.success(s"Loaded ${s.readings.size} readings."))
        s
      case Left(err) =>
        println(Display.error(s"Could not load data: $err\n  Starting with empty dataset."))
        empty
    }
  }

  // ── Main recursive loop ────────────────────────────────────────────────────

  /**
   * Main application loop. Uses tail recursion (via @tailrec) to replace
   * imperative while loops — pure functional iteration.
   *
   * @param state Current (immutable) plant state
   */
  private def readInput(prompt: String): String = {
    print(prompt)
    Console.flush()
    StdIn.readLine()
  }

  @tailrec
  def mainLoop(state: PlantState): Unit = {
    printMainMenu()
    val choice = readInput("> ").trim
    choice match {
      case "1" => mainLoop(menuRecordReading(state))
      case "2" => mainLoop(menuViewOverview(state))
      case "3" => mainLoop(menuFilterData(state))
      case "4" => mainLoop(menuAnalyseData(state))
      case "5" => mainLoop(menuSearchData(state))
      case "6" => mainLoop(menuAlerts(state))
      case "7" => mainLoop(menuSortData(state))
      case "8" => mainLoop(menuFetchFingrid(state))
      case "0" =>
        println(Display.info("Shutting down REPS. Goodbye."))
      case _ =>
        println(Display.error("Invalid option. Please enter a number 0–8."))
        mainLoop(state)
    }
  }

  mainLoop(initialState)

  // ── Menu printers ──────────────────────────────────────────────────────────

  private def printMainMenu(): Unit = println(
    s"""
       |${Display.sep}
       |  Main Menu
       |${Display.sep}
       |  1. Record energy reading
       |  2. View plant overview
       |  3. Filter data (hourly / daily / weekly / monthly)
       |  4. Analyse data (mean, median, mode, range, midrange)
       |  5. Search data
       |  6. View alerts
       |  7. Sort data
       |  8. Fetch live data from Fingrid API
       |  0. Exit
       |${Display.sep}""".stripMargin
  )

  // ── Menu handlers ──────────────────────────────────────────────────────────

  /** Prompt the user to record a new energy reading */
  private def menuRecordReading(state: PlantState): PlantState = {
    println(Display.subHeader("Record Energy Reading"))
    val result = for {
      srcStr <- promptValidated("Source (Solar / Wind / Hydro): ")(Validation.validateNonEmpty(_, "Source"))
      source <- EnergySource.fromString(srcStr)
      kwhStr <- promptValidated("Output (kWh): ")(Validation.validatePositiveDouble(_, "Output"))
      kwh     = kwhStr
      stStr  <- promptValidated("Status (Operational / Degraded / Offline / Maintenance): ")(
                  Validation.validateNonEmpty(_, "Status"))
      status <- SourceStatus.fromString(stStr)
      newState <- PlantMonitor.recordReading(state, source, kwh, status)
    } yield newState

    result match {
      case Right(newState) =>
        println(Display.success("Reading recorded successfully."))
        newState
      case Left(err) =>
        println(Display.error(err))
        state
    }
  }

  /** Show the plant overview */
  private def menuViewOverview(state: PlantState): PlantState = {
    println(Display.subHeader("Plant Overview"))
    println(Display.plantOverview(state.readings, state.storageKWh, state.maxStorageKWh))
    state
  }

  /** Filter data by time period */
  private def menuFilterData(state: PlantState): PlantState = {
    println(Display.subHeader("Filter Data"))
    println(
      """  Filter by:
        |  1. Hour
        |  2. Day
        |  3. Week
        |  4. Month""".stripMargin)
    val choice = readInput("> ").trim
    val result: Either[String, List[EnergyReading]] = choice match {
      case "1" =>
        for {
          date <- promptDate("Date (DD/MM/YYYY): ")
          hour <- promptValidated("Hour (0-23): ")(Validation.validateHour)
        } yield DataService.filterByHour(date, hour)(state.readings)

      case "2" =>
        for {
          date <- promptDate("Date (DD/MM/YYYY): ")
        } yield DataService.filterByDay(date)(state.readings)

      case "3" =>
        for {
          date <- promptDate("Any date in the week (DD/MM/YYYY): ")
          weekStart = DataService.weekStartFor(date)
        } yield {
          println(Display.info(s"Showing week starting ${weekStart.format(DateFmt)}"))
          DataService.filterByWeek(weekStart)(state.readings)
        }

      case "4" =>
        for {
          year  <- promptValidated("Year (e.g. 2024): ")(Validation.validateYear)
          month <- promptValidated("Month (1-12): ")(Validation.validateMonth)
        } yield DataService.filterByMonth(year, month)(state.readings)

      case _ =>
        Left("Invalid filter option.")
    }

    result match {
      case Right(filtered) if filtered.isEmpty =>
        println(Display.info("No available data for the selected period. Please choose another date/period."))
      case Right(filtered) =>
        println(Display.subHeader(s"Filtered Results (${filtered.size} readings)"))
        println(Display.readingsTable(filtered))
      case Left(err) =>
        println(Display.error(err))
    }
    state
  }

  /** Statistical analysis menu */
  private def menuAnalyseData(state: PlantState): PlantState = {
    println(Display.subHeader("Data Analysis"))
    println(
      """  Analyse:
        |  1. All sources (combined)
        |  2. By source
        |  3. Filter then analyse""".stripMargin)
    val choice = readInput("> ").trim
    choice match {
      case "1" =>
        AnalysisService.analyse(state.readings) match {
          case Some(summary) =>
            println(Display.subHeader("Overall Statistics"))
            println(Display.analysisSummary(summary))
          case None =>
            println(Display.info("No data available for analysis."))
        }
      case "2" =>
        val summaries = AnalysisService.analyseBySource(state.readings)
        println(Display.analysisBySource(summaries))
      case "3" =>
        println(Display.info("First, apply a filter:"))
        val filtered = collectFilteredReadings(state)
        AnalysisService.analyse(filtered) match {
          case Some(summary) =>
            println(Display.subHeader("Statistics for Filtered Data"))
            println(Display.analysisSummary(summary))
          case None =>
            println(Display.info("No data in filtered range."))
        }
      case _ =>
        println(Display.error("Invalid option."))
    }
    state
  }

  /** Search menu */
  private def menuSearchData(state: PlantState): PlantState = {
    println(Display.subHeader("Search Data"))
    println(
      """  Search by:
        |  1. Keyword (source, status, ID)
        |  2. Output range (kWh)""".stripMargin)
    val choice = readInput("> ").trim
    val result: Either[String, List[EnergyReading]] = choice match {
      case "1" =>
        for {
          q <- promptValidated("Enter keyword: ")(Validation.validateSearchQuery)
        } yield DataService.search(q)(state.readings)

      case "2" =>
        for {
          minStr <- promptValidated("Minimum kWh: ")(Validation.validatePositiveDouble(_, "Minimum kWh"))
          maxStr <- promptValidated("Maximum kWh: ")(Validation.validatePositiveDouble(_, "Maximum kWh"))
          results <- DataService.searchByOutputRange(minStr, maxStr)(state.readings)
        } yield results

      case _ =>
        Left("Invalid search option.")
    }

    result match {
      case Right(found) if found.isEmpty =>
        println(Display.info("No matching records found."))
      case Right(found) =>
        println(Display.subHeader(s"Search Results (${found.size} match(es))"))
        println(Display.readingsTable(found))
      case Left(err) =>
        println(Display.error(err))
    }
    state
  }

  /** Alerts menu */
  private def menuAlerts(state: PlantState): PlantState = {
    println(Display.subHeader("System Alerts"))
    val alerts = AlertService.detectAlerts(state.readings)
    println(Display.alertsList(alerts))
    state
  }

  /** Sort menu */
  private def menuSortData(state: PlantState): PlantState = {
    println(Display.subHeader("Sort Data"))
    println(
      """  Sort by:
        |  1. Timestamp
        |  2. Output (kWh)
        |  3. Source
        |
        |  Order:
        |  a. Ascending
        |  d. Descending""".stripMargin)
    val fieldChoice = readInput("Sort field (1/2/3): ").trim
    val orderChoice = readInput("Order (a/d): ").trim

    val fieldOpt: Option[SortField] = fieldChoice match {
      case "1" => Some(SortByTimestamp)
      case "2" => Some(SortByOutput)
      case "3" => Some(SortBySource)
      case _   => None
    }
    val ascending = orderChoice.toLowerCase != "d"

    fieldOpt match {
      case None =>
        println(Display.error("Invalid sort field."))
      case Some(field) =>
        val config  = SortConfig(field, ascending)
        val sorted  = DataService.sortReadings(config)(state.readings)
        println(Display.subHeader(s"Sorted Results (${sorted.size} readings)"))
        println(Display.readingsTable(sorted))
    }
    state
  }

  /** Fetch live data from the Fingrid Open Data API */
  private def menuFetchFingrid(state: PlantState): PlantState = {
    println(Display.subHeader("Fetch Live Data from Fingrid API"))
    val apiKey = FingridAPI.apiKeyFromEnv.getOrElse {
      println(Display.info("No FINGRID_API_KEY env var found."))
      readInput("  Enter your Fingrid API key: ").trim
    }
    if (apiKey.isEmpty) {
      println(Display.error("No API key provided. Get one free at https://data.fingrid.fi/en/instructions"))
      return state
    }
    val hoursInput = readInput("  Hours of history to fetch (default 24): ").trim
    val hours = if (hoursInput.isEmpty) 24 else hoursInput.toIntOption.getOrElse(24)

    println(Display.info(s"Fetching last $hours hours of wind, hydro, and solar data from Fingrid..."))
    FingridAPI.fetchAll(apiKey, hours) match {
      case Left(err) =>
        println(Display.error(err))
        state
      case Right(fetched) if fetched.isEmpty =>
        println(Display.info("No data returned for the requested period."))
        state
      case Right(fetched) =>
        println(Display.success(s"Fetched ${fetched.size} readings (values in MW as reported by Fingrid)."))
        val merged = state.copy(readings = state.readings ++ fetched)
        new java.io.File(merged.dataFilePath).getParentFile.mkdirs()
        CsvIO.writeReadings(merged.dataFilePath, merged.readings) match {
          case scala.util.Failure(ex) =>
            println(Display.error(s"Could not save to CSV: ${ex.getMessage}"))
          case _ =>
        }
        merged
    }
  }

  // ── Helper: collect filtered readings inline ───────────────────────────────

  private def collectFilteredReadings(state: PlantState): List[EnergyReading] = {
    println(
      """  Filter type:
        |  1. Day
        |  2. Month""".stripMargin)
    val c = readInput("> ").trim
    val result = c match {
      case "1" =>
        promptDate("Date (DD/MM/YYYY): ").map(d => DataService.filterByDay(d)(state.readings))
      case "2" =>
        for {
          year  <- promptValidated("Year: ")(Validation.validateYear)
          month <- promptValidated("Month: ")(Validation.validateMonth)
        } yield DataService.filterByMonth(year, month)(state.readings)
      case _ => Left("Invalid option.")
    }
    result match {
      case Right(r) => r
      case Left(e)  =>
        println(Display.error(e))
        List.empty
    }
  }

  // ── Prompt helpers ─────────────────────────────────────────────────────────

  /**
   * Prompt the user and validate their input.
   * Loops until valid input is received — uses recursion instead of a while loop.
   */
  @tailrec
  private def promptValidated[A](prompt: String)(validate: String => Either[String, A]): Either[String, A] = {
    val input = readInput(s"  $prompt")
    if (input == null) Left("Input stream closed.")
    else validate(input) match {
      case r @ Right(_) => r
      case Left(err) =>
        println(Display.error(err))
        promptValidated(prompt)(validate)
    }
  }

  /** Shortcut to prompt for and validate a date string */
  private def promptDate(prompt: String): Either[String, LocalDate] =
    promptValidated(prompt)(Validation.validateDate)
}
