package reps.services

import reps.models._
import reps.io.CsvIO

import java.time.LocalDateTime
import java.util.UUID

/**
 * The central plant monitoring service.
 *
 * In the functional paradigm the "plant state" is an immutable value.
 * Mutations (adding readings, updating storage) return a new PlantState
 * rather than modifying an existing one.
 */
object PlantMonitor {

  /**
   * Immutable snapshot of the plant's current state.
   *
   * @param readings       All loaded/recorded energy readings
   * @param storageKWh     Current battery/storage capacity in kWh
   * @param maxStorageKWh  Plant's maximum storage capacity
   * @param dataFilePath   Path to the CSV persistence file
   */
  case class PlantState(
    readings:       List[EnergyReading],
    storageKWh:     Double,
    maxStorageKWh:  Double,
    dataFilePath:   String
  ) {
    /** Total kWh produced across all sources (read-only derived value) */
    def totalProducedKWh: Double = readings.map(_.outputKWh).sum

    /** Latest reading for each source */
    def latestBySource: Map[EnergySource, EnergyReading] =
      readings.groupBy(_.source).collect {
        case (src, rs) if rs.nonEmpty => src -> rs.maxBy(_.timestamp)
      }

    /** Storage fill percentage (0–100) */
    def storagePercent: Double =
      if (maxStorageKWh == 0) 0.0
      else (storageKWh / maxStorageKWh * 100).min(100.0)
  }

  /** Create an initial (empty) plant state */
  def initialState(dataFilePath: String, maxStorageKWh: Double = 10000.0): PlantState =
    PlantState(
      readings      = List.empty,
      storageKWh    = 0.0,
      maxStorageKWh = maxStorageKWh,
      dataFilePath  = dataFilePath
    )

  /**
   * Load readings from disk and merge them into the plant state.
   * Returns Right(newState) or Left(errorMessage).
   */
  def loadFromFile(state: PlantState): Either[String, PlantState] =
    CsvIO.loadReadings(state.dataFilePath).map { loaded =>
      state.copy(readings = loaded)
    }

  /**
   * Record a new energy reading: adds it to state and persists to CSV.
   * Returns Right(newState) or Left(error).
   */
  def recordReading(
    state:     PlantState,
    source:    EnergySource,
    outputKWh: Double,
    status:    SourceStatus = Operational
  ): Either[String, PlantState] = {
    val reading = EnergyReading(
      id        = UUID.randomUUID().toString.take(8).toUpperCase,
      source    = source,
      timestamp = LocalDateTime.now(),
      outputKWh = outputKWh,
      status    = status
    )
    CsvIO.appendReading(state.dataFilePath, reading) match {
      case scala.util.Success(_) =>
        Right(state.copy(readings = state.readings :+ reading))
      case scala.util.Failure(ex) =>
        Left(s"Failed to persist reading: ${ex.getMessage}")
    }
  }

  /**
   * Update storage level, clamping to [0, maxStorageKWh].
   * Returns a new PlantState — immutable update.
   */
  def updateStorage(state: PlantState, deltaKWh: Double): PlantState = {
    val newLevel = (state.storageKWh + deltaKWh).max(0.0).min(state.maxStorageKWh)
    state.copy(storageKWh = newLevel)
  }

  /**
   * Summarise the current production status — used by the overview display.
   */
  def productionSummary(state: PlantState): String = {
    val latest = state.latestBySource
    val lines  = EnergySource.all.map { src =>
      val entry = latest.get(src).map(r =>
        f"${r.outputKWh}%8.2f kWh  [${r.status}]"
      ).getOrElse("      -- no data --")
      f"  ${src.toString}%-6s : $entry"
    }
    lines.mkString("\n")
  }
}
