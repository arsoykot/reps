package reps.services

import reps.models._

import java.time.LocalDateTime
import java.util.UUID

/**
 * Pure functional alert detection service.
 *
 * Generates Alert values from a list of EnergyReadings without
 * mutating any state. All alert generation is done via map/flatMap/filter.
 */
object AlertService {

  /**
   * Analyse a list of readings and return all applicable alerts.
   *
   * @param readings   The energy readings to inspect
   * @param thresholds Threshold configuration per source (defaults provided)
   * @return           A list of generated Alert records, possibly empty
   */
  def detectAlerts(
    readings:   List[EnergyReading],
    thresholds: Map[EnergySource, AlertThreshold] = AlertThreshold.defaults
  ): List[Alert] = {
    val grouped: Map[EnergySource, List[EnergyReading]] = readings.groupBy(_.source)

    grouped.toList.flatMap { case (source, sourceReadings) =>
      val threshold = thresholds.getOrElse(source, AlertThreshold.defaults(source))
      detectForSource(source, sourceReadings, threshold)
    }.sortBy(_.severity.toString) // Critical first (lexicographic: C < I < W)
  }

  /**
   * Detect alerts for a single energy source.
   * Returns an immutable list of zero or more Alert values.
   */
  private def detectForSource(
    source:    EnergySource,
    readings:  List[EnergyReading],
    threshold: AlertThreshold
  ): List[Alert] = {
    List(
      detectLowOutput(source, readings, threshold),
      detectOffline(source, readings),
      detectDegradation(source, readings, threshold),
      detectZeroOutput(source, readings)
    ).flatten
  }

  /** Alert if most recent reading is below critical minimum */
  private def detectLowOutput(
    source:    EnergySource,
    readings:  List[EnergyReading],
    threshold: AlertThreshold
  ): Option[Alert] = {
    val latestOpt = readings.maxByOption(_.timestamp)
    latestOpt.flatMap { latest =>
      if (latest.outputKWh < threshold.criticalMinOutputKWh && latest.status != Offline) {
        Some(makeAlert(source, Critical,
          s"${source} output critically low: ${latest.outputKWh} kWh (threshold: ${threshold.criticalMinOutputKWh} kWh)"))
      } else if (latest.outputKWh < threshold.minOutputKWh && latest.status == Operational) {
        Some(makeAlert(source, Warning,
          s"${source} output below expected minimum: ${latest.outputKWh} kWh (threshold: ${threshold.minOutputKWh} kWh)"))
      } else None
    }
  }

  /** Alert if latest reading shows the source is offline */
  private def detectOffline(source: EnergySource, readings: List[EnergyReading]): Option[Alert] =
    readings.maxByOption(_.timestamp).flatMap { latest =>
      if (latest.status == Offline)
        Some(makeAlert(source, Critical, s"${source} source is OFFLINE. Immediate inspection required."))
      else None
    }

  /** Alert if more than degradedRatioThreshold fraction of readings are Degraded */
  private def detectDegradation(
    source:    EnergySource,
    readings:  List[EnergyReading],
    threshold: AlertThreshold
  ): Option[Alert] = {
    if (readings.isEmpty) None
    else {
      val degradedCount = readings.count(_.status == Degraded)
      val ratio         = degradedCount.toDouble / readings.size
      if (ratio >= threshold.degradedRatioThreshold)
        Some(makeAlert(source, Warning,
          s"${source}: ${(ratio * 100).toInt}% of readings show degraded status. Equipment check recommended."))
      else None
    }
  }

  /** Alert if multiple consecutive readings show zero output (possible malfunction) */
  private def detectZeroOutput(source: EnergySource, readings: List[EnergyReading]): Option[Alert] = {
    val recent   = readings.sortBy(_.timestamp).takeRight(3)
    val allZero  = recent.nonEmpty && recent.forall(_.outputKWh == 0.0)
    if (allZero)
      Some(makeAlert(source, Critical,
        s"${source}: Three or more consecutive readings with zero output. Possible equipment malfunction."))
    else None
  }

  // ── Helpers ────────────────────────────────────────────────────────────────

  private def makeAlert(source: EnergySource, severity: AlertSeverity, message: String): Alert =
    Alert(
      id        = UUID.randomUUID().toString.take(8),
      source    = source,
      severity  = severity,
      message   = message,
      timestamp = LocalDateTime.now()
    )
}
