package reps.models

import java.time.LocalDateTime

/**
 * Severity level of an alert — ordered from informational to critical.
 */
sealed trait AlertSeverity
case object Info     extends AlertSeverity { override def toString: String = "INFO"     }
case object Warning  extends AlertSeverity { override def toString: String = "WARNING"  }
case object Critical extends AlertSeverity { override def toString: String = "CRITICAL" }

/**
 * Represents a system-generated alert for operators.
 *
 * @param id          Unique alert identifier
 * @param source      The energy source that triggered the alert
 * @param severity    Severity level
 * @param message     Human-readable description of the issue
 * @param timestamp   When the alert was generated
 * @param resolved    Whether the alert has been acknowledged/resolved
 */
case class Alert(
  id:        String,
  source:    EnergySource,
  severity:  AlertSeverity,
  message:   String,
  timestamp: LocalDateTime,
  resolved:  Boolean = false
)

/**
 * Thresholds used by the alert detection service.
 * Immutable and parameterised per energy source.
 */
case class AlertThreshold(
  source:                EnergySource,
  minOutputKWh:          Double,   // Below this triggers low-output warning
  criticalMinOutputKWh:  Double,   // Below this triggers critical alert
  degradedRatioThreshold: Double   // Fraction of degraded readings before alerting
)

object AlertThreshold {
  /** Default thresholds for each source type */
  val defaults: Map[EnergySource, AlertThreshold] = Map(
    Solar -> AlertThreshold(Solar, minOutputKWh = 5.0,  criticalMinOutputKWh = 1.0,  degradedRatioThreshold = 0.3),
    Wind  -> AlertThreshold(Wind,  minOutputKWh = 10.0, criticalMinOutputKWh = 2.0,  degradedRatioThreshold = 0.3),
    Hydro -> AlertThreshold(Hydro, minOutputKWh = 20.0, criticalMinOutputKWh = 5.0,  degradedRatioThreshold = 0.3)
  )
}
