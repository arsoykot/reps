package reps.models

import java.time.LocalDateTime

/**
 * Sealed trait hierarchy representing all supported renewable energy source types.
 * Using sealed trait ensures exhaustive pattern matching.
 */
sealed trait EnergySource
case object Solar  extends EnergySource { override def toString: String = "Solar"  }
case object Wind   extends EnergySource { override def toString: String = "Wind"   }
case object Hydro  extends EnergySource { override def toString: String = "Hydro"  }

object EnergySource {
  /** Parse an EnergySource from a string, returning Either for functional error handling */
  def fromString(s: String): Either[String, EnergySource] = s.trim.toLowerCase match {
    case "solar" => Right(Solar)
    case "wind"  => Right(Wind)
    case "hydro" => Right(Hydro)
    case other   => Left(s"Unknown energy source: '$other'. Valid sources: Solar, Wind, Hydro")
  }

  val all: List[EnergySource] = List(Solar, Wind, Hydro)
}

/**
 * Immutable data record representing a single energy reading from a plant source.
 *
 * @param id          Unique identifier for this reading
 * @param source      The renewable energy source type
 * @param timestamp   When the reading was recorded
 * @param outputKWh   Energy produced in kilowatt-hours
 * @param status      Operational status of the source at time of reading
 */
case class EnergyReading(
  id:         String,
  source:     EnergySource,
  timestamp:  LocalDateTime,
  outputKWh:  Double,
  status:     SourceStatus
)

/**
 * Sealed trait hierarchy for equipment operational statuses.
 */
sealed trait SourceStatus
case object Operational  extends SourceStatus { override def toString: String = "Operational"  }
case object Degraded     extends SourceStatus { override def toString: String = "Degraded"     }
case object Offline      extends SourceStatus { override def toString: String = "Offline"      }
case object Maintenance  extends SourceStatus { override def toString: String = "Maintenance"  }

object SourceStatus {
  def fromString(s: String): Either[String, SourceStatus] = s.trim.toLowerCase match {
    case "operational" => Right(Operational)
    case "degraded"    => Right(Degraded)
    case "offline"     => Right(Offline)
    case "maintenance" => Right(Maintenance)
    case other         => Left(s"Unknown status: '$other'. Valid: Operational, Degraded, Offline, Maintenance")
  }
}

/**
 * A time-bounded filter for querying energy data.
 */
sealed trait TimeFilter
case class HourlyFilter(hour: Int, date: java.time.LocalDate)  extends TimeFilter
case class DailyFilter(date: java.time.LocalDate)              extends TimeFilter
case class WeeklyFilter(weekStart: java.time.LocalDate)        extends TimeFilter
case class MonthlyFilter(year: Int, month: Int)                extends TimeFilter

/**
 * Sort configuration — functionally parameterised so callers can compose sorts.
 */
sealed trait SortField
case object SortByTimestamp extends SortField
case object SortByOutput    extends SortField
case object SortBySource    extends SortField

case class SortConfig(field: SortField, ascending: Boolean = true)

/**
 * Summary statistics for a set of energy readings.
 * All fields are immutable; constructed by the analysis service.
 */
case class AnalysisSummary(
  source:    Option[EnergySource],
  count:     Int,
  mean:      Double,
  median:    Double,
  mode:      Double,
  range:     Double,
  midrange:  Double,
  min:       Double,
  max:       Double
)
