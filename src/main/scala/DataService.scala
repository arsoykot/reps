package reps.services

import reps.models._

import java.time.LocalDate
import java.time.temporal.WeekFields
import java.util.Locale

/**
 * Pure functional service for filtering, sorting, and searching energy readings.
 * All methods are pure functions with no side effects.
 */
object DataService {

  // ── Filtering ─────────────────────────────────────────────────────────────

  /**
   * Generic higher-order filter — applies a predicate to a list of readings.
   * Used as the backbone for all specific filter functions.
   */
  def filterBy(readings: List[EnergyReading])(predicate: EnergyReading => Boolean): List[EnergyReading] =
    readings.filter(predicate)

  /** Filter readings to only those from a specific energy source */
  def filterBySource(source: EnergySource): List[EnergyReading] => List[EnergyReading] =
    filterBy(_)(_.source == source)

  /** Filter readings by a specific date and hour (0-23) */
  def filterByHour(date: LocalDate, hour: Int): List[EnergyReading] => List[EnergyReading] =
    filterBy(_) { r =>
      r.timestamp.toLocalDate == date && r.timestamp.getHour == hour
    }

  /** Filter readings to a specific calendar date */
  def filterByDay(date: LocalDate): List[EnergyReading] => List[EnergyReading] =
    filterBy(_)(_.timestamp.toLocalDate == date)

  /**
   * Filter readings to a ISO-standard week.
   * @param weekStart Monday of the target week.
   */
  def filterByWeek(weekStart: LocalDate): List[EnergyReading] => List[EnergyReading] = {
    val weekEnd = weekStart.plusDays(6)
    filterBy(_) { r =>
      val d = r.timestamp.toLocalDate
      !d.isBefore(weekStart) && !d.isAfter(weekEnd)
    }
  }

  /** Filter readings to a specific year/month */
  def filterByMonth(year: Int, month: Int): List[EnergyReading] => List[EnergyReading] =
    filterBy(_) { r =>
      r.timestamp.getYear == year && r.timestamp.getMonthValue == month
    }

  /** Apply a TimeFilter algebraically — dispatches to the correct filter function */
  def applyTimeFilter(filter: TimeFilter)(readings: List[EnergyReading]): List[EnergyReading] =
    filter match {
      case HourlyFilter(hour, date)   => filterByHour(date, hour)(readings)
      case DailyFilter(date)          => filterByDay(date)(readings)
      case WeeklyFilter(weekStart)    => filterByWeek(weekStart)(readings)
      case MonthlyFilter(year, month) => filterByMonth(year, month)(readings)
    }

  // ── Sorting ────────────────────────────────────────────────────────────────

  /**
   * Sort readings by a given field.
   * Returns a new list — original is unchanged (immutability).
   */
  def sortReadings(config: SortConfig)(readings: List[EnergyReading]): List[EnergyReading] = {
    val comparator: (EnergyReading, EnergyReading) => Boolean = config.field match {
      case SortByTimestamp => (a, b) => a.timestamp.isBefore(b.timestamp)
      case SortByOutput    => (a, b) => a.outputKWh < b.outputKWh
      case SortBySource    => (a, b) => a.source.toString < b.source.toString
    }
    val sorted = readings.sortWith(comparator)
    if (config.ascending) sorted else sorted.reverse
  }

  // ── Searching ──────────────────────────────────────────────────────────────

  /**
   * Full-text search across source, status, and ID fields.
   * Case-insensitive substring match.
   */
  def search(query: String)(readings: List[EnergyReading]): List[EnergyReading] = {
    val q = query.trim.toLowerCase
    filterBy(readings) { r =>
      r.source.toString.toLowerCase.contains(q) ||
      r.status.toString.toLowerCase.contains(q) ||
      r.id.toLowerCase.contains(q) ||
      r.outputKWh.toString.contains(q)
    }
  }

  /**
   * Search for readings within a kWh output range [minKWh, maxKWh].
   * Returns Right on success, Left if the range is invalid.
   */
  def searchByOutputRange(
    minKWh: Double,
    maxKWh: Double
  )(readings: List[EnergyReading]): Either[String, List[EnergyReading]] =
    if (minKWh > maxKWh)
      Left(s"Minimum kWh ($minKWh) cannot be greater than maximum ($maxKWh).")
    else
      Right(filterBy(readings)(r => r.outputKWh >= minKWh && r.outputKWh <= maxKWh))

  // ── Week helper ────────────────────────────────────────────────────────────

  /** Return the Monday of the ISO week containing the given date */
  def weekStartFor(date: LocalDate): LocalDate = {
    val wf = WeekFields.of(Locale.getDefault)
    date.`with`(wf.dayOfWeek(), 1)
  }
}
