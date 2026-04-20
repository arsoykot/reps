package reps.analysis

import reps.models._

/**
 * Pure functional statistical analysis service.
 *
 * All functions are referentially transparent — given the same inputs they
 * always return the same outputs with no side effects.
 */
object AnalysisService {

  // ── Core statistics (pure functions, work on any Seq[Double]) ─────────────

  /**
   * Mean: sum of all values divided by the count.
   * Returns None for an empty sequence.
   */
  def mean(values: Seq[Double]): Option[Double] =
    if (values.isEmpty) None
    else Some(values.sum / values.size)

  /**
   * Median: the middle value in a sorted sequence.
   * For even-length sequences, returns the average of the two middle values.
   * Returns None for an empty sequence.
   */
  def median(values: Seq[Double]): Option[Double] =
    if (values.isEmpty) None
    else {
      val sorted = values.sorted
      val n      = sorted.size
      if (n % 2 == 1) Some(sorted(n / 2))
      else            Some((sorted(n / 2 - 1) + sorted(n / 2)) / 2.0)
    }

  /**
   * Mode: the value that appears most frequently.
   * If multiple values tie, returns the smallest.
   * Returns None for an empty sequence.
   */
  def mode(values: Seq[Double]): Option[Double] =
    if (values.isEmpty) None
    else {
      val freq = values.groupBy(identity).view.mapValues(_.size)
      val maxFreq = freq.values.max
      Some(freq.filter(_._2 == maxFreq).keys.min)
    }

  /**
   * Range: difference between maximum and minimum values.
   * Returns None for an empty sequence.
   */
  def range(values: Seq[Double]): Option[Double] =
    if (values.isEmpty) None
    else Some(values.max - values.min)

  /**
   * Midrange: (min + max) / 2 — the value exactly halfway between extremes.
   * Returns None for an empty sequence.
   */
  def midrange(values: Seq[Double]): Option[Double] =
    if (values.isEmpty) None
    else Some((values.min + values.max) / 2.0)

  // ── Higher-order composed analyser ────────────────────────────────────────

  /**
   * Type alias: a statistical function that operates on a Seq[Double].
   * Demonstrates type parameterisation and higher-order functions.
   */
  type StatFn = Seq[Double] => Option[Double]

  /**
   * Apply a list of statistical functions to a dataset, returning
   * a map from function label to result. Demonstrates HOF composition.
   *
   * @param labelledFns List of (label, function) pairs
   * @param values      The data to analyse
   */
  def applyAll(labelledFns: List[(String, StatFn)])(values: Seq[Double]): Map[String, Option[Double]] =
    labelledFns.foldLeft(Map.empty[String, Option[Double]]) { case (acc, (label, fn)) =>
      acc + (label -> fn(values))
    }

  /** Standard set of statistics used in REPS reports */
  val standardStats: List[(String, StatFn)] = List(
    "Mean"     -> mean,
    "Median"   -> median,
    "Mode"     -> mode,
    "Range"    -> range,
    "Midrange" -> midrange
  )

  // ── Full analysis of energy readings ──────────────────────────────────────

  /**
   * Produce a full AnalysisSummary for a list of readings, optionally
   * labelled with the source for context.
   *
   * Returns None if the reading list is empty.
   */
  def analyse(readings: List[EnergyReading], source: Option[EnergySource] = None): Option[AnalysisSummary] = {
    if (readings.isEmpty) return None
    val values = readings.map(_.outputKWh)
    for {
      m    <- mean(values)
      med  <- median(values)
      mo   <- mode(values)
      r    <- range(values)
      mr   <- midrange(values)
    } yield AnalysisSummary(
      source   = source,
      count    = readings.size,
      mean     = round2(m),
      median   = round2(med),
      mode     = round2(mo),
      range    = round2(r),
      midrange = round2(mr),
      min      = round2(values.min),
      max      = round2(values.max)
    )
  }

  /**
   * Analyse readings grouped by energy source.
   * Returns a map from source to AnalysisSummary.
   * Uses higher-order function (groupBy + mapValues).
   */
  def analyseBySource(readings: List[EnergyReading]): Map[EnergySource, AnalysisSummary] =
    readings
      .groupBy(_.source)
      .collect { case (src, rs) if rs.nonEmpty =>
        src -> analyse(rs, Some(src)).get
      }

  // ── Curried helper — demonstrates currying ────────────────────────────────

  /**
   * Curried function: given a threshold, returns a predicate that tests
   * whether a reading's output is above that threshold.
   * Demonstrates currying in a practical context.
   */
  def aboveThreshold(threshold: Double)(reading: EnergyReading): Boolean =
    reading.outputKWh > threshold

  /**
   * Curried function: compute percentage of readings above a threshold.
   */
  def percentageAbove(threshold: Double)(readings: List[EnergyReading]): Double =
    if (readings.isEmpty) 0.0
    else {
      val above = readings.count(aboveThreshold(threshold))
      round2(above.toDouble / readings.size * 100)
    }

  // ── Private ───────────────────────────────────────────────────────────────

  private def round2(v: Double): Double =
    BigDecimal(v).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
}
