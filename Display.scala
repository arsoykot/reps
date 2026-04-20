package reps.ui

import reps.models._

import java.time.format.DateTimeFormatter

/**
 * Pure display utilities — all functions take data and return formatted strings.
 * No I/O is performed here; callers decide how to print.
 */
object Display {

  private val DateTimeFmt  = DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm")
  private val SepLine      = "─" * 68

  // ── Section headers ────────────────────────────────────────────────────────

  def header(title: String): String = {
    val padded = s"  $title  "
    val bar    = "═" * padded.length
    s"\n$bar\n$padded\n$bar"
  }

  def subHeader(title: String): String =
    s"\n$SepLine\n  $title\n$SepLine"

  // ── Reading table ──────────────────────────────────────────────────────────

  /** Render a list of energy readings as a formatted table */
  def readingsTable(readings: List[EnergyReading]): String = {
    if (readings.isEmpty) return "  (no readings to display)"
    val hdr  = f"  ${"ID"}%-10s ${"Source"}%-8s ${"Timestamp"}%-18s ${"kWh"}%8s  ${"Status"}"
    val sep  = "  " + "─" * 64
    val rows = readings.map { r =>
      val ts = r.timestamp.format(DateTimeFmt)
      f"  ${r.id}%-10s ${r.source.toString}%-8s ${ts}%-18s ${r.outputKWh}%8.2f  ${r.status}"
    }
    (hdr :: sep :: rows).mkString("\n")
  }

  // ── Analysis summary ───────────────────────────────────────────────────────

  /** Format an AnalysisSummary for display */
  def analysisSummary(summary: AnalysisSummary): String = {
    val srcLabel = summary.source.map(s => s"  Source: $s\n").getOrElse("")
    s"""|$srcLabel  Count    : ${summary.count} readings
        |  Mean     : ${summary.mean} kWh
        |  Median   : ${summary.median} kWh
        |  Mode     : ${summary.mode} kWh
        |  Range    : ${summary.range} kWh
        |  Midrange : ${summary.midrange} kWh
        |  Min      : ${summary.min} kWh
        |  Max      : ${summary.max} kWh""".stripMargin
  }

  /** Format a map of per-source summaries */
  def analysisBySource(summaries: Map[EnergySource, AnalysisSummary]): String = {
    if (summaries.isEmpty) return "  (no data available)"
    summaries.toList.sortBy(_._1.toString).map { case (_, s) =>
      subHeader(s"${s.source.getOrElse("All sources")} Statistics") + "\n" + analysisSummary(s)
    }.mkString("\n")
  }

  // ── Alerts ─────────────────────────────────────────────────────────────────

  /** Format a single alert */
  def alert(a: Alert): String = {
    val icon = a.severity match {
      case Critical => "[!]"
      case Warning  => "[W]"
      case Info     => "[i]"
    }
    val ts   = a.timestamp.format(DateTimeFmt)
    val res  = if (a.resolved) " [RESOLVED]" else ""
    s"  $icon ${a.severity} | ${a.source} | $ts$res\n      ${a.message}"
  }

  /** Format a list of alerts */
  def alertsList(alerts: List[Alert]): String = {
    if (alerts.isEmpty) return "  No alerts — all systems nominal."
    alerts.map(alert).mkString("\n\n")
  }

  // ── Plant overview ─────────────────────────────────────────────────────────

  def plantOverview(
    readings:       List[EnergyReading],
    storageKWh:     Double,
    maxStorageKWh:  Double
  ): String = {
    val latestBySource = readings.groupBy(_.source).collect {
      case (src, rs) if rs.nonEmpty => src -> rs.maxBy(_.timestamp)
    }
    val totalProduced = readings.map(_.outputKWh).sum
    val storagePct    = if (maxStorageKWh > 0) storageKWh / maxStorageKWh * 100 else 0.0
    val storageBar    = progressBar(storagePct)

    val sourceRows = EnergySource.all.map { src =>
      latestBySource.get(src) match {
        case Some(r) =>
          f"  ${src.toString}%-6s  ${r.outputKWh}%8.2f kWh  [${r.status}]  (${r.timestamp.format(DateTimeFmt)})"
        case None =>
          f"  ${src.toString}%-6s  -- no data --"
      }
    }.mkString("\n")

    s"""  Total produced : ${f"$totalProduced%.2f"} kWh
       |  Storage        : ${f"$storageKWh%.2f"} / ${f"$maxStorageKWh%.2f"} kWh  (${"%.1f".format(storagePct)}%)
       |  Storage level  : $storageBar
       |
       |  Latest readings per source:
       |$sourceRows""".stripMargin
  }

  // ── Progress bar helper ────────────────────────────────────────────────────

  private def progressBar(pct: Double, width: Int = 30): String = {
    val filled = ((pct / 100.0) * width).toInt.min(width)
    val empty  = width - filled
    "[" + "█" * filled + "░" * empty + s"] ${"%.1f".format(pct)}%"
  }

  // ── Error / success helpers ────────────────────────────────────────────────

  def error(msg: String): String   = s"\n  [ERROR] $msg"
  def success(msg: String): String = s"\n  [OK]    $msg"
  def info(msg: String): String    = s"\n  [INFO]  $msg"

  def sep: String = SepLine
}
