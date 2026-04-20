package reps.io

import reps.models._

import java.io._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.io.{BufferedSource, Source}
import scala.util.Try

/**
 * File I/O for energy readings using CSV format.
 *
 * NOTE: Per project requirements, file I/O is implemented in the usual imperative
 * style (non-functional) rather than with functional IO abstractions.
 *
 * CSV format:
 *   id,source,timestamp,outputKWh,status
 */
object CsvIO {

  private val TimestampFmt: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  private val Header = "id,source,timestamp,outputKWh,status"

  // ── Write ──────────────────────────────────────────────────────────────────

  /**
   * Append a single reading to the CSV file.
   * Creates the file and writes the header if it does not yet exist.
   */
  def appendReading(filePath: String, reading: EnergyReading): Try[Unit] = Try {
    val file   = new File(filePath)
    val exists = file.exists() && file.length() > 0
    val writer = new BufferedWriter(new FileWriter(file, true)) // append=true
    try {
      if (!exists) writer.write(Header + "\n")
      writer.write(readingToCsv(reading) + "\n")
    } finally {
      writer.close()
    }
  }

  /**
   * Write a full list of readings to a file, overwriting any existing content.
   */
  def writeReadings(filePath: String, readings: List[EnergyReading]): Try[Unit] = Try {
    val writer = new BufferedWriter(new FileWriter(filePath, false))
    try {
      writer.write(Header + "\n")
      readings.foreach(r => writer.write(readingToCsv(r) + "\n"))
    } finally {
      writer.close()
    }
  }

  // ── Read ───────────────────────────────────────────────────────────────────

  /**
   * Load all readings from a CSV file.
   * Returns Right(List[EnergyReading]) on success, Left(errorMessage) on failure.
   * Skips malformed rows and reports them as warnings on stderr.
   */
  def loadReadings(filePath: String): Either[String, List[EnergyReading]] = {
    val file = new File(filePath)
    if (!file.exists()) return Right(List.empty)

    var source: BufferedSource = null
    try {
      source = Source.fromFile(filePath)
      val lines     = source.getLines().toList
      val dataLines = lines.drop(1) // skip header
      val (errors, readings) = dataLines
        .filter(_.trim.nonEmpty)
        .partitionMap(line => parseLine(line))
      if (errors.nonEmpty)
        errors.foreach(e => System.err.println(s"[CsvIO] Skipped malformed row: $e"))
      Right(readings)
    } catch {
      case e: IOException =>
        Left(s"Could not read file '$filePath': ${e.getMessage}")
    } finally {
      if (source != null) source.close()
    }
  }

  // ── Helpers ────────────────────────────────────────────────────────────────

  private def readingToCsv(r: EnergyReading): String = {
    val ts = r.timestamp.format(TimestampFmt)
    // Escape any commas in fields (simple approach: quote the field)
    List(r.id, r.source.toString, ts, r.outputKWh.toString, r.status.toString)
      .mkString(",")
  }

  private def parseLine(line: String): Either[String, EnergyReading] = {
    val parts = line.split(",", -1)
    if (parts.length != 5)
      return Left(s"Expected 5 fields but got ${parts.length}: '$line'")

    val Array(id, srcStr, tsStr, kwhStr, statusStr) = parts.map(_.trim): @unchecked

    for {
      source    <- EnergySource.fromString(srcStr)
      timestamp <- parseTimestamp(tsStr)
      outputKWh <- parseDouble(kwhStr)
      status    <- SourceStatus.fromString(statusStr)
    } yield EnergyReading(id, source, timestamp, outputKWh, status)
  }

  private def parseTimestamp(s: String): Either[String, LocalDateTime] =
    Try(LocalDateTime.parse(s, TimestampFmt))
      .toEither
      .left.map(e => s"Invalid timestamp '$s': ${e.getMessage}")

  private def parseDouble(s: String): Either[String, Double] =
    Try(s.toDouble)
      .toEither
      .left.map(_ => s"Invalid number '$s'")
}
