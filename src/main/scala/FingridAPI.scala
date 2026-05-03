package reps.io

import reps.models._

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.util.Try

/**
 * Client for the Fingrid Open Data API (data.fingrid.fi).
 * Fetches real wind, hydro, and solar (forecast) production data for Finland.
 * Values are in MW (megawatts) as returned by Fingrid — stored in the outputKWh
 * field for compatibility with the existing model.
 *
 * Requires a free API key from https://data.fingrid.fi/en/instructions
 * Set it as the environment variable FINGRID_API_KEY, or enter it at the prompt 8.
 */
object FingridAPI {

  private val BaseUrl      = "https://data.fingrid.fi/api/datasets"
  private val WindDataset  = 181   // Wind power production, 3-min intervals, MW
  private val HydroDataset = 191   // Hydro power production, 3-min intervals, MW
  private val SolarDataset = 248   // Solar power forecast, 15-min intervals, MW

  private val client = HttpClient.newHttpClient()
  private val UtcFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'")
  private val IdFmt  = DateTimeFormatter.ofPattern("MMddHHmm")

  /** Read API key from environment variable, or return None. */
  def apiKeyFromEnv: Option[String] =
    Option(System.getenv("FINGRID_API_KEY")).filter(_.nonEmpty)

  /**
   * Fetch the last `hoursBack` hours of wind, hydro, and solar readings.
   * Returns Right(readings) or Left(error message).
   */
  def fetchAll(apiKey: String, hoursBack: Int = 24): Either[String, List[EnergyReading]] = {
    val now      = LocalDateTime.now(ZoneOffset.UTC)
    val start    = now.minusHours(hoursBack)
    val startStr = start.format(UtcFmt)
    val endStr   = now.format(UtcFmt)

    val sources = List(
      (WindDataset,  Wind:  EnergySource),
      (HydroDataset, Hydro: EnergySource),
      (SolarDataset, Solar: EnergySource)
    )

    sources.zipWithIndex.foldLeft[Either[String, List[EnergyReading]]](Right(List.empty)) {
      case (Left(err), _) => Left(err)
      case (Right(acc), ((datasetId, source), idx)) =>
        if (idx > 0) Thread.sleep(3000) // Fingrid rate limit: 10 req/min
        fetchDataset(apiKey, datasetId, source, startStr, endStr).map(acc ++ _)
    }
  }

  private def fetchDataset(
    apiKey:    String,
    datasetId: Int,
    source:    EnergySource,
    startTime: String,
    endTime:   String
  ): Either[String, List[EnergyReading]] = {
    val url = s"$BaseUrl/$datasetId/data" +
      s"?startTime=$startTime&endTime=$endTime&pageSize=10000&sortOrder=asc"

    val request = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("x-api-key", apiKey)
      .header("Accept", "application/json")
      .GET()
      .build()

    Try {
      val response = client.send(request, HttpResponse.BodyHandlers.ofString())
      val status   = response.statusCode()
      if (status == 401 || status == 403)
        Left("Invalid or missing Fingrid API key. Get one at https://data.fingrid.fi/en/instructions")
      else if (status != 200)
        Left(s"Fingrid API returned HTTP $status: ${response.body().take(300)}")
      else
        parseResponse(response.body(), source)
    }.toEither.left.map(e => s"Network error fetching ${source}: ${e.getMessage}").flatten
  }

  private def parseResponse(json: String, source: EnergySource): Either[String, List[EnergyReading]] =
    Try {
      val root = ujson.read(json)
      root("data").arr.toList.flatMap { item =>
        // Fingrid uses "startTime" (camelCase) in their v2 API
        val tsStr = Try(item("startTime").str).getOrElse(item("start_time").str)
        val value = item("value").num
        val ts    = parseTimestamp(tsStr)
        val status = if (value > 0) Operational else Offline
        val id     = s"FG-${source.toString.take(2).toUpperCase}-${ts.format(IdFmt)}"
        Some(EnergyReading(id = id, source = source, timestamp = ts, outputKWh = value, status = status))
      }
    }.toEither.left.map(e => s"Failed to parse Fingrid response: ${e.getMessage}")

  private def parseTimestamp(s: String): LocalDateTime = {
    // Strip trailing Z / milliseconds, then parse as local date-time
    val clean = s.replaceAll("\\.\\d+Z$", "").replaceAll("Z$", "")
    Try(LocalDateTime.parse(clean, DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      .getOrElse(LocalDateTime.parse(s.take(19), DateTimeFormatter.ISO_LOCAL_DATE_TIME))
  }
}
