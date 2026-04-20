package reps.services

import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeParseException}

/**
 * Validation result — an Either-based approach for composable, functional error handling.
 * Left carries a user-friendly error message; Right carries the validated value.
 */
object Validation {

  val dateFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  /**
   * Validate a date string in DD/MM/YYYY format.
   * Returns Right[LocalDate] on success, Left[String] with a helpful error message on failure.
   */
  def validateDate(input: String): Either[String, LocalDate] = {
    val trimmed = input.trim
    if (trimmed.isEmpty) {
      Left("Date cannot be empty. Please enter a date in the format 'DD/MM/YYYY'.")
    } else {
      try {
        Right(LocalDate.parse(trimmed, dateFormatter))
      } catch {
        case _: DateTimeParseException =>
          Left(
            s"Invalid date format. Please enter the date in the format 'DD/MM/YYYY'.\n" +
            s"For example, enter '12/04/2024' for April 12, 2024.\n" +
            s"You entered: '$trimmed'"
          )
      }
    }
  }

  /**
   * Validate that a string can be parsed as a positive Double.
   */
  def validatePositiveDouble(input: String, fieldName: String): Either[String, Double] = {
    try {
      val v = input.trim.toDouble
      if (v >= 0) Right(v)
      else Left(s"$fieldName must be a non-negative number. You entered: $v")
    } catch {
      case _: NumberFormatException =>
        Left(s"$fieldName must be a valid number. You entered: '${input.trim}'")
    }
  }

  /**
   * Validate an integer in an inclusive range [min, max].
   */
  def validateIntInRange(input: String, fieldName: String, min: Int, max: Int): Either[String, Int] = {
    try {
      val v = input.trim.toInt
      if (v >= min && v <= max) Right(v)
      else Left(s"$fieldName must be between $min and $max. You entered: $v")
    } catch {
      case _: NumberFormatException =>
        Left(s"$fieldName must be a whole number. You entered: '${input.trim}'")
    }
  }

  /**
   * Validate that a non-empty string is provided.
   */
  def validateNonEmpty(input: String, fieldName: String): Either[String, String] = {
    val trimmed = input.trim
    if (trimmed.nonEmpty) Right(trimmed)
    else Left(s"$fieldName cannot be empty.")
  }

  /**
   * Validate a search query — must be non-empty and reasonably short.
   */
  def validateSearchQuery(input: String): Either[String, String] = {
    val trimmed = input.trim
    if (trimmed.isEmpty)          Left("Search query cannot be empty.")
    else if (trimmed.length > 200) Left(s"Search query too long (max 200 characters). You entered ${trimmed.length} characters.")
    else                           Right(trimmed)
  }

  /**
   * Validate a year value (1900–2100).
   */
  def validateYear(input: String): Either[String, Int] =
    validateIntInRange(input, "Year", 1900, 2100)

  /**
   * Validate a month value (1–12).
   */
  def validateMonth(input: String): Either[String, Int] =
    validateIntInRange(input, "Month", 1, 12)

  /**
   * Validate an hour value (0–23).
   */
  def validateHour(input: String): Either[String, Int] =
    validateIntInRange(input, "Hour", 0, 23)
}
