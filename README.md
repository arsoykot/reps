# Renewable Energy Plant System (REPS)

A functional Scala implementation of a multi-source renewable energy plant monitoring system.

## Features

- **Monitor** Solar, Wind, and Hydro energy sources
- **Record** energy readings (persisted to CSV)
- **View** plant overview with live storage level
- **Filter** readings hourly, daily, weekly, and monthly
- **Analyse** with Mean, Median, Mode, Range, and Midrange statistics
- **Search** by keyword or output kWh range
- **Sort** by timestamp, output, or source
- **Alerts** for low output, equipment offline, and degraded status

## Prerequisites

- Java 11+
- [sbt](https://www.scala-sbt.org/) 1.9+

## Running

```bash
cd reps
sbt run
```

## Project Structure

```
src/main/scala/reps/
├── models/
│   ├── Models.scala     # EnergyReading, EnergySource, TimeFilter, AnalysisSummary …
│   └── Alerts.scala     # Alert, AlertSeverity, AlertThreshold
├── services/
│   ├── Validation.scala  # Functional input validation (Either-based)
│   ├── DataService.scala # Filtering, sorting, searching (pure functions)
│   ├── AlertService.scala# Alert detection (pure functions)
│   └── PlantMonitor.scala# Plant state management (immutable state threading)
├── analysis/
│   └── AnalysisService.scala  # Mean, Median, Mode, Range, Midrange + HOF composition
├── io/
│   └── CsvIO.scala      # CSV file I/O (imperative, as per requirements)
└── ui/
    ├── Display.scala     # Pure formatting utilities
    └── REPSApp.scala     # Main CLI application (recursive loop)
```

## Functional Programming Highlights

| Concept | Where |
|---|---|
| **Immutability** | `PlantState`, all model case classes, readings list |
| **Recursion** | `mainLoop` (@tailrec), `promptValidated` (@tailrec) |
| **Higher-order functions** | `filterBy`, `applyAll`, `analyseBySource` |
| **Error handling (Either)** | `Validation`, `CsvIO`, `DataService`, all menus |
| **Type parameterisation** | `StatFn` type alias, `filterBy[A]` |
| **Currying** | `aboveThreshold(threshold)(reading)` in AnalysisService |
| **Sealed traits** | `EnergySource`, `SourceStatus`, `TimeFilter`, `AlertSeverity` |
| **Pattern matching** | Exhaustive match on all sealed traits throughout |
| **For-comprehensions** | Menu handlers compose multiple Either validations |

## Data File

Readings are stored in `data/energy_readings.csv`. A sample file is provided.
Format: `id,source,timestamp,outputKWh,status`
