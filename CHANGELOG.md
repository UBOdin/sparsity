# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Multi-dialect support for SQL parsing:
  - Oracle SQL dialect
  - PostgreSQL dialect
  - ANSI SQL dialect
- CLI application (`Main.scala`) with comprehensive options:
  - Multiple output formats: text, JSON, JQ queries
  - File pattern matching (glob support)
  - Configurable output destinations (console or file)
- Error recovery for unparseable statements
- Circe-based JSON encoding for all AST types
- Assembly plugin support for building standalone JARs
- Comprehensive test suite with Oracle-specific tests
- Support for parsing multiple statements from a single file with accurate line number tracking

### Changed
- **BREAKING**: Major refactoring of package structure:
  - Moved core types to `sparsity.common.*` package
  - Parser base classes now in `sparsity.common.parser.*`
  - Statement types in `sparsity.common.statement.*`
  - Expression types in `sparsity.common.expression.*`
- Refactored parser architecture:
  - Moved logic from `Elements` into `SQLBase` for better subclass extensibility
  - Created dialect-specific parser objects
- Enhanced build configuration:
  - Updated SBT to 1.11.7
  - Cross-compilation for Scala 2.12, 2.13, and 3.3
  - Added assembly, scalafmt, and cross-project plugins
- Updated dependencies:
  - fastparse to 3.1.1
  - Added scribe for logging
  - Added mainargs for CLI argument parsing
  - Added monocle for optics
  - Added java-jq for JQ query support

### Fixed
- Improved error messages with better context
- Better handling of parsing failures with fallback mechanisms

## [1.7.0] - Previous Release

### Added
- Cross-compiled support for Scala 2.12, 2.13, and 3.3
- Updated fastparse dependency

[Unreleased]: https://github.com/UBOdin/sparsity/compare/v1.7.0...HEAD
[1.7.0]: https://github.com/UBOdin/sparsity/releases/tag/v1.7.0
