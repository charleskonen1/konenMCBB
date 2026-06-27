# Set the ESPN local database path

Configures the root directory where \`konenMCBB\` stores and reads ESPN
game data. Set this once per session (or in your \`.Rprofile\`) before
calling any ESPN data functions.

## Usage

``` r
espn_set_db_path(path)
```

## Arguments

- path:

  Character. Full path to the ESPN database root. The package expects
  the structure \`\<path\>/\<season\>/\<YYYY-MM-DD\>/\<game_id\>/\`.

## Value

The path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Point to a folder on your machine
  espn_set_db_path("~/data/espn_db")

  # Or within a project
  espn_set_db_path(here::here("data/espn_db"))
} # }
```
