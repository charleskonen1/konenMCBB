# Get ESPN men's college basketball team directory

Retrieves all D1 men's college basketball teams from ESPN's API,
including ESPN team IDs, display names, abbreviations, and conference
membership. ESPN team IDs are required for many other ESPN API calls and
are not the same as Torvik or Sports Reference team names.

## Usage

``` r
espn_teams(limit = 1000L, timeout = 15)
```

## Arguments

- limit:

  Integer. Maximum teams to fetch per API page. ESPN returns up to 1000
  at once; default \`1000\` fetches everything in a single call.

- timeout:

  Numeric. Request timeout in seconds. Default \`15\`.

## Value

A tibble with one row per team, containing:

- team_id:

  ESPN team ID (character). Use this in ESPN API calls and to join
  against \`espn_game()\` box score output.

- display_name:

  Full team name (e.g. \`"Duke Blue Devils"\`).

- short_name:

  Short name (e.g. \`"Duke"\`).

- abbreviation:

  Abbreviation (e.g. \`"DUKE"\`).

- location:

  School location (e.g. \`"Duke"\`).

- nickname:

  Team nickname (e.g. \`"Blue Devils"\`).

- color:

  Primary brand color as hex (e.g. \`"003087"\`).

- alternate_color:

  Alternate brand color as hex.

- logo_url:

  URL of the team logo PNG.

- conference_id:

  ESPN conference ID.

- conference_name:

  Conference display name.

## Details

ESPN team IDs are stable identifiers used throughout the ESPN ecosystem
and in \`konenMCBB\`'s ESPN pipeline. For example, Duke's ESPN team ID
is \`"150"\`. Use \[espn_teams()\] to look up the ID for any school
before querying game-level data.

## See also

\[espn_scoreboard()\] for daily game listings, \[espn_game()\] to fetch
game data using ESPN game IDs.

## Examples

``` r
if (FALSE) { # \dontrun{
  teams <- espn_teams()

  # Find a specific team's ESPN ID
  teams |> dplyr::filter(grepl("Duke", display_name))

  # All ACC teams
  teams |> dplyr::filter(conference_name == "ACC")

  # Teams with their brand colors (useful for viz)
  teams |> dplyr::select(display_name, abbreviation, color, logo_url)
} # }
```
