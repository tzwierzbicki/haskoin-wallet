# Database

The schema of the SQLite database and the queries on it.

## Schema

- A column added to V5 reaches no database of an earlier build: the tables
  are made in a file without any only (`createDatabase` in `Migration.hs`).
  The memory `database-version` has the decisions on versions.
- A column that is missing raises no failure of SQLite when persistent reads
  it (`P.get`, `selectList`): SQLite takes the quoted name for a text. A
  number column then fails to read, which `otherColumns` of `Database.hs`
  answers as a database of another build. A text column reads as its own name.

## Queries

- A join inside an `exists` with no constant on its second table: SQLite may
  read it the other way round, the whole second table for each row. Pin the
  plan in `DatabaseSpec` (`queryPlanOf`), and nest a second `exists` in place
  of the join to settle the order, as `txKept` in `Database/Tag.hs` does.
