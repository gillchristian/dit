## Resources

- https://github.com/wakatara/harsh
- [sqlite-simple](https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple.html)

---

## TODO

- [x] Test parser with invalid `ditfile`s
- [x] Validate `ditfile`: no duplicate habits
- [x] Validate `ditfile`: formatting (extra spaces)
- [x] Expose functions to work with Ditfile
- [x] Event logs: entries and habit status (new, done, deleted, etc)
- [x] Move DB actions to a separate module?
- [x] Move Env code to Env.hs
- [x] Optimization: do not write "add" event twice
- [ ] Event processing: calculate state of the "system" (ie. habits status, streaks, etc).
- [ ] Revisit config: what paths to include? (ditfile, db)
- [ ] Add doc comments (Haddock)
