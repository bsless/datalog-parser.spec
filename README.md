[![Clojars Project](https://img.shields.io/clojars/v/io.github.bsless/datalog-parser.spec.svg)](https://clojars.org/io.github.bsless/datalog-parser.spec)

# io.github.bsless/datalog-parser.spec

1-1 translation of datalog query spec based on the official Datomic documentation

- [Query](https://docs.datomic.com/on-prem/query/query.html)
- [Pull](https://docs.datomic.com/on-prem/query/pull.html)

NOTE: Legacy spec not implemented

Exists as a reference implementation for other parsers.

## Usage

```clojure
(require '[bsless.datalog-parser.spec :as spec]
         '[clojure.spec.alpha :as s])

(s/conform
 ::spec/query
 '[:find ?track-name ?minutes
   :in $ ?artist-name
   :where [?artist :artist/name ?artist-name]
   [?track :track/artists ?artist]
   [?track :track/duration ?millis]
   [(quot ?millis 60000) ?minutes]
   [?track :track/name ?track-name]])
```

## License

Copyright © 2022 Ben Sless

Distributed under the Eclipse Public License version 1.0.
