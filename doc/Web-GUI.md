# conexp-clj Web GUI

A browser-based GUI for conexp-clj, built on the JSON-RPC API (`conexp.api`).
ClojureScript (shadow-cljs + re-frame) renders the concept lattice as SVG; the
JVM does all FCA/layout computation. Ships as an installable PWA and works on
Android via the browser.

## For users: just `java -jar`

The web GUI is compiled into the uberjar, so end users need **only Java** — no
Node, no npm:

    java -jar conexp-clj-<version>-standalone.jar -a   # http://127.0.0.1:8080

Open http://127.0.0.1:8080 . To expose it on your network (e.g. to a phone),
start on all interfaces from a REPL — the default binds localhost because the
API has no authentication and a broad function whitelist:

    (require 'conexp.api)
    (conexp.api/start-server false 8080 "0.0.0.0")

If the jar was built without the frontend, `/` shows a "GUI not built" notice
(the API still works) rather than a blank page.

## Building a self-contained jar (needs Node, build-time only)

    make uberjar     # npm ci + shadow-cljs release + lein uberjar
                     # -> builds/uberjar/conexp-clj-<version>-standalone.jar

Node/npm are needed only here, by whoever builds the release — never by users.

### With nix

    nix build .#conexp-clj-with-gui    # self-contained jar incl. the web GUI
    nix build                          # API-only jar (GUI shows a "not built"
                                       # notice); this is what CI builds, so it
                                       # stays fast and independent of the frontend

The GUI is compiled by a fixed-output derivation (`conexp-clj-frontend`). Its
`outputHash` in `flake.nix` pins the deterministic `main.js`; if a frontend
dependency changes and the hash mismatches, `nix build .#conexp-clj-with-gui`
prints the correct value to paste into `flake.nix`.

## Develop (hot reload)

    npx shadow-cljs watch app       # SPA + hot reload on http://localhost:8280
    lein run -d                     # API with hot code reload on :8080

The dev SPA (:8280) calls the API on :8080; CORS is open for development.

## What it does (MVP)

- **Context editor:** toggle incidences, rename objects/attributes, add rows/columns.
- **Show Concept Lattice:** SVG Hasse diagram (attribute concepts = blue upper
  semicircle, object concepts = black lower semicircle; labels + valuation).
- **Layouts:** standard, inf-additive, layered, chain, DimDraw, Freese, force.
- **Valuations:** none, |extent|, |intent|, support, stability, separation,
  modularity, distributivity, probability.
- **Interaction:** drag nodes (order-clamped; move-modes single/ideal/filter/
  chain/inf/sup share `conexp.layouts.movement` with the JVM GUI), wheel zoom,
  background pan, right-click highlight.
- **Export:** SVG and layout JSON.

## Architecture

The browser is a thin client: it POSTs the existing JSON-RPC batch protocol
(see `doc/REST-API-usage.md`) and renders the `layout->json` payload. Layout and
valuation choices are single calls (`sh-lattice-layout`, `sh-layout-with-valuation`
in `conexp.api.shorthands`) since JSON can't pass Clojure functions. Node-drag
math is shared with the JVM via the pure `conexp.layouts.movement` (`.cljc`).

Frontend sources: `src/frontend/` (cljs), `src/shared/` (cljc), build config
`shadow-cljs.edn` / `package.json`, static shell `src/main/resources/public/`.

## Not yet included

Context-algebra toolbar, snapshots, tabbed workspace, attribute exploration and
the REPL (the last two need a stateful/WebSocket channel), and fully-offline
mobile compute (the layout algorithms are JVM/Java-backed).
