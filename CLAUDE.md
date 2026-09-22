# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

The SWI-Prolog website (https://www.swi-prolog.org), written entirely in SWI-Prolog
using `library(http/...)` and PlDoc. It serves the wiki content, the online manual,
the download area, the pack (add-on) registry, the blog and the examples collection.

Content lives in git **submodules** under `data/git/` (`www`, `blog`, `examples`),
plus the pack libraries under `packs/` (`recaptcha`, `smtp`, `googleclient`, `libssh`).
After cloning: `git submodule update --init`.

## Running

```bash
swipl load.pl -p 8080 -i        # interactive, as documented in README.md
./run                           # loop: swipl -s load.pl -g server, restarts unless halt(42)
swipl daemon.pl --no-fork --port=80 --user=www-data   # production (systemd/plweb.service)
swipl debug.pl                  # loads load.pl, starts server, enables debug/style checks
```

`load.pl` is the entry point: it attaches `packs/`, loads every server module, then
re-reads PlDoc comments for files loaded before the server started. `stop.` (in the
toplevel) halts with 42 so `./run` exits instead of restarting. If `library(ssh_server)`
is available, an SSH toplevel is started on port 2022 using keys from `data/private/etc/ssh/`.

Default port is the `http:port` setting (3040, see `parms.pl`).

Docker (production image): `cd docker && make image && make run`. The Dockerfile builds
swipl from git plus plweb; `make update-swipl` / `make update-plweb` bump the cache-busting
`ENV` lines to force a partial rebuild.

`scripts/fix-permissions` sets up the group-writable layout the server needs
(`data/log`, `data/pack`, `data/*.db`, the wiki repo). `scripts/install-custom` copies
`download-custom/**/*.txt` into `data/download/`. `scripts/sync-server` rsyncs the live
databases and download tree from the production host.

## Tests

There is no test suite. What exists:

- `test_plweb.pl` — `test_links/0` walks the menu in `page.pl` and HTTP-GETs every
  external link.
- `tests.pl` — an HTTP handler (`/Tests/chunked/data`) used to test *clients*, not this code.
- `test_recaptcha.pl` — `/test/recaptcha` form to check the reCAPTCHA keys.

Changes are normally verified by running the server locally and loading pages.
`make/0` (also exposed as `/make`, admin only) recompiles changed files in a running server.

## Data model

All dynamic data is `library(persistency)` files under `data/` (writable by the server
process). These are **not** in git; see README.md. Each module owns its store:

| File | Module | Content |
|---|---|---|
| `packs.db` | `pack.pl` | pack registry (registrations, mirrors, hashes) |
| `post.db` | `post.pl` | news items and comments |
| `openid.db` | `openid.pl` | site users, grants |
| `reviews.db` | `review.pl` | pack ratings/reviews |
| `tags.db` | `tagit.pl` | tags on objects |
| `annotations.db` | `annotateit.pl` | legacy comments (converter only) |
| `checksum.db` | `download.pl` | cached SHA256 of download files |

`parms.pl` defines `server/3`, which marks one host as `master`. Slaves sync their
`.db` files hourly (`db_sync_thread/1` in `update.pl`) and proxy mutating pack requests
to the master (`proxy_master/1` in `pack.pl`, via `proxy.pl`).

## Architecture

**Paths.** `parms.pl` is the central configuration: HTTP settings, `http:location/3`
aliases (`download`, `icons`, `css`, `jq`, `pldoc`), external URL shorthands
(`user:url_path/2`), `html_resource/2` declarations, and the `file_search_path/2`
chain — `data` → `git_data` → `document_root` (= `data/git/www`), `examples`, `blog`,
`private`, `log`, `download`. Nearly every other module reads files through these
aliases, so add new locations here rather than hard-coding paths.

**Page skin.** `page.pl` defines `user:body//2`, which wraps everything in the site
chrome (upper header with search + "Did you know", menu bar, footer, breadcrumb).
Pages select a skin by passing a *style term* to `reply_html_page/3`, e.g.
`wiki(Path, Title)`, `download(Dir, Title)`, `pack(Type, Title)`, `homepage`, `plain`.
Accepted styles are enumerated by `page_style/2` in `page.pl` — a style not listed there
renders as "Unknown page style". The site menu is the `menu/2` clause list in `page.pl`.
`footer.pl` renders the footer. PlDoc's own headers are suppressed via the
`prolog:doc_page_header//2` / `prolog:doc_links//2` hooks in `page.pl` and `customise.pl`.

**Wiki serving.** `plweb.pl` installs the catch-all handler on `root(.)`. `find_file/2`
maps a requested `foo.html` onto `foo.txt`/`.md` (PlDoc/markdown wiki source), `.frg`
(HTML fragment in content layout) or `.hom` (HTML fragment in homepage layout), falling
back to a plain file or directory index. `wiki.pl` converts wiki files to DOM, maintains
the page-title index (`index_wiki_pages/0`, started as a thread at server init) and
implements the `[[include]]` machinery. `wiki_edit.pl` serves `/wiki_edit` and
`/wiki_save`, writing the file back into the `www` submodule and running `git add`
(commits come from the site user's identity; production runs the wiki repo on its own
branch, see README.md).

**Packs.** `pack.pl` is the registry. `/pack/query` is the machine endpoint used by
`pack_install/1` in SWI-Prolog itself — it speaks `application/x-prolog` in and out,
so changes there affect every Prolog installation in the field. `pack_mirror.pl`
mirrors pack archives into `data/pack`, `pack_analyzer.pl` inspects an archive without
loading it (xref, dependencies), and `pack_info.pl` renders the results and runs
`update_pack_metadata_in_background/0` at server start. `review.pl` handles ratings.

**Downloads.** `download.pl` classifies files in `data/download/{stable,devel,old,daily}`
by parsing their names into platform/version terms, renders the tables, serves
`.sha256` sidecars, and resolves `.../latest...` to a `303 See Other`. Per-directory
prose comes from `header.txt` / `footer.txt` and per-platform notes from the
`download-custom/` tree.

**Blog and examples.** `blog.pl` and `examples.pl` render markdown from their
submodules. Both expose a pull endpoint (`/blog/pull`, `/examples/pull`) intended as a
GitHub webhook: `git pull` followed by re-indexing and, for the blog, a CDN purge via
`fastly.pl`.

**Users.** `openid.pl` is the login/profile/authorisation module (OpenID plus Google
OAuth via the `googleclient` pack; reCAPTCHA on registration). Admin-only handlers
check `site_user_property(User, granted(admin))` — see `make.pl` for the pattern.
`/update` instead uses HTTP basic auth against `data/private/passwd`.

**Other entry points.** `/man` (manual pages from the swipl installation, `plweb.pl`),
`/git` and `/cgi-bin` (`gitweb.pl` + `http_cgi.pl`, which runs external CGI scripts),
`/stats` and `/health` (`stats.pl`), `/ChangeLog` (`changelog.pl`, reads the swipl git
log), `/autocomplete/ac_predicate` (`autocomplete.pl`, feeds the search box),
`/doc_link` (`api.pl`), `/.well-known/` (`well_known.pl`).

**Thread pools.** `plweb.pl` declares pools `wiki`, `download`, `cgi` and `complete`;
handlers opt in with `spawn(Pool)`. `library(http/http_dyn_workers)` handles the rest.
`watchdog.pl` maps resource errors to HTTP status codes and broadcasts on overload.

**Settings/secrets.** Loaded at server init from `data/private/plweb.conf`
(`load_settings/1` in `plweb.pl:server_init`). Never commit that file. The Fastly API
key and reCAPTCHA keys live there.

## Conventions

- Use the licence header in `.fileheader` for new files (it is the template used by the
  editor's file-header command).
- Indentation is inconsistent across the tree by age: older modules (`page.pl`,
  `download.pl`, `wiki.pl`, `openid.pl`, `plweb.pl`, …) use tabs at 8-column stops;
  newer ones (`pack.pl`, `api.pl`, `blog.pl`, `fastly.pl`, `well_known.pl`) use 4 spaces
  and `%!` PlDoc comments. Match the file you are editing.
- The site's own source is documented with PlDoc and served by the site, so module and
  predicate comments are user-visible.
