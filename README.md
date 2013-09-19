# Bitlove (Web Interface)

The Bitlove web interface is built with Haskell using the Yesod framework. See [Bitlove.org](http://bitlove.org/) for the actual site.

The Yesod project has [a little book](http://www.yesodweb.com/book). For beginners, read [Real World Haskell](http://book.realworldhaskell.org/read/).

## Installation (for the less experienced)

You’ll need Haskell, Cabal, PostgreSQL and a UNIX OS. You might want to try the [Haskell Platform](http://hackage.haskell.org/platform/), available in most package managers.

Some commands need to be adjusted or left off. Start the installation by running the following ones:

    initdb /usr/local/var/postgres -E utf8
    pg_ctl -D /usr/local/var/postgres start
    createdb prittorrent

Next get the database dump in your project folder:

    https://spaceboyz.net/~astro/prittorrent-2012-08-27.sql.bz2
    bzip2 -d prittorrent-2012-08-27.sql.bz2

Open a shell with `psql prittorrent` and enter:

    CREATE USER prittorrent WITH SUPERUSER PASSWORD '1234';
    CREATE ROLE postgres;
    \i prittorrent-2012-08-27.sql
    \q
    rm prittorrent-2012-08-27.sql

Compile and run:

    cabal update && cabal install --only-dependencies
    yesod devel

Now point your browser to `http://localhost:8081/`.


## TODO

PBKDF2 auth
404 → search

* graphs
  * .torrents checkbox
  * time navigation

* PSHB

* global statistics numbers
* change feed URL
* Feed Links größer
* <itunes:new-feed-url/>
* "My feeds" -> clickable username
* User -> Shows -> Feeds
  * Default-Feed
* Subscribers zählen: Hash(IP + User-Agent) über 24h
* RFC5005: Feed Archiving
* Feeds: gzip
* OAuth für writing API (automatisch Feeds aus Podlove)
* Overwrite title: current title as template
* Filter: not only what is on current page
* de-ch choosable machen

* Per-item pages
* MapFeed content-type
* URL longener?
* Fix empty downloads.type
* filter.js:
  * Fix z-index (Android?)
* New {feeds,downloads}.{lang,summary,type} in:
  * Downloads Feeds
  * HTML
* enforce https for log in
* `<atom:link rel="self">`
* `<atom:link rel="alternate">`
* style:
  * Fonts
  * Flattr donate
* Edit user:
  * About field
* Edit feeds:
  * Add & fetch immediately
* graphs:
  * refactor
* more configurability
* Fetch & display feed summaries
* Feed summaries: X items, Y torrents
* OEmbed
* Installation: automation


http://bokardo.com/principles-of-user-interface-design/ ← 15
