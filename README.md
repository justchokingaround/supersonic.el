# supersonic.el

[![CI](https://github.com/systemfreund/supersonic.el/actions/workflows/ci.yml/badge.svg)](https://github.com/systemfreund/supersonic.el/actions/workflows/ci.yml)
<!-- [![MELPA](https://melpa.org/packages/supersonic-badge.svg)](https://melpa.org/#/supersonic) -->

A music client for Emacs: browse your library's artists, albums,
album lists and podcasts, search it, and play it locally or on the
speakers around your home.

## Supported servers and playback

| Server (provider)                    | Playback backend     | Where the music plays                                    |
|--------------------------------------|----------------------|----------------------------------------------------------|
| [Subsonic](#subsonic)                | [mpv](#local-playback-with-mpv) (default) | locally, on the machine running Emacs |
|                                      | [Jukebox](#jukebox)  | the Subsonic server's own speakers                       |
|                                      | [UPnP/DLNA](#upnpdlna) | AV receivers, network speakers, smart TVs              |
| [Music Assistant](#music-assistant)  | [Music Assistant player](#playing-on-a-music-assistant-player) | any player Music Assistant has: speakers, Chromecast, AirPlay, Sonos, its web player |

*Subsonic* means any server implementing the Subsonic API, such as
[Navidrome](https://www.navidrome.org/),
[Nextcloud Music](https://apps.nextcloud.com/apps/music),
[Airsonic](https://airsonic.github.io/),
[Gonic](https://github.com/sentriz/gonic) or
[Ampache](https://ampache.org/).
[Music Assistant](https://www.music-assistant.io/) is supported from
server schema version 28 on.

Contents:

- [Installation](#installation)
- [Subsonic](#subsonic): [connection](#connecting-to-a-subsonic-server), [mpv](#local-playback-with-mpv), [jukebox](#jukebox), [UPnP/DLNA](#upnpdlna)
- [Music Assistant](#music-assistant): [connection](#connecting-to-music-assistant), [players](#playing-on-a-music-assistant-player)
- [Switching providers](#switching-providers)
- [Credentials in KeePassXC](#credentials-in-keepassxc-via-secret-service), [elsewhere](#credentials-from-elsewhere)
- [Usage](#usage): [play queue](#play-queue), [now playing](#now-playing), [cover art](#cover-art), [MPRIS](#mpris)

<img width="1590" height="982" alt="image" src="https://github.com/user-attachments/assets/4879a05f-e749-4a7a-9dd1-0420a9006648" />
<img width="825" height="998" alt="image" src="https://github.com/user-attachments/assets/44945d52-72d2-4825-9e31-d5faca1172ff" />

<img width="1445" height="1041" alt="image" src="https://github.com/user-attachments/assets/095571c2-0c8c-4c38-9fa8-d1ae1b5c10ec" />

Based on [subsonic.el](https://git.sr.ht/~amk/subsonic.el)

## Installation

~The package is available on melpa as `supersonic`~ (not available, yet. Please clone the repository instead.)

Example use-package config, for a Subsonic server played with mpv:

```elisp
(use-package supersonic
  :load-path "~/src/supersonic.el" ;; clone the repository here
  :commands supersonic
  :bind (("C-c m" . supersonic))
  :custom
  (supersonic-host "https://mysubsonicserver:4355") ;; see "Connecting to a Subsonic server"
  (supersonic-enable-scrobbling t))
```

The sections below show what to add for the other backends and for
Music Assistant. Use the `supersonic` command to open a transient
with commonly used commands available. For a list of available
configuration options check `customize-group supersonic`.

## Subsonic

The `subsonic` provider is built in and active by default.

### Connecting to a Subsonic server

Set `supersonic-host` to your server's URL. It may be given without a
scheme, in which case `https://` is assumed. In case you are running a
Subsonic server without HTTPS, prefix it with `http://` instead.

Then add a `~/.authinfo.gpg` or `~/.authinfo` file with the following
contents

    machine SUBSONIC_URL login USERNAME password PASSWORD

`SUBSONIC_URL` is the URL of your server, e.g.
`https://mysubsonicserver:4355` or `http://localhost:1234`.

Make sure the `machine` field in your authinfo entry matches `supersonic-host`
exactly, scheme included. To keep the credentials in KeePassXC
instead, see [below](#credentials-in-keepassxc-via-secret-service);
for anywhere else, see [here](#credentials-from-elsewhere).

### Local playback with mpv

The default backend: tracks play in a local [mpv](https://mpv.io/)
process, which needs to be installed. Nothing else to configure.
mpv plays for any provider that can name a URL to stream from, which
today is Subsonic.

### Jukebox

`supersonic-jukebox.el` plays back through the Subsonic server's own
remote jukebox instead of a local mpv process, so tracks play out of
the server's own speakers. It is not loaded or activated automatically,
and supersonic.el has no dependency on it. Enable it explicitly and
select it as the active backend:

```elisp
(use-package supersonic
  :custom
  (supersonic-playback-backend 'jukebox)
  :config
  (require 'supersonic-jukebox))
```

The jukebox only plays for the `subsonic` provider, since only a
Subsonic server has a jukebox to control. With any other provider,
playback commands report that the two do not fit, and
`supersonic-playback-switch-backend` does not offer `jukebox`.

### UPnP/DLNA

`supersonic-upnp.el` plays to UPnP/DLNA renderers: AV receivers,
network speakers, smart TVs. Like the jukebox, it is opt-in:

```elisp
(use-package supersonic
  :custom
  (supersonic-playback-backend 'upnp)
  :config
  (require 'supersonic-upnp))
```

or `M-x supersonic-playback-switch-backend` once it is loaded. Like
mpv, it plays for any provider that can name a URL to stream from.

To pick the renderer to play to, run
`M-x supersonic-upnp-select-renderer`. It searches your LAN for
renderers for a few seconds (`supersonic-upnp-discovery-timeout`) and
offers them by name. If a renderer does not show up, for example
because multicast does not reach it, enter the URL of its device
description instead; with a prefix argument (`C-u`), no search is
made at all. The choice is set as `supersonic-upnp-renderer`, and
saved to your `custom-file` only if you confirm.

Most renderers hold only one track at a time, so the play queue is
kept in Emacs, which polls the renderer every
`supersonic-upnp-poll-interval` seconds and gives it the next track
once one has ended. The queue therefore only moves on while Emacs
is running.

Renderers that can hold a next track (`SetNextAVTransportURI`) are
given it as soon as the current one plays, and move on to it
themselves, without waiting for a poll. Whether that is gapless is up
to the renderer: some, such as LG webOS TVs, still pause briefly
between tracks. Renderers without it are detected automatically and
keep the poll-driven advance.

A renderer fetches the audio itself, so it is handed the same stream
URL mpv would play, plus a cover art URL for its display. With the
`subsonic` provider, both URLs carry your Subsonic username and the
non-expiring token derived from your password. They are sent:

- to whichever device you select, without asking first,
- in plain text over your LAN, since UPnP control is unencrypted
  SOAP over HTTP,
- to a device that may log or cache them.

Anyone holding those URLs can use the Subsonic API as that user
until the password changes. For casting, use a dedicated Subsonic
user with as few privileges as possible (no admin, no settings or
upload rights).

The renderer also has to reach `supersonic-host` on its own: a
`localhost` address, a VPN-only address or a reverse proxy with its
own login will not work from the renderer. supersonic.el warns when the
stream URL points at `localhost`.

By default a renderer gets the same stream mpv would. Many cheap
renderers cannot decode FLAC or Opus; for those, have the server
transcode with `supersonic-upnp-stream-format`:

```elisp
(setq supersonic-upnp-stream-format '(:format "mp3" :max-bit-rate 320))
```

Before a renderer is given a track, supersonic.el asks the server
whether it can serve parts of the stream, which is what a renderer
needs to seek within it. A transcoded stream usually cannot be
seeked: its length is not known until it has been transcoded.
Navidrome, for one, serves it with seeking once it has been
transcoded before and kept in its transcoding cache.

## Music Assistant

`supersonic-music-assistant.el` browses a
[Music Assistant](https://www.music-assistant.io/) server instead of a
Subsonic one: artists, albums, album lists, search, podcasts and cover
art.

Music Assistant plays on its own players and gives a client no URL to
stream a whole track from, so neither mpv nor UPnP can play for it.
Its only backend is the [Music Assistant player](#playing-on-a-music-assistant-player).

### Connecting to Music Assistant

The provider is opt-in:

```elisp
(use-package supersonic
  :custom
  (supersonic-provider 'music-assistant)
  (supersonic-music-assistant-url "http://192.168.1.10:8095")
  :config
  (require 'supersonic-music-assistant))
```

It needs a long-lived token, created in Music Assistant's web UI under
your profile, and a server with schema version 28 or later (see the
server's `/info`). The token goes into your authinfo with the server's
URL as the `machine`, matching `supersonic-music-assistant-url`
exactly, as for Subsonic:

    machine http://192.168.1.10:8095 password TOKEN

To keep the token in KeePassXC instead, see
[below](#credentials-in-keepassxc-via-secret-service); for anywhere
else, see [here](#credentials-from-elsewhere).

It is sent in a request header only, never in a URL. Cover art comes
from the server's image proxy, which needs no token, so none is sent
with it. A server older than schema 31 does not tell its clients how
to reach images through the proxy; there, only images with a public
URL, such as podcast covers, are shown.

Podcasts are whatever podcasts are in the server's library; subscribe
to one in Music Assistant itself, since it has no equivalent of adding
a feed by URL or downloading an episode. The transient hides those two
commands while Music Assistant is the active provider.

### Playing on a Music Assistant player

`supersonic-music-assistant-player.el` plays on one of Music
Assistant's players -- a speaker, a Chromecast, an AirPlay device, the
web player in your browser -- the way the jukebox plays on a Subsonic
server. It loads the provider too, so this replaces the `require`
above:

```elisp
(use-package supersonic
  :custom
  (supersonic-provider 'music-assistant)
  (supersonic-music-assistant-url "http://192.168.1.10:8095")
  (supersonic-playback-backend 'music-assistant)
  :config
  (require 'supersonic-music-assistant-player))
```

Pick the player with `M-x supersonic-music-assistant-select-player`,
which offers the server's players by name. The choice is set as
`supersonic-music-assistant-player`, and saved for future sessions if
you say so. Playing, enqueueing and the transport commands then drive
that player's queue. The backend polls the queue every
`supersonic-music-assistant-poll-interval` seconds, so the queue
buffer, now-playing and MPRIS follow it, changes made from other Music
Assistant clients included. As in Music Assistant's own clients,
going to the previous track more than a few seconds into one starts
that one over. Music Assistant keeps its own play history, so nothing
is scrobbled.

## Local files

`supersonic-local.el` adds a `local` provider: the music files under
`supersonic-local-directories`, played through mpv with no server --
for when it is unreachable, or for music it does not have.

```elisp
(use-package supersonic
  :config
  (require 'supersonic-local)
  (setq supersonic-local-directories '("~/music/")
        ;; Leave out Syncthing's conflict copies, say:
        supersonic-local-ignore-regexp "\\.sync-conflict-"))
```

Tags are read with `ffprobe`, so FFmpeg must be installed.  The index
is cached in `supersonic-cache-path`; only new or changed files are
read again, in the background, once a session or on
`supersonic-local-rescan`.  Tags missing are guessed from an
`Artist/Album (Year)/NN - Title` layout, artists and albums whose tags
differ only in case are one, cover art is a `cover.jpg`/`folder.jpg`
next to the files or else the art embedded in them, and lyrics are an
`.lrc` file named like the track or else its lyrics tag.  "Recent
albums" are those played from the local files.

`supersonic-local-toggle` switches to the local files and back to the
provider before.  While `supersonic-local-offer-fallback` is non-nil, a
request failing because the server cannot be reached offers that
switch.  Like any provider switch, it stops playback.

## Switching providers

With Music Assistant loaded alongside the built-in Subsonic provider,
`M-x supersonic-provider-switch` (`P` in the `supersonic` transient)
switches between them. Playback stops. If the current playback backend
cannot play for the new provider, the backend switches too: to the
only one that can, or to the one you last used with that provider. It
only asks when neither settles it, and declining then switches
nothing. The open artist, album, track, search and podcast buffers
belong to the old provider, so they are closed. The transient hides
what the active provider cannot do.

`M-x supersonic-playback-switch-backend` (`k` in the transient)
switches the backend alone, offering only those that can play for the
active provider.

## Credentials in KeePassXC via Secret Service

If you'd rather keep the Subsonic password or the Music Assistant
token in KeePassXC than in an authinfo file, enable *Secret Service
Integration* under `Tools -> Settings -> Secret Service Integration`
and unlock the database. Then [tell](https://www.gnu.org/software/emacs/manual/html_node/auth/Secret-Service-API.html#Secret-Service-API-1) Emacs to also search that collection

`auth-source`'s Secret Service backend only matches on an entry's
custom *Attributes*, not on its regular URL/username fields, so add
these on the `Advanced` tab of the entry:

- `host` set to the same value as `supersonic-host` (with scheme, e.g.
  `https://coolsupersonic.example.com`), or as
  `supersonic-music-assistant-url` for a Music Assistant token
- `user` set to your Subsonic username (not needed for Music
  Assistant)

The entry's regular password field is used as the secret.

## Credentials from elsewhere

If auth-source cannot find the password by the server's URL -- say it
lives in a [pass](https://www.passwordstore.org/) entry that other
programs use too, under a name of its own -- tell supersonic.el how to
get it instead. `supersonic-auth-function` returns the Subsonic user
name and password, `supersonic-music-assistant-token-function` the
Music Assistant token:

```elisp
;; Subsonic
(setq supersonic-auth-function
      (lambda (_host)
        (list :user "USERNAME"
              :secret (auth-source-pass-get 'secret "my/custom_entry"))))

;; Music Assistant
(setq supersonic-music-assistant-token-function
      (lambda (_server) (auth-source-pass-get 'secret "my/music_assistant")))
```

Each is called with the server's URL, so one function can serve
several servers. The secret may also be a function returning it.

By default neither result is kept: the Subsonic function is called
for every URL built -- each track queued, each cover shown -- and the
Music Assistant one once per operation, and also on every poll of the
player's state while it is the backend, every
`supersonic-music-assistant-poll-interval` seconds. That is fine for
something cheap, such as reading a plain file, but
`auth-source-pass-get` decrypts the entry every time. To keep what it
found for a while, wrap the function in `supersonic-cached`:

```elisp
;; Subsonic
(setq supersonic-auth-function
      (supersonic-cached
       (lambda (_host)
         (list :user "USERNAME"
               :secret (auth-source-pass-get 'secret "my/custom_entry")))))

;; Music Assistant
(setq supersonic-music-assistant-token-function
      (supersonic-cached
       (lambda (_server) (auth-source-pass-get 'secret "my/music_assistant"))))
```

What it found is kept for each server for
`supersonic-credentials-cache-expiry` seconds, an hour by default.
A server rejecting the password or token drops what was kept for it,
so trying again asks afresh; `M-x supersonic-forget-credentials` drops
everything by hand, e.g. right after changing a password. Only a
rejection of a request supersonic.el reads itself counts: a Subsonic
stream mpv fails to play, or a cover that does not load, drops
nothing until the next list or search is rejected too.

## Usage

Everything below works the same with every provider and backend.

### Keys

`supersonic-command-map` holds the commands used most, for binding to
a prefix key of your own:

```elisp
(global-set-key (kbd "C-c m") 'supersonic-command-map)
```

| Key | Command |
|-----|---------|
| `SPC` / `n` / `p` | play/pause, next, previous |
| `f` / `b` | seek forward/back |
| `+` `=` / `-` | volume up/down |
| `z` / `R` | toggle shuffle, cycle repeat (off, queue, track) |
| `s` / `S` | search in the minibuffer / in a list buffer |
| `o` / `q` / `l` | now playing, play queue, lyrics |
| `a` `r` `e` `w` | artists, random, recent, newest albums |
| `C` | clear the play queue but the current track |
| `O` | switch to local files and back, see [Local files](#local-files) |
| `m` | the `supersonic` transient |

Every supersonic buffer shares the playback keys `SPC`, `<` `>`
(previous/next), `[` `]` (seek), `+` `-`, `z`, `R`, `/` (search),
`N` (now playing), `Q` (queue), `L` (lyrics) and `M` (transient).

`supersonic-search-read` searches and lets you pick a result with
`completing-read`, grouped into artists, albums and songs: a song is
played, an album or artist opened.  With `C-u`, a song or album is
added to the play queue instead.

### Play queue

In the tracks and albums buffers, `RET` replaces the current play
queue and starts playing immediately, while `a` adds to the play
queue instead, so that e.g. two albums can be queued up to play back
to back:

- Tracks buffer: `RET`/`a` act on the current track and every track
  after it in the buffer.
- Albums buffer: `RET` opens the album's track list; `a` adds the
  whole album to the play queue directly, without opening it.

The play queue buffer (`supersonic-show-queue`) edits the queue:
`RET` plays the entry at point, `d`/`DEL` removes it, `M-p`/`M-n`
(or `M-<up>`/`M-<down>`) move it, and `C` clears the queue but for
the track playing.  Shuffle, repeat, and editing the queue are up to
the backend; mpv does all of them.

### Now playing

`N` in the `supersonic` transient (`supersonic-show-now-playing`) opens
a buffer for the track currently playing: cover art, title, artist,
album, format, duration and size, plus clickable playback controls.

Keys in that buffer: `SPC` play/pause, `n`/`p` next/previous track,
`f`/`b` seek, `g` refresh manually.

By default, the title (optionally rotating through title/artist/album
via `supersonic-now-playing-cycle-fields`) and a clickable waveform
seekbar (`supersonic-enable-waveform`) are both layered onto the cover
art. Which rows exist, what layers onto the art vs. gets its own row,
and what info shows below the buttons, is all controlled by a handful
of function-list variables -- `supersonic-now-playing-layout-functions`,
`supersonic-now-playing-animation-functions`, `supersonic-art-overlay-layers`
and `supersonic-now-playing-render-functions` -- see their docstrings
(`C-h v`) for the specifics. For example, to show the waveform as its
own row below the cover art instead of layered onto it:

```elisp
;; Rows, top to bottom: art, waveform, label, buttons.
(setq supersonic-now-playing-layout-functions
      (list #'supersonic-now-playing-layout-art
            #'supersonic-now-playing-layout-waveform
            #'supersonic-now-playing-layout-label
            #'supersonic-now-playing-layout-buttons))
;; Take the lane back out of the art overlay. `supersonic-art-overlay-layers'
;; is what `supersonic-now-playing-animate-art-overlay' draws with;
;; `-scroll-layers' is the scrolling variant's own list, so drop it from
;; whichever of the two `supersonic-now-playing-animation-functions' uses.
(setq supersonic-art-overlay-layers
      (remove #'supersonic-art-overlay-layer-waveform supersonic-art-overlay-layers))
```

The functions in `supersonic-now-playing-layout-functions` and
`supersonic-now-playing-render-functions` are called with the buffer and
the track playing, as a plist -- `:title`, `:artist`, `:album`,
`:duration`, `:art`, `:suffix`, `:content-type`, `:size` and so on,
whichever of them the library knows. A row of your own reads it with `plist-get`:

```elisp
(add-to-list 'supersonic-now-playing-render-functions
             (lambda (_buff song)
               (when-let ((genre (plist-get song :genre)))
                 (insert (format "%-10s%s\n" "Genre:" genre))))
             t)
```

By default the now-playing buffer opens in the selected window, replacing
whatever was there. To keep it pinned in its own window instead, e.g. to
browse albums or tracks alongside it as in the screenshot above, put it
in a side window via `display-buffer-alist`:

```elisp
(add-to-list 'display-buffer-alist
             `(,supersonic-now-playing-buffer-name
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.3)))
```

`N`/`supersonic-show-now-playing` then always opens that buffer on the
right, leaving whichever list buffer you had open (artist albums, search
results, ...) in place on the left.

**Migrating custom functions:** before, these functions were handed the
raw Subsonic `getSong` alist. They now get the plist above instead, so
`(assoc-default "title" song)` becomes `(plist-get song :title)`, and
the camel-cased keys become keywords: `"coverArt"` is `:art`,
`"contentType"` is `:content-type`.

### Cover art

Cover art is on by default (`supersonic-enable-art`) and needs a
graphical frame to draw it in. The artist list shows the artist image
the server has for each artist (e.g. what Navidrome or Music Assistant
fetched for it), fetching only the rows in view as you scroll. Its size
is set per view: `supersonic-artist-art-size` for the artist list (`nil`
turns its images off), `supersonic-list-art-size` for the album and
podcast lists, `supersonic-now-playing-art-size` for
the now-playing buffer.

### Lyrics

`supersonic-lyrics` shows the lyrics of the track playing.  Synced
lyrics have the line being sung highlighted and kept centred; `RET`
or a click on a line plays from there, `(`/`)` shift the lyrics a
quarter second later/sooner should they run off, `f` toggles
following playback and `g` looks the lyrics up again.

It turns on `supersonic-lyrics-follow-mode`, which looks lyrics up
for every track played -- from the server first (Subsonic: the
OpenSubsonic `getLyricsBySongId`, serving lyrics embedded in the files
and `.lrc` files next to them), then, if the server has none synced,
from [LRCLIB](https://lrclib.net) -- and also shows the line being sung
in the now-playing buffer (`supersonic-lyrics-in-now-playing`) and, if
you like, the echo area:

```elisp
(setq supersonic-lyrics-lrclib t        ; sends artist/title/album/duration to lrclib.net
      supersonic-lyrics-echo-area t)
(supersonic-lyrics-follow-mode 1)
```

For a mode line of your own, `supersonic-lyrics-line` holds the line
being sung and `supersonic-lyrics-line-change-hook` runs as it changes.

### Mode line

`supersonic-mode-line-mode` shows the track playing in the mode line
(through `global-mode-string`), with a marker while it is paused.
`mouse-1` on it opens now playing and `mouse-3` toggles playback.  If
your mode line does not show `global-mode-string`, put
`supersonic-mode-line-string` into it yourself.

### MPRIS

`supersonic-mpris.el` exposes supersonic.el's playback as an MPRIS
player on the D-Bus session bus, so desktop environments and tools such
as `playerctl` can see and control it. It is not loaded or activated
automatically, and supersonic.el has no dependency on it. Enable it
explicitly:

```elisp
(use-package supersonic
  :config
  (require 'supersonic-mpris)
  (supersonic-mpris-mode t))
```
