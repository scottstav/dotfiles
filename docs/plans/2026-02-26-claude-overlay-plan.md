# Claude Overlay Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a C program that renders streaming text from claude-ask as a fast Wayland overlay using wlr-layer-shell + Cairo/Pango.

**Architecture:** A long-lived process listens on a Unix domain socket for JSON-line commands. When text arrives, it creates/updates a wlr-layer-shell surface at top-center, renders text with Pango/Cairo, and manages growth/scroll/fade animations via timerfd. The overlay drops down visually from the claude-ask waybar module.

**Tech Stack:** C, Meson, wayland-client, wlr-layer-shell-unstable-v1, Cairo, Pango, json-c, Unix domain sockets, poll(), timerfd.

**Reference implementation:** wob (github.com/francma/wob) — same stack minus text rendering.

**Design doc:** `docs/plans/2026-02-26-claude-overlay-design.md`

---

## Task 1: Project Scaffold + Build System

**Files:**
- Create: `.local/share/claude-overlay/meson.build`
- Create: `.local/share/claude-overlay/protocols/wlr-layer-shell-unstable-v1.xml`
- Create: `.local/share/claude-overlay/src/main.c`

**Step 1: Install build dependency**

Run: `yay -S --needed wlr-protocols`

Verify: `ls /usr/share/wlr-protocols/unstable/wlr-layer-shell-unstable-v1.xml`

**Step 2: Create directory structure**

```bash
mkdir -p ~/.local/share/claude-overlay/src
mkdir -p ~/.local/share/claude-overlay/protocols
```

Wait — these should be in the dotfiles repo:

```bash
mkdir -p /home/ifit/dotfiles/.local/share/claude-overlay/src
mkdir -p /home/ifit/dotfiles/.local/share/claude-overlay/protocols
cd /home/ifit/dotfiles && stow .
```

**Step 3: Copy protocol XML**

Copy from system into the project (bundled, like wob does):

```bash
cp /usr/share/wlr-protocols/unstable/wlr-layer-shell-unstable-v1.xml \
   /home/ifit/dotfiles/.local/share/claude-overlay/protocols/
```

**Step 4: Write meson.build**

```meson
project('claude-overlay', 'c',
  version: '0.1.0',
  default_options: ['c_std=c11', 'warning_level=2'],
)

cc = meson.get_compiler('c')

# Dependencies
dep_wayland_client = dependency('wayland-client')
dep_wayland_protocols = dependency('wayland-protocols')
dep_cairo = dependency('cairo')
dep_pangocairo = dependency('pangocairo')
dep_json_c = dependency('json-c')
dep_rt = cc.find_library('rt')

# Wayland scanner for protocol code generation
wayland_scanner = find_program('wayland-scanner', native: true)

wayland_scanner_code = generator(wayland_scanner,
  output: '@BASENAME@-protocol.c',
  arguments: ['private-code', '@INPUT@', '@OUTPUT@'],
)

wayland_scanner_header = generator(wayland_scanner,
  output: '@BASENAME@-protocol.h',
  arguments: ['client-header', '@INPUT@', '@OUTPUT@'],
)

# Protocol sources
protocol_dir = meson.current_source_dir() / 'protocols'

protocols = [
  protocol_dir / 'wlr-layer-shell-unstable-v1.xml',
]

# Also need xdg-shell from wayland-protocols
wl_protocol_dir = dep_wayland_protocols.get_variable('pkgdatadir')
protocols += [
  wl_protocol_dir / 'stable/xdg-shell/xdg-shell.xml',
]

src_protocols = []
foreach p : protocols
  src_protocols += wayland_scanner_code.process(p)
  src_protocols += wayland_scanner_header.process(p)
endforeach

# Main executable
executable('claude-overlay',
  'src/main.c',
  src_protocols,
  dependencies: [
    dep_wayland_client,
    dep_cairo,
    dep_pangocairo,
    dep_json_c,
    dep_rt,
  ],
  install: true,
)
```

**Step 5: Write minimal main.c**

```c
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    printf("claude-overlay v0.1.0\n");
    return EXIT_SUCCESS;
}
```

**Step 6: Build**

```bash
cd ~/.local/share/claude-overlay
meson setup build
ninja -C build
```

Expected: Compiles successfully. Protocol headers generated in build dir.

**Step 7: Run**

```bash
./build/claude-overlay
```

Expected: Prints `claude-overlay v0.1.0` and exits.

**Step 8: Commit**

```bash
cd /home/ifit/dotfiles
git add .local/share/claude-overlay/
git commit -m "scaffold claude-overlay: meson build, protocol generation, minimal main"
```

---

## Task 2: Wayland Connection + Layer-Shell Surface

**Files:**
- Create: `.local/share/claude-overlay/src/wayland.h`
- Create: `.local/share/claude-overlay/src/wayland.c`
- Create: `.local/share/claude-overlay/src/shm.h`
- Create: `.local/share/claude-overlay/src/shm.c`
- Modify: `.local/share/claude-overlay/src/main.c`
- Modify: `.local/share/claude-overlay/meson.build` (add source files)

This task gets a blank colored rectangle on screen via layer-shell. This is the
Wayland boilerplate task — the trickiest part to get right.

**Step 1: Write shm.h/shm.c — shared memory buffer allocation**

Pattern from wob. Allocates anonymous shared memory for the pixel buffer that
gets shared with the compositor.

```c
// shm.h
#ifndef SHM_H
#define SHM_H

#include <stddef.h>
#include <stdint.h>

// Create anonymous shm fd, mmap buffer. Returns pointer to pixel data.
// Caller gets fd back via out_fd (needed for wl_shm_pool).
uint32_t *shm_alloc(size_t size, int *out_fd);

#endif
```

Implementation:
- Use `shm_open()` with `O_RDWR | O_CREAT | O_EXCL`, try names `/claude-overlay-0` through `-255`
- `shm_unlink()` immediately (fd keeps it alive)
- `ftruncate()` to requested size
- `mmap()` with `PROT_READ | PROT_WRITE | MAP_SHARED`

**Step 2: Write wayland.h — core structures and API**

```c
// wayland.h
#ifndef WAYLAND_H
#define WAYLAND_H

#include <stdbool.h>
#include <stdint.h>
#include <wayland-client.h>
#include "wlr-layer-shell-unstable-v1-protocol.h"

struct overlay_state {
    // Wayland globals
    struct wl_display *display;
    struct wl_registry *registry;
    struct wl_compositor *compositor;
    struct wl_shm *shm;
    struct zwlr_layer_shell_v1 *layer_shell;

    // Surface
    struct wl_surface *surface;
    struct zwlr_layer_surface_v1 *layer_surface;
    struct wl_buffer *buffer;

    // Pixel buffer
    uint32_t *pixels;
    int shm_fd;

    // Dimensions
    uint32_t width;
    uint32_t height;
    uint32_t configured_width;
    uint32_t configured_height;

    // State flags
    bool surface_visible;
    bool configured;
    bool closed;
    bool needs_redraw;
};

// Connect to Wayland display, bind globals. Returns false on failure.
bool wayland_init(struct overlay_state *state);

// Create and show the layer-shell surface with given dimensions.
bool wayland_create_surface(struct overlay_state *state,
                            uint32_t width, uint32_t height);

// Destroy the surface (hide overlay).
void wayland_destroy_surface(struct overlay_state *state);

// Allocate/reallocate the shm buffer for current dimensions.
bool wayland_alloc_buffer(struct overlay_state *state);

// Attach buffer and commit (display the rendered frame).
void wayland_commit(struct overlay_state *state);

// Clean up everything.
void wayland_cleanup(struct overlay_state *state);

// Get the display fd for poll().
int wayland_get_fd(struct overlay_state *state);

// Dispatch pending events. Returns false if display disconnected.
bool wayland_dispatch(struct overlay_state *state);

#endif
```

**Step 3: Write wayland.c**

Key implementation details:

**Registry listener** — bind `wl_compositor`, `wl_shm`, `zwlr_layer_shell_v1`:
```c
static void registry_handle_global(void *data, struct wl_registry *registry,
                                   uint32_t name, const char *interface,
                                   uint32_t version)
{
    struct overlay_state *state = data;
    if (strcmp(interface, wl_compositor_interface.name) == 0)
        state->compositor = wl_registry_bind(registry, name,
                                             &wl_compositor_interface, 4);
    else if (strcmp(interface, wl_shm_interface.name) == 0)
        state->shm = wl_registry_bind(registry, name,
                                      &wl_shm_interface, 1);
    else if (strcmp(interface, zwlr_layer_shell_v1_interface.name) == 0)
        state->layer_shell = wl_registry_bind(registry, name,
                                              &zwlr_layer_shell_v1_interface, 1);
}
```

**Layer surface creation** — anchor top, centered:
```c
bool wayland_create_surface(struct overlay_state *state,
                            uint32_t width, uint32_t height)
{
    state->width = width;
    state->height = height;
    state->surface = wl_compositor_create_surface(state->compositor);

    state->layer_surface = zwlr_layer_shell_v1_get_layer_surface(
        state->layer_shell,
        state->surface,
        NULL,  // NULL = default output
        ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY,
        "claude-overlay");

    // Anchor to top edge only — this centers horizontally
    zwlr_layer_surface_v1_set_anchor(state->layer_surface,
        ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP);

    // No exclusive zone — don't push other surfaces
    zwlr_layer_surface_v1_set_exclusive_zone(state->layer_surface, -1);

    // Margin below waybar
    zwlr_layer_surface_v1_set_margin(state->layer_surface,
        8, 0, 0, 0);  // top margin (configurable later)

    zwlr_layer_surface_v1_set_size(state->layer_surface, width, height);

    zwlr_layer_surface_v1_add_listener(state->layer_surface,
        &layer_surface_listener, state);

    wl_surface_commit(state->surface);
    return true;
}
```

**Layer surface configure callback** — compositor tells us the actual size:
```c
static void layer_surface_configure(void *data,
                                    struct zwlr_layer_surface_v1 *surface,
                                    uint32_t serial,
                                    uint32_t width, uint32_t height)
{
    struct overlay_state *state = data;
    state->configured_width = width;
    state->configured_height = height;
    state->configured = true;
    state->needs_redraw = true;
    zwlr_layer_surface_v1_ack_configure(surface, serial);
}
```

**Step 4: Update main.c — simple event loop with colored rectangle**

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <poll.h>
#include "wayland.h"

static volatile sig_atomic_t quit = 0;

static void handle_signal(int sig)
{
    (void)sig;
    quit = 1;
}

int main(int argc, char *argv[])
{
    (void)argc; (void)argv;

    struct overlay_state state = {0};

    signal(SIGINT, handle_signal);
    signal(SIGTERM, handle_signal);

    if (!wayland_init(&state)) {
        fprintf(stderr, "Failed to initialize Wayland\n");
        return EXIT_FAILURE;
    }

    // Show a 600x200 test surface
    if (!wayland_create_surface(&state, 600, 200)) {
        fprintf(stderr, "Failed to create surface\n");
        return EXIT_FAILURE;
    }

    struct pollfd fds[1] = {
        { .fd = wayland_get_fd(&state), .events = POLLIN },
    };

    while (!quit && !state.closed) {
        if (state.needs_redraw && state.configured) {
            wayland_alloc_buffer(&state);

            // Fill with a dark semi-transparent color (ARGB premultiplied)
            uint32_t color = 0xe61a1a2e;  // ~90% opacity, dark blue
            for (uint32_t i = 0;
                 i < state.configured_width * state.configured_height; i++)
                state.pixels[i] = color;

            wayland_commit(&state);
            state.needs_redraw = false;
        }

        wl_display_flush(state.display);
        poll(fds, 1, -1);
        wayland_dispatch(&state);
    }

    wayland_cleanup(&state);
    return EXIT_SUCCESS;
}
```

**Step 5: Update meson.build — add new source files**

Add `src/wayland.c` and `src/shm.c` to the executable sources list.

**Step 6: Build and test**

```bash
cd ~/.local/share/claude-overlay && ninja -C build
./build/claude-overlay
```

Expected: A dark semi-transparent rectangle appears at top-center of screen,
below waybar. Ctrl+C exits. If it doesn't appear, check:
- Is `WAYLAND_DISPLAY` set?
- Does `zwlr_layer_shell_v1` bind succeed? (add fprintf debug)

**Step 7: Commit**

```bash
cd /home/ifit/dotfiles
git add .local/share/claude-overlay/
git commit -m "add wayland connection and layer-shell surface rendering"
```

---

## Task 3: Config File Parsing

**Files:**
- Create: `.local/share/claude-overlay/src/config.h`
- Create: `.local/share/claude-overlay/src/config.c`
- Create: `.config/claude-overlay/config`
- Modify: `.local/share/claude-overlay/src/main.c` (load config)
- Modify: `.local/share/claude-overlay/meson.build` (add config.c)

**Step 1: Write config.h**

```c
#ifndef CONFIG_H
#define CONFIG_H

#include <stdint.h>

struct overlay_config {
    // Font (Pango description string)
    char font[256];

    // Colors (RGBA — 8 bits per channel, stored as uint32_t)
    uint32_t background;   // e.g. 0x1a1a2ee6
    uint32_t text_color;   // e.g. 0xe0e0e0ff
    uint32_t border_color; // e.g. 0x333355ff

    // Dimensions
    uint32_t width;
    uint32_t max_lines;
    uint32_t padding_x;
    uint32_t padding_y;
    uint32_t corner_radius;
    uint32_t border_width;

    // Position
    uint32_t margin_top;

    // Animation (milliseconds)
    uint32_t scroll_duration;
    uint32_t fade_duration;

    // Socket path (empty = default $XDG_RUNTIME_DIR/claude-overlay.sock)
    char socket_path[512];
};

// Fill config with compiled-in defaults.
void config_defaults(struct overlay_config *cfg);

// Load config from file. Missing keys keep defaults. Returns false on
// file read error (missing file is OK — just keeps defaults).
bool config_load(struct overlay_config *cfg, const char *path);

#endif
```

**Step 2: Write config.c**

Simple line-by-line parser. For each line:
- Skip empty lines and `#` comments
- Split on first `=`, trim whitespace
- Match key name, parse value into config struct
- Color parsing: `#RRGGBBAA` hex string → uint32_t

No external library needed — the format is trivial.

Defaults:
```c
void config_defaults(struct overlay_config *cfg)
{
    snprintf(cfg->font, sizeof(cfg->font), "Sans 13");
    cfg->background   = 0x1a1a2ee6;
    cfg->text_color   = 0xe0e0e0ff;
    cfg->border_color = 0x333355ff;
    cfg->width         = 600;
    cfg->max_lines     = 12;
    cfg->padding_x     = 12;
    cfg->padding_y     = 8;
    cfg->corner_radius = 8;
    cfg->border_width  = 1;
    cfg->margin_top    = 0;
    cfg->scroll_duration = 150;
    cfg->fade_duration   = 500;
    cfg->socket_path[0]  = '\0';
}
```

**Step 3: Write default config file**

Create `.config/claude-overlay/config` with the defaults shown in the design
doc (commented out — serves as documentation).

**Step 4: Wire into main.c**

Load config at startup, use `cfg.width` and `cfg.margin_top` for the surface.

**Step 5: Build and test**

```bash
ninja -C build && ./build/claude-overlay
```

Test: modify `~/.config/claude-overlay/config` to set `width = 400`, restart.
Surface should be narrower.

**Step 6: Stow + commit**

```bash
cd /home/ifit/dotfiles && stow .
git add .local/share/claude-overlay/ .config/claude-overlay/
git commit -m "add config file parsing with compiled-in defaults"
```

---

## Task 4: Text Rendering with Pango/Cairo

**Files:**
- Create: `.local/share/claude-overlay/src/render.h`
- Create: `.local/share/claude-overlay/src/render.c`
- Modify: `.local/share/claude-overlay/src/main.c` (use renderer)
- Modify: `.local/share/claude-overlay/meson.build` (add render.c)

This task replaces the flat-color fill with proper text rendering.

**Step 1: Write render.h**

```c
#ifndef RENDER_H
#define RENDER_H

#include <stdint.h>
#include <pango/pangocairo.h>
#include "config.h"

struct renderer {
    PangoFontDescription *font_desc;
    PangoContext *pango_ctx;
    int line_height;  // Pixel height of one line of text
};

// Initialize Pango font and measure line height.
void renderer_init(struct renderer *r, const struct overlay_config *cfg);

// Compute the pixel height needed to render the given text at the given
// width (accounting for word-wrap, padding). Returns total content height.
int renderer_measure(struct renderer *r, const struct overlay_config *cfg,
                     const char *text);

// Render text into an ARGB8888 pixel buffer.
// scroll_y: how many pixels of content to skip from the top (for scrolling).
// opacity: 0.0–1.0 for fade animation.
void renderer_draw(struct renderer *r, const struct overlay_config *cfg,
                   uint32_t *pixels, uint32_t buf_width, uint32_t buf_height,
                   const char *text, double scroll_y, double opacity);

void renderer_cleanup(struct renderer *r);

#endif
```

**Step 2: Write render.c**

Key implementation:

**renderer_init** — create font description, create a dummy Cairo surface to
get a PangoContext, measure line height from font metrics:

```c
void renderer_init(struct renderer *r, const struct overlay_config *cfg)
{
    r->font_desc = pango_font_description_from_string(cfg->font);

    // Create a tiny Cairo image surface just to get a PangoContext
    cairo_surface_t *tmp = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
    cairo_t *cr = cairo_create(tmp);
    PangoLayout *layout = pango_cairo_create_layout(cr);
    pango_layout_set_font_description(layout, r->font_desc);

    // Measure line height
    PangoFontMetrics *metrics = pango_context_get_metrics(
        pango_layout_get_context(layout), r->font_desc, NULL);
    r->line_height = (pango_font_metrics_get_ascent(metrics) +
                      pango_font_metrics_get_descent(metrics)) / PANGO_SCALE;
    pango_font_metrics_unref(metrics);

    g_object_unref(layout);
    cairo_destroy(cr);
    cairo_surface_destroy(tmp);
}
```

**renderer_draw** — the main render function:

1. Create a Cairo image surface backed by the pixel buffer
2. Clear to transparent
3. Draw rounded rectangle background (with `cfg->corner_radius`)
4. Draw border
5. Set up clipping rect inside padding
6. Create PangoLayout with word wrap at `width - 2*padding_x`
7. Apply `cairo_translate(cr, 0, -scroll_y)` for scroll offset
8. Render text with `pango_cairo_show_layout()`
9. Apply opacity by painting with `cairo_paint_with_alpha()`

Rounded rectangle helper using Cairo arc:
```c
static void rounded_rect(cairo_t *cr, double x, double y,
                         double w, double h, double r)
{
    cairo_new_sub_path(cr);
    cairo_arc(cr, x + w - r, y + r, r, -M_PI/2, 0);
    cairo_arc(cr, x + w - r, y + h - r, r, 0, M_PI/2);
    cairo_arc(cr, x + r, y + h - r, r, M_PI/2, M_PI);
    cairo_arc(cr, x + r, y + r, r, M_PI, 3*M_PI/2);
    cairo_close_path(cr);
}
```

**Step 3: Wire into main.c**

Replace the flat-color fill loop with:
```c
struct renderer rend = {0};
renderer_init(&rend, &cfg);

// In the redraw block:
renderer_draw(&rend, &cfg, state.pixels,
              state.configured_width, state.configured_height,
              "Hello from claude-overlay!\nThis is a test of Pango text rendering.",
              0.0, 1.0);
```

**Step 4: Build and test**

```bash
ninja -C build && ./build/claude-overlay
```

Expected: Dark rounded rectangle at top-center with "Hello from
claude-overlay!" text in the configured font.

**Step 5: Commit**

```bash
git add .local/share/claude-overlay/
git commit -m "add Pango/Cairo text rendering with rounded rect background"
```

---

## Task 5: Unix Socket Server

**Files:**
- Create: `.local/share/claude-overlay/src/socket.h`
- Create: `.local/share/claude-overlay/src/socket.c`
- Modify: `.local/share/claude-overlay/src/main.c` (add socket to poll loop)
- Modify: `.local/share/claude-overlay/meson.build` (add socket.c)

**Step 1: Write socket.h**

```c
#ifndef SOCKET_H
#define SOCKET_H

#include <stdbool.h>

// Command types from clients
enum overlay_cmd {
    CMD_NONE = 0,
    CMD_OPEN,
    CMD_TEXT,
    CMD_DONE,
    CMD_CLEAR,
    CMD_REPLACE,
};

struct overlay_command {
    enum overlay_cmd cmd;
    char data[4096];  // Text payload for CMD_TEXT and CMD_REPLACE
};

struct socket_server {
    int listen_fd;
    int client_fd;          // Currently connected client (-1 if none)
    char read_buf[8192];    // Accumulates partial reads
    size_t read_len;
    char path[512];
};

// Create and bind the listening socket. Returns false on failure.
bool socket_init(struct socket_server *srv, const char *path);

// Get the fd to poll on (listen_fd or client_fd depending on state).
// Sets up fds array. Returns count of fds added.
int socket_get_fds(struct socket_server *srv, struct pollfd *fds, int offset);

// Process socket events after poll. Populates cmd if a complete command
// was received. Returns false if the server should shut down.
bool socket_process(struct socket_server *srv, struct pollfd *fds, int offset,
                    struct overlay_command *cmd);

// Clean up socket.
void socket_cleanup(struct socket_server *srv);

#endif
```

**Step 2: Write socket.c**

Key implementation:

- `socket_init`: create `AF_UNIX` `SOCK_STREAM`, `bind()`, `listen()`
  - Unlink existing socket path first
  - Set `SO_REUSEADDR`

- `socket_process`: handle incoming connections and data
  - If `listen_fd` is readable: `accept()` new connection (close old client if any)
  - If `client_fd` is readable: `read()` into `read_buf`, scan for `\n`
  - When a complete line is found: parse JSON with `json-c`
    - `json_object_object_get_ex()` for `"cmd"` and `"data"` fields
    - Map string cmd to enum
  - On client disconnect (`read` returns 0): close client_fd, set to -1

**Step 3: Wire into main.c event loop**

```c
struct socket_server srv = {0};
char sock_path[512];
// Build default path: $XDG_RUNTIME_DIR/claude-overlay.sock
snprintf(sock_path, sizeof(sock_path), "%s/claude-overlay.sock",
         getenv("XDG_RUNTIME_DIR") ?: "/tmp");
socket_init(&srv, cfg.socket_path[0] ? cfg.socket_path : sock_path);

// Poll on both Wayland fd and socket fds
struct pollfd fds[3];
fds[0].fd = wayland_get_fd(&state);
fds[0].events = POLLIN;
int nfds = 1 + socket_get_fds(&srv, fds, 1);

// In the loop:
struct overlay_command cmd = {0};
socket_process(&srv, fds, 1, &cmd);
switch (cmd.cmd) {
    case CMD_OPEN:  /* create surface, clear text */ break;
    case CMD_TEXT:  /* append cmd.data to text buffer */ break;
    case CMD_DONE:  /* start fade timer */ break;
    case CMD_CLEAR: /* hide surface */ break;
    default: break;
}
```

**Step 4: Build and test with socat**

```bash
ninja -C build && ./build/claude-overlay &

# In another terminal:
echo '{"cmd": "open"}' | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-overlay.sock
echo '{"cmd": "text", "data": "Hello from socat!"}' | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-overlay.sock
```

At this point the commands are parsed but not yet wired to rendering — that's
the next task. Verify via fprintf debug that commands are received.

**Step 5: Commit**

```bash
git add .local/share/claude-overlay/
git commit -m "add unix socket server with JSON-line command parsing"
```

---

## Task 6: Wire Commands to Rendering

**Files:**
- Modify: `.local/share/claude-overlay/src/main.c` (integrate commands with surface lifecycle)

This task connects the socket commands to the surface + renderer. After this
task, you can stream text to the overlay from the command line.

**Step 1: Add text buffer to main**

```c
// Text accumulator
char text_buf[65536] = {0};
size_t text_len = 0;

// Helper: append to text buffer
static void text_append(const char *chunk) {
    size_t chunk_len = strlen(chunk);
    if (text_len + chunk_len < sizeof(text_buf) - 1) {
        memcpy(text_buf + text_len, chunk, chunk_len);
        text_len += chunk_len;
        text_buf[text_len] = '\0';
    }
}
```

**Step 2: Handle commands in the event loop**

```c
switch (cmd.cmd) {
    case CMD_OPEN:
        text_buf[0] = '\0';
        text_len = 0;
        if (!state.surface_visible) {
            uint32_t initial_h = cfg.padding_y * 2 + rend.line_height;
            wayland_create_surface(&state, cfg.width, initial_h);
        }
        state.needs_redraw = true;
        break;

    case CMD_TEXT:
        text_append(cmd.data);
        // Measure new height, resize surface if needed
        int content_h = renderer_measure(&rend, &cfg, text_buf);
        int max_h = cfg.padding_y * 2 + rend.line_height * cfg.max_lines;
        int target_h = content_h + cfg.padding_y * 2;
        if (target_h > max_h) target_h = max_h;
        if ((uint32_t)target_h != state.height) {
            zwlr_layer_surface_v1_set_size(state.layer_surface,
                                           cfg.width, target_h);
            wl_surface_commit(state.surface);
        }
        state.needs_redraw = true;
        break;

    case CMD_DONE:
        // Start fade timer (handled in Task 9)
        state.needs_redraw = true;
        break;

    case CMD_CLEAR:
        text_buf[0] = '\0';
        text_len = 0;
        wayland_destroy_surface(&state);
        break;

    case CMD_REPLACE:
        text_buf[0] = '\0';
        text_len = 0;
        text_append(cmd.data);
        state.needs_redraw = true;
        break;
}
```

**Step 3: Update redraw to use text buffer**

```c
if (state.needs_redraw && state.configured) {
    wayland_alloc_buffer(&state);
    renderer_draw(&rend, &cfg, state.pixels,
                  state.configured_width, state.configured_height,
                  text_buf, 0.0, 1.0);
    wayland_commit(&state);
    state.needs_redraw = false;
}
```

**Step 4: Build and test streaming**

```bash
ninja -C build && ./build/claude-overlay &

# Stream text word by word:
(
  echo '{"cmd": "open"}'
  sleep 0.1
  for word in "The" "quick" "brown" "fox" "jumps" "over" "the" "lazy" "dog."; do
    echo "{\"cmd\": \"text\", \"data\": \"$word \"}"
    sleep 0.15
  done
  sleep 1
  echo '{"cmd": "done"}'
) | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-overlay.sock
```

Expected: Overlay appears at top-center, text appears word by word, surface
grows as text wraps. After 1 second, "done" fires (fade not yet implemented).

**Step 5: Commit**

```bash
git add .local/share/claude-overlay/
git commit -m "wire socket commands to surface lifecycle and text rendering"
```

---

## Task 7: Dynamic Height Growth

**Files:**
- Modify: `.local/share/claude-overlay/src/main.c`
- Modify: `.local/share/claude-overlay/src/wayland.c` (surface resize)

This should mostly work from Task 6, but needs polish: smooth resize when
surface height changes, proper re-allocation of shm buffer on resize.

**Step 1: Handle surface resize in wayland.c**

The layer surface configure callback fires when the compositor acknowledges a
size change. The buffer needs re-allocation at the new size. Make sure
`wayland_alloc_buffer()` handles size changes — compare current buffer size to
configured size, free and re-mmap if different.

**Step 2: Ensure resize only happens on actual height change**

Track `current_surface_height` and only call `set_size` + commit when it
actually changes. Rapid text chunks shouldn't spam resize requests.

**Step 3: Test growth**

```bash
# Send text that wraps to many lines
(
  echo '{"cmd": "open"}'
  for i in $(seq 1 50); do
    echo "{\"cmd\": \"text\", \"data\": \"Word$i \"}"
    sleep 0.05
  done
  sleep 2
  echo '{"cmd": "clear"}'
) | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-overlay.sock
```

Expected: Surface starts at 1 line, grows as words wrap, stops growing at
12 lines.

**Step 4: Commit**

```bash
git add .local/share/claude-overlay/
git commit -m "polish dynamic height growth with proper buffer reallocation"
```

---

## Task 8: Scroll Animation

**Files:**
- Create: `.local/share/claude-overlay/src/animation.h`
- Create: `.local/share/claude-overlay/src/animation.c`
- Modify: `.local/share/claude-overlay/src/main.c`
- Modify: `.local/share/claude-overlay/meson.build`

**Step 1: Write animation.h**

```c
#ifndef ANIMATION_H
#define ANIMATION_H

#include <stdbool.h>
#include <stdint.h>

struct animation {
    double current;      // Current value
    double target;       // Target value
    double duration_ms;  // Animation duration
    double start_value;  // Value when animation started
    uint64_t start_time; // Timestamp when animation started (ms)
    bool active;
};

// Set a new target. Starts animating from current value.
void anim_set_target(struct animation *a, double target, double duration_ms);

// Update current value based on elapsed time. Uses ease-out cubic.
// Returns true if animation is still in progress.
bool anim_update(struct animation *a, uint64_t now_ms);

// Get current monotonic time in milliseconds.
uint64_t anim_now_ms(void);

#endif
```

**Step 2: Write animation.c**

Ease-out cubic: `t' = 1 - (1-t)^3` where `t` is normalized progress [0,1].

```c
bool anim_update(struct animation *a, uint64_t now_ms)
{
    if (!a->active) return false;

    double elapsed = (double)(now_ms - a->start_time);
    double t = elapsed / a->duration_ms;
    if (t >= 1.0) {
        a->current = a->target;
        a->active = false;
        return false;
    }

    // Ease-out cubic
    double ease = 1.0 - (1.0 - t) * (1.0 - t) * (1.0 - t);
    a->current = a->start_value + (a->target - a->start_value) * ease;
    return true;
}
```

**Step 3: Add timerfd to main event loop**

```c
#include <sys/timerfd.h>

int timer_fd = timerfd_create(CLOCK_MONOTONIC, TFD_NONBLOCK);

// When animation starts, arm the timer at ~60fps:
struct itimerspec ts = {
    .it_interval = { .tv_nsec = 16666667 },  // ~60fps
    .it_value    = { .tv_nsec = 16666667 },
};
timerfd_settime(timer_fd, 0, &ts, NULL);

// When animation stops, disarm:
struct itimerspec ts_off = {0};
timerfd_settime(timer_fd, 0, &ts_off, NULL);
```

Add `timer_fd` to the `pollfd` array. On timer events, read the timerfd
(required to clear it), update scroll animation, trigger redraw.

**Step 4: Compute scroll target on text change**

```c
// After CMD_TEXT:
int content_h = renderer_measure(&rend, &cfg, text_buf);
int visible_h = cfg.max_lines * rend.line_height;
if (content_h > visible_h) {
    double new_target = content_h - visible_h;
    anim_set_target(&scroll_anim, new_target, cfg.scroll_duration);
    // Arm timer if not already running
}
```

Pass `scroll_anim.current` as the `scroll_y` parameter to `renderer_draw()`.

**Step 5: Test scroll**

```bash
# Send more text than fits in 12 lines
(
  echo '{"cmd": "open"}'
  for i in $(seq 1 200); do
    echo "{\"cmd\": \"text\", \"data\": \"word$i \"}"
    sleep 0.03
  done
  sleep 2
  echo '{"cmd": "clear"}'
) | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-overlay.sock
```

Expected: Text fills up to 12 lines, then smoothly scrolls as new text arrives.
The scroll should feel snappy (~150ms) with ease-out.

**Step 6: Commit**

```bash
git add .local/share/claude-overlay/
git commit -m "add smooth scroll animation with ease-out cubic"
```

---

## Task 9: Fade Animation

**Files:**
- Modify: `.local/share/claude-overlay/src/main.c`

**Step 1: Add fade animation state**

```c
struct animation fade_anim = {0};
fade_anim.current = 1.0;  // Fully opaque
```

**Step 2: Handle CMD_DONE — start fade**

```c
case CMD_DONE:
    anim_set_target(&fade_anim, 0.0, cfg.fade_duration);
    // Arm timer if not running
    break;
```

**Step 3: Handle CMD_OPEN — cancel fade, reset opacity**

```c
case CMD_OPEN:
    fade_anim.current = 1.0;
    fade_anim.active = false;
    // ... rest of open logic
    break;
```

**Step 4: Pass opacity to renderer**

```c
renderer_draw(&rend, &cfg, state.pixels,
              state.configured_width, state.configured_height,
              text_buf, scroll_anim.current, fade_anim.current);
```

**Step 5: Destroy surface when fade completes**

```c
// In timer event handler:
if (!anim_update(&fade_anim, now) && fade_anim.current <= 0.01) {
    wayland_destroy_surface(&state);
}
```

**Step 6: Test**

```bash
(
  echo '{"cmd": "open"}'
  echo '{"cmd": "text", "data": "This will fade out..."}'
  sleep 1
  echo '{"cmd": "done"}'
) | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-overlay.sock
```

Expected: Text appears, after "done" the overlay fades out smoothly over 500ms
then the surface is destroyed.

**Step 7: Commit**

```bash
git add .local/share/claude-overlay/
git commit -m "add fade-out animation on done command"
```

---

## Task 10: Systemd Service + Setup Integration

**Files:**
- Create: `.config/systemd/user/claude-overlay.service`
- Modify: `setup.sh` (add build + enable section)

**Step 1: Write service file**

```ini
[Unit]
Description=Claude streaming text overlay
After=graphical-session.target

[Service]
Type=simple
ExecStart=%h/.local/bin/claude-overlay
Restart=on-failure
RestartSec=1

[Install]
WantedBy=graphical-session.target
```

**Step 2: Add setup.sh section**

Add after the existing claude-voice/claude-hid sections:

```bash
# ------------------------------------------------------------------
# N. Claude Overlay (Wayland streaming text overlay)
# ------------------------------------------------------------------
step "Claude Overlay"

OVERLAY_DIR="$HOME/.local/share/claude-overlay"
OVERLAY_BIN="$HOME/.local/bin/claude-overlay"

if [ -f "$OVERLAY_DIR/meson.build" ]; then
    if [ ! -d "$OVERLAY_DIR/build" ]; then
        meson setup "$OVERLAY_DIR/build" "$OVERLAY_DIR"
        ok "meson configured"
    else
        skip "meson already configured"
    fi

    ninja -C "$OVERLAY_DIR/build"
    cp "$OVERLAY_DIR/build/claude-overlay" "$OVERLAY_BIN"
    ok "claude-overlay built and installed"

    if systemctl --user is-enabled claude-overlay.service &>/dev/null; then
        ok "claude-overlay.service already enabled"
    else
        systemctl --user enable claude-overlay.service
        ok "claude-overlay.service enabled"
    fi

    systemctl --user restart claude-overlay.service
    ok "claude-overlay.service restarted"
else
    warn "claude-overlay source not found"
fi
```

**Step 3: Add wlr-protocols to packages-base.txt**

Append `wlr-protocols` to the packages list if not already present.

**Step 4: Stow + build + enable**

```bash
cd /home/ifit/dotfiles && stow .
cd ~/.local/share/claude-overlay && ninja -C build
cp build/claude-overlay ~/.local/bin/
systemctl --user daemon-reload
systemctl --user enable --now claude-overlay.service
```

**Step 5: Verify service runs**

```bash
systemctl --user status claude-overlay.service
# Should be active (running)

# Test:
echo '{"cmd":"open"}{"cmd":"text","data":"Service works!"}' \
  | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/claude-overlay.sock
```

**Step 6: Commit**

```bash
cd /home/ifit/dotfiles
git add .config/systemd/user/claude-overlay.service setup.sh
git commit -m "add systemd service and setup.sh build integration"
```

---

## Task 11: Python Integration

**Files:**
- Modify: `.local/share/claude-ask/query.py`

**Step 1: Add overlay socket helpers**

Add near the top of query.py, after imports:

```python
import json
import socket as socket_mod  # avoid shadowing

def _connect_overlay():
    """Connect to the claude-overlay Unix socket. Returns socket or None."""
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    sock_path = os.path.join(runtime_dir, "claude-overlay.sock")
    try:
        sock = socket_mod.socket(socket_mod.AF_UNIX, socket_mod.SOCK_STREAM)
        sock.connect(sock_path)
        return sock
    except (ConnectionRefusedError, FileNotFoundError, OSError):
        return None


def _overlay_send(sock, msg):
    """Send a JSON-line command to the overlay. Swallows errors."""
    if sock is None:
        return
    try:
        sock.sendall((json.dumps(msg) + "\n").encode("utf-8"))
    except (BrokenPipeError, OSError):
        pass
```

Note: `json` and `socket` are likely already imported — check before adding
duplicate imports. Rename socket import to `socket_mod` since query.py already
uses `socket` for the voice control function.

**Step 2: Modify stream_response()**

Replace the `notify()` call in the streaming loop:

```python
def stream_response(messages, tag, cancel_event=None, prior_text=""):
    # ... existing setup ...

    overlay_sock = _connect_overlay()
    _overlay_send(overlay_sock, {"cmd": "open"})

    with client.messages.stream(**api_kwargs) as stream:
        for event in stream:
            if cancel_event and cancel_event.is_set():
                _overlay_send(overlay_sock, {"cmd": "clear"})
                stream.close()
                break
            if event.type == "content_block_delta":
                if event.delta.type == "text_delta":
                    accumulated_text += event.delta.text

                    if speak_on:
                        for sentence in sentence_buf.add(event.delta.text):
                            tts.speak(sentence)

                    # Send delta to overlay (no debounce needed)
                    _overlay_send(overlay_sock, {
                        "cmd": "text",
                        "data": event.delta.text,
                    })

        response = stream.get_final_message()

    _overlay_send(overlay_sock, {"cmd": "done"})
    if overlay_sock:
        try:
            overlay_sock.close()
        except OSError:
            pass

    # ... rest unchanged (TTS flush, return) ...
```

Changes:
- Remove `last_notify_time` variable and debounce logic
- Remove `notify(tag, accumulated_text)` call
- Add overlay open/text/done/clear calls
- Send text deltas, not accumulated text
- On cancel, send "clear" instead of "done"

**Do NOT remove** the `notify()` function definition itself — it may still be
called from other code paths. Do remove only the call inside `stream_response`.

**Step 3: Verify notify_final() is unchanged**

`notify_final()` still shows the final notification with actions via swaync.
No changes needed.

**Step 4: Test end-to-end**

```bash
# Restart claude-ask service (which imports query.py)
systemctl --user restart claude-voice.service

# Send a test query via claude-ask
claude-ask "What is 2+2?"
```

Expected: Overlay appears at top-center, Claude's response streams in word by
word, overlay fades out when done, then the final swaync notification appears
with action buttons.

**Step 5: Commit**

```bash
cd /home/ifit/dotfiles
git add .local/share/claude-ask/query.py
git commit -m "integrate claude-overlay: stream text deltas to overlay socket"
```

---

## Summary of File Layout

After all tasks:

```
.local/share/claude-overlay/
├── meson.build
├── protocols/
│   └── wlr-layer-shell-unstable-v1.xml
└── src/
    ├── main.c
    ├── wayland.h
    ├── wayland.c
    ├── shm.h
    ├── shm.c
    ├── render.h
    ├── render.c
    ├── config.h
    ├── config.c
    ├── socket.h
    ├── socket.c
    ├── animation.h
    └── animation.c

.config/claude-overlay/
└── config

.config/systemd/user/
└── claude-overlay.service
```

## Dependencies (Arch packages)

- `wayland` (already installed)
- `wayland-protocols` (already installed)
- `wlr-protocols` (needs install)
- `cairo` (already installed)
- `pango` (already installed)
- `json-c` (already installed)
- `meson` + `ninja` (build tools, already installed)
