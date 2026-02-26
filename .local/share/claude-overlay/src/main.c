#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <poll.h>
#include "wayland.h"
#include "config.h"
#include "render.h"

static volatile sig_atomic_t quit = 0;

static void handle_signal(int sig) { (void)sig; quit = 1; }

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    /* Load configuration */
    struct overlay_config cfg;
    config_defaults(&cfg);

    char config_path[512];
    const char *config_home = getenv("XDG_CONFIG_HOME");
    if (config_home)
        snprintf(config_path, sizeof(config_path),
                 "%s/claude-overlay/config", config_home);
    else
        snprintf(config_path, sizeof(config_path),
                 "%s/.config/claude-overlay/config", getenv("HOME"));
    config_load(&cfg, config_path);

    struct renderer rend = {0};
    renderer_init(&rend, &cfg);

    struct overlay_state state = {0};
    state.shm_fd = -1;

    signal(SIGINT, handle_signal);
    signal(SIGTERM, handle_signal);

    if (!wayland_init(&state)) {
        fprintf(stderr, "Failed to initialize Wayland\n");
        renderer_cleanup(&rend);
        return EXIT_FAILURE;
    }

    uint32_t initial_h = cfg.padding_y * 2 + (uint32_t)rend.line_height;
    if (!wayland_create_surface(&state, cfg.width, initial_h, cfg.margin_top)) {
        fprintf(stderr, "Failed to create surface\n");
        wayland_cleanup(&state);
        renderer_cleanup(&rend);
        return EXIT_FAILURE;
    }

    struct pollfd fds[1] = {
        { .fd = wayland_get_fd(&state), .events = POLLIN },
    };

    const char *test_text =
        "Hello from claude-overlay!\n"
        "This is a test of Pango text rendering with word wrapping "
        "and a nice rounded rectangle.";

    while (!quit && !state.closed) {
        if (state.needs_redraw && state.configured) {
            if (!wayland_alloc_buffer(&state)) {
                fprintf(stderr, "Failed to allocate buffer\n");
                break;
            }

            renderer_draw(&rend, &cfg, state.pixels,
                          state.configured_width, state.configured_height,
                          test_text, 0.0, 1.0);

            wayland_commit(&state);
            state.needs_redraw = false;
        }

        wl_display_flush(state.display);

        if (poll(fds, 1, -1) < 0) {
            if (quit)
                break;
            perror("poll");
            break;
        }

        if (fds[0].revents & POLLIN) {
            if (!wayland_dispatch(&state))
                break;
        }
    }

    renderer_cleanup(&rend);
    wayland_cleanup(&state);
    return EXIT_SUCCESS;
}
