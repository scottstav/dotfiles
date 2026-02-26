#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <poll.h>
#include "wayland.h"
#include "config.h"

static volatile sig_atomic_t quit = 0;

static void handle_signal(int sig) { (void)sig; quit = 1; }

/* Convert RGBA (0xRRGGBBAA) to premultiplied ARGB for Wayland */
static uint32_t rgba_to_premul_argb(uint32_t rgba)
{
    uint8_t r = (rgba >> 24) & 0xFF;
    uint8_t g = (rgba >> 16) & 0xFF;
    uint8_t b = (rgba >> 8)  & 0xFF;
    uint8_t a = rgba & 0xFF;

    uint8_t pr = (uint16_t)r * a / 255;
    uint8_t pg = (uint16_t)g * a / 255;
    uint8_t pb = (uint16_t)b * a / 255;

    return ((uint32_t)a << 24) | ((uint32_t)pr << 16) |
           ((uint32_t)pg << 8) | pb;
}

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

    struct overlay_state state = {0};
    state.shm_fd = -1;

    signal(SIGINT, handle_signal);
    signal(SIGTERM, handle_signal);

    if (!wayland_init(&state)) {
        fprintf(stderr, "Failed to initialize Wayland\n");
        return EXIT_FAILURE;
    }

    if (!wayland_create_surface(&state, cfg.width, 200, cfg.margin_top)) {
        fprintf(stderr, "Failed to create surface\n");
        wayland_cleanup(&state);
        return EXIT_FAILURE;
    }

    struct pollfd fds[1] = {
        { .fd = wayland_get_fd(&state), .events = POLLIN },
    };

    uint32_t bg_color = rgba_to_premul_argb(cfg.background);

    while (!quit && !state.closed) {
        if (state.needs_redraw && state.configured) {
            if (!wayland_alloc_buffer(&state)) {
                fprintf(stderr, "Failed to allocate buffer\n");
                break;
            }

            for (uint32_t i = 0; i < state.configured_width * state.configured_height; i++)
                state.pixels[i] = bg_color;

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

    wayland_cleanup(&state);
    return EXIT_SUCCESS;
}
