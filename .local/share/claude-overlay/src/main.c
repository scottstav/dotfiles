#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <poll.h>
#include "wayland.h"

static volatile sig_atomic_t quit = 0;

static void handle_signal(int sig) { (void)sig; quit = 1; }

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    struct overlay_state state = {0};
    state.shm_fd = -1;

    signal(SIGINT, handle_signal);
    signal(SIGTERM, handle_signal);

    if (!wayland_init(&state)) {
        fprintf(stderr, "Failed to initialize Wayland\n");
        return EXIT_FAILURE;
    }

    if (!wayland_create_surface(&state, 600, 200)) {
        fprintf(stderr, "Failed to create surface\n");
        wayland_cleanup(&state);
        return EXIT_FAILURE;
    }

    struct pollfd fds[1] = {
        { .fd = wayland_get_fd(&state), .events = POLLIN },
    };

    while (!quit && !state.closed) {
        if (state.needs_redraw && state.configured) {
            if (!wayland_alloc_buffer(&state)) {
                fprintf(stderr, "Failed to allocate buffer\n");
                break;
            }

            /* Fill with dark semi-transparent color (ARGB premultiplied)
             * 0xe6 alpha, 0x1a R, 0x1a G, 0x2e B */
            uint8_t a = 0xe6, r = 0x1a, g = 0x1a, b = 0x2e;
            uint8_t pr = (uint16_t)r * a / 255;
            uint8_t pg = (uint16_t)g * a / 255;
            uint8_t pb = (uint16_t)b * a / 255;
            uint32_t color = ((uint32_t)a << 24) | ((uint32_t)pr << 16) |
                             ((uint32_t)pg << 8) | pb;

            for (uint32_t i = 0; i < state.configured_width * state.configured_height; i++)
                state.pixels[i] = color;

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
