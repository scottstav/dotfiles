#include "wayland.h"
#include "shm.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

/* --- Registry listener --- */

static void registry_handle_global(void *data, struct wl_registry *registry,
                                   uint32_t name, const char *interface,
                                   uint32_t version)
{
    struct overlay_state *state = data;
    (void)version;

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

static void registry_handle_global_remove(void *data,
                                          struct wl_registry *registry,
                                          uint32_t name)
{
    (void)data;
    (void)registry;
    (void)name;
}

static const struct wl_registry_listener registry_listener = {
    .global = registry_handle_global,
    .global_remove = registry_handle_global_remove,
};

/* --- Layer surface listener --- */

static void layer_surface_configure(void *data,
                                    struct zwlr_layer_surface_v1 *surface,
                                    uint32_t serial,
                                    uint32_t width, uint32_t height)
{
    struct overlay_state *state = data;
    state->configured_width = width ? width : state->width;
    state->configured_height = height ? height : state->height;
    state->configured = true;
    state->needs_redraw = true;
    zwlr_layer_surface_v1_ack_configure(surface, serial);
}

static void layer_surface_closed(void *data,
                                 struct zwlr_layer_surface_v1 *surface)
{
    (void)surface;
    struct overlay_state *state = data;
    state->closed = true;
}

static const struct zwlr_layer_surface_v1_listener layer_surface_listener = {
    .configure = layer_surface_configure,
    .closed = layer_surface_closed,
};

/* --- Public API --- */

bool wayland_init(struct overlay_state *state)
{
    state->display = wl_display_connect(NULL);
    if (!state->display) {
        fprintf(stderr, "wayland_init: failed to connect to display\n");
        return false;
    }

    state->registry = wl_display_get_registry(state->display);
    wl_registry_add_listener(state->registry, &registry_listener, state);

    /* First roundtrip discovers globals */
    wl_display_roundtrip(state->display);
    /* Second roundtrip completes setup (e.g. wl_shm formats) */
    wl_display_roundtrip(state->display);

    if (!state->compositor) {
        fprintf(stderr, "wayland_init: compositor not found\n");
        return false;
    }
    if (!state->shm) {
        fprintf(stderr, "wayland_init: wl_shm not found\n");
        return false;
    }
    if (!state->layer_shell) {
        fprintf(stderr, "wayland_init: zwlr_layer_shell_v1 not found\n");
        return false;
    }

    return true;
}

bool wayland_create_surface(struct overlay_state *state,
                            uint32_t width, uint32_t height)
{
    state->width = width;
    state->height = height;

    state->surface = wl_compositor_create_surface(state->compositor);
    if (!state->surface) {
        fprintf(stderr, "wayland_create_surface: failed to create surface\n");
        return false;
    }

    state->layer_surface = zwlr_layer_shell_v1_get_layer_surface(
        state->layer_shell, state->surface, NULL,
        ZWLR_LAYER_SHELL_V1_LAYER_OVERLAY, "claude-overlay");

    if (!state->layer_surface) {
        fprintf(stderr, "wayland_create_surface: failed to create layer surface\n");
        return false;
    }

    /* Anchor top only -- centers horizontally */
    zwlr_layer_surface_v1_set_anchor(state->layer_surface,
        ZWLR_LAYER_SURFACE_V1_ANCHOR_TOP);

    /* -1 exclusive zone = don't reserve space, don't push others */
    zwlr_layer_surface_v1_set_exclusive_zone(state->layer_surface, -1);

    /* Margin below waybar (8px for now, will be configurable) */
    zwlr_layer_surface_v1_set_margin(state->layer_surface, 8, 0, 0, 0);

    zwlr_layer_surface_v1_set_size(state->layer_surface, width, height);
    zwlr_layer_surface_v1_add_listener(state->layer_surface,
                                       &layer_surface_listener, state);

    wl_surface_commit(state->surface);
    state->surface_visible = true;
    return true;
}

void wayland_destroy_surface(struct overlay_state *state)
{
    if (state->buffer) {
        wl_buffer_destroy(state->buffer);
        state->buffer = NULL;
    }
    if (state->pixels && state->shm_size > 0) {
        munmap(state->pixels, state->shm_size);
        state->pixels = NULL;
    }
    if (state->shm_fd >= 0) {
        close(state->shm_fd);
        state->shm_fd = -1;
    }
    if (state->layer_surface) {
        zwlr_layer_surface_v1_destroy(state->layer_surface);
        state->layer_surface = NULL;
    }
    if (state->surface) {
        wl_surface_destroy(state->surface);
        state->surface = NULL;
    }
    state->surface_visible = false;
    state->configured = false;
}

bool wayland_alloc_buffer(struct overlay_state *state)
{
    int stride = (int)state->configured_width * 4;
    size_t size = (size_t)stride * state->configured_height;

    /* Reuse existing buffer if size matches */
    if (state->buffer && state->shm_size == size)
        return true;

    /* Free old buffer */
    if (state->buffer) {
        wl_buffer_destroy(state->buffer);
        state->buffer = NULL;
    }
    if (state->pixels && state->shm_size > 0) {
        munmap(state->pixels, state->shm_size);
        state->pixels = NULL;
    }
    if (state->shm_fd >= 0) {
        close(state->shm_fd);
        state->shm_fd = -1;
    }

    state->pixels = shm_alloc(size, &state->shm_fd);
    if (!state->pixels) {
        fprintf(stderr, "wayland_alloc_buffer: shm_alloc failed\n");
        return false;
    }
    state->shm_size = size;

    struct wl_shm_pool *pool = wl_shm_create_pool(state->shm, state->shm_fd,
                                                   (int32_t)size);
    state->buffer = wl_shm_pool_create_buffer(pool, 0,
        (int32_t)state->configured_width, (int32_t)state->configured_height,
        stride, WL_SHM_FORMAT_ARGB8888);
    wl_shm_pool_destroy(pool);

    return true;
}

void wayland_commit(struct overlay_state *state)
{
    wl_surface_attach(state->surface, state->buffer, 0, 0);
    wl_surface_damage_buffer(state->surface, 0, 0,
                             (int32_t)state->configured_width,
                             (int32_t)state->configured_height);
    wl_surface_commit(state->surface);
}

void wayland_cleanup(struct overlay_state *state)
{
    wayland_destroy_surface(state);

    if (state->layer_shell) {
        zwlr_layer_shell_v1_destroy(state->layer_shell);
        state->layer_shell = NULL;
    }
    if (state->compositor) {
        wl_compositor_destroy(state->compositor);
        state->compositor = NULL;
    }
    if (state->shm) {
        wl_shm_destroy(state->shm);
        state->shm = NULL;
    }
    if (state->registry) {
        wl_registry_destroy(state->registry);
        state->registry = NULL;
    }
    if (state->display) {
        wl_display_disconnect(state->display);
        state->display = NULL;
    }
}

int wayland_get_fd(struct overlay_state *state)
{
    return wl_display_get_fd(state->display);
}

bool wayland_dispatch(struct overlay_state *state)
{
    if (wl_display_dispatch(state->display) < 0)
        return false;
    return !state->closed;
}
