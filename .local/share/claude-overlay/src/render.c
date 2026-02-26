#define _GNU_SOURCE
#include <math.h>
#include <string.h>
#include <cairo/cairo.h>
#include <pango/pangocairo.h>
#include "render.h"

void renderer_init(struct renderer *r, const struct overlay_config *cfg)
{
    r->font_desc = pango_font_description_from_string(cfg->font);

    /* Create a tiny dummy surface just to measure font metrics */
    cairo_surface_t *tmp_surf = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
    cairo_t *tmp_cr = cairo_create(tmp_surf);
    PangoLayout *tmp_layout = pango_cairo_create_layout(tmp_cr);
    pango_layout_set_font_description(tmp_layout, r->font_desc);

    PangoContext *ctx = pango_layout_get_context(tmp_layout);
    PangoFontMetrics *metrics = pango_context_get_metrics(ctx, r->font_desc, NULL);

    int ascent = pango_font_metrics_get_ascent(metrics);
    int descent = pango_font_metrics_get_descent(metrics);
    r->line_height = (ascent + descent) / PANGO_SCALE;

    pango_font_metrics_unref(metrics);
    g_object_unref(tmp_layout);
    cairo_destroy(tmp_cr);
    cairo_surface_destroy(tmp_surf);
}

int renderer_measure(struct renderer *r, const struct overlay_config *cfg,
                     const char *text)
{
    cairo_surface_t *tmp_surf = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
    cairo_t *tmp_cr = cairo_create(tmp_surf);
    PangoLayout *layout = pango_cairo_create_layout(tmp_cr);

    pango_layout_set_font_description(layout, r->font_desc);
    pango_layout_set_width(layout, (int)(cfg->width - 2 * cfg->padding_x) * PANGO_SCALE);
    pango_layout_set_wrap(layout, PANGO_WRAP_WORD_CHAR);
    pango_layout_set_text(layout, text, -1);

    int text_w, text_h;
    pango_layout_get_pixel_size(layout, &text_w, &text_h);

    g_object_unref(layout);
    cairo_destroy(tmp_cr);
    cairo_surface_destroy(tmp_surf);

    return text_h;
}

static void rounded_rect(cairo_t *cr, double x, double y,
                         double w, double h, double r)
{
    cairo_new_sub_path(cr);
    cairo_arc(cr, x + w - r, y + r,     r, -M_PI / 2, 0);
    cairo_arc(cr, x + w - r, y + h - r, r, 0,          M_PI / 2);
    cairo_arc(cr, x + r,     y + h - r, r, M_PI / 2,   M_PI);
    cairo_arc(cr, x + r,     y + r,     r, M_PI,        3 * M_PI / 2);
    cairo_close_path(cr);
}

/* Extract RGBA channels from 0xRRGGBBAA color */
static void color_rgba(uint32_t c, double *r, double *g, double *b, double *a)
{
    *r = ((c >> 24) & 0xFF) / 255.0;
    *g = ((c >> 16) & 0xFF) / 255.0;
    *b = ((c >> 8)  & 0xFF) / 255.0;
    *a = (c & 0xFF) / 255.0;
}

void renderer_draw(struct renderer *r, const struct overlay_config *cfg,
                   uint32_t *pixels, uint32_t buf_width, uint32_t buf_height,
                   const char *text, double scroll_y, double opacity)
{
    cairo_surface_t *surface = cairo_image_surface_create_for_data(
        (unsigned char *)pixels, CAIRO_FORMAT_ARGB32,
        (int)buf_width, (int)buf_height, (int)(buf_width * 4));
    cairo_t *cr = cairo_create(surface);

    /* Clear to transparent */
    cairo_set_operator(cr, CAIRO_OPERATOR_CLEAR);
    cairo_paint(cr);
    cairo_set_operator(cr, CAIRO_OPERATOR_OVER);

    /* Push a group so we can apply global opacity at the end */
    cairo_push_group(cr);

    /* Draw rounded rectangle background */
    double bw = cfg->border_width;
    double inset = bw / 2.0;
    rounded_rect(cr, inset, inset,
                 buf_width - 2 * inset, buf_height - 2 * inset,
                 cfg->corner_radius);

    double bg_r, bg_g, bg_b, bg_a;
    color_rgba(cfg->background, &bg_r, &bg_g, &bg_b, &bg_a);
    cairo_set_source_rgba(cr, bg_r, bg_g, bg_b, bg_a);
    cairo_fill_preserve(cr);

    /* Stroke border if border_width > 0 */
    if (cfg->border_width > 0) {
        double br, bg, bb, ba;
        color_rgba(cfg->border_color, &br, &bg, &bb, &ba);
        cairo_set_source_rgba(cr, br, bg, bb, ba);
        cairo_set_line_width(cr, bw);
        cairo_stroke(cr);
    } else {
        cairo_new_path(cr);
    }

    /* Clip to the content area inside padding */
    cairo_save(cr);
    cairo_rectangle(cr, cfg->padding_x, cfg->padding_y,
                    buf_width - 2 * cfg->padding_x,
                    buf_height - 2 * cfg->padding_y);
    cairo_clip(cr);

    /* Apply scroll offset */
    cairo_translate(cr, cfg->padding_x, cfg->padding_y - scroll_y);

    /* Create Pango layout for text rendering */
    PangoLayout *layout = pango_cairo_create_layout(cr);
    pango_layout_set_font_description(layout, r->font_desc);
    pango_layout_set_width(layout, (int)(buf_width - 2 * cfg->padding_x) * PANGO_SCALE);
    pango_layout_set_wrap(layout, PANGO_WRAP_WORD_CHAR);
    pango_layout_set_text(layout, text, -1);

    /* Set text color and render */
    double tc_r, tc_g, tc_b, tc_a;
    color_rgba(cfg->text_color, &tc_r, &tc_g, &tc_b, &tc_a);
    cairo_set_source_rgba(cr, tc_r, tc_g, tc_b, tc_a);
    pango_cairo_show_layout(cr, layout);

    g_object_unref(layout);
    cairo_restore(cr);

    /* Pop group and paint with global opacity */
    cairo_pop_group_to_source(cr);
    cairo_paint_with_alpha(cr, opacity);

    cairo_destroy(cr);
    cairo_surface_destroy(surface);
}

void renderer_cleanup(struct renderer *r)
{
    if (r->font_desc) {
        pango_font_description_free(r->font_desc);
        r->font_desc = NULL;
    }
}
