#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "config.h"

#if MYPAINT_CONFIG_USE_GLIB
#include <glib.h>
#endif

#include "mypaint-fixed-tiled-surface.h"


struct MyPaintFixedTiledSurface {
    MyPaintTiledSurface parent;

    size_t tile_size; // Size (in bytes) of single tile
    uint16_t *tile_buffer; // Stores tiles in a linear chunk of memory (16bpc RGBA)
    uint16_t *null_tile; // Single tile that we hand out and ignore writes to
    int tiles_width; // width in tiles
    int tiles_height; // height in tiles
    int width; // width in pixels
    int height; // height in pixels

};

void free_simple_tiledsurf(MyPaintSurface *surface);

void reset_null_tile(MyPaintFixedTiledSurface *self)
{
    memset(self->null_tile, 0, self->tile_size);
}

static void
tile_request_start(MyPaintTiledSurface *tiled_surface, MyPaintTileRequest *request)
{
    MyPaintFixedTiledSurface *self = (MyPaintFixedTiledSurface *)tiled_surface;

    const int tx = request->tx;
    const int ty = request->ty;

    uint16_t *tile_pointer = NULL;

    if (tx >= self->tiles_width || ty >= self->tiles_height || tx < 0 || ty < 0) {
        // Give it a tile which we will ignore writes to
        tile_pointer = self->null_tile;

    } else {
        // Compute the offset for the tile into our linear memory buffer of tiles
        size_t rowstride = self->tiles_width * self->tile_size;
        size_t x_offset = tx * self->tile_size;
        size_t tile_offset = (rowstride * ty) + x_offset;

        tile_pointer = self->tile_buffer + tile_offset/sizeof(uint16_t);
    }

    request->buffer = tile_pointer;
}

static void
tile_request_end(MyPaintTiledSurface *tiled_surface, MyPaintTileRequest *request)
{
    MyPaintFixedTiledSurface *self = (MyPaintFixedTiledSurface *)tiled_surface;

    const int tx = request->tx;
    const int ty = request->ty;

    if (tx >= self->tiles_width || ty >= self->tiles_height || tx < 0 || ty < 0) {
        // Wipe any changed done to the null tile
        reset_null_tile(self);
    } else {
        // We hand out direct pointers to our buffer, so for the normal case nothing needs to be done
    }
}

MyPaintSurface *
mypaint_fixed_tiled_surface_interface(MyPaintFixedTiledSurface *self)
{
    return (MyPaintSurface *)self;
}

int
mypaint_fixed_tiled_surface_get_width(MyPaintFixedTiledSurface *self)
{
    return self->width;
}

int
mypaint_fixed_tiled_surface_get_height(MyPaintFixedTiledSurface *self)
{
    return self->height;
}

MyPaintFixedTiledSurface *
mypaint_fixed_tiled_surface_new(int width, int height)
{
    assert(width > 0);
    assert(height > 0);

    MyPaintFixedTiledSurface *self = (MyPaintFixedTiledSurface *)malloc(sizeof(MyPaintFixedTiledSurface));

    mypaint_tiled_surface_init(&self->parent, tile_request_start, tile_request_end);

    const int tile_size_pixels = self->parent.tile_size;

    // MyPaintSurface vfuncs
    self->parent.parent.destroy = free_simple_tiledsurf;

    const int tiles_width = ceil((float)width / tile_size_pixels);
    const int tiles_height = ceil((float)height / tile_size_pixels);
    const size_t tile_size = tile_size_pixels * tile_size_pixels * 4 * sizeof(uint16_t);
    const size_t buffer_size = tiles_width * tiles_height * tile_size;

    assert(tile_size_pixels*tiles_width >= width);
    assert(tile_size_pixels*tiles_height >= height);
    assert(buffer_size >= width*height*4*sizeof(uint16_t));

    uint16_t * buffer = (uint16_t *)malloc(buffer_size);
    if (!buffer) {
        fprintf(stderr, "CRITICAL: unable to allocate enough memory: %zu bytes", buffer_size);
        free(self);
        return NULL;
    }
    memset(buffer, 255, buffer_size);

    self->tile_buffer = buffer;
    self->tile_size = tile_size;
    self->null_tile = (uint16_t *)malloc(tile_size);
    self->tiles_width = tiles_width;
    self->tiles_height = tiles_height;
    self->height = height;
    self->width = width;

    reset_null_tile(self);

    return self;
}

void free_simple_tiledsurf(MyPaintSurface *surface)
{
    MyPaintFixedTiledSurface *self = (MyPaintFixedTiledSurface *)surface;

    mypaint_tiled_surface_destroy(&self->parent);

    free(self->tile_buffer);
    free(self->null_tile);

    free(self);
}


// Naive conversion code from the internal MyPaint format and 8 bit RGB
void
fix15_to_rgba8(uint16_t *src, uint8_t *dst, int length)
{
    for (int i = 0; i < length; i++) {
      uint32_t r, g, b, a;

      r = *src++;
      g = *src++;
      b = *src++;
      a = *src++;

      // un-premultiply alpha (with rounding)
      if (a != 0) {
        r = ((r << 15) + a/2) / a;
        g = ((g << 15) + a/2) / a;
        b = ((b << 15) + a/2) / a;
      } else {
        r = g = b = 0;
      }

      // Variant A) rounding
      const uint32_t add_r = (1<<15)/2;
      const uint32_t add_g = (1<<15)/2;
      const uint32_t add_b = (1<<15)/2;
      const uint32_t add_a = (1<<15)/2;

      *dst++ = (r * 255 + add_r) / (1<<15);
      *dst++ = (g * 255 + add_g) / (1<<15);
      *dst++ = (b * 255 + add_b) / (1<<15);
      *dst++ = (a * 255 + add_a) / (1<<15);
    }
}

// Utility code for writing out scanline-based formats like PPM
typedef void (*LineChunkCallback) (uint16_t *chunk, int chunk_length, void *user_data);

/* Iterate over chunks of data in the MyPaintTiledSurface,
    starting top-left (0,0) and stopping at bottom-right (width-1,height-1)
    callback will be called with linear chunks of horizontal data, up to MYPAINT_TILE_SIZE long
*/
void
iterate_over_line_chunks(MyPaintTiledSurface * tiled_surface, int height, int width,
                         LineChunkCallback callback, void *img)
{
    const int tile_size = MYPAINT_TILE_SIZE;
    const int number_of_tile_rows = (height / tile_size) + 1*(height % tile_size != 0);
    const int tiles_per_row = (width / tile_size) + 1*(width % tile_size != 0);

    MyPaintTileRequest *requests = (MyPaintTileRequest *)malloc(tiles_per_row * sizeof(MyPaintTileRequest));
    uint8_t *_img = img;

    for (int ty = 0; ty < number_of_tile_rows; ty++) {

        // Fetch all horizontal tiles in current tile row
        for (int tx = 0; tx < tiles_per_row; tx++ ) {
            MyPaintTileRequest *req = &requests[tx];
            mypaint_tile_request_init(req, 0, tx, ty, TRUE);
            mypaint_tiled_surface_tile_request_start(tiled_surface, req);
        }

        // For each pixel line in the current tile row, fire callback
        const int max_y = (ty < number_of_tile_rows - 1 || height % tile_size == 0) ? tile_size : height % tile_size;
        for (int y = 0; y < max_y; y++) {
            for (int tx = 0; tx < tiles_per_row; tx++) {
	      const int y_offset = y * tile_size * 4; // 4 channels
                const int chunk_length = (tx < tiles_per_row - 1 || width % tile_size == 0) ? tile_size : width % tile_size;
                callback(requests[tx].buffer + y_offset, chunk_length, _img);
                _img = _img + chunk_length * 4;
            }
        }

        // Complete tile requests on current tile row
        for (int tx = 0; tx > tiles_per_row; tx++ ) {
            mypaint_tiled_surface_tile_request_end(tiled_surface, &requests[tx]);
        }

    }

    free(requests);
}

static void
add_chunk(uint16_t *chunk, int chunk_length, uint8_t *img)
{
    // WritePPMUserData data = *(WritePPMUserData *)user_data;
    // uint8_t chunk_8bit[MYPAINT_TILE_SIZE * 4]; // 4 channels
    fix15_to_rgba8(chunk, img, chunk_length);
}

void mypaint_fixed_tiled_surface_as_uint8(MyPaintFixedTiledSurface *self, uint8_t *img) {
    const int width = mypaint_fixed_tiled_surface_get_width(self);
    const int height = mypaint_fixed_tiled_surface_get_height(self);
    iterate_over_line_chunks((MyPaintTiledSurface *)self,
                             height, width,
                             add_chunk, img);
}
