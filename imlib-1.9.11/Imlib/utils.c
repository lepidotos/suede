#define _GNU_SOURCE
#include <config.h>
#include "Imlib.h"
#include "Imlib_private.h"

#ifdef __EMX__
#include <io.h>
#include <process.h>
#endif

#ifndef HAVE_SNPRINTF
#define snprintf my_snprintf
#ifdef HAVE_STDARGS
int                 my_snprintf(char *str, size_t count, const char *fmt,...);

#else
int                 my_snprintf(va_alist);

#endif
#endif

void
calc_map_tables(ImlibData * id, ImlibImage * im)
{
  int                 i;
  double              g, b, c, ii, v;

  if (!im)
    return;

  g = ((double)im->mod.gamma) / 256;
  b = ((double)im->mod.brightness) / 256;
  c = ((double)im->mod.contrast) / 256;
  if (g < 0.01)
    g = 0.01;

  for (i = 0; i < 256; i++)
    {
      ii = ((double)i) / 256;
      v = ((ii - 0.5) * c) + 0.5 + (b - 1);
      if (v > 0)
	v = pow(((ii - 0.5) * c) + 0.5 + (b - 1), 1 / g) * 256;
      else
	v = 0;
      if (v > 255)
	v = 255;
      else if (v < 0)
	v = 0;
      im->rmap[i] = (unsigned char)v;
      im->gmap[i] = (unsigned char)v;
      im->bmap[i] = (unsigned char)v;
    }
  g = ((double)im->rmod.gamma) / 256;
  b = ((double)im->rmod.brightness) / 256;
  c = ((double)im->rmod.contrast) / 256;
  if (g < 0.01)
    g = 0.01;

  for (i = 0; i < 256; i++)
    {
      ii = ((double)im->rmap[i]) / 256;
      v = ((ii - 0.5) * c) + 0.5 + (b - 1);
      if (v > 0)
	v = pow(((ii - 0.5) * c) + 0.5 + (b - 1), 1 / g) * 256;
      else
	v = 0;
      if (v > 255)
	v = 255;
      else if (v < 0)
	v = 0;
      im->rmap[i] = (unsigned char)v;
    }
  g = ((double)im->gmod.gamma) / 256;
  b = ((double)im->gmod.brightness) / 256;
  c = ((double)im->gmod.contrast) / 256;
  if (g < 0.01)
    g = 0.01;

  for (i = 0; i < 256; i++)
    {
      ii = ((double)im->gmap[i]) / 256;
      v = ((ii - 0.5) * c) + 0.5 + (b - 1);
      if (v > 0)
	v = pow(((ii - 0.5) * c) + 0.5 + (b - 1), 1 / g) * 256;
      else
	v = 0;
      if (v > 255)
	v = 255;
      else if (v < 0)
	v = 0;
      im->gmap[i] = (unsigned char)v;
    }
  g = ((double)im->bmod.gamma) / 256;
  b = ((double)im->bmod.brightness) / 256;
  c = ((double)im->bmod.contrast) / 256;
  if (g < 0.01)
    g = 0.01;
  for (i = 0; i < 256; i++)
    {
      ii = ((double)im->bmap[i]) / 256;
      v = ((ii - 0.5) * c) + 0.5 + (b - 1);
      if (v > 0)
	v = pow(((ii - 0.5) * c) + 0.5 + (b - 1), 1 / g) * 256;
      else
	v = 0;
      if (v > 255)
	v = 255;
      else if (v < 0)
	v = 0;
      im->bmap[i] = (unsigned char)v;
    }
  dirty_pixmaps(id, im);
}

int
Imlib_load_file_to_pixmap(ImlibData * id, char *filename, Pixmap * pmap, Pixmap * mask)
{
  ImlibImage         *im;

  im = Imlib_load_image(id, filename);
  if (!im)
    {
      if (pmap)
	*pmap = 0;
      if (mask)
	*mask = 0;
      return 0;
    }
  if (!Imlib_render(id, im, im->rgb_width, im->rgb_height))
    {
      Imlib_destroy_image(id, im);
      if (pmap)
	*pmap = 0;
      if (mask)
	*mask = 0;
      return 0;
    }
  if (pmap)
    *pmap = Imlib_move_image(id, im);
  if (mask)
    *mask = Imlib_move_mask(id, im);
  Imlib_destroy_image(id, im);
  return 1;
}

void
Imlib_set_image_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod)
{
  if ((!im) | (!mod))
    return;
  if ((im->mod.gamma != mod->gamma) || (im->mod.brightness != mod->brightness) || (im->mod.contrast != mod->contrast))
    {
      im->mod.gamma = mod->gamma;
      im->mod.brightness = mod->brightness;
      im->mod.contrast = mod->contrast;
      calc_map_tables(id, im);
      if (im->pixmap)
	{
	  free_pixmappmap(id, im->pixmap);
	  im->pixmap = 0;
	}
      dirty_pixmaps(id, im);
    }
}

void
Imlib_set_image_red_modifier(ImlibData * id, ImlibImage * im, 
			     ImlibColorModifier * mod)
{
  if ((!im) | (!mod))
    return;
  if ((im->rmod.gamma != mod->gamma) || 
      (im->rmod.brightness != mod->brightness) || 
      (im->rmod.contrast != mod->contrast))
    {
      im->rmod.gamma = mod->gamma;
      im->rmod.brightness = mod->brightness;
      im->rmod.contrast = mod->contrast;
      calc_map_tables(id, im);
      if (im->pixmap)
	{
	  free_pixmappmap(id, im->pixmap);
	  im->pixmap = 0;
	}
      dirty_pixmaps(id, im);
    }
}

void
Imlib_set_image_green_modifier(ImlibData * id, ImlibImage * im, 
			       ImlibColorModifier * mod)
{
  if ((!im) | (!mod))
    return;
  if ((im->gmod.gamma != mod->gamma) || 
      (im->gmod.brightness != mod->brightness) ||
      (im->gmod.contrast != mod->contrast))
    {
      im->gmod.gamma = mod->gamma;
      im->gmod.brightness = mod->brightness;
      im->gmod.contrast = mod->contrast;
      calc_map_tables(id, im);
      if (im->pixmap)
	{
	  free_pixmappmap(id, im->pixmap);
	  im->pixmap = 0;
	}
      dirty_pixmaps(id, im);
    }
}

void
Imlib_set_image_blue_modifier(ImlibData * id, ImlibImage * im, 
			      ImlibColorModifier * mod)
{
  if ((!im) | (!mod))
    return;
  if ((im->bmod.gamma != mod->gamma) || 
      (im->bmod.brightness != mod->brightness) || 
      (im->bmod.contrast != mod->contrast))
    {
      im->bmod.gamma = mod->gamma;
      im->bmod.brightness = mod->brightness;
      im->bmod.contrast = mod->contrast;
      calc_map_tables(id, im);
      if (im->pixmap)
	{
	  free_pixmappmap(id, im->pixmap);
	  im->pixmap = 0;
	}
      dirty_pixmaps(id, im);
    }
}

void
Imlib_get_image_modifier(ImlibData * id, ImlibImage * im, 
			 ImlibColorModifier * mod)
{
  if ((!im) | (!mod))
    return;
  mod->gamma = im->mod.gamma;
  mod->brightness = im->mod.brightness;
  mod->contrast = im->mod.contrast;
  calc_map_tables(id, im);
}

void
Imlib_get_image_red_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod)
{
  if ((!im) | (!mod))
    return;
  mod->gamma = im->rmod.gamma;
  mod->brightness = im->rmod.brightness;
  mod->contrast = im->rmod.contrast;
}

void
Imlib_get_image_green_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod)
{
  if ((!im) | (!mod))
    return;
  mod->gamma = im->gmod.gamma;
  mod->brightness = im->gmod.brightness;
  mod->contrast = im->gmod.contrast;
}

void
Imlib_get_image_blue_modifier(ImlibData * id, ImlibImage * im, ImlibColorModifier * mod)
{
  if ((!im) | (!mod))
    return;
  mod->gamma = im->bmod.gamma;
  mod->brightness = im->bmod.brightness;
  mod->contrast = im->bmod.contrast;
}

void
Imlib_set_image_red_curve(ImlibData * id, ImlibImage * im, unsigned char *mod)
{
  int                 i;
  char                same = 1;

  if ((!im) || (!mod))
    return;
  
  for (i = 0; i < 256; i++)
    {
      if (im->rmap[i] != mod[i])
	{
	  same = 0;
	  break;
	}
    }
  if (same)
    return;
  
  if (im->pixmap)
    {
      free_pixmappmap(id, im->pixmap);
      im->pixmap = 0;
    }
  dirty_pixmaps(id, im);

  for (i = 0; i < 256; i++)
    im->rmap[i] = mod[i];

  im->mod.contrast = 257;
}

void
Imlib_set_image_green_curve(ImlibData * id, ImlibImage * im, unsigned char *mod)
{
  int                 i;
  char                same = 1;

  if ((!im) || (!mod))
    return;

  for (i = 0; i < 256; i++)
    {
      if (im->gmap[i] != mod[i])
	{
	  same = 0;
	  break;
	}
    }
  if (same)
    return;

  if (im->pixmap)
    {
      free_pixmappmap(id, im->pixmap);
      im->pixmap = 0;
    }
  dirty_pixmaps(id, im);
  for (i = 0; i < 256; i++)
    im->gmap[i] = mod[i];

  im->mod.contrast = 257;
}

void
Imlib_set_image_blue_curve(ImlibData * id, ImlibImage * im, unsigned char *mod)
{
  int                 i;
  char                same = 1;

  if ((!im) || (!mod))
    return;

  for (i = 0; i < 256; i++)
    {
      if (im->bmap[i] != mod[i])
	{
	  same = 0;
	  break;
	}
    }
  if (same)
    return;

  if (im->pixmap)
    {
      free_pixmappmap(id, im->pixmap);
      im->pixmap = 0;
    }
  dirty_pixmaps(id, im);
  for (i = 0; i < 256; i++)
    im->bmap[i] = mod[i];

  im->mod.contrast = 257;
}

void
Imlib_get_image_red_curve(ImlibData * id, ImlibImage * im, unsigned char *mod)
{
  int                 i;

  if ((!im) || (!mod))
    return;
  for (i = 0; i < 256; i++)
    mod[i] = im->rmap[i];
}

void
Imlib_get_image_green_curve(ImlibData * id, ImlibImage * im, unsigned char *mod)
{
  int                 i;

  if ((!im) || (!mod))
    return;
  for (i = 0; i < 256; i++)
    mod[i] = im->gmap[i];
}

void
Imlib_get_image_blue_curve(ImlibData * id, ImlibImage * im, unsigned char *mod)
{
  int                 i;

  if ((!im) || (!mod))
    return;
  for (i = 0; i < 256; i++)
    mod[i] = im->bmap[i];
}

void
Imlib_apply_modifiers_to_rgb(ImlibData * id, ImlibImage * im)
{
  int                 x, y;
  unsigned char      *ptr;

  if (!im)
    return;
  ptr = im->rgb_data;
  for (y = 0; y < im->rgb_height; y++)
    {
      for (x = 0; x < im->rgb_width; x++)
	{
	  *ptr = im->rmap[*ptr];
	  ptr++;
	  *ptr = im->gmap[*ptr];
	  ptr++;
	  *ptr = im->bmap[*ptr];
	  ptr++;
	}
    }
  im->mod.gamma = 256;
  im->mod.brightness = 256;
  im->mod.contrast = 256;
  im->rmod.gamma = 256;
  im->rmod.brightness = 256;
  im->rmod.contrast = 256;
  im->gmod.gamma = 256;
  im->gmod.brightness = 256;
  im->gmod.contrast = 256;
  im->bmod.gamma = 256;
  im->bmod.brightness = 256;
  im->bmod.contrast = 256;
  if (im->pixmap)
    {
      free_pixmappmap(id, im->pixmap);
      im->pixmap = 0;
    }
  dirty_pixmaps(id, im);
  calc_map_tables(id, im);
  dirty_images(id, im);
}

#define SHADE_PIXEL(pixel, dir, tmp) do {(tmp) = ((((double)pixel)/256.0) +\
 ((dir) ? 0.2 : -0.2)) * 256.0; \
 if ((tmp) > 255) (tmp) = 255; else if ((tmp) < 0) (tmp) = 0;} while (0)

void
Imlib_bevel_image(ImlibData *id, ImlibImage *im, ImlibBorder *bord, 
		  unsigned char up)
{
  register unsigned char *ptr;
  double v;
  int x, y, xbound, ybound;

  if ((!im) || (!bord))
    return;

  /* Left edge */
  ptr = im->rgb_data + 3 * im->rgb_width * bord->top;
  for (y = bord->top; y < im->rgb_height; y++)
    {
      xbound = im->rgb_height - y;
      if (xbound > bord->left)
	xbound = bord->left;
      for (x = 0; x < xbound; x++)
	{
	  SHADE_PIXEL(*ptr, up, v);
	  *ptr++ = (unsigned char) v;
	  SHADE_PIXEL(*ptr, up, v);
	  *ptr++ = (unsigned char) v;
	  SHADE_PIXEL(*ptr, up, v);
	  *ptr++ = (unsigned char) v;
	}
      ptr += 3 * (im->rgb_width - xbound);
    }

  /* Right edge */
  ybound = im->rgb_height - bord->bottom;
  for (ptr = im->rgb_data, y = 0; y < ybound; y++)
    {
      xbound = bord->right - y;
      if (xbound < 0)
	xbound = 0;
      ptr += 3 * (im->rgb_width - bord->right + xbound);
      for (x = xbound; x < bord->right; x++)
	{
	  SHADE_PIXEL(*ptr, !up, v);
	  *ptr++ = (unsigned char) v;
	  SHADE_PIXEL(*ptr, !up, v);
	  *ptr++ = (unsigned char) v;
	  SHADE_PIXEL(*ptr, !up, v);
	  *ptr++ = (unsigned char) v;
	}
    }

  /* Top edge */
  ptr = im->rgb_data;
  for (y = 0; y < bord->top; y++)
    {
      xbound = im->rgb_width - y;
      for (x = 0; x < xbound; x++)
	{
	  SHADE_PIXEL(*ptr, up, v);
	  *ptr++ = (unsigned char) v;
	  SHADE_PIXEL(*ptr, up, v);
	  *ptr++ = (unsigned char) v;
	  SHADE_PIXEL(*ptr, up, v);
	  *ptr++ = (unsigned char) v;
	}
      ptr += y * 3;
    }

  /* Bottom edge */
  ptr = im->rgb_data + ((im->rgb_height - bord->bottom) * im->rgb_width * 3);
  for (y = bord->bottom - 1; y >= 0; y--)
    {
      ptr += y * 3;
      for (x = y; x < im->rgb_width; x++)
	{
	  SHADE_PIXEL(*ptr, !up, v);
	  *ptr++ = (unsigned char) v;
	  SHADE_PIXEL(*ptr, !up, v);
	  *ptr++ = (unsigned char) v;
	  SHADE_PIXEL(*ptr, !up, v);
	  *ptr++ = (unsigned char) v;
	}
    }
}

#undef SHADE_PIXEL
#define SHADE_PIXEL(pixel, dir, tmp) do {(tmp) = ((((double)pixel)/depth_factor) + ((dir) ? 0.2 : -0.2)) * depth_factor; \
                                         if ((tmp) > (depth_factor-1)) (tmp) = depth_factor - 1; else if ((tmp) < 0) (tmp) = 0;} while (0)
#define MOD_PIXEL_LOW(x, y, up) do {v = XGetPixel(ximg, (x), (y)); r = (int) ctab[v & 0xff].r; g = (int) ctab[v & 0xff].g; b = (int) ctab[v & 0xff].b; \
	                            SHADE_PIXEL(r, (up), dv); r = (int) dv; SHADE_PIXEL(g, (up), dv); g = (int) dv; SHADE_PIXEL(b, (up), dv); b = (int) dv; \
	                            v = Imlib_best_color_match(id, &r, &g, &b); XPutPixel(ximg, (x), (y), v);} while (0)
#define MOD_PIXEL_HIGH(x, y, up) do {v = XGetPixel(ximg, (x), (y)); r = (int) ((v >> br) & mr); g = (int) ((v >> bg) & mg); b = (int) ((v << bb) & mb); \
	                             SHADE_PIXEL(r, (up), dv); r = (int) dv; SHADE_PIXEL(g, (up), dv); g = (int) dv; SHADE_PIXEL(b, (up), dv); b = (int) dv; \
	                             v = ((r & mr) << br) | ((g & mg) << bg) | ((b & mb) >> bb); XPutPixel(ximg, (x), (y), v);} while (0)

void
Imlib_bevel_pixmap(ImlibData *id, Pixmap p, int w, int h, ImlibBorder *bord, unsigned char up)
{

  XImage *ximg;
  register unsigned long i, v;
  double dv;
  short x, y, xbound, ybound;
  unsigned int r, g, b;
  ImlibColor ctab[256];
  int real_depth = 0, depth_factor;
  register int br, bg, bb;  /* Bitshifts */
  register unsigned int mr, mg, mb;  /* Bitmasks */
  XGCValues gcv;
  GC gc;

  if ((!id) || (!bord))
    return;

  depth_factor = 1 << id->x.depth;
  if (id->x.depth <= 8) {

    XColor cols[256];

    for (i = 0; i < depth_factor; i++) {
      cols[i].pixel = i;
      cols[i].flags = DoRed | DoGreen | DoBlue;
    }
    XQueryColors(id->x.disp, id->x.root_cmap, cols, depth_factor);
    for (i = 0; i < depth_factor; i++) {
      ctab[i].r = cols[i].red >> 8;
      ctab[i].g = cols[i].green >> 8;
      ctab[i].b = cols[i].blue >> 8;
      ctab[i].pixel = cols[i].pixel;
    }
  } else if (id->x.depth == 16) {

    XWindowAttributes xattr;

    XGetWindowAttributes(id->x.disp, id->x.root, &xattr);
    if ((xattr.visual->red_mask == 0x7c00) && (xattr.visual->green_mask == 0x3e0) && (xattr.visual->blue_mask == 0x1f)) {
      real_depth = 15;
      depth_factor = 1 << 15;
    }
  }
  if (!real_depth) {
    real_depth = id->x.depth;
  }
  ximg = XGetImage(id->x.disp, p, 0, 0, w, h, -1, ZPixmap);
  if (ximg == NULL) {
    return;
  }
  if (id->x.depth <= 8) {
    /* Left edge */
    for (y = bord->top; y < h; y++)
      {
	xbound = h - y;
	if (xbound > bord->left)
	  xbound = bord->left;
	for (x = 0; x < xbound; x++)
	  {
	    MOD_PIXEL_LOW(x, y, up);
	  }
      }

    /* Right edge */
    ybound = h - bord->bottom;
    for (y = 0; y < ybound; y++)
      {
	xbound = bord->right - y;
	if (xbound < 0)
	  xbound = 0;
	for (x = xbound; x < bord->right; x++)
	  {
	    MOD_PIXEL_LOW(x + (w - bord->right), y, !up);
	  }
      }

    /* Top edge */
    for (y = 0; y < bord->top; y++)
      {
	xbound = w - y;
	for (x = 0; x < xbound; x++)
	  {
	    MOD_PIXEL_LOW(x, y, up);
	  }
      }

    /* Bottom edge */
    for (y = h - bord->bottom; y < h; y++)
      {
	for (x = h - y - 1; x < w; x++)
	  {
	    MOD_PIXEL_LOW(x, y, !up);
	  }
      }
  } else {
    /* Determine bitshift and bitmask values */
    switch (real_depth) {
      case 15:
	br = 7;
	bg = 2;
	bb = 3;
	mr = mg = mb = 0xf8;
	break;
      case 16:
	br = 8;
	bg = bb = 3;
	mr = mb = 0xf8;
	mg = 0xfc;
	break;
      case 24:
      case 32:
	br = 16;
	bg = 8;
	bb = 0;
	mr = mg = mb = 0xff;
	break;
      default:
	return;
    }

    /* Left edge */
    for (y = bord->top; y < h; y++)
      {
	xbound = h - y;
	if (xbound > bord->left)
	  xbound = bord->left;
	for (x = 0; x < xbound; x++)
	  {
	    MOD_PIXEL_HIGH(x, y, up);
	  }
      }

    /* Right edge */
    ybound = h - bord->bottom;
    for (y = 0; y < ybound; y++)
      {
	xbound = bord->right - y;
	if (xbound < 0)
	  xbound = 0;
	for (x = xbound; x < bord->right; x++)
	  {
	    MOD_PIXEL_HIGH(x + (w - bord->right), y, !up);
	  }
      }

    /* Top edge */
    for (y = 0; y < bord->top; y++)
      {
	xbound = w - y;
	for (x = 0; x < xbound; x++)
	  {
	    MOD_PIXEL_HIGH(x, y, up);
	  }
      }

    /* Bottom edge */
    for (y = h - bord->bottom; y < h; y++)
      {
	for (x = h - y - 1; x < w; x++)
	  {
	    MOD_PIXEL_HIGH(x, y, !up);
	  }
      }
  }
  gc = XCreateGC(id->x.disp, p, 0, &gcv);
  XPutImage(id->x.disp, p, gc, ximg, 0, 0, 0, 0, w, h);
  XFreeGC(id->x.disp, gc);
  XDestroyImage(ximg);
}

void
Imlib_crop_image(ImlibData * id, ImlibImage * im, int x, int y, int w, int h)
{
  unsigned char      *data;
  int                 xx, yy, w3, w4;
  unsigned char      *ptr1, *ptr2;

  if (!im)
    return;
  if (x < 0)
    {
      w += x;
      x = 0;
    }
  if (y < 0)
    {
      h += y;
      y = 0;
    }
  if (x >= im->rgb_width)
    return;
  if (y >= im->rgb_height)
    return;
  if (w <= 0)
    return;
  if (h <= 0)
    return;
  if (x + w > im->rgb_width)
    w = im->rgb_width - x;
  if (y + h > im->rgb_height)
    h = im->rgb_height - y;
  if (w <= 0)
    return;
  if (h <= 0)
    return;

  w3 = im->rgb_width * 3;
  w4 = (im->rgb_width - w) * 3;
  data = malloc(w * h * 3);
  if (data == NULL)
    return;
  ptr1 = im->rgb_data + (y * w3) + (x * 3);
  ptr2 = data;
  for (yy = 0; yy < h; yy++)
    {
      for (xx = 0; xx < w; xx++)
	{
	  *ptr2++ = *ptr1++;
	  *ptr2++ = *ptr1++;
	  *ptr2++ = *ptr1++;
	}
      ptr1 += w4;
    }
  free(im->rgb_data);
  im->rgb_data = data;
  if (im->border.left > x)
    im->border.left = im->border.left - x;
  else
    im->border.left = 0;
  if (im->border.top > y)
    im->border.top = im->border.top - y;
  else
    im->border.top = 0;
  if (im->rgb_width - im->border.right < x + w)
    im->border.right = im->border.right - (im->rgb_width - (x + w));
  else
    im->border.right = 0;
  if (im->rgb_height - im->border.bottom < y + h)
    im->border.bottom = im->border.bottom - (im->rgb_height - (y + h));
  else
    im->border.bottom = 0;
  im->rgb_width = w;
  im->rgb_height = h;
  dirty_images(id, im);
  dirty_pixmaps(id, im);
}

ImlibImage         *
Imlib_crop_and_clone_image(ImlibData * id, ImlibImage * im, int x, int y, int w, int h)
{
  unsigned char      *data;
  int                 xx, yy, w3, w4;
  unsigned char      *ptr1, *ptr2;
  ImlibImage         *im2;
  char               *s;

  if (!im)
    return NULL;
  im2 = malloc(sizeof(ImlibImage));
  if (!im2)
    return NULL;

  if (x < 0)
    {
      w += x;
      x = 0;
    }
  if (y < 0)
    {
      h += y;
      y = 0;
    }
  if (x >= im->rgb_width)
    return NULL;
  if (y >= im->rgb_height)
    return NULL;
  if (w <= 0)
    return NULL;
  if (h <= 0)
    return NULL;
  if (x + w > im->rgb_width)
    w = im->rgb_width - x;
  if (y + h > im->rgb_height)
    h = im->rgb_height - y;
  if (w <= 0)
    return NULL;
  if (h <= 0)
    return NULL;

  w3 = im->rgb_width * 3;
  w4 = (im->rgb_width - w) * 3;
  data = malloc(w * h * 3);
  if (data == NULL)
    return NULL;
  ptr1 = im->rgb_data + (y * w3) + (x * 3);
  ptr2 = data;
  for (yy = 0; yy < h; yy++)
    {
      for (xx = 0; xx < w; xx++)
	{
	  *ptr2++ = *ptr1++;
	  *ptr2++ = *ptr1++;
	  *ptr2++ = *ptr1++;
	}
      ptr1 += w4;
    }
  if (im->border.left > x)
    im2->border.left = im->border.left - x;
  else
    im2->border.left = 0;
  if (im->border.top > y)
    im2->border.top = im->border.top - y;
  else
    im2->border.top = 0;
  if (im->rgb_width - im->border.right < x + w)
    im2->border.right = im->border.right - (im->rgb_width - (x + w));
  else
    im2->border.right = 0;
  if (im->rgb_height - im->border.bottom < y + h)
    im2->border.bottom = im->border.bottom - (im->rgb_height - (y + h));
  else
    im2->border.bottom = 0;
  im2->rgb_data = data;
  im2->rgb_width = w;
  im2->rgb_height = h;
  im2->alpha_data = NULL;
  s = malloc(strlen(im->filename) + 320);
  if (s)
    {
      snprintf(s, sizeof(s), "%s_%x_%x", im->filename, (int)time(NULL), (int)rand());
      im2->filename = malloc(strlen(s) + 1);
      if (im2->filename)
	strcpy(im2->filename, s);
      free(s);
    }
  else
    im2->filename = NULL;
  im2->width = 0;
  im2->height = 0;
  im2->shape_color.r = im->shape_color.r;
  im2->shape_color.g = im->shape_color.g;
  im2->shape_color.b = im->shape_color.b;
  im2->pixmap = 0;
  im2->shape_mask = 0;
  im2->cache = 1;
  im2->mod.gamma = im->mod.gamma;
  im2->mod.brightness = im->mod.brightness;
  im2->mod.contrast = im->mod.contrast;
  im2->rmod.gamma = im->rmod.gamma;
  im2->rmod.brightness = im->rmod.brightness;
  im2->rmod.contrast = im->rmod.contrast;
  im2->gmod.gamma = im->gmod.gamma;
  im2->gmod.brightness = im->gmod.brightness;
  im2->gmod.contrast = im->gmod.contrast;
  im2->bmod.gamma = im->bmod.gamma;
  im2->bmod.brightness = im->bmod.brightness;
  im2->bmod.contrast = im->bmod.contrast;
  calc_map_tables(id, im2);
  if (id->cache.on_image)
    add_image(id, im2, im2->filename);
  return im2;
}

void
Imlib_changed_image(ImlibData * id, ImlibImage * im)
{
  if (!im)
    return;
  dirty_images(id, im);
  if (im->pixmap)
    {
      free_pixmappmap(id, im->pixmap);
      im->pixmap = 0;
    }
  dirty_pixmaps(id, im);
}

void
Imlib_apply_image(ImlibData * id, ImlibImage * im, Window p)
{
  Pixmap              pp, mm;
  int                 x, y;
  unsigned int        w, h, bd, d;
  Window              ww;

  if ((!im) || (!p))
    return;
  XGetGeometry(id->x.disp, p, &ww, &x, &y, &w, &h, &bd, &d);
  if ((w <= 0) || (h <= 0))
    return;
  Imlib_render(id, im, w, h);
  pp = Imlib_move_image(id, im);
  mm = Imlib_move_mask(id, im);
  XSetWindowBackgroundPixmap(id->x.disp, p, pp);
  if (mm)
    XShapeCombineMask(id->x.disp, p, ShapeBounding, 0, 0, mm, ShapeSet);
  else
    XShapeCombineMask(id->x.disp, p, ShapeBounding, 0, 0, 0, ShapeSet);
  XClearWindow(id->x.disp, p);
  Imlib_free_pixmap(id, pp);
}

void
Imlib_paste_image(ImlibData * id, ImlibImage * im, Window p, int x, int y, int w, int h)
{
  GC                  gc;
  XGCValues           gcv;
  Pixmap              pp, mm;

  if ((!im) || (!p))
    return;
  if ((w <= 0) || (h <= 0))
    return;
  gc = XCreateGC(id->x.disp, p, 0, &gcv);
  Imlib_render(id, im, w, h);
  pp = Imlib_move_image(id, im);
  mm = Imlib_move_mask(id, im);
  if (mm)
    {
      XSetClipMask(id->x.disp, gc, mm);
      XSetClipOrigin(id->x.disp, gc, x, y);
    }
  XCopyArea(id->x.disp, pp, p, gc, 0, 0, w, h, x, y);
  Imlib_free_pixmap(id, pp);
  XFreeGC(id->x.disp, gc);
}

void
Imlib_paste_image_border(ImlibData * id, ImlibImage * im, Window p, int x, int y, int w, int h)
{
  GC                  gc;
  XGCValues           gcv;
  Pixmap              pp, mm;

  if (!im)
    return;
  if ((w <= 0) || (h <= 0))
    return;
  gc = XCreateGC(id->x.disp, p, 0, &gcv);
  Imlib_render(id, im, w, h);
  pp = Imlib_move_image(id, im);
  mm = Imlib_move_mask(id, im);
  if (mm)
    {
      XSetClipMask(id->x.disp, gc, mm);
      XSetClipOrigin(id->x.disp, gc, x, y);
    }
  if ((w <= (im->border.left + im->border.right)) ||
      (h <= (im->border.top + im->border.bottom)))
    XCopyArea(id->x.disp, pp, p, gc, 0, 0, w, h, x, y);
  else
    {
      XCopyArea(id->x.disp, pp, p, gc,
		0, 0,
		x, y,
		w, im->border.top);
      XCopyArea(id->x.disp, pp, p, gc,
		0, h - im->border.bottom,
		x, y + (h - im->border.bottom),
		w, im->border.bottom);
      XCopyArea(id->x.disp, pp, p, gc,
		0, im->border.top,
		x, y + im->border.top,
		im->border.left, h - (im->border.top + im->border.bottom));
      XCopyArea(id->x.disp, pp, p, gc,
		w - im->border.right, im->border.top,
		x + (w - im->border.right), y + im->border.top,
		im->border.right, h - (im->border.top + im->border.bottom));
    }
  Imlib_free_pixmap(id, pp);
  XFreeGC(id->x.disp, gc);
}

void
Imlib_flip_image_horizontal(ImlibData * id, ImlibImage * im)
{
  unsigned char      *ptr1, *ptr2, r, rr;
  int                 x, y;
  int                 w3;

  if (!im)
    return;
  w3 = im->rgb_width * 3;
  for (y = 0; y < im->rgb_height; y++)
    {
      ptr1 = im->rgb_data + (y * w3);
      ptr2 = im->rgb_data + (y * w3) + w3 - 3;
      for (x = 0; x < im->rgb_width >> 1; x++)
	{
	  r = *ptr1;
	  rr = *ptr2;
	  *ptr2++ = r;
	  *ptr1++ = rr;
	  r = *ptr1;
	  rr = *ptr2;
	  *ptr2++ = r;
	  *ptr1++ = rr;
	  r = *ptr1;
	  rr = *ptr2;
	  *ptr2 = r;
	  *ptr1++ = rr;
	  ptr2 -= 5;
	}
    }
  w3 = im->border.left;
  im->border.left = im->border.right;
  im->border.right = w3;
  dirty_images(id, im);
  if (im->pixmap)
    {
      free_pixmappmap(id, im->pixmap);
      im->pixmap = 0;
    }
  dirty_pixmaps(id, im);
}

void
Imlib_flip_image_vertical(ImlibData * id, ImlibImage * im)
{
  unsigned char      *ptr1, *ptr2, r, rr;
  int                 x, y, yy;
  int                 w3;

  if (!im)
    return;
  w3 = im->rgb_width * 3;
  for (yy = im->rgb_height - 1, y = 0; y < im->rgb_height >> 1; y++, yy--)
    {
      ptr1 = im->rgb_data + (y * w3);
      ptr2 = im->rgb_data + (yy * w3);
      for (x = 0; x < im->rgb_width; x++)
	{
	  r = *ptr1;
	  rr = *ptr2;
	  *ptr2++ = r;
	  *ptr1++ = rr;
	  r = *ptr1;
	  rr = *ptr2;
	  *ptr2++ = r;
	  *ptr1++ = rr;
	  r = *ptr1;
	  rr = *ptr2;
	  *ptr2++ = r;
	  *ptr1++ = rr;
	}
    }
  w3 = im->border.top;
  im->border.top = im->border.bottom;
  im->border.bottom = w3;
  dirty_images(id, im);
  if (im->pixmap)
    {
      free_pixmappmap(id, im->pixmap);
      im->pixmap = 0;
    }
  dirty_pixmaps(id, im);
}

void
Imlib_rotate_image(ImlibData * id, ImlibImage * im, int d)
{
  unsigned char      *data;
  int                 x, y, w3, w4;
  unsigned char      *ptr1, *ptr2;

  if (!im)
    return;
  w3 = im->rgb_width * 3;
  w4 = im->rgb_height * 3;
  data = malloc(im->rgb_width * im->rgb_height * 3);
  if (data == NULL)
    return;
  for (y = 0; y < im->rgb_height; y++)
    {
      ptr1 = im->rgb_data + (y * w3);
      ptr2 = data + (y * 3);
      for (x = 0; x < im->rgb_width; x++)
	{
	  *ptr2++ = *ptr1++;
	  *ptr2++ = *ptr1++;
	  *ptr2 = *ptr1++;
	  ptr2 += w4 - 2;
	}
    }
  free(im->rgb_data);
  im->rgb_data = data;
  w3 = im->rgb_width;
  im->rgb_width = im->rgb_height;
  im->rgb_height = w3;
  w3 = im->border.top;
  im->border.top = im->border.left;
  im->border.left = w3;
  w3 = im->border.bottom;
  im->border.bottom = im->border.right;
  im->border.right = w3;
  dirty_images(id, im);
  if (im->pixmap)
    {
      free_pixmappmap(id, im->pixmap);
      im->pixmap = 0;
    }
  dirty_pixmaps(id, im);
}

ImlibImage         *
Imlib_create_image_from_data(ImlibData * id, unsigned char *data, unsigned char *alpha, int w, int h)
{
  ImlibImage         *im;
  char                s[1024];

  if ((!data) || (w <= 0) || (h <= 0))
    return NULL;
  im = malloc(sizeof(ImlibImage));
  if (!im)
    return NULL;
  im->rgb_width = w;
  im->rgb_height = h;
  im->rgb_data = malloc(im->rgb_width * im->rgb_height * 3);
  if (!im->rgb_data)
    {
      free(im);
      return NULL;
    }
  memcpy(im->rgb_data, data, im->rgb_width * im->rgb_height * 3);
/*   im->alpha_data=alpha; */
  im->alpha_data = NULL;
  snprintf(s, sizeof(s), "creat_%x_%x", (int)time(NULL), (int)rand());
  im->filename = malloc(strlen(s) + 1);
  if (im->filename)
    strcpy(im->filename, s);
  im->width = 0;
  im->height = 0;
  im->shape_color.r = -1;
  im->shape_color.g = -1;
  im->shape_color.b = -1;
  im->border.left = 0;
  im->border.right = 0;
  im->border.top = 0;
  im->border.bottom = 0;
  im->pixmap = 0;
  im->shape_mask = 0;
  im->cache = 1;
  im->mod.gamma = id->mod.gamma;
  im->mod.brightness = id->mod.brightness;
  im->mod.contrast = id->mod.contrast;
  im->rmod.gamma = id->rmod.gamma;
  im->rmod.brightness = id->rmod.brightness;
  im->rmod.contrast = id->rmod.contrast;
  im->gmod.gamma = id->gmod.gamma;
  im->gmod.brightness = id->gmod.brightness;
  im->gmod.contrast = id->gmod.contrast;
  im->bmod.gamma = id->bmod.gamma;
  im->bmod.brightness = id->bmod.brightness;
  im->bmod.contrast = id->bmod.contrast;
  if (id->cache.on_image)
    add_image(id, im, im->filename);
  calc_map_tables(id, im);
  return im;
}

ImlibImage         *
Imlib_clone_image(ImlibData * id, ImlibImage * im)
{
  ImlibImage         *im2;
  char               *s;

  if (!im)
    return NULL;
  im2 = malloc(sizeof(ImlibImage));
  if (!im2)
    return NULL;
  im2->rgb_width = im->rgb_width;
  im2->rgb_height = im->rgb_height;
  im2->rgb_data = malloc(im2->rgb_width * im2->rgb_height * 3);
  if (!im2->rgb_data)
    {
      free(im2);
      return NULL;
    }
  memcpy(im2->rgb_data, im->rgb_data, im2->rgb_width * im2->rgb_height * 3);
  if (im->alpha_data)
    {
      im2->alpha_data = malloc(im2->rgb_width * im2->rgb_height);
      if (!im2->alpha_data)
	{
	  free(im2->rgb_data);
	  free(im2);
	  return NULL;
	}
      memcpy(im2->alpha_data, im->alpha_data, im2->rgb_width * im2->rgb_height);
    }
  else
    im2->alpha_data = NULL;
  s = malloc(strlen(im->filename) + 320);
  if (s)
    {
      snprintf(s, sizeof(s), "%s_%x_%x", im->filename, (int)time(NULL), (int)rand());
      im2->filename = malloc(strlen(s) + 1);
      if (im2->filename)
	strcpy(im2->filename, s);
      free(s);
    }
  else
    im2->filename = NULL;
  im2->width = 0;
  im2->height = 0;
  im2->shape_color.r = im->shape_color.r;
  im2->shape_color.g = im->shape_color.g;
  im2->shape_color.b = im->shape_color.b;
  im2->border.left = im->border.left;
  im2->border.right = im->border.right;
  im2->border.top = im->border.top;
  im2->border.bottom = im->border.bottom;
  im2->pixmap = 0;
  im2->shape_mask = 0;
  im2->cache = 1;
  im2->mod.gamma = im->mod.gamma;
  im2->mod.brightness = im->mod.brightness;
  im2->mod.contrast = im->mod.contrast;
  im2->rmod.gamma = im->rmod.gamma;
  im2->rmod.brightness = im->rmod.brightness;
  im2->rmod.contrast = im->rmod.contrast;
  im2->gmod.gamma = im->gmod.gamma;
  im2->gmod.brightness = im->gmod.brightness;
  im2->gmod.contrast = im->gmod.contrast;
  im2->bmod.gamma = im->bmod.gamma;
  im2->bmod.brightness = im->bmod.brightness;
  im2->bmod.contrast = im->bmod.contrast;
  calc_map_tables(id, im2);
  if (id->cache.on_image)
    add_image(id, im2, im2->filename);
  return im2;
}

ImlibImage         *
Imlib_clone_scaled_image(ImlibData * id, ImlibImage * im, int w, int h)
{
  ImlibImage         *im2;
  char               *s;

  if ((!im) || (w <= 0) || (h <= 0))
    return NULL;

  im2 = malloc(sizeof(ImlibImage));
  if (!im2)
    return NULL;
  im2->rgb_width = w;
  im2->rgb_height = h;
  im2->rgb_data = malloc(w * h * 3);
  if (!im2->rgb_data)
    {
      free(im2);
      return NULL;
    }
  {
    int                 x, y, *xarray;
    unsigned char     **yarray, *ptr, *ptr2, *ptr22;
    int                 l, r, m, pos, inc, w3;

    xarray = malloc(sizeof(int) * w);

    if (!xarray)
      {
	fprintf(stderr, "ERROR: Cannot allocate X co-ord buffer\n");
	free(im2->rgb_data);
	free(im2);
	return NULL;
      }
    yarray = malloc(sizeof(unsigned char *) * h);

    if (!yarray)
      {
	fprintf(stderr, "ERROR: Cannot allocate Y co-ord buffer\n");
	free(xarray);
	free(im2->rgb_data);
	free(im2);
	return NULL;
      }
    ptr22 = im->rgb_data;
    w3 = im->rgb_width * 3;
    inc = 0;
    if (w < im->border.left + im->border.right)
      {
	l = w >> 1;
	r = w - l;
	m = 0;
      }
    else
      {
	l = im->border.left;
	r = im->border.right;
	m = w - l - r;
      }
    if (m > 0)
      inc = ((im->rgb_width - im->border.left - im->border.right) << 16) / m;
    pos = 0;
    if (l)
      for (x = 0; x < l; x++)
	{
	  xarray[x] = (pos >> 16) + (pos >> 16) + (pos >> 16);
	  pos += 0x10000;
	}
    if (m)
      {
	for (x = l; x < l + m; x++)
	  {
	    xarray[x] = (pos >> 16) + (pos >> 16) + (pos >> 16);
	    pos += inc;
	  }
      }
    pos = (im->rgb_width - r) << 16;
    for (x = w - r; x < w; x++)
      {
	xarray[x] = (pos >> 16) + (pos >> 16) + (pos >> 16);
	pos += 0x10000;
      }

    if (h < im->border.top + im->border.bottom)
      {
	l = h >> 1;
	r = h - l;
	m = 0;
      }
    else
      {
	l = im->border.top;
	r = im->border.bottom;
	m = h - l - r;
      }
    if (m > 0)
      inc = ((im->rgb_height - im->border.top - im->border.bottom) << 16) / m;
    pos = 0;
    for (x = 0; x < l; x++)
      {
	yarray[x] = ptr22 + ((pos >> 16) * w3);
	pos += 0x10000;
      }
    if (m)
      {
	for (x = l; x < l + m; x++)
	  {
	    yarray[x] = ptr22 + ((pos >> 16) * w3);
	    pos += inc;
	  }
      }
    pos = (im->rgb_height - r) << 16;
    for (x = h - r; x < h; x++)
      {
	yarray[x] = ptr22 + ((pos >> 16) * w3);
	pos += 0x10000;
      }

    ptr = im2->rgb_data;
    for (y = 0; y < h; y++)
      {
	for (x = 0; x < w; x++)
	  {
	    ptr2 = yarray[y] + xarray[x];
	    *ptr++ = (int)*ptr2++;
	    *ptr++ = (int)*ptr2++;
	    *ptr++ = (int)*ptr2;
	  }
      }
  }
  if (im->alpha_data)
    {
      im2->alpha_data = NULL;
/* yet to be filled in */
    }
  else
    im2->alpha_data = NULL;
  s = malloc(strlen(im->filename) + 320);
  if (s)
    {
      snprintf(s, sizeof(s), "%s_%x_%x_%x_%x", im->filename, (int)time(NULL), w, h, (int)rand());
      im2->filename = malloc(strlen(s) + 1);
      if (im2->filename)
	strcpy(im2->filename, s);
      free(s);
    }
  else
    im2->filename = NULL;
  im2->width = 0;
  im2->height = 0;
  im2->shape_color.r = im->shape_color.r;
  im2->shape_color.g = im->shape_color.g;
  im2->shape_color.b = im->shape_color.b;
  im2->border.left = im->border.left;
  im2->border.right = im->border.right;
  im2->border.top = im->border.top;
  im2->border.bottom = im->border.bottom;
  im2->pixmap = 0;
  im2->shape_mask = 0;
  im2->cache = 1;
  im2->mod.gamma = im->mod.gamma;
  im2->mod.brightness = im->mod.brightness;
  im2->mod.contrast = im->mod.contrast;
  im2->rmod.gamma = im->rmod.gamma;
  im2->rmod.brightness = im->rmod.brightness;
  im2->rmod.contrast = im->rmod.contrast;
  im2->gmod.gamma = im->gmod.gamma;
  im2->gmod.brightness = im->gmod.brightness;
  im2->gmod.contrast = im->gmod.contrast;
  im2->bmod.gamma = im->bmod.gamma;
  im2->bmod.brightness = im->bmod.brightness;
  im2->bmod.contrast = im->bmod.contrast;
  calc_map_tables(id, im2);
  if (id->cache.on_image)
    add_image(id, im2, im2->filename);
  return im2;
}

ImlibImage         *
Imlib_create_image_from_xpm_data(ImlibData * id, char **data)
{
  ImlibImage         *im;
  unsigned char      *ptr, *end;
  int                 c, i, j, k, ncolors, cpp, comment, transp, quote, context,
                      len, count, done;
  int                 w, h;
  char               *line, s[256], tok[128], col[256];
  XColor              xcol;
  struct _cmap
    {
      char                str[6];
      char                transp;
      short                 r, g, b;
    }
                     *cmap;
  short                 lookup[128 - 32][128 - 32];

  if (!data)
    return NULL;
  if (! id->x.disp)
    return NULL;
  im = malloc(sizeof(ImlibImage));
  if (!im)
    return NULL;
  count = 0;
  transp = 0;
  done = 0;

  j = 0;
  cmap = NULL;
  c = ' ';
  comment = 0;
  quote = 0;
  context = 0;
  ptr = NULL;
  end = NULL;

  while (!done)
    {
      line = data[count++];
      if (context == 0)
	{
	  /* Header */
	  sscanf(line, "%i %i %i %i", &w, &h, &ncolors, &cpp);
	  if (ncolors > 32766)
	    {
	      fprintf(stderr, "IMLIB ERROR: XPM data wth colors > 32766 not supported\n");
	      free(im);
	      return NULL;
	    }
	  if (cpp > 5)
	    {
	      fprintf(stderr, "IMLIB ERROR: XPM data with characters per pixel > 5 not supported\n");
	      free(im);
	      return NULL;
	    }
	  if (w > 32767)
	    {
	      fprintf(stderr, "IMLIB ERROR: Image width > 32767 pixels for data\n");
	      free(im);
	      return NULL;
	    }
	  if (h > 32767)
	    {
	      fprintf(stderr, "IMLIB ERROR: Image height > 32767 pixels for data\n");
	      free(im);
	      return NULL;
	    }
	  cmap = malloc(sizeof(struct _cmap) * ncolors);

	  if (!cmap)
	    {
	      free(im);
	      return NULL;
	    }
	  im->rgb_width = w;
	  im->rgb_height = h;
	  im->rgb_data = malloc(im->rgb_width * im->rgb_height * 3);
	  if (!im->rgb_data)
	    {
	      free(cmap);
	      free(im);
	      return NULL;
	    }
	  im->alpha_data = NULL;
          snprintf (s, sizeof (s), "creat_%lx_%x", time(NULL), rand());
	  im->filename = strdup(s);
	  im->width = 0;
	  im->height = 0;
	  im->border.left = 0;
	  im->border.right = 0;
	  im->border.top = 0;
	  im->border.bottom = 0;
	  im->pixmap = 0;
	  im->shape_mask = 0;
	  im->cache = 1;
	  im->mod.gamma = id->mod.gamma;
	  im->mod.brightness = id->mod.brightness;
	  im->mod.contrast = id->mod.contrast;
	  im->rmod.gamma = id->rmod.gamma;
	  im->rmod.brightness = id->rmod.brightness;
	  im->rmod.contrast = id->rmod.contrast;
	  im->gmod.gamma = id->gmod.gamma;
	  im->gmod.brightness = id->gmod.brightness;
	  im->gmod.contrast = id->gmod.contrast;
	  im->bmod.gamma = id->bmod.gamma;
	  im->bmod.brightness = id->bmod.brightness;
	  im->bmod.contrast = id->bmod.contrast;
	  ptr = im->rgb_data;
	  end = im->rgb_data + (im->rgb_width * im->rgb_height * 3);
	  j = 0;
	  context++;
	}
      else if (context == 1)
	{
          int                 colptr;
	  int                 hascolor, iscolor;
	  
	  /* Color Table */
	  if (j < ncolors)
	    {
	      iscolor = 0;
	      hascolor = 0;
	      tok[0] = 0;
	      col[0] = 0;
	      s[0] = 0;
	      colptr = 0;
	      len = strlen(line);
	      strncpy(cmap[j].str, line, cpp);
	      cmap[j].str[cpp] = 0;
	      cmap[j].r = -1;
	      cmap[j].transp = 0;
	      for (k = cpp; k < len; k++)
		{
		  if (line[k] != ' ')
		    {
		      sscanf(&line[k], "%65536s", s);
		      k += strlen(s);
		      if (!strcmp(s, "c"))
			iscolor = 1;
		      if ((!strcmp(s, "m")) || (!strcmp(s, "s")) ||
			  (!strcmp(s, "g4")) || (!strcmp(s, "g")) ||
			  (!strcmp(s, "c")) || (k >= len))
			{
			  if (k >= len)
			    {
			      int                 ls;
			      
			      ls = strlen(s);
			      
			      if (col[0] && colptr < sizeof(col))
				{
				  strcpy(col + colptr, " ");
				  colptr++;
				}
			      if (colptr + ls <= sizeof(col))
				{
				  strcpy(col + colptr, s);
				  colptr += ls;
				}
			      
			    }
			  if (col[0])
			    {
			      if (!strcasecmp(col, "none"))
				{
				  transp = 1;
				  cmap[j].transp = 1;
				}
			      else
				{
				  if ((((cmap[j].r < 0) ||
					(!strcmp(tok, "c"))) &&
				       (!hascolor)))
				    {
				      XParseColor(id->x.disp,
						  id->x.root_cmap,
						  col, &xcol);
				      cmap[j].r = xcol.red >> 8;
				      cmap[j].g = xcol.green >> 8;
				      cmap[j].b = xcol.blue >> 8;
				      if ((cmap[j].r == 255) &&
					  (cmap[j].g == 0) &&
					  (cmap[j].b == 255))
					cmap[j].r = 254;
				      if (iscolor)
					hascolor = 1;
				    }
				}
			    }
			  if (strlen(s) < sizeof(tok))
			    strcpy(tok, s);
			  col[0] = 0;
			}
		      else
			{
			  int                 ls;
			  
			  ls = strlen(s);
			  if (col[0] && colptr < sizeof(col))
			    {
			      strcpy(col + colptr, " ");
			      colptr++;
			    }
			  if (ls + colptr < sizeof(col))
			    {
			      strcpy(col + colptr, s);
			      colptr += ls;
			    }
			}
		    }
		}
	    }
	  j++;
	  if (j >= ncolors)
	    {
	      if (cpp == 1)
		for (i = 0; i < ncolors; i++)
		  lookup[(int)cmap[i].str[0] - 32][0] = i;
	      else if (cpp == 2)
		for (i = 0; i < ncolors; i++)
		  lookup[(int)cmap[i].str[0] - 32][(int)cmap[i].str[1] - 32] = i;
	      context++;
	    }
	}
      else
	{
	  /* Image Data */
	  i = 0;
	  if (cpp == 0)
	    {
	    }
	  else if (cpp == 1)
	    {
	      if (transp)
		{
		  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
		    {
		      col[0] = line[i];
		      if (cmap[lookup[(int)col[0] - 32][0]].transp)
			{
			  *ptr++ = 255;
			  *ptr++ = 0;
			  *ptr++ = 255;
			}
		      else
			{
			  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].r;
			  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].g;
			  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].b;
			}
		    }
		}
	      else
		{
		  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
		    {
		      col[0] = line[i];
		      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].r;
		      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].g;
		      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][0]].b;
		    }
		}
	    }
	  else if (cpp == 2)
	    {
	      if (transp)
		{
		  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
		    {
		      col[0] = line[i++];
		      col[1] = line[i];
		      if (cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].transp)
			{
			  *ptr++ = 255;
			  *ptr++ = 0;
			  *ptr++ = 255;
			}
		      else
			{
			  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].r;
			  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].g;
			  *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].b;
			}
		    }
		}
	      else
		{
		  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
		    {
		      col[0] = line[i++];
		      col[1] = line[i];
		      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].r;
		      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].g;
		      *ptr++ = (unsigned char)cmap[lookup[(int)col[0] - 32][(int)col[1] - 32]].b;
		    }
		}
	    }
	  else
	    {
	      if (transp)
		{
		  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
		    {
		      for (j = 0; j < cpp; j++, i++)
			{
			  col[j] = line[i];
			}
		      col[j] = 0;
		      i--;
		      for (j = 0; j < ncolors; j++)
			{
			  if (!strcmp(col, cmap[j].str))
			    {
			      if (cmap[j].transp)
				{
				  *ptr++ = 255;
				  *ptr++ = 0;
				  *ptr++ = 255;
				}
			      else
				{
				  *ptr++ = (unsigned char)cmap[j].r;
				  *ptr++ = (unsigned char)cmap[j].g;
				  *ptr++ = (unsigned char)cmap[j].b;
				}
			      j = ncolors;
			    }
			}
		    }
		}
	      else
		{
		  for (i = 0; ((i < 65536) && (ptr < end) && (line[i])); i++)
		    {
		      for (j = 0; j < cpp; j++, i++)
			{
			  col[j] = line[i];
			}
		      col[j] = 0;
		      i--;
		      for (j = 0; j < ncolors; j++)
			{
			  if (!strcmp(col, cmap[j].str))
			    {
			      *ptr++ = (unsigned char)cmap[j].r;
			      *ptr++ = (unsigned char)cmap[j].g;
			      *ptr++ = (unsigned char)cmap[j].b;
			      j = ncolors;
			    }
			}
		    }
		}
	    }
	}
      if ((ptr) && ((ptr - im->rgb_data) >= w * h * 3))
	done = 1;
    }
  if (!transp)
    {
      im->shape_color.r = -1;
      im->shape_color.g = -1;
      im->shape_color.b = -1;
    }
  else
    {
      im->shape_color.r = 255;
      im->shape_color.g = 0;
      im->shape_color.b = 255;
    }
  if (id->cache.on_image)
    add_image(id, im, im->filename);
  calc_map_tables(id, im);
  free(cmap);
  return im;
}

int
Imlib_data_to_pixmap(ImlibData * id, char **data, Pixmap * pmap, Pixmap * mask)
{
  ImlibImage         *im;
  
  im = Imlib_create_image_from_xpm_data(id, data);
  if (!im)
    {
      if (pmap)
	*pmap = 0;
      if (mask)
	*mask = 0;
      return 0;
    }
  if (!Imlib_render(id, im, im->rgb_width, im->rgb_height))
    {
      Imlib_destroy_image(id, im);
      if (pmap)
	*pmap = 0;
      if (mask)
	*mask = 0;
      return 0;
    }
  if (pmap)
    *pmap = Imlib_move_image(id, im);
  if (mask)
    *mask = Imlib_move_mask(id, im);
  Imlib_kill_image(id, im);
  return 1;
}

struct _io_info
{
  unsigned char *data;
  unsigned char *ptr;
  unsigned char *end;
};

#ifdef HAVE_LIBPNG
static void
_png_io_read(png_structp png_ptr,
	     png_bytep data, png_uint_32 size)
{
  struct _io_info *io_ptr;
  int bytes;
  
  io_ptr = (struct _io_info *)png_get_io_ptr(png_ptr);
  
  if ((io_ptr->end - io_ptr->ptr) >= size)
    {
      memcpy(data, io_ptr->ptr, size);
      io_ptr->ptr += size;
      return;
    }
  bytes = io_ptr->end - io_ptr->ptr;
  memcpy(data, io_ptr->ptr, bytes);
  io_ptr->ptr = io_ptr->end;
  return;
}
#endif /* HAVE_LIBPNG */

ImlibImage *
Imlib_inlined_png_to_image(ImlibData *id, unsigned char *data, int data_size)
{
#ifdef HAVE_LIBPNG
  ImlibImage         *im;
  png_structp         png_ptr;
  png_infop           info_ptr;
  unsigned char      *ptr, **lines, *ptr2, r, g, b, a;
  int                 i, x, y, transp, bit_depth, color_type, interlace_type;
  png_uint_32         ww, hh;
  char                s[512];
  struct _io_info     io_info;
  
  im = malloc(sizeof(ImlibImage));
  if (!im)
    return NULL;
  im->rgb_width = 0;
  im->rgb_height = 0;
  im->rgb_data = NULL;
  im->alpha_data = NULL;
  snprintf(s, sizeof(s), "creat_%x_%x", (int)time(NULL), (int)rand());
  im->filename = malloc(strlen(s) + 1);
  if (im->filename)
    strcpy(im->filename, s);
  im->width = 0;
  im->height = 0;
  im->border.left = 0;
  im->border.right = 0;
  im->border.top = 0;
  im->border.bottom = 0;
  im->pixmap = 0;
  im->shape_mask = 0;
  im->cache = 1;
  im->mod.gamma = id->mod.gamma;
  im->mod.brightness = id->mod.brightness;
  im->mod.contrast = id->mod.contrast;
  im->rmod.gamma = id->rmod.gamma;
  im->rmod.brightness = id->rmod.brightness;
  im->rmod.contrast = id->rmod.contrast;
  im->gmod.gamma = id->gmod.gamma;
  im->gmod.brightness = id->gmod.brightness;
  im->gmod.contrast = id->gmod.contrast;
  im->bmod.gamma = id->bmod.gamma;
  im->bmod.brightness = id->bmod.brightness;
  im->bmod.contrast = id->bmod.contrast;
  im->shape_color.r = -1;
  im->shape_color.g = -1;
  im->shape_color.b = -1;
  /* Init PNG Reader */
  transp = 0;
  png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if (!png_ptr)
    return NULL;
  
  info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr)
    {
      png_destroy_read_struct(&png_ptr, NULL, NULL);
      return NULL;
    }
  
  if (setjmp(png_ptr->jmpbuf))
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }
  
  if (info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA)
    {
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }
  
  io_info.data = data;
  io_info.ptr = data;
  io_info.end = data + data_size;
  png_set_read_fn(png_ptr, (void *)(&io_info), (png_rw_ptr)_png_io_read);
  
  /* Read Header */
  png_read_info(png_ptr, info_ptr);
  png_get_IHDR(png_ptr, info_ptr, &ww, &hh, &bit_depth, &color_type, &interlace_type,
	       NULL, NULL);
  im->rgb_width = ww;
  im->rgb_height = hh;
  /* Setup Translators */
  if (color_type == PNG_COLOR_TYPE_PALETTE)
    png_set_expand(png_ptr);
  png_set_strip_16(png_ptr);
  png_set_packing(png_ptr);
  if (png_get_valid(png_ptr, info_ptr, PNG_INFO_tRNS))
    png_set_expand(png_ptr);
  png_set_filler(png_ptr, 0xff, PNG_FILLER_AFTER);
  im->rgb_data = malloc(ww * hh * 3);
  if (!(im->rgb_data))
    {
      free(im->filename);
      free(im);
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }
  lines = (unsigned char **)malloc(hh * sizeof(unsigned char *));
  
  if (lines == NULL)
    {
      free(im->filename);
      free(im);
      free(im->rgb_data);
      png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
      return NULL;
    }
  for (i = 0; i < hh; i++)
    {
      if ((lines[i] = malloc(ww * (sizeof(unsigned char) * 4))) == NULL)
	{
	  int                 n;
	  
	  free(im->filename);
	  free(im);
	  free(im->rgb_data);
	  for (n = 0; n < i; n++)
	    free(lines[n]);
	  free(lines);
	  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
	  return NULL;
	}
    }
  png_read_image(png_ptr, lines);
  png_destroy_read_struct(&png_ptr, &info_ptr, NULL);
  ptr = im->rgb_data;
  if ((color_type == PNG_COLOR_TYPE_GRAY) ||
      (color_type == PNG_COLOR_TYPE_GRAY_ALPHA))
    {
      for (y = 0; y < hh; y++)
	{
	  ptr2 = lines[y];
	  for (x = 0; x < ww; x++)
	    {
	      r = *ptr2++;
	      a = *ptr2++;
	      if (a < 128)
		{
		  *ptr++ = 255;
		  *ptr++ = 0;
		  *ptr++ = 255;
		  transp = 1;
		}
	      else
		{
		  *ptr++ = r;
		  *ptr++ = r;
		  *ptr++ = r;
		}
	    }
	}
    }
  else
    {
      for (y = 0; y < hh; y++)
	{
	  ptr2 = lines[y];
	  for (x = 0; x < ww; x++)
	    {
	      r = *ptr2++;
	      g = *ptr2++;
	      b = *ptr2++;
	      a = *ptr2++;
	      if (a < 128)
		{
		  *ptr++ = 255;
		  *ptr++ = 0;
		  *ptr++ = 255;
		  transp = 1;
		}
	      else
		{
		  if ((r == 255) && (g == 0) && (b == 255))
		    r = 254;
		  *ptr++ = r;
		  *ptr++ = g;
		  *ptr++ = b;
		}
	    }
	}
    }
  for (i = 0; i < hh; i++)
    free(lines[i]);
  free(lines);
  if (transp)
    {
      im->shape_color.r = 255;
      im->shape_color.g = 0;
      im->shape_color.b = 255;
    }
  if (id->cache.on_image)
    add_image(id, im, im->filename);
  calc_map_tables(id, im);
  return im;
#else
  return NULL;
#endif /* HAVE_LIBPNG */
}


#include <signal.h>
#include <sys/wait.h>

/*    Helper library */

static int          hpid;
void                (*oldpiper) (int);	/* actually sighandler_t but BSD uses sig_t. */

FILE               *
open_helper(const char *instring, const char *fn, const char *mode)
{
  char                buf[256];	/* This is safe since our input strings

				 * 
				 * * 
				 * * * 
				 * * * * are bounded */
  static char        *vec[16];
  char               *p = strdup(instring);
  char               *pp;
  char               *ep;
  int                 vn = 0;
  int                 pid;
  FILE               *fp = NULL;
  char               *ofil = NULL;
  int                 ofd = -1;

  int                 pfd[2];

  if (p == NULL)
    return NULL;

  if (strncmp(instring, "%Q", 2) == 0)
    {
      /*
       *    Generate a quanting pipeline
       */
      fprintf(stderr, "Not currently supported: install ImageMagic.\n");
      return NULL;
    }
  /*
   *    Ok split the instring on spaces and translate
   *      %C %P %F and %s
   *
   *      FIXME: We need to handle a format string that begins
   *      %Q to indicate an 8bit quant in the pipeline first.
   */

  pp = p;

  while (vn < 15)
    {
      while (*pp && isspace(*pp))
	pp++;
      ep = pp;
      while (*ep && !isspace(*ep))
	ep++;
      if (*pp == 0)
	break;
      /* pp->ep is now the input string block */
      if (*ep)
	*ep++ = 0;

      if (strcmp(pp, "%s") == 0)
	vec[vn] = strdup(fn);
      else if (strncmp(pp, "%P/", 3) == 0)
	{
#ifndef __EMX__
	  strcpy(buf, NETPBM_PATH);
	  strcat(buf, pp + 2);
#else
	  strcpy(buf, pp + 3);
#endif
	  if ((vec[vn] = strdup(buf)) == NULL)
	    break;
	}
      else if (strncmp(pp, "%J", 3) == 0)
	{
	  if ((vec[vn] = strdup(DJPEG_PROG)) == NULL)
	    break;
	}
      else if (strncmp(pp, "%H", 3) == 0)
	{
	  if ((vec[vn] = strdup(CJPEG_PROG)) == NULL)
	    break;
	}
      else if (strncmp(pp, "%C/", 3) == 0)
	{
#ifndef __EMX__
	  strcpy(buf, CONVERT_PATH);
	  strcat(buf, pp + 2);
#else
	  strcat(buf, pp + 3);
#endif
	  if ((vec[vn] = strdup(buf)) == NULL)
	    break;
	}
      else if (strncmp(pp, ">%s", 3) == 0)
	{
	  ofil = pp;
	  vn++;
	  pp = ep;
	  continue;
	}
      else
	{
	  if ((vec[vn] = strdup(pp)) == NULL)
	    break;
	}
      vn++;
      pp = ep;
    }

  vec[vn] = NULL;

  if (pipe(pfd) == -1)
    goto oops;

#ifdef __EMX__
  setmode(pfd[0], O_BINARY);
  setmode(pfd[1], O_BINARY);
#endif

  if (*mode == 'r')
    {
      fp = fdopen(pfd[0], "r");
      if (fp == NULL)
	goto oops;
    }
  else if (*mode == 'w')
    {
      fp = fdopen(pfd[1], "w");
      if (fp == NULL)
	goto oops;
    }
  else
    goto oops;

  if (ofil != NULL)
    if ((ofd = open(ofil, O_WRONLY | O_TRUNC | O_CREAT)) == -1)
      goto oops;

#ifndef __EMX__
  switch (pid = fork())
    {
    case -1:
      break;
    case 0:
      signal(SIGPIPE, SIG_DFL);
      if (*mode == 'r')
	dup2(pfd[1], 1);
      if (*mode == 'w')
	{
	  dup2(pfd[0], 0);
	  if (ofd != -1)
	    {
	      dup2(ofd, 1);
	      close(1);
	    }
	}
      close(pfd[0]);
      close(pfd[1]);
      execv(vec[0], vec);
      perror(vec[0]);
      /* This MUST be _exit or we will hit the SIGPIPE
       * handler in ways we dont want. We want our parent
       * to flush the inherited file buffers not us. */
      _exit(1);
    default:
      hpid = pid;
      
      if (ofd != -1)
	close(ofd);
      if (*mode == 'r')
	close(pfd[1]);
      else
	close(pfd[0]);
    }
#else
  {
    int flag, tfd0, tfd1;
    flag = fcntl(pfd[0], F_GETFD);
    fcntl(pfd[0], F_SETFD, flag | FD_CLOEXEC);
    flag = fcntl(pfd[1], F_GETFD);
    fcntl(pfd[1], F_SETFD, flag | FD_CLOEXEC);
    tfd0 = dup(0);
    tfd1 = dup(1);
    if (*mode == 'r')
      dup2(pfd[1], 1);
    if (*mode == 'w')
      dup2(pfd[0], 0);
    pid = spawnv(P_NOWAIT, vec[0], vec);
    if (pid != -1) hpid = pid;
    dup2(tfd0, 0);
    dup2(tfd1, 1);
    close(tfd0);
    close(tfd1);
    if (ofd != -1)
      close(ofd);
    if (*mode == 'r')
      close(pfd[1]);
    else
      close(pfd[0]);
  }
#endif

  for (vn = 0; vn < 16; vn++)
    if (vec[vn])
      free(vec[vn]);
  oldpiper = signal(SIGPIPE, SIG_IGN);
  return fp;

oops:
  if (ofd != -1)
    close(ofd);
  if (fp)
    fclose(fp);
  for (vn = 0; vn < 16; vn++)
    if (vec[vn])
      free(vec[vn]);
  return NULL;
}

int
close_helper(FILE * fp)
{
  int                 info;

  fclose(fp);
  signal(SIGPIPE, oldpiper);
  waitpid(hpid, &info, 0);
  return WEXITSTATUS(info);
}

static char         x_error = 0;

static void
__handle_x_error(Display * d, XErrorEvent * ev)
{
  d = NULL;
  ev = NULL;
  x_error = 1;
}

ImlibImage         *
Imlib_create_image_from_drawable(ImlibData * id, Drawable win, Pixmap mask,
				 int x, int y, int width, int height)
{
  unsigned char      *data = NULL, *ptr, r, g, b;
  unsigned long       pixel;
  int                 i, xx, yy, w, h, inx, iny, clipx, clipy, rx, ry;
  XImage             *xim;

#ifdef HAVE_SHM
  XShmSegmentInfo     shminfo;

#endif
  XWindowAttributes   xatt, ratt;
  Colormap            cmap;
  static char         shm_checked = 0, shm = 1;
  XErrorHandler       erh = NULL;
  Window              chld;
  char                is_pixmap = 0;
  Visual             *vis;
  ImlibImage         *im;
  ImlibColor          ctab[256];
  Display            *disp;

  inx = 0;
  iny = 0;
  w = width;
  h = height;
  vis = id->x.visual;
  disp = id->x.disp;

  XGrabServer(disp);
  erh = XSetErrorHandler((XErrorHandler) __handle_x_error);
  x_error = 0;
  XGetWindowAttributes(disp, win, &xatt);
  XFlush(disp);
  if (x_error)
    {
      x_error = 0;
      is_pixmap = 1;
      XGetGeometry(disp, win, &chld, &rx, &rx,
		   (unsigned int *)&xatt.width, (unsigned int *)&xatt.height,
		   (unsigned int *)&rx, (unsigned int *)&xatt.depth);
      XFlush(disp);
      if (x_error)
	{
	  XUngrabServer(disp);
	  XFlush(disp);
	  XSetErrorHandler((XErrorHandler) erh);
	  return NULL;
	}
    }
  XSetErrorHandler((XErrorHandler) erh);
  if (!is_pixmap)
    {
      XGetWindowAttributes(disp, xatt.root, &ratt);
      XTranslateCoordinates(disp, win, xatt.root, 0, 0, &rx, &ry, &chld);
      if ((xatt.map_state != IsViewable) &&
	  (xatt.backing_store == NotUseful))
	{
	  XUngrabServer(disp);
	  XFlush(disp);
	  return NULL;
	}
    }
  clipx = 0;
  clipy = 0;

  x = x - inx;
  y = y - iny;

  width = xatt.width - x;
  height = xatt.height - y;
  if (width > w)
    width = w;
  if (height > h)
    height = h;

  if (!is_pixmap)
    {
      if ((rx + x + width) > ratt.width)
	width = ratt.width - (rx + x);
      if ((ry + y + height) > ratt.height)
	height = ratt.height - (ry + y);
    }
  if (x < 0)
    {
      clipx = -x;
      width += x;
      x = 0;
    }
  if (y < 0)
    {
      clipy = -y;
      height += y;
      y = 0;
    }
  if (!is_pixmap)
    {
      if ((rx + x) < 0)
	{
	  clipx -= (rx + x);
	  width += (rx + x);
	  x = -rx;
	}
      if ((ry + y) < 0)
	{
	  clipy -= (ry + y);
	  height += (ry + y);
	  y = -ry;
	}
    }
  if ((width <= 0) || (height <= 0))
    {
      XUngrabServer(disp);
      XSync(disp, False);
      return NULL;
    }
#ifdef HAVE_SHM
  if (shm)
    {
      if (!shm_checked)
	{
	  erh = XSetErrorHandler((XErrorHandler) __handle_x_error);
	}
      xim = XShmCreateImage(disp, vis, xatt.depth, ZPixmap, NULL,
			    &shminfo, width, height);
      if (!shm_checked)
	{
	  XSync(disp, False);
	  if (x_error)
	    {
	      shm = 0;
	      XDestroyImage(xim);
	      xim = XGetImage(disp, win, x, y, width, height, 0xffffffff, ZPixmap);
	      XSetErrorHandler((XErrorHandler) erh);
	      shm_checked = 1;
	    }
	  else
	    {
	      shminfo.shmid = shmget(IPC_PRIVATE, xim->bytes_per_line *
				     xim->height, IPC_CREAT | 0666);
	      if (shminfo.shmid < 0)
		{
		  shm = 0;
		  XDestroyImage(xim);
		  xim = XGetImage(disp, win, x, y, width, height, 0xffffffff, ZPixmap);
		  XSetErrorHandler((XErrorHandler) erh);
		  shm_checked = 1;
		}
	      else
		{
		  shminfo.shmaddr = xim->data = shmat(shminfo.shmid, 0, 0);
		  if (shminfo.shmaddr == (char *)-1)
		    {
		      shm = 0;
		      XDestroyImage(xim);
		      shmctl(shminfo.shmid, IPC_RMID, 0);
		      xim = XGetImage(disp, win, x, y, width, height, 0xffffffff, ZPixmap);
		      XSetErrorHandler((XErrorHandler) erh);
		      shm_checked = 1;
		    }
		  else
		    {
		      shminfo.readOnly = False;
		      XShmAttach(disp, &shminfo);
		    }
		}
	    }
	}
      else
	{
	  shminfo.shmid = shmget(IPC_PRIVATE, xim->bytes_per_line *
				 xim->height, IPC_CREAT | 0666);
	  if (shminfo.shmid < 0)
	    {
	      shm = 0;
	      XDestroyImage(xim);
	      xim = XGetImage(disp, win, x, y, width, height, 0xffffffff, ZPixmap);
	      XSetErrorHandler((XErrorHandler) erh);
	      shm_checked = 1;
	    }
	  else
	    {
	      shminfo.shmaddr = xim->data = shmat(shminfo.shmid, 0, 0);
	      if (shminfo.shmaddr == (char *)-1)
		{
		  shm = 0;
		  XDestroyImage(xim);
		  shmctl(shminfo.shmid, IPC_RMID, 0);
		  xim = XGetImage(disp, win, x, y, width, height, 0xffffffff, ZPixmap);
		  XSetErrorHandler((XErrorHandler) erh);
		  shm_checked = 1;
		}
	      else
		{
		  shminfo.readOnly = False;
		  XShmAttach(disp, &shminfo);
		}
	    }
	}
      if (!shm_checked)
	{
	  XSync(disp, False);
	  if (x_error)
	    {
	      shm = 0;
	      XDestroyImage(xim);
	      xim = XGetImage(disp, win, x, y, width, height, 0xffffffff, ZPixmap);
	      shm_checked = 1;
	    }
	  XSetErrorHandler((XErrorHandler) erh);
	  shm_checked = 1;
	}
    }
  else
#endif /* HAVE_SHM */
    xim = XGetImage(disp, win, x, y, width, height, 0xffffffff, ZPixmap);
#ifdef HAVE_SHM
  if (shm)
    XShmGetImage(disp, win, xim, x, y, 0xffffffff);
#endif
  XUngrabServer(disp);
  XFlush(disp);

  if (xatt.depth == 1)
    {
      ctab[0].r = 255;
      ctab[0].g = 255;
      ctab[0].b = 255;
      ctab[1].r = 0;
      ctab[1].g = 0;
      ctab[1].b = 0;
    }
  else if (xatt.depth <= 8)
    {
      XColor              cols[256];

      if (!is_pixmap)
	{
	  cmap = xatt.colormap;
	  if (cmap == None)
	    cmap = id->x.root_cmap;
	}
      else
	cmap = id->x.root_cmap;

      for (i = 0; i < (1 << id->x.depth); i++)
	{
	  cols[i].pixel = i;
	  cols[i].flags = DoRed | DoGreen | DoBlue;
	}
      XQueryColors(disp, cmap, cols, 1 << id->x.depth);
      for (i = 0; i < (1 << id->x.depth); i++)
	{
	  ctab[i].r = cols[i].red >> 8;
	  ctab[i].g = cols[i].green >> 8;
	  ctab[i].b = cols[i].blue >> 8;
	  ctab[i].pixel = cols[i].pixel;
	}
    }
  data = malloc(width * height * 3);
  if (data)
    {
      ptr = data;
      switch (xatt.depth)
	{
	case 0:
	case 1:
	case 2:
	case 3:
	case 4:
	case 5:
	case 6:
	case 7:
	case 8:
	  for (yy = 0; yy < height; yy++)
	    {
	      for (xx = 0; xx < width; xx++)
		{
		  pixel = XGetPixel(xim, xx, yy);
		  r = ctab[pixel & 0xff].r;
		  g = ctab[pixel & 0xff].g;
		  b = ctab[pixel & 0xff].b;
		  *ptr++ = r;
		  *ptr++ = g;
		  *ptr++ = b;
		}
	    }
	  break;
	case 16:
	case 15:
	  if (id->x.render_depth == 16)
	    {
	      for (yy = 0; yy < height; yy++)
		{
		  for (xx = 0; xx < width; xx++)
		    {
		      pixel = XGetPixel(xim, xx, yy);
		      r = (pixel >> 8) & 0xf8;
		      g = (pixel >> 3) & 0xfc;
		      b = (pixel << 3) & 0xf8;
		      *ptr++ = r;
		      *ptr++ = g;
		      *ptr++ = b;
		    }
		}
	    }
	  else
	    {
	      for (yy = 0; yy < height; yy++)
		{
		  for (xx = 0; xx < width; xx++)
		    {
		      pixel = XGetPixel(xim, xx, yy);
		      r = (pixel >> 7) & 0xf8;
		      g = (pixel >> 2) & 0xf8;
		      b = (pixel << 3) & 0xf8;
		      *ptr++ = r;
		      *ptr++ = g;
		      *ptr++ = b;
		    }
		}
	    }
	  break;
	case 24:
	case 32:
	  for (yy = 0; yy < height; yy++)
	    {
	      for (xx = 0; xx < width; xx++)
		{
		  pixel = XGetPixel(xim, xx, yy);
		  r = (pixel >> 16) & 0xff;
		  g = (pixel >> 8) & 0xff;
		  b = pixel & 0xff;
		  *ptr++ = r;
		  *ptr++ = g;
		  *ptr++ = b;
		}
	    }
	  break;
	default:
	  for (yy = 0; yy < height; yy++)
	    {
	      for (xx = 0; xx < width; xx++)
		{
		  r = rand();
		  g = rand();
		  b = rand();
		  *ptr++ = r;
		  *ptr++ = g;
		  *ptr++ = b;
		}
	    }
	  break;
	}
    }

#ifdef HAVE_SHM
  if (shm)
    {
      XSync(disp, False);
      XShmDetach(disp, &shminfo);
      shmdt(shminfo.shmaddr);
      shmctl(shminfo.shmid, IPC_RMID, 0);
    }
#endif
  XDestroyImage(xim);

  if (data)
    {
      im = Imlib_create_image_from_data(id, data, NULL, width, height);
      free(data);
      return im;
    }
  else
    return NULL;
}
