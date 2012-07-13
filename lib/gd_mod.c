#include <dfsch/lib/gd.h>
#include <dfsch/load.h>

#include <gdfonts.h>
#include <gdfontl.h>
#include <gdfontmb.h>
#include <gdfontg.h>
#include <gdfontt.h>

DFSCH_DEFINE_PRIMITIVE(make_image,
                       "Create new pallete-based image"){
  long width;
  long height;
  DFSCH_LONG_ARG(args, width);
  DFSCH_LONG_ARG(args, height);
  DFSCH_ARG_END(args);

  return dfsch_gd_cons_image(gdImageCreate(width, height));
}

DFSCH_DEFINE_PRIMITIVE(make_image_true_color,
                       "Create new pallete-based image"){
  long width;
  long height;
  DFSCH_LONG_ARG(args, width);
  DFSCH_LONG_ARG(args, height);
  DFSCH_ARG_END(args);

  return dfsch_gd_cons_image(gdImageCreateTrueColor(width, height));
}

DFSCH_DEFINE_PRIMITIVE(make_image_from_jpeg,
                       "Create image from JPEG data"){
  dfsch_strbuf_t* data;
  DFSCH_BUFFER_ARG(args, data);
  DFSCH_ARG_END(args);

  return dfsch_gd_cons_image(gdImageCreateFromJpegPtr(data->len, data->ptr));  
}
DFSCH_DEFINE_PRIMITIVE(make_image_from_png,
                       "Create image from PNG data"){
  dfsch_strbuf_t* data;
  DFSCH_BUFFER_ARG(args, data);
  DFSCH_ARG_END(args);

  return dfsch_gd_cons_image(gdImageCreateFromPngPtr(data->len, data->ptr));  
}
DFSCH_DEFINE_PRIMITIVE(make_image_from_gif,
                       "Create image from GIF data"){
  dfsch_strbuf_t* data;
  DFSCH_BUFFER_ARG(args, data);
  DFSCH_ARG_END(args);

  return dfsch_gd_cons_image(gdImageCreateFromGifPtr(data->len, data->ptr));  
}

DFSCH_DEFINE_PRIMITIVE(image_2_jpeg,
                       "Convert image to JPEG data"){
  gdImagePtr image;
  dfsch_object_t* res;
  int quality;
  int size;
  char* buf;
  
  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG_OPT(args, quality, -1);
  DFSCH_ARG_END(args);
  
  buf = gdImageJpegPtr(image, &size, quality);
  res = dfsch_make_byte_vector(buf, size);
  gdFree(buf);

  return res;
}
DFSCH_DEFINE_PRIMITIVE(image_2_gif,
                       "Convert image to GIF data"){
  gdImagePtr image;
  dfsch_object_t* res;
  int size;
  char* buf;
  
  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_ARG_END(args);
  
  buf = gdImageGifPtr(image, &size);
  res = dfsch_make_byte_vector(buf, size);
  gdFree(buf);

  return res;
}
DFSCH_DEFINE_PRIMITIVE(image_2_png,
                       "Convert image to PNG data"
                       DFSCH_DOC_SYNOPSIS("(image &opt compression)"){
  gdImagePtr image;
  dfsch_object_t* res;
  int compression;
  int size;
  char* buf;
  
  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG_OPT(args, compression, -1);
  DFSCH_ARG_END(args);
  
  buf = gdImagePngPtrEx(image, &size, compression);
  res = dfsch_make_byte_vector(buf, size);
  gdFree(buf);

  return res;
}

DFSCH_DEFINE_PRIMITIVE(color, 
                       "Allocate color with given RGB values"){
  gdImagePtr image;
  int r;
  int g;
  int b;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, r);
  DFSCH_LONG_ARG(args, g);
  DFSCH_LONG_ARG(args, b);
  DFSCH_ARG_END(args);

  color = gdImageColorResolve(image, r, g, b);
  if (color < 0){
    dfsch_error("No more space in pallete", NULL);
  }
  return dfsch_make_number_from_long(color);
}
DFSCH_DEFINE_PRIMITIVE(color_alpha, 
                       "Allocate color with given RGBA values"){
  gdImagePtr image;
  int r;
  int g;
  int b;
  int a;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, r);
  DFSCH_LONG_ARG(args, g);
  DFSCH_LONG_ARG(args, b);
  DFSCH_LONG_ARG(args, a);
  DFSCH_ARG_END(args);

  color = gdImageColorResolveAlpha(image, r, g, b, a);
  if (color < 0){
    dfsch_error("No more space in pallete", NULL);
  }
  return dfsch_make_number_from_long(color);
}

DFSCH_DEFINE_PRIMITIVE(set_pixel, 
                       "Draw one pixel"){
  gdImagePtr image;
  int x;
  int y;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageSetPixel(image, x, y, color);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(line, 
                       "Draw straight line"){
  gdImagePtr image;
  int x1;
  int y1;
  int x2;
  int y2;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, x1);
  DFSCH_LONG_ARG(args, y1);
  DFSCH_LONG_ARG(args, x2);
  DFSCH_LONG_ARG(args, y2);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageLine(image, x1, y1, x2, y2, color);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(rectangle, 
                       "Draw an axis aligned rectangle outline"){
  gdImagePtr image;
  int x1;
  int y1;
  int x2;
  int y2;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, x1);
  DFSCH_LONG_ARG(args, y1);
  DFSCH_LONG_ARG(args, x2);
  DFSCH_LONG_ARG(args, y2);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageRectangle(image, x1, y1, x2, y2, color);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(filled_rectangle, 
                       "Draw an axis aligned filled rectangle"){
  gdImagePtr image;
  int x1;
  int y1;
  int x2;
  int y2;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, x1);
  DFSCH_LONG_ARG(args, y1);
  DFSCH_LONG_ARG(args, x2);
  DFSCH_LONG_ARG(args, y2);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageFilledRectangle(image, x1, y1, x2, y2, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(arc, 
                       "Draw an axis aligned elliptical arc"){
  gdImagePtr image;
  int x;
  int y;
  int w;
  int h;
  int start;
  int end;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_LONG_ARG(args, w);
  DFSCH_LONG_ARG(args, h);
  DFSCH_LONG_ARG(args, start);
  DFSCH_LONG_ARG(args, end);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageArc(image, x, y, w, h, start, end, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(filled_arc, 
                       "Draw an axis aligned filled elliptical arc"){
  gdImagePtr image;
  int x;
  int y;
  int w;
  int h;
  int start;
  int end;
  int color;
  int style = 0;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_LONG_ARG(args, w);
  DFSCH_LONG_ARG(args, h);
  DFSCH_LONG_ARG(args, start);
  DFSCH_LONG_ARG(args, end);
  DFSCH_LONG_ARG(args, color);
  DFSCH_FLAG_PARSER_BEGIN(args);
  DFSCH_FLAG_SET("arc", gdArc, style);
  DFSCH_FLAG_SET("chord", gdChord, style);
  DFSCH_FLAG_SET("pie", gdPie, style);
  DFSCH_FLAG_SET("no-fill", gdNoFill, style);
  DFSCH_FLAG_SET("edged", gdEdged, style);
  DFSCH_FLAG_PARSER_END(args);
  DFSCH_ARG_END(args);
  
  gdImageFilledArc(image, x, y, w, h, start, end, color, style);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(ellipse, 
                       "Draw an axis aligned ellipse outline"){
  gdImagePtr image;
  int x;
  int y;
  int w;
  int h;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_LONG_ARG(args, w);
  DFSCH_LONG_ARG(args, h);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageArc(image, x, y, w, h, 0, 360, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(filled_ellipse, 
                       "Draw an axis aligned filled ellipse"){
  gdImagePtr image;
  int x;
  int y;
  int w;
  int h;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_LONG_ARG(args, w);
  DFSCH_LONG_ARG(args, h);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageFilledEllipse(image, x, y, w, h, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(fill, 
                       "Flood-fill region of same color"){
  gdImagePtr image;
  int x;
  int y;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageFill(image, x, y, color);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(fill_to_border, 
                       "Flood-fill region bounded by given color"){
  gdImagePtr image;
  int x;
  int y;
  int border;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_LONG_ARG(args, border);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageFillToBorder(image, x, y, border, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(set_antialiased, 
                       "Set actual color used for antialiased drawing"){
  gdImagePtr image;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageSetAntiAliased(image, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(set_thickness, 
                       "Set thickness of drawn lines"){
  gdImagePtr image;
  int thickness;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_LONG_ARG(args, thickness);
  DFSCH_ARG_END(args);
  
  gdImageSetThickness(image, thickness);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(set_alpha_blending, 
                       "Set whetever alpha channel value is used when "
                       "drawing instead of simply copied into image"){
  gdImagePtr image;
  dfsch_object_t* blending;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_OBJECT_ARG(args, blending);
  DFSCH_ARG_END(args);
  
  gdImageAlphaBlending(image, blending != NULL);

  return NULL;
}
DFSCH_DEFINE_PRIMITIVE(set_save_alpha, 
                       "Set whetever alpha channel channel should be stored "
                       "in output PNG images"){
  gdImagePtr image;
  dfsch_object_t* save;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_OBJECT_ARG(args, save);
  DFSCH_ARG_END(args);
  
  gdImageSaveAlpha(image, save != NULL);

  return NULL;
}

static void parse_points(dfsch_object_t* points, gdPointPtr* pp, int* pc){
  int count = 0;
  int size = 16;
  dfsch_object_t* it = dfsch_collection_get_iterator(points);
  int x;
  int y;
  dfsch_object_t* it2;
  gdPointPtr res = GC_MALLOC_ATOMIC(sizeof(gdPoint) * 16);
  
  while (it){
    if (count >= size){
      size *= 2;
      res = GC_MALLOC_ATOMIC(sizeof(gdPoint) * size);
    }
    it2 = dfsch_collection_get_iterator(dfsch_iterator_this(it));
    res[count].x = dfsch_number_to_long(dfsch_iterator_this(it2));
    it2 = dfsch_iterator_next(it2);
    res[count].y = dfsch_number_to_long(dfsch_iterator_this(it2));
    it2 = dfsch_iterator_next(it2);
    if (it2){
      dfsch_error("Point has excess elements", dfsch_iterator_this(it));
    }
    count++;
    it = dfsch_iterator_next(it);
  }

  *pp = res;
  *pc = count;
}

DFSCH_DEFINE_PRIMITIVE(polygon, 
                       "Draw a polygon"){
  gdImagePtr image;
  int color;
  dfsch_object_t* point_list;
  gdPointPtr points;
  int count;
  
  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_OBJECT_ARG(args, point_list);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  parse_points(point_list, &points, &count);
  
  gdImagePolygon(image, points, count, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(polyline,
                       "Draw a polyline (an open polygon)"){
  gdImagePtr image;
  int color;
  dfsch_object_t* point_list;
  gdPointPtr points;
  int count;
  
  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_OBJECT_ARG(args, point_list);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  parse_points(point_list, &points, &count);
  
  gdImageOpenPolygon(image, points, count, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(filled_polygon, 
                       "Draw a filled polygon"){
  gdImagePtr image;
  int color;
  dfsch_object_t* point_list;
  gdPointPtr points;
  int count;
  
  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_OBJECT_ARG(args, point_list);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  parse_points(point_list, &points, &count);
  
  gdImageFilledPolygon(image, points, count, color);

  return NULL;
}


DFSCH_DEFINE_PRIMITIVE(char, 
                       "Draw one character of bitmap font"){
  gdImagePtr image;
  gdFontPtr font;
  int x;
  int y;
  int ch;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_GD_FONT_ARG(args, font);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_LONG_ARG(args, ch);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageChar(image, font, x, y, ch, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(string, 
                       "Draw string with bitmap font"){
  gdImagePtr image;
  gdFontPtr font;
  int x;
  int y;
  char* str;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_GD_FONT_ARG(args, font);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_STRING_ARG(args, str);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageString(image, font, x, y, str, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(char_up, 
                       "Draw one character of bitmap font vertically"){
  gdImagePtr image;
  gdFontPtr font;
  int x;
  int y;
  int ch;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_GD_FONT_ARG(args, font);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_LONG_ARG(args, ch);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageCharUp(image, font, x, y, ch, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(string_up, 
                       "Draw string with bitmap font vertically"){
  gdImagePtr image;
  gdFontPtr font;
  int x;
  int y;
  char* str;
  int color;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_GD_FONT_ARG(args, font);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_STRING_ARG(args, str);
  DFSCH_LONG_ARG(args, color);
  DFSCH_ARG_END(args);
  
  gdImageStringUp(image, font, x, y, str, color);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(string_ft, 
                       "Draw string with FreeType/TrueType font"){
  gdImagePtr image;
  char* fontname;
  int x;
  int y;
  char* str;
  int color;
  int brect[8];
  double ptsize;
  double angle;
  char* res;
  int i;

  DFSCH_GD_IMAGE_ARG(args, image);
  DFSCH_STRING_ARG(args, fontname);
  DFSCH_DOUBLE_ARG(args, ptsize);
  DFSCH_LONG_ARG(args, x);
  DFSCH_LONG_ARG(args, y);
  DFSCH_STRING_ARG(args, str);
  DFSCH_LONG_ARG(args, color);
  DFSCH_DOUBLE_ARG_OPT(args, angle, 0);
  DFSCH_ARG_END(args);
  
  res = gdImageStringFT(image, brect, color, fontname, ptsize, angle, x, y, str);

  if (res){
    dfsch_error("Error drawing string",
                dfsch_make_string_cstr(res));
  }

  return dfsch_vector(4,
                      dfsch_vector(2,
                                   DFSCH_MAKE_FIXNUM(brect[0]),
                                   DFSCH_MAKE_FIXNUM(brect[1])),
                      dfsch_vector(2,
                                   DFSCH_MAKE_FIXNUM(brect[2]),
                                   DFSCH_MAKE_FIXNUM(brect[3])),
                      dfsch_vector(2,
                                   DFSCH_MAKE_FIXNUM(brect[4]),
                                   DFSCH_MAKE_FIXNUM(brect[5])),
                      dfsch_vector(2,
                                   DFSCH_MAKE_FIXNUM(brect[6]),
                                   DFSCH_MAKE_FIXNUM(brect[7])));
}

DFSCH_DEFINE_PRIMITIVE(copy_rectangle, 
                       "Copy region between images"){
  gdImagePtr src;
  gdImagePtr dst;
  int dst_x;
  int dst_y;
  int src_x;
  int src_y;
  int w;
  int h;
  
  DFSCH_GD_IMAGE_ARG(args, dst);
  DFSCH_LONG_ARG(args, dst_x);
  DFSCH_LONG_ARG(args, dst_y);
  DFSCH_GD_IMAGE_ARG(args, src);
  DFSCH_LONG_ARG(args, src_x);
  DFSCH_LONG_ARG(args, src_y);
  DFSCH_LONG_ARG(args, w);
  DFSCH_LONG_ARG(args, h);
  DFSCH_ARG_END(args);
  
  gdImageCopy(dst, src, dst_x, dst_y, src_x, src_y, w, h);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(copy_rectangle_resize, 
                       "Copy region between images resizing it"){
  gdImagePtr src;
  gdImagePtr dst;
  int dst_x;
  int dst_y;
  int dst_w;
  int dst_h;
  int src_x;
  int src_y;
  int src_w;
  int src_h;
  
  DFSCH_GD_IMAGE_ARG(args, dst);
  DFSCH_LONG_ARG(args, dst_x);
  DFSCH_LONG_ARG(args, dst_y);
  DFSCH_LONG_ARG(args, dst_w);
  DFSCH_LONG_ARG(args, dst_h);
  DFSCH_GD_IMAGE_ARG(args, src);
  DFSCH_LONG_ARG(args, src_x);
  DFSCH_LONG_ARG(args, src_y);
  DFSCH_LONG_ARG(args, src_w);
  DFSCH_LONG_ARG(args, src_h);
  DFSCH_ARG_END(args);
  
  gdImageCopyResized(dst, src, dst_x, dst_y, src_x, src_y, dst_w, dst_h, src_w, src_h);

  return NULL;
}

DFSCH_DEFINE_PRIMITIVE(copy_rectangle_resample, 
                       "Copy region between images resizing it and interpolating pixel values"){
  gdImagePtr src;
  gdImagePtr dst;
  int dst_x;
  int dst_y;
  int dst_w;
  int dst_h;
  int src_x;
  int src_y;
  int src_w;
  int src_h;
  
  DFSCH_GD_IMAGE_ARG(args, dst);
  DFSCH_LONG_ARG(args, dst_x);
  DFSCH_LONG_ARG(args, dst_y);
  DFSCH_LONG_ARG(args, dst_w);
  DFSCH_LONG_ARG(args, dst_h);
  DFSCH_GD_IMAGE_ARG(args, src);
  DFSCH_LONG_ARG(args, src_x);
  DFSCH_LONG_ARG(args, src_y);
  DFSCH_LONG_ARG(args, src_w);
  DFSCH_LONG_ARG(args, src_h);
  DFSCH_ARG_END(args);
  
  gdImageCopyResampled(dst, src, dst_x, dst_y, src_x, src_y, dst_w, dst_h, src_w, src_h);

  return NULL;
}


void dfsch_module_gd_register(dfsch_object_t* env){
  dfsch_package_t* gd = dfsch_make_package("gd",
                                           "GD bitmap graphics library");
  dfsch_provide(env, "libgd");

  dfsch_defcanon_pkgcstr(env, gd, "<image>",
                         DFSCH_GD_IMAGE_TYPE);
  dfsch_defcanon_pkgcstr(env, gd, "<font>",
                         DFSCH_GD_FONT_TYPE);

  dfsch_defcanon_pkgcstr(env, gd, "*font-small*",
                         dfsch_gd_cons_font(gdFontGetSmall()));
  dfsch_defcanon_pkgcstr(env, gd, "*font-large*",
                         dfsch_gd_cons_font(gdFontGetLarge()));
  dfsch_defcanon_pkgcstr(env, gd, "*font-medium-bold*",
                         dfsch_gd_cons_font(gdFontGetMediumBold()));
  dfsch_defcanon_pkgcstr(env, gd, "*font-giant*",
                         dfsch_gd_cons_font(gdFontGetGiant()));
  dfsch_defcanon_pkgcstr(env, gd, "*font-tiny*",
                         dfsch_gd_cons_font(gdFontGetTiny()));

  dfsch_defcanon_pkgcstr(env, gd, "make-image",
                         DFSCH_PRIMITIVE_REF(make_image));
  dfsch_defcanon_pkgcstr(env, gd, "make-image-true-color",
                         DFSCH_PRIMITIVE_REF(make_image_true_color));
  dfsch_defcanon_pkgcstr(env, gd, "make-image-from-jpeg",
                         DFSCH_PRIMITIVE_REF(make_image_from_jpeg));
  dfsch_defcanon_pkgcstr(env, gd, "make-image-from-png",
                         DFSCH_PRIMITIVE_REF(make_image_from_png));
  dfsch_defcanon_pkgcstr(env, gd, "make-image-from-gif",
                         DFSCH_PRIMITIVE_REF(make_image_from_gif));

  dfsch_defcanon_pkgcstr(env, gd, "image->jpeg",
                         DFSCH_PRIMITIVE_REF(image_2_jpeg));
  dfsch_defcanon_pkgcstr(env, gd, "image->gif",
                         DFSCH_PRIMITIVE_REF(image_2_gif));
  dfsch_defcanon_pkgcstr(env, gd, "image->png",
                         DFSCH_PRIMITIVE_REF(image_2_png));

  dfsch_defcanon_pkgcstr(env, gd, "color",
                         DFSCH_PRIMITIVE_REF(color));
  dfsch_defcanon_pkgcstr(env, gd, "color-alpha",
                         DFSCH_PRIMITIVE_REF(color_alpha));
  
  dfsch_defcanon_pkgcstr(env, gd, "set-pixel",
                         DFSCH_PRIMITIVE_REF(set_pixel));
  dfsch_defcanon_pkgcstr(env, gd, "line",
                         DFSCH_PRIMITIVE_REF(line));
  dfsch_defcanon_pkgcstr(env, gd, "rectangle",
                         DFSCH_PRIMITIVE_REF(rectangle));
  dfsch_defcanon_pkgcstr(env, gd, "filled-rectangle",
                         DFSCH_PRIMITIVE_REF(filled_rectangle));
  dfsch_defcanon_pkgcstr(env, gd, "arc",
                         DFSCH_PRIMITIVE_REF(arc));
  dfsch_defcanon_pkgcstr(env, gd, "filled-arc",
                         DFSCH_PRIMITIVE_REF(filled_arc));
  dfsch_defcanon_pkgcstr(env, gd, "ellipse",
                         DFSCH_PRIMITIVE_REF(ellipse));
  dfsch_defcanon_pkgcstr(env, gd, "filled-ellipse",
                         DFSCH_PRIMITIVE_REF(filled_ellipse));
  dfsch_defcanon_pkgcstr(env, gd, "fill",
                         DFSCH_PRIMITIVE_REF(fill));
  dfsch_defcanon_pkgcstr(env, gd, "fill-to-border",
                         DFSCH_PRIMITIVE_REF(fill_to_border));

  dfsch_defcanon_pkgcstr(env, gd, "set-antialiased!",
                         DFSCH_PRIMITIVE_REF(set_antialiased));
  dfsch_defcanon_pkgcstr(env, gd, "set-thickness!",
                         DFSCH_PRIMITIVE_REF(set_thickness));
  dfsch_defcanon_pkgcstr(env, gd, "set-alpha-blending!",
                         DFSCH_PRIMITIVE_REF(set_alpha_blending));
  dfsch_defcanon_pkgcstr(env, gd, "set-save-alpha!",
                         DFSCH_PRIMITIVE_REF(set_save_alpha));

  dfsch_defcanon_pkgcstr(env, gd, "polygon",
                         DFSCH_PRIMITIVE_REF(polygon));
  dfsch_defcanon_pkgcstr(env, gd, "polyline",
                         DFSCH_PRIMITIVE_REF(polyline));
  dfsch_defcanon_pkgcstr(env, gd, "filled-polygon",
                         DFSCH_PRIMITIVE_REF(filled_polygon));

  dfsch_defcanon_pkgcstr(env, gd, "char",
                         DFSCH_PRIMITIVE_REF(char));
  dfsch_defcanon_pkgcstr(env, gd, "string",
                         DFSCH_PRIMITIVE_REF(string));
  dfsch_defcanon_pkgcstr(env, gd, "char-up",
                         DFSCH_PRIMITIVE_REF(char_up));
  dfsch_defcanon_pkgcstr(env, gd, "string-up",
                         DFSCH_PRIMITIVE_REF(string_up));
  dfsch_defcanon_pkgcstr(env, gd, "string-ft",
                         DFSCH_PRIMITIVE_REF(string_ft));

  dfsch_defcanon_pkgcstr(env, gd, "copy-rectangle",
                         DFSCH_PRIMITIVE_REF(copy_rectangle));
  dfsch_defcanon_pkgcstr(env, gd, "copy-rectangle-resize",
                         DFSCH_PRIMITIVE_REF(copy_rectangle_resize));
  dfsch_defcanon_pkgcstr(env, gd, "copy-rectangle-resample",
                         DFSCH_PRIMITIVE_REF(copy_rectangle_resample));

}
