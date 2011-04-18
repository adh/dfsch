#include <dfsch/lib/gd.h>
#include <dfsch/load.h>

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
                       "Convert image to PNG data"){
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
                       "Allocate color with given RGB values"){
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
                       "Draw an axis aligned eliptical arc"){
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
                       "Draw an axis aligned filled eliptical arc"){
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


void dfsch_module_gd_register(dfsch_object_t* env){
  dfsch_package_t* gd = dfsch_make_package("gd",
                                           "GD bitmap graphics library");
  dfsch_provide(env, "libgd");

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
  dfsch_defcanon_pkgcstr(env, gd, "rectangle",
                         DFSCH_PRIMITIVE_REF(arc));
  dfsch_defcanon_pkgcstr(env, gd, "filled-rectangle",
                         DFSCH_PRIMITIVE_REF(filled_arc));
  
}
