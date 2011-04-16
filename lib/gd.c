#include <dfsch/lib/gd.h>

typedef struct gd_image_t {
  dfsch_type_t* type;
  gdImagePtr img;
} gd_image_t;

typedef struct gd_font_t {
  dfsch_type_t* type;
  gdFontPtr font;
} gd_font_t;

dfsch_type_t dfsch_gd_image_type = {};

dfsch_type_t dfsch_gd_font_type = {};


gdImagePtr dfsch_gd_image(dfsch_object_t* obj);
dfsch_object_t* dfsch_gd_cons_image(gdImagePtr img);

gdFontPtr dfsch_gd_font(dfsch_object_t* obj);
dfsch_object_t* dfsch_gd_cons_font(gdFontPtr font);
