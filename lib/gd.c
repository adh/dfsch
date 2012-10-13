#include <dfsch/lib/gd.h>

typedef struct gd_image_t {
  dfsch_type_t* type;
  gdImagePtr img;
} gd_image_t;

typedef struct gd_font_t {
  dfsch_type_t* type;
  gdFontPtr font;
} gd_font_t;

static void image_destroy(gd_image_t* img){
  gdImageDestroy(img->img);
}


dfsch_type_t dfsch_gd_image_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "gd:image",
  .size = sizeof(gd_image_t),
  .destroy = image_destroy,
};

dfsch_type_t dfsch_gd_font_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "gd:font",
  .size = sizeof(gd_font_t),
};


gdImagePtr dfsch_gd_image(dfsch_object_t* obj){
  gd_image_t* i = DFSCH_ASSERT_TYPE(obj, DFSCH_GD_IMAGE_TYPE);
  return i->img;
}


dfsch_object_t* dfsch_gd_cons_image(gdImagePtr img){
  gd_image_t* i = dfsch_make_object(DFSCH_GD_IMAGE_TYPE);
  if (!img){
    dfsch_error("Error creating GD image", NULL);
  }
  i->img = img;

  return i;
}

gdFontPtr dfsch_gd_font(dfsch_object_t* obj){
  gd_font_t* f = DFSCH_ASSERT_TYPE(obj, DFSCH_GD_FONT_TYPE);
  return f->font;
}
dfsch_object_t* dfsch_gd_cons_font(gdFontPtr font){
  gd_font_t* f = dfsch_make_object(DFSCH_GD_FONT_TYPE);
  f->font = font;
  return f;
}
