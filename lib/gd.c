#include <dfsch/lib/gd.h>

typedef struct gd_image_t {
  dfsch_type_t* type;
  gdImagePtr img;
} gd_image_t;

typedef struct gd_font_t {
  dfsch_type_t* type;
  gdFontPtr font;
} gd_font_t;

dfsch_type_t dfsch_gd_image_type = {
  .type = DFSCH_STANDARD_TYPE,
  .name = "gd:image",
  .size = sizeof(gd_image_t),
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

static image_finalizer(gd_image_t* img, void* discard){
  if (img->type == DFSCH_GD_IMAGE_TYPE){
    gdImageDestroy(img->img);
  }
}

dfsch_object_t* dfsch_gd_cons_image(gdImagePtr img){
  gd_image_t* i = dfsch_make_object(DFSCH_GD_IMAGE_TYPE);
  i->img = img;
  GC_REGISTER_FINALIZER(i, (GC_finalization_proc)image_finalizer,
                        NULL, NULL, NULL);

  return i;
}
void dfsch_gd_destroy_image(dfsch_object_t* img){
  gd_image_t* i = DFSCH_ASSERT_TYPE(img, DFSCH_GD_IMAGE_TYPE);
  gdImageDestroy(i->img);
  dfsch_invalidate_object(i);
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
