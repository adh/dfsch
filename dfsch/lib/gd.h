#ifndef H__dfsch__lib__gd__
#define H__dfsch__lib__gd__

#include <gd.h>
#include <dfsch/dfsch.h>

gdImagePtr dfsch_gd_image(dfsch_object_t* obj);
dfsch_object_t* dfsch_gd_cons_image(gdImagePtr img);

gdFontPtr dfsch_gd_font(dfsch_object_t* obj);
dfsch_object_t* dfsch_gd_cons_font(gdFontPtr font);

extern dfsch_type_t dfsch_gd_image_type;
#define DFSCH_GD_IMAGE_TYPE (&dfsch_gd_image_type)

extern dfsch_type_t dfsch_gd_font_type;
#define DFSCH_GD_FONT_TYPE (&dfsch_gd_font_type)

#define DFSCH_GD_IMAGE_ARG(al, name)                            \
  DFSCH_GENERIC_ARG(al, name, gdImagePtr, dfsch_gd_image)

#define DFSCH_GD_FONT_ARG(al, name)                     \
  DFSCH_GENERIC_ARG(al, name, gdFontPtr, dfsch_gd_font)

#endif
