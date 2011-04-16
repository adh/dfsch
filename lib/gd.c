#include <dfsch/lib/gd.h>

gdImagePtr dfsch_gd_image(dfsch_object_t* obj);
dfsch_object_t* dfsch_gd_cons_image(gdImagePtr img);

gdFontPtr dfsch_gd_font(dfsch_object_t* obj);
dfsch_object_t* dfsch_gd_cons_font(gdFontPtr font);
