#include <dfsch/lib/csv.h>

DFSCH_DEFINE_PRIMITIVE(read_line, 
                       "Read one line of CSV data into vector"){
  dfsch_csv_params_t* params;
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);
  params = dfsch_csv_params(args);

  return dfsch_csv_read_line(port, params);
}

DFSCH_DEFINE_PRIMITIVE(read_file,
                       "Read CSV file into list of vectors"){
  dfsch_csv_params_t* params;
  dfsch_object_t* port;
  DFSCH_OBJECT_ARG(args, port);
  params = dfsch_csv_params(args);

  return dfsch_csv_read_file(port, params);  
}

void dfsch_module_csv_register(dfsch_object_t* env){
  dfsch_package_t* csv = dfsch_make_package("csv");
  dfsch_provide(env, "csv");
  
  dfsch_defconst_pkgcstr(env, csv, "read-line",
                         DFSCH_PRIMITIVE_REF(read_line));
  dfsch_defconst_pkgcstr(env, csv, "read-file",
                         DFSCH_PRIMITIVE_REF(read_file));
}
