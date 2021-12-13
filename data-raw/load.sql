COPY pick FROM 'data-raw\2_pick.csv' (FORMAT 'csv', quote '"', header 0, delimiter ',');
COPY stat FROM 'data-raw\0_stat.csv' (FORMAT 'csv', quote '"', header 0, delimiter ',');
COPY tipper FROM 'data-raw\1_tipper.csv' (FORMAT 'csv', quote '"', header 0, delimiter ',');
