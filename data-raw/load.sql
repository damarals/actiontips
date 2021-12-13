COPY tip FROM 'data-raw\2_tip.csv' (FORMAT 'csv', quote '"', header 0, delimiter ',');
COPY stat FROM 'data-raw\0_stat.csv' (FORMAT 'csv', quote '"', header 0, delimiter ',');
COPY tipper FROM 'data-raw\1_tipper.csv' (FORMAT 'csv', quote '"', header 0, delimiter ',');
