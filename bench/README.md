
# 1000 tables

## Diffing from noSchema

-

## Diffing using genSimilarSchemas (avg case)

benchmarking diff/reference/10_000 tables avg. case (similar schema)
time                 1.942 s    (78.76 ms .. 2.803 s)
                     0.897 R²   (0.692 R² .. 1.000 R²)
mean                 3.352 s    (2.692 s .. 4.658 s)
std dev              1.301 s    (633.9 μs .. 1.508 s)
variance introduced by outliers: 74% (severely inflated)

benchmarking diff/efficient/10_000 tables avg. case (similar schema)
time                 2.613 s    (2.018 s .. 3.081 s)
                     0.994 R²   (0.979 R² .. 1.000 R²)
mean                 2.944 s    (2.756 s .. 3.114 s)
std dev              204.6 ms   (175.0 ms .. 224.9 ms)
variance introduced by outliers: 20% (moderately inflated)

# 10000 tables

## Diffing from noSchema

benchmarking diff/reference/10_000 tables worst case (no previous schema)
time                 102.9 s    (102.0 s .. 103.6 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 103.0 s    (102.7 s .. 103.2 s)
std dev              283.2 ms   (34.26 ms .. 396.7 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking diff/efficient/10_000 tables worst case (no previous schema)
time                 91.17 s    (74.11 s .. 115.3 s)
                     0.992 R²   (0.977 R² .. 1.000 R²)
mean                 78.37 s    (70.52 s .. 84.61 s)
std dev              8.349 s    (6.181 s .. 9.988 s)
variance introduced by outliers: 23% (moderately inflated)

## Diffing using genSimilarSchemas (avg case)

benchmarking diff/reference/10_000 tables avg. case (similar schema)
time                 91.02 s    (73.03 s .. 122.3 s)
                     0.987 R²   (0.969 R² .. 1.000 R²)
mean                 76.64 s    (70.40 s .. 83.94 s)
std dev              7.617 s    (3.490 s .. 10.62 s)
variance introduced by outliers: 23% (moderately inflated)

benchmarking diff/efficient/10_000 tables avg. case (similar schema)
time                 75.94 s    (59.69 s .. 88.71 s)
                     0.995 R²   (0.981 R² .. 1.000 R²)
mean                 86.28 s    (80.63 s .. 91.17 s)
std dev              6.274 s    (5.016 s .. 7.096 s)
variance introduced by outliers: 20% (moderately inflated)
