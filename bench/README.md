
# 1000 tables

```
benchmarking diff/reference/1_000 tables avg. case (similar schema)
time                 1.942 s    (78.76 ms .. 2.803 s)
                     0.897 R²   (0.692 R² .. 1.000 R²)
mean                 3.352 s    (2.692 s .. 4.658 s)
std dev              1.301 s    (633.9 μs .. 1.508 s)
variance introduced by outliers: 74% (severely inflated)

benchmarking diff/efficient/1_000 tables avg. case (similar schema)
time                 2.613 s    (2.018 s .. 3.081 s)
                     0.994 R²   (0.979 R² .. 1.000 R²)
mean                 2.944 s    (2.756 s .. 3.114 s)
std dev              204.6 ms   (175.0 ms .. 224.9 ms)
variance introduced by outliers: 20% (moderately inflated)
```

# 10000 tables

## getSchema + diff

```
benchmarking diff/reference/10_000 tables avg. case (similar schema)
time                 112.0 s    (108.2 s .. 116.8 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 126.2 s    (119.6 s .. 136.1 s)
std dev              9.366 s    (472.2 ms .. 11.70 s)
variance introduced by outliers: 21% (moderately inflated)

benchmarking diff/efficient/10_000 tables avg. case (similar schema)
time                 84.06 s    (66.57 s .. 95.27 s)
                     0.995 R²   (0.985 R² .. 1.000 R²)
mean                 75.19 s    (70.39 s .. 78.70 s)
std dev              4.901 s    (1.765 s .. 6.152 s)
variance introduced by outliers: 19% (moderately inflated)

benchmarking diff/reference/10_000 tables worst case (no previous schema)
time                 131.9 s    (112.7 s .. 141.0 s)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 139.9 s    (133.9 s .. 142.1 s)
std dev              4.013 s    (417.9 ms .. 5.048 s)
variance introduced by outliers: 19% (moderately inflated)

benchmarking diff/efficient/10_000 tables worst case (no previous schema)
time                 71.27 s    (60.70 s .. 82.07 s)
                     0.996 R²   (0.995 R² .. 1.000 R²)
mean                 73.81 s    (71.54 s .. 75.99 s)
std dev              2.638 s    (1.731 s .. 3.192 s)
variance introduced by outliers: 19% (moderately inflated)
```

## diff only

```
benchmarking diff/reference/10_000 tables avg. case (similar schema)
time                 376.4 ms   (347.3 ms .. 433.2 ms)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 416.9 ms   (396.4 ms .. 434.2 ms)
std dev              20.86 ms   (17.17 ms .. 23.92 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking diff/efficient/10_000 tables avg. case (similar schema)
time                 140.6 ms   (129.7 ms .. 153.8 ms)
                     0.993 R²   (0.982 R² .. 1.000 R²)
mean                 138.4 ms   (125.3 ms .. 146.8 ms)
std dev              15.39 ms   (6.588 ms .. 24.58 ms)
variance introduced by outliers: 27% (moderately inflated)

benchmarking diff/reference/10_000 tables worst case (no schema)
time                 122.5 μs   (122.1 μs .. 122.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 123.2 μs   (122.7 μs .. 123.8 μs)
std dev              1.788 μs   (1.361 μs .. 2.516 μs)

benchmarking diff/efficient/10_000 tables worst case (no schema)
time                 1.645 ms   (1.582 ms .. 1.702 ms)
                     0.974 R²   (0.954 R² .. 0.985 R²)
mean                 1.687 ms   (1.592 ms .. 1.854 ms)
std dev              406.5 μs   (274.7 μs .. 647.6 μs)
variance introduced by outliers: 93% (severely inflated)
```

## getSchema

```
benchmarking getSchema/10_000 tables
time                 103.5 s    (57.50 s .. 159.5 s)
                     0.967 R²   (0.900 R² .. 1.000 R²)
mean                 80.62 s    (65.63 s .. 94.30 s)
std dev              16.51 s    (6.071 s .. 20.35 s)
variance introduced by outliers: 48% (moderately inflated)
```
