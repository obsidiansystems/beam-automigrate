
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
