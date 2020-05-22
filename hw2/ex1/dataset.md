```r
> sapply(dataset, mean)
  factor1   factor2   factor3   factor4   factor5   factor6   factor7   factor8   factor9  factor10    answer
0.7842933 0.5028142 1.9607797 0.7873607 0.5052390 1.2871075 4.7058528 1.5716540 3.3135556 4.5404868 4.5699915
```

```r
> sapply(dataset, sd)
  factor1   factor2   factor3   factor4   factor5   factor6   factor7   factor8   factor9  factor10    answer
0.5897107 0.2854027 1.9608116 0.6018937 0.2853287 0.6579975 3.9701470 0.8470009 1.5661690 2.2034930 1.9700760
```

### Response

factor |   min     |   max     |
-------|-----------|-----------|
f1     | 0.000836  | 3.483189  |
f2     | 0.0000648 | 0.9997571 |
f4     | 0.00125   | 3.43439   |
f5     | 0.0001625 | 0.9991915 |

Answer = 0.005562 + 1.032*f_1 + 0.988*f_2 + 0.988*f_4 + 4.94*f_5

### Validation of exercise 3

Operational Validation: results of the codification of the model

1. Black Box validation
  - Divide the data set in testing area
  - Compare this test with the results using ANOVA

2. GPSS Traces - Validation Trace
  - Go step by step and compare traces
