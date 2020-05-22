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


          GENERATE         5,2
          QUEUE            ALM
          SEIZE            OPER
          DEPART           ALM
          SPLIT            1,MED    ;start calibration
          ADVANCE          3
MED1      MATCH            MED2     ;wait for calibration
          ADVANCE          2
          RELEASE          OPER
          TERMINATE        1

MED       ADVANCE          5,3
MED2      MATCH            MED1    ;we need to wait for MED1 to be finished before finishing
          TERMINATE

          ;START           200
