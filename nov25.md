
``` r
library(tidyverse)
library(kableExtra)
library(knitr)
mcma_objs = readRDS("mcma_objs")
all_conjs_expanded = readRDS("all_conjs_expanded")

top50 = mcma_objs %>% 
  arrange(desc(totalRisk)) %>%
  head(50)

top50 = all_conjs_expanded %>%
  filter(noradId %in% top50$noradId) %>%
  group_by(noradId) %>%
  summarise(numConjs = n(),
            `closestApproach(km)` = min(Range),
            avgNumOpSats = mean(numOpSats),
            medianNumOpSats = median(numOpSats),
            `avgPersistenceOfConjAlt(yrs)` = mean(persistence),
            `medianPersistenceOfConjAlt(yrs)` = median(persistence)) %>% 
  right_join(top50, by="noradId")

head(top50) %>% kable() %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:right;">

noradId

</th>

<th style="text-align:right;">

numConjs

</th>

<th style="text-align:right;">

closestApproach(km)

</th>

<th style="text-align:right;">

avgNumOpSats

</th>

<th style="text-align:right;">

medianNumOpSats

</th>

<th style="text-align:right;">

avgPersistenceOfConjAlt(yrs)

</th>

<th style="text-align:right;">

medianPersistenceOfConjAlt(yrs)

</th>

<th style="text-align:right;">

totalRisk

</th>

<th style="text-align:left;">

name

</th>

<th style="text-align:left;">

apogee

</th>

<th style="text-align:left;">

perigee

</th>

<th style="text-align:left;">

cluster

</th>

<th style="text-align:left;">

cluster\_new

</th>

<th style="text-align:left;">

launch

</th>

<th style="text-align:left;">

inclination

</th>

<th style="text-align:left;">

mass

</th>

<th style="text-align:left;">

country

</th>

<th style="text-align:left;">

type

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

22220

</td>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

0.8822671

</td>

<td style="text-align:right;">

214.09091

</td>

<td style="text-align:right;">

218

</td>

<td style="text-align:right;">

567.4955

</td>

<td style="text-align:right;">

562.7905

</td>

<td style="text-align:right;">

8881114922

</td>

<td style="text-align:left;">

SL-16 R/B

</td>

<td style="text-align:left;">

847

</td>

<td style="text-align:left;">

827

</td>

<td style="text-align:left;">

c850

</td>

<td style="text-align:left;">

cc850

</td>

<td style="text-align:left;">

1992-11-17

</td>

<td style="text-align:left;">

71

</td>

<td style="text-align:left;">

8900

</td>

<td style="text-align:left;">

CIS

</td>

<td style="text-align:left;">

ROCKET BODY

</td>

</tr>

<tr>

<td style="text-align:right;">

18794

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

1.8063775

</td>

<td style="text-align:right;">

86.50000

</td>

<td style="text-align:right;">

86

</td>

<td style="text-align:right;">

17773.4466

</td>

<td style="text-align:right;">

17228.3048

</td>

<td style="text-align:right;">

6989777706

</td>

<td style="text-align:left;">

SL-14 R/B

</td>

<td style="text-align:left;">

1471

</td>

<td style="text-align:left;">

1411

</td>

<td style="text-align:left;">

c1500

</td>

<td style="text-align:left;">

cc1500

</td>

<td style="text-align:left;">

1988-01-15

</td>

<td style="text-align:left;">

82.6

</td>

<td style="text-align:left;">

1407

</td>

<td style="text-align:left;">

CIS

</td>

<td style="text-align:left;">

ROCKET BODY

</td>

</tr>

<tr>

<td style="text-align:right;">

15172

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

1.6876847

</td>

<td style="text-align:right;">

77.66667

</td>

<td style="text-align:right;">

85

</td>

<td style="text-align:right;">

23815.0206

</td>

<td style="text-align:right;">

23589.7838

</td>

<td style="text-align:right;">

6469932110

</td>

<td style="text-align:left;">

SL-14 R/B

</td>

<td style="text-align:left;">

1501

</td>

<td style="text-align:left;">

1490

</td>

<td style="text-align:left;">

c1500

</td>

<td style="text-align:left;">

cc1500

</td>

<td style="text-align:left;">

1984-08-08

</td>

<td style="text-align:left;">

82.6

</td>

<td style="text-align:left;">

1407

</td>

<td style="text-align:left;">

CIS

</td>

<td style="text-align:left;">

ROCKET BODY

</td>

</tr>

<tr>

<td style="text-align:right;">

6938

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

2.6364042

</td>

<td style="text-align:right;">

87.00000

</td>

<td style="text-align:right;">

87

</td>

<td style="text-align:right;">

15356.2508

</td>

<td style="text-align:right;">

15482.8559

</td>

<td style="text-align:right;">

6296053131

</td>

<td style="text-align:left;">

OPS 6630 (2)

</td>

<td style="text-align:left;">

1456

</td>

<td style="text-align:left;">

1414

</td>

<td style="text-align:left;">

c1500N

</td>

<td style="text-align:left;">

cc1500

</td>

<td style="text-align:left;">

1973-11-10

</td>

<td style="text-align:left;">

96.92

</td>

<td style="text-align:left;">

2653

</td>

<td style="text-align:left;">

US

</td>

<td style="text-align:left;">

PAYLOAD

</td>

</tr>

<tr>

<td style="text-align:right;">

39486

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

1.6497056

</td>

<td style="text-align:right;">

71.00000

</td>

<td style="text-align:right;">

85

</td>

<td style="text-align:right;">

13476.0157

</td>

<td style="text-align:right;">

18182.5967

</td>

<td style="text-align:right;">

6076818481

</td>

<td style="text-align:left;">

BREEZE-KM R/B

</td>

<td style="text-align:left;">

1509

</td>

<td style="text-align:left;">

1150

</td>

<td style="text-align:left;">

elsewhere

</td>

<td style="text-align:left;">

cleo

</td>

<td style="text-align:left;">

2013-12-25

</td>

<td style="text-align:left;">

82.48

</td>

<td style="text-align:left;">

2390

</td>

<td style="text-align:left;">

CIS

</td>

<td style="text-align:left;">

ROCKET BODY

</td>

</tr>

<tr>

<td style="text-align:right;">

22803

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

1.5265223

</td>

<td style="text-align:right;">

214.85714

</td>

<td style="text-align:right;">

214

</td>

<td style="text-align:right;">

557.1727

</td>

<td style="text-align:right;">

570.0589

</td>

<td style="text-align:right;">

5748144678

</td>

<td style="text-align:left;">

SL-16 R/B

</td>

<td style="text-align:left;">

850

</td>

<td style="text-align:left;">

823

</td>

<td style="text-align:left;">

c850

</td>

<td style="text-align:left;">

cc850

</td>

<td style="text-align:left;">

1993-09-16

</td>

<td style="text-align:left;">

70.99

</td>

<td style="text-align:left;">

8900

</td>

<td style="text-align:left;">

CIS

</td>

<td style="text-align:left;">

ROCKET BODY

</td>

</tr>

</tbody>

</table>
