Replicating values from Cleaveland and McGill (1984)
================

# The desire to replicate

Part of our experiment was to try and replicate the values and
comparisons used in the original experiment from Cleveland and McGill
(1984). By recreating these values, we aim to address two goals.

1)  It is important that our comparisons resemble the results from the
    original experiment. If the conclusions are the same, it will not
    only verify the original results, but also opens the door for us to
    address the other comparisons from 2D to 3D graphics.

2)  By replicating the process used to generate the values in the
    original study, we hope to inspire other researchers to share their
    experimental design process. In the case of Cleveland and McGill, we
    had to make certain assumptions to try and match their data
    generation process, as well as measure the scaling on their plots
    for the identifying marker heights.

# Replication process

In Cleveland and McGill, we used two pieces of information to try and
replicate the compared values. For the design of the position-length
experiment, it is specified that the values compared were

$$s_i=10\cdot 10^{(i-1)/12}, i=1,...,10$$

These values are equally spaced on a log scale, but the reasoning for
these particular values was not provided. The authors then wrote that
“\[s\]ubjects judged the ratios of 10 pairs of values\[.\]” Although not
specifically mentioned, we assumed that this means that each value was
only used twice and that no value was compared to itself.

From Figure 11 of their paper, Cleveland and McGill plotted the log
error by graph type of the position-length experiment with results
faceted by the true ratio. In the written experimental design, it was
only specified that these values ranged from 0.18 to 0.83.

In conjuncture, we created a script that checks all possible
combinations of the specified values and their corresponding ratios for
the criterion listed above. Although our script accounts for all
possible combinations of values with the matching ratios, it is easy to
spot that the 10 and 56.2 combination have the only instance for a ratio
of 17.8%. The process of narrowing down the value pairs is shown below.

| Criteria                         | Number of combinations |
|----------------------------------|------------------------|
| Values only                      | 100 choose 10          |
| Values and matching ratios       | 39 choose 10           |
| 10 and 56.2 removed              | 38 choose 9            |
| Two per value and matching ratio | unknown for now…       |

It was also stated that the other bars were chosen “essentially at
random, but subject to certain constraints.” The constraint listed
seemed to pertain to the stacked bar charts, which was not a focus of
our study. For our process, we opted to give two possible scenarios of
random: $Uniform(0,100)$ and $100\cdot Beta(2, 2)$. For the purposes of
our paper, we chose to use the beta distribution since the uniform
distribution created distracting differences for the bars surrounding
the values.

From here, we randomly assigned the values into the bars that matched
the original study design. For Type 1 from Cleveland and McGill, the
values were placed in the second and third positions within the first
grouping. For Type 3, the bars were placed into the second position of
the first and second groupings.

**Perhaps I could set the positioning arguments if we wanted to make
this a versatile process rather than specifically to mimic Cleveland and
McGill**

# The importance of sharing results

In our study, we spent time carefully reconstructing conditions from
Cleveland and McGill. Despite our best efforts, this is only an
approximation of their study design. Most notably, the random generation
process of the surrounding bars is unlikely to be the same. We chose to
use a scaled beta distribution centered with a height of 50 to help
minimize distracting bar differences. With only two examples from Figure
4 in the Cleveland and McGill paper, it is unclear from which
distribution these bars could have originated from.

`Given the number of possible combinations using the specified values, it was no surprise that there were multiple possible comparison scenarios. While we settled on one particular set, we would have preferred to use the same values.`

**NOTE THAT I NEED TO GET RESULTS BACK FROM THE COMBINATIONS SCRIPT. I’M
NOT SURE HOW MANY RESULTS MATCH THE CRITERIA, BUT I’M GUESSING IT’S NOT
A PERFECT FIT**
