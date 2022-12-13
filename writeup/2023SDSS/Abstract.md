# WORKNOTES
- Need figures. Include Figure 4 from CM?
- Improve introduction, including 

# Abstract

How do different mediums of graphics compare? 
There has been extensive research showing that 2D data visualizations perform better than 3D visualizations, but these results are almost always relating to 3D graphics projected on a 2D surface. 
This research aims to address perception of 3D bar charts within a 3-dimensional space.

**Keywords:** graphics, 3D bar charts, replication


# Introduction (NEED WORK HERE)

It is well documented that 2-dimensional projections of 3-dimensional graphics perform worse than their 2-dimensional equivalents when it comes to the ability to accurately extract information. (REFERENCES HERE) 
The goal of our study is to replicate part of Cleveland and McGill's 1984 study of ordering elementary perceptual tasks using three mediums of data visualization: 2D digital bar charts, 3D digital bar charts, and 3D printed bar charts.

## Introduction: Research Objectives

The goal of our research is to replicate partial results from Cleveland and McGill and extend their study to multiple mediums of data visualization. 
Here, we provide the process of replication and modernization of testing length and position judgements to various presentations of data.

## Selected Components From Cleveland and McGill

Cleveland and McGill provided a theory and tested for the ordering of perceptual importance for the elements of length, position, and angle. 
The first experiment, referenced as the position-length experiment, used two bar charts and three stacked bar charts.
Each chart had two bars used for comparison and participants were asked what bar was smaller and what was the ratio of the smaller to the larger bar.
The two bar charts are for the perceptual element of position, where one has zero distance between bars and the other has a fixed distance between bars.
Cleveland and McGill found that the distance between bars could influence the error of responses and that position along a common scale comparisons are better than length comparisons.

Our study will replicate the procedure of the comparisons for the two types of bar charts, but with an objective of detecting differences between 2D graphs, 3D digital graphs, and 3D printed graphs.

# Methods

## Replicating Cleveland and McGill

As part of our replication process, we decided to match graph aspect ratios and value comparisons as closely as possible. 
The plot aspect ratio was estimated to be 4:3.3 from a figure in Cleveland and McGill's paper.

The values of comparison are linear on a log scale and are given by

$$s_i=10\cdot 10^{(i-1)/12}, i=1,...,10$$

we referenced the given ratios to approximate the comparisons in the study, which were 17.8, 26.1, 38.3, 46.4 (twice), 56.2, 68.1 (twice), and 82.5 (twice).

We subjected these comparisons to the possible constraint that each value of comparison was used no more than twice. 
Once the values were determined, we used a scaled Beta distribution to generate the remaining bars values between 0 and 100.
We provided parameters to the Beta distribution that limited the amount of obsessive noise across the bar charts.

The participants in our study will be asked the same two questions that were asked in the position-length experiment.
Responses will be measured as a binary response for correctly stating the smaller bar and accuracy of the relative size proportion as measured by $\text{log}_2(|\text{judged percent}-\text{true percent}|+1/8)$.

## Designing Graphics

Due to limitations, we made creative adjustments from Cleveland and McGill to closely match both types of 3D charts to our 2D charts. 
The graphs share a common layout across the three chart types. 
There are two groupings of five bars each. 
Each grouping is identified by either "A" or "B". 
The values of comparison are marked by either a circle or a triangle at a height of 5 out of 100, which was estimated from Cleveland and McGill's paper. 

**FIGURE WITH PLOT TYPES. USE PROTOTYPE FOR 3D PRINTED?**

### 2D Bar Charts

To create our 2D bar charts, we utilized ggplot2. 
We identify the bars of comparison with a circle and triangle within the bar.
The scale axis was removed, leaving only the bars and a bar grouping identifier. 

### 3D Bar Charts

For the 3D bar charts, we used the rayshader package. 
The scaling of the height from rayshader was determined by saving plots from a direct viewing angle and comparing aspect ratios of one bar to the width of all bars as measured by pixel distances to the equivalent 2D data representation. 
However, small discrepancies exist for the scaling factor for different bar heights, so we opted to use one of the "middle" bar heights of 26.1 to determine our final scale. 

We identified the values of comparison by placing the circle and triangle on top of the corresponding bar.
For the 3D printed charts, we raised the identifing marks by a marginal amount so that they can be immediately identified after printing. 
Other non-data related elements were also raised for the same reason.

The digital 3D charts are initially angled to mimic the default 3D bar charts of Microsoft Excel, but the interactivity of Shiny will allow users to adjust the viewing angle. For a homogeneous appearance of data elements, we keep a solid light grey appearance for the digital 3D representation. 

# Pilot Study

Data is generated for an Incomplete Block Design. 
In the spirit of replicating Cleveland and McGill's study, we asked members of our Statistics department and their spouses/partners to particpate in our study.

Participants were randomly assigned 1 of 12 kits, with each kit containing five graphs of each chart type for a total of 15 graphs. 
Five of seven unique ratios were chosen at random and used for each graph type within a kit. 
The graphs were then randomly assigned a distance-betwen-bars comparison.

To administer the graphs and questions, the 2D and digital 3D charts are fully enclosed within a Shiny application. 
The Shiny application randomly determines an order for participants to be shown graphs.
For the 3D printed graphs, Shiny directs participants to grab a single graph from a bag. 
The graph will have a short numeric identifier for participants to input into the Shiny application along with their answers. 
Participants are asked which bar is smaller and to estimate the ratio of the smaller bar to the larger bar.

We will present the results and assess the implications for the design of charts, and reflect what this means for 3D charts.

# Future Work

For Spring semester of 2023, we hope to bring the study into the semester curriculum of an introductory statistics course. 
The large sample size will help validate the results from our pilot study and provide 


