# WORKNOTES

- Need to talk about Type 1 vs Type 3
- Current draft is an outline of ideas. Need to make the flow better
- 975 words. Estimated <500 remain based on Emily's submission (one figure and approx. 1466 words)
- Need figures. Include Figure 4 from CM?

# Abstract

How do different mediums of graphics compare? There has been extensive research showing that 2D data visualizations perform better than 3D visualizations, but these results are almost always relating to 3D graphics projected on a 2D surface. This research aims to address perception of 3D bar charts within a 3-dimensional space.

**Keywords:** graphics, 3D bar charts, replication


# Introduction

It is well documented that 2-dimensional projections of 3-dimensional graphics perform worse than their 2-dimensional equivalents when it comes to the ability to accurately extract information. (REFERENCES HERE) The goal of our study is to replicate part of Cleveland and McGill's original study of ordering elementary perceptual tasks using three mediums of data visualization: 2D digital bar charts, 3D digital bar charts, and 3D printed bar charts.

## Introduction: Cleveland and McGill

In 1984, William S. Cleveland and Robert McGill wrote a paper outlining a theory for ordering the importance of graphical elements known as elementary perceptual tasks (EPTs). For their first experiment, known as the position-length experiment, they used two bar charts and three stacked bar charts to compare an accuracy response of relative size proportions for the EPTs of position and length. 

Participants were provided 50 graphs with two bars marked by a dot for comparison. Subjects were asked to identify which of the two bars were smaller and to judge what percentage of the smaller was of the larger. They were informed to make "a quick visual judgment and not try to make precise measurements, either mentally or with a physical object such as a pencil or your finger." 

Nearly all responses were correct for identifying the smaller bar. For the size judgement, the response for the participants is a log distance deviation away from the true value.


**MORE DETAILS HERE? PERHAPS QUICK RESULTS SUMMARY??**

## Introduction: Research Objectives

The goal of our research is to replicate partial results from Cleveland and McGill and extending their study to multiple mediums of data visualization. Here, we provide the process of replication and  modernization of testing length and position judgements to various presentations of data.



# Methods

## Replicating the original study

The first step of replication is to approximate the aspect ratios and value comparisons. To get the aspect ratios, we measured the pixel ratios of Figure 4 from Cleveland and McGill's paper. Figure 4 provides the graph types used in the position-length experiment, but it is unclear if the aspect ratios of these plots match those used for their experiment.

The values of comparison are linear on a log scale and are given by

$$s_i=10\cdot 10^{(i-1)/12}, i=1,...,10$$

To approximate the comparisons in the study, we referenced the ratios given from Figure 11 of Cleveland and McGill's paper and subjected the possible constraint that each value was used no more than twice. The end result was 252 possible comparisons, from which we randomly chose one set of comparisons. All other bars within the plots are randomly distributed, which we opted to use values from a Beta(3,5) distribution scaled from 0 to 100.

The participants in our study will be asked the same two questions that were asked in the position-length experiment. Responses will be measured as a binary response for correctly stating the smaller bar and accuracy of the relative size proportion as measured by $\text{log}_2(|\text{judged percent}-\text{true percent}|+1/8)$

## Designing Graphics



Due to some limitations, we made creative adjustments to closely match both types of 3D charts to our 2D charts.

**FIGURE WITH PLOT TYPES. USE PROTOTYPE FOR 3D PRINTED?**

### 2D Bar Charts

To create our 2D bar charts, we utilized ggplot2. We identify the bars for comparison with dots where the heights were predicted to be at 5 out of 100 from Figure 4 of Cleveland and McGill's study.

### 3D Bar Charts

**CLEAN UP THE FLOW OF THIS SECTION**

For the 3D bar charts, we used the rayshader package. To keep a homogeneous appearance of data elements, we keep a solid light grey appearance for the digital 3D representation. The scaling of the height from rayshader was determined by saving plots from a direct viewing angle and comparing aspect ratios of one bar to the width of all bars as measured by pixel distances to the equivalent 2D data representation. However, small discrepancies exist for the scaling factor for different bar heights, so we opted to use one of the "middle" bar heights of 26.1 to determine our final scale. 

Bars are identified by a circle or triangle on the top 

The digital 3D charts are initially angled to mimic the default 3D bar charts of Microsoft Excel, but the interactivity of Shiny will allow users to adjust the viewing angle.

The only difference between the digital and 3D printed versions of these charts is that we need to raise elements for identification with the 3D printed versions. We 

# Pilot Study

Data is generated for a Incomplete Block Design. 
Participants will be given one of 12 kits, with each kit containing five graphs of each graph type for a total of 15 graphs. 
Five of seven unique ratios will be chosen at random and used for each graph type within a kit. 
The graphs will then be randomly assigned a length comparison or a position comparison. The goal is to detect differences between graph types.

The 2D and digital 3D charts will be fully enclosed within a Shiny application. **HOW WILL PARTICIPANTS ANSWER QUESTIONS ABOUT 3D PRINTS?? SHINY WITH IDENTIFIER? PAPER?** Subjects will identify which bar is smaller and what percentage the smaller bar is of the larger bar. 



# Future Work

We will soon perform our pilot study in the statistics department of our university. For Spring semester of 2023, we will bring the study into the semester curriculum of an introductory statistics course after any adjustments are made to the pilot study.

