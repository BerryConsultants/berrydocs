# Graphs of Simulation Results

To enable swift visualization and analysis of the simulation results, FACTS has a number of pre-defined graphs it can display. Full and detailed simulation results are available in ‘csv’ format files that can be loaded into other analysis tools allowing any aspect of the simulation to be explored. These files are described in Section 16, below.

To view the graphs of the results of the simulations of a particular design variant in a particular scenario, select a row of scenario results by clicking on it and then click on the ‘View Graph’ button and select “Show Per Scenario Graphs”.

To view graphs showing the results of the simulations of possibly all the design variants and all the scenarios click on the ‘View Graph’ button and select “Show Across Scenario Graphs”. This launches a graph display that displays multiple graphs in a trellis plot.

![](../coreUGattachments/tte/media/image51.png)

Figure 1: Select the scenarios to include in Across Scenario graphs.

The graph display supports copying an image of the graph to the clipboard, to facilitate pasting them into documents and presentations. Right clicking on a graph brings up a short menu that allows the image of the graph to be copied to the clipboard or saved in ‘.png’ format to a file.

Many graphs have a number of controls to allow the graph to be tailored, standard graph controls available on most graphs are:

- Set Y axis – this displays a dialog boxing allowing the user to fix the minimum and maximum of each of the Y axes and the number of ‘tick’ marks. (Not displayed if the ‘y’ value must lie in the interval 0-1.

- Space doses evenly – if the x-value is ‘dose’, then data for each dose can either be spaced equally or proportionate to the doses’ effective dose strength.

#### Box and whisker plot conventions

- The mean probability is plotted as a large dot.

- The median value is plotted as a dashed line.

- The 25-75^(th) quantile range is plotted as the “box” portion of each point.

- The “whiskers” extend to the largest and smallest values within 1 ½ times the interquartile range from either end of the box.

- Points outside the whisker range are considered outliers, and are plotted as small blue dots. Note that it may be difficult to see all of these symbols if they are plotted at the same value.

# Per Scenario Graphs

## Allocation Box and Whisker Plot

![](../coreUGattachments/tte/media/image52.png)

Figure 2: Distribution of Subject Allocation Across Simulations.

This graph displays a box and whisker plot of the number of subjects recruited into each arm. These plots show:

- The distribution over all simulations of the number of subjects allocated to each arm shown as a box and whisker plot. If the design is not adaptive, the number allocated will be the same in every simulation and the box and whiskers collapse to a single line.

- The mean number of events observed in each arm across the simulations shown as a red triangle.

## Events Boxplot

![](../coreUGattachments/tte/media/image53.png)

Figure 3: Distribution of Number of Events Across Simulations

This graph displays a box and whisker plot of the number of events observed in each arm. These plots show:

- The distribution over all simulations of the number of events observed in each arm shown as a box and whisker plot.

## Hazard Ratio and Subject Allocation

![](../coreUGattachments/tte/media/image54.png)

Figure 4: Hazard Ratio and Subject Allocation

This graph shows for each treatment arm, the mean subject allocation, mean number of events observed and the mean estimated hazard ratio. Specifically:

- The blue bars show the mean number of subjects without events and the brown bars mean number of subjects who had events.

- The black line shows the true Hazard Ratio being simulated (but without allowing for any effect of the predictor)

- The green dashed line (drawn if a response model is fitted), shows the mean of the estimated hazard ratios across the simulations

- The green ‘error bars’ show the spread of the central 95% of the estimated hazard ratios (if less than 20 simulations have been run it simply shows the full spread).

## Hazard Ratio and Target Selection graphs

![](../coreUGattachments/tte/media/image55.png)

Figure 5: Hazard Ratio and $Pr$(MED relative to Control: Delta=-0.2) Selection

These plots show the true simulated hazard ratio (without allowance for the effect of the predictor) and the mean and 95% spread of the mean fitted hazard ratios along with bars showing the proportion of simulations the different treatment arms are the finally selected target. The target displayed is selected by a control on the graph, from any of the defined Target QOIs.:

## Predictor: Response and Allocation

![](../coreUGattachments/tte/media/image56.png)

Figure 6: Predictor Hazard Ratio and Subject Allocation

These plots show the true simulated predictor response and the mean and 95% spread of the mean fitted predictor response model, with the blue bars showing the mean number of subjects allocated to each arm across the simulations.

## Per Dose: QOIs

![](../coreUGattachments/tte/media/image57.png)

Figure 7: Distribution of P(Succ. Future Trial: N=250; Sup. $\alpha=0.025$, $\delta=0$ Adjusted Significance) Across Simulations

These plots show a box plot showing the distribution of the values for any of the defined QOIs for each dose.

Which QOI is displayed can be selected by the user from a drop down list on the graph. Any of the Posterior Probability, Predictive Probability, P-value and Target QOI’s can be selected.

## Target Hazard Ratio Scatter Plot

![](../coreUGattachments/tte/media/image58.png)

Figure 8: Posterior Mean Hazard Ratio at Pr(Max) vs Total Sample Size per Simulation Run

This graph shows a scatter plot of trial outcomes with the estimate hazard ratio as the y-axis and total number of subjects recruited as the x-axis. A drop down list on the graph allows the user to select which target QOI is used to supply the hazard ratio:

The trials are plotted with a symbol that indicated the final outcome:

- A light blue circle indicates a trial that stopped early for success

- A dark blue circle indicates a trial that was a late success

- A dark red square indicates a trial that was a late futility

- A light red square indicates a trial that stopped early for futility

- A light pink diamond indicates a trial that stopped early for success but where the final analysis was futility

- A dark pink diamond indicates a trial that stopped early for futility but where the final analysis was success

- A yellow cross indicates a trial that completed full enrolment but was inconclusive at the end.

These graphs are not particularly useful if there is no early stopping.

If the design does allow early stopping this graph shows

- The overall distribution of sample sizes – in particular is stopping evenly distributed from the moment stopping is possible, or is there are large cohort of trials that stop as soon as its possible?

- When and with what response rate trials stop incorrectly, in particular having trials stopping and the decision being reversed suggests stopping is allowed to soon or the criteria is not conservative enough.

There are two further variants of this graph, these have alternative y-axes: the hazard ratio at the EDx or at the MED.

## Cumulative Operating Characteristics Plot

![](../coreUGattachments/tte/media/image59.png)

\(a\) Cumulative Proportion of Duration

![](../coreUGattachments/tte/media/image60.png)

\(b\) Cumulative Proportion of Subjects

Figure 9

There are two graphs, one that shows the cumulative proportion of durations across all simulations, and the other shows the cumulative proportion of subjects across all simulations.

## Time course for stopping

There are two graphs, one that shows the distribution over time for stopping for futility and one for stopping for success.

![](../coreUGattachments/tte/media/image61.png)

\(a\) Cumulative Proportion of Futility Over Time

![](../coreUGattachments/tte/media/image62.png)

\(b\) Cumulative Proportion of Success Over Time

Figure 10

These plots show as cumulative curves the proportion of the simulations that stopped at different time points from the start of the trial.

The x-axis is configurable, the user can select cumulative stopping to be plotted relative to time, sample size or number of events observed.

## Arm Dropping Graphs

If the design has used arm dropping, the following graphs are available.

### Hazard ratio and Ppn Arms Dropped

![](../coreUGattachments/tte/media/image63.png)

Figure 11: Hazard Ratio and Proportion Arm Dropped

This graph shows the true (simulated) mean response and the average fitted response with 2.5% & 97.5% quantiles.

The histogram bars show for each arm the proportion of simulations for which the arm was dropped.

### Time Course for Arm Dropping

This graph shows, for each arm, the cumulative proportion of simulations when it was dropped. The user can select whether time or number of subjects allocated to the arm is used as the x-axis.

![](../coreUGattachments/tte/media/image64.png)

Figure 12: Cumulative Proportion of Trials where Arm Dropped

### Time Course for Arm retention

This graph shows, for each arm, the cumulative proportion of simulations when the arm was retained. The user can select whether time or number of subjects allocated to the arm is used as the x-axis. This is simply the inverse of the arm dropping graph above.

![](../coreUGattachments/tte/media/image65.png)

Figure 13: Cumulative Proportion of Trials Where Arm Retained

### Arm Retention Proportion

This graph shows, for each arm, the simple proportion of simulations where the arm was retained.

![](../coreUGattachments/tte/media/image66.png)

Figure 14: Arm Retention Proportion

## Frequentist

These graphs are available if frequentist analysis is enabled.

### Frequentist P(significance)

These graphs are the box and whisker plots of various p-values that are automatically calculated (that is to say they do not need to be defined as QOIs for them to be calculated) for the final analysis. The user can select from Unadjusted, Bonferroni and Dunnett's and LOCF, BOCF (if baseline is simulated) or PP (per-protocol) treatment of missing values.

![](../coreUGattachments/tte/media/image67.png)

Figure 15: Distribution of $P$(Log Rand Adjusted Significance) Across Simulations

### Frequentist: Response and Significance

This graph plots the mean and 2.5-97.5 percentile of the mean across the simulations or the simple frequentist estimate of the response for each arm, along with a histogram plot of the ppn of times the response on each arm was significant. The user can select from the Unadjusted, Bonferroni or Log Rank, Log Rank Bonferroni, Wilcox and Wilcox Bonferroni p-values.

![](../coreUGattachments/tte/media/image68.png)

Figure 16: Hazard Ratio and Ppn Log Rank Significance

## Per Sim Graphs

This set of graphs includes a control that allows the user to select which simulation to graph the results from. Each graph shows the output from only 1 simulated trial.

![](../coreUGattachments/tte/media/image70.png)

Figure 17: The plot window when selecting a Per Sim graph. Note the Simulation control in the bottom left allowing for the selection of which simulated trial the graph should be made for.

### Hazard Ratio and Subject Allocation

This graph shows the final analysis at the end of individual simulations.

![](../coreUGattachments/tte/media/image69.png)

Figure 18: Hazard Ratio and Subject Allocation

On each graph

- The black line shows the ‘True’ Hazard Ratio being simulated, (not adjusted to include any predictor effects)

- The blue bars show the mean number of subjects without events and the brown bars mean number of subjects who had events.

- If a response model has been fitted:

  - The green solid line shows the estimate of the mean hazard ratio

  - The green dashed lines show the boundaries of the 95% credible interval for the estimate of the hazard ratio.

- Otherwise if the Independent Dose model has been fitted:

  - The light green circle shows the mean estimate of the hazard ratio for each arm

  - The vertical green arms show the extent of the 95% credible interval for the estimate of the hazard ratio for each arm.

### Posterior Quantities

This graph shows the final analysis at the end of individual simulations, including the distribution of a selected QOI.

![](../coreUGattachments/tte/media/image71.png)

Figure 19: Hazard Ratio and $Pr$($HR_d$ - 1 \< -0.2) Probability

This graph includes a control that allows the user to select which simulation to graph the results from and which QOI to plot. On each graph

- The black line shows the ‘True’ mean response being simulated.

- The brown circles and arms show the mean raw data and its 95% CI for each arm.

- If a response model has been fitted:

  - The green solid line shows the estimate of the mean hazard ratio

  - The green dashed lines show the boundaries of the 95% credible interval for the estimate of the hazard ratio.

- Otherwise if the Independent Dose model has been fitted:

  - The light green circle shows the mean estimate of the hazard ratio for each arm

  - The vertical green arms show the extent of the 95% credible interval for the estimate of the hazard ratio for each arm.

- Brown bars showing the final value of the selected QOI for each arm at the end of the simulated trial.

The user can select from any of the Posterior Probability, Predictive Probability or Target Probability QOIs to plot, including QOI’s based on the predictor.

### Predictor Response and Allocation

![](../coreUGattachments/tte/media/image72.png)

Figure 20: Predictor Hazard Ratio and Subject Allocation (Week: 80)

If the simulations include a predictor this graph is available.

This graph includes a control that allows the user to select which simulation to graph the results from. On each graph

- The black line shows the ‘True’ Predictor response being simulated

- If a response model has been fitted:

  - The green solid line shows the estimate of the mean hazard ratio

  - The green dashed lines show the boundaries of the 95% credible interval for the estimate of the hazard ratio.

- Otherwise if the Independent Dose model has been fitted:

  - The light green circle shows the mean estimate of the hazard ratio for each arm

  - The vertical green arms show the extent of the 95% credible interval for the estimate of the hazard ratio for each arm.

- Blue Bars showing number of subjects allocated to each treatment arm.

## Per Interim Graphs

This is an identical set of graphs to the Per Sim graphs, the difference is that in addition to a control to select which simulation to graph the results from, there is a control to select which interim within the simulation to graph the results from. These graphs are only available for those simulations for which ‘weeks’ files have been output (by default the first 100). Note the control in the bottom left that allows for specification of Simulation number as well as Interim number.

![](../coreUGattachments/tte/media/image73.png)

Figure 21: The plot window when selecting a Per Interim graph. Note the Simulation control in the bottom left allowing for the selection of which simulated trial and which interim analysis the graph should be made for.

## Post Simulation Boundary Finding Graphs

### Explore Success/Futility Eval Criteria

These graphs can be used to explore what proportion of simulated trials of a particular scenario would have been a success/failure at final evaluation. Using the two drop down controls the user can select the target dose to use: Max, EDx or MED, and set and lower/upper limits to explore for the threshold (setting the range used on the x-axis).

For the given target the proportion of trials that would meet each of the criteria over the range of threshold values is plotted. As in the examples above, the plots will be somewhat jagged if only a small number of simulations have been run. These graphs can be used to select thresholds that can be expected to yield a certain level of type-1 or power, but the user must remember these will only be approximate (depending on the number of simulations) but can be useful to understand the designs sensitivity to the thresholds and to set initial thresholds early on in the design / simulation process that will get close to the desired type-1 error and power from the outset.

### Explore Early Success/Futility Eval Criteria

![](../coreUGattachments/tte/media/image74.png)

\(a\) Cumulative Proportion of Simulations Satisfying Futility Criterion by Interim

![](../coreUGattachments/tte/media/image75.png)

\(b\) Cumulative Proportion of Simulations Satisfying Success Criterion by Interim

Figure 22

These graphs can be used to explore what proportion of simulated trials of a particular scenario would have stopped early for success/futility. NOTE these graphs require weeks files to have been output, they are also most use where the design has been simulated with interims but no early stopping (as in the examples above where the shape of the “existing stopping rules” line indicates that no early stopping occurred in these simulations).

Using the two drop down controls, the user can select which stopping criteria is evaluated and from which interim stopping will be permitted. Lines are then displayed for the proportion of simulations that would have stopped by each interim for a fixed set of thresholds.

Typically these can be used to see at what threshold (and starting at what interim) stopping for success or futility introduces an unacceptably level of ‘incorrect’ early stopping – stopping for futility in successful scenarios and stopping for success in futile scenarios and whether at below/above these levels there may be a useful probability of correct stopping.

### Explore Arm Dropping Criteria

![](../coreUGattachments/tte/media/image76.png)

Figure 23: Cumulative Proportion of Simulations Where Selected Arm Satisfies Criterion by Interim

This graph is similar to the Explore Early Success/Futility Criteria graph but for arm dropping criteria. The user has an additional control to select which dose the proportion of trials in which the arm would be dropped is displayed.

### Success/Futility Stopping Contours

![](../coreUGattachments/tte/media/image77.png)

Figure 24

These graphs allow early stopping and final evaluation criteria to be considered jointly. Like the explore early stopping criteria plots, these graphs require weeks files to have been output and will work best if interims were evaluated but no trials actually stopped early.

The user selects the criterion to plot, the first interim at which early stopping is allowed, and the upper/lower limit of the threshold to consider.

FACTS will then plot contours where (final evaluation threshold, early stopping threshold) yield the same proportion of trials that are a success/futile. Contours are only plotted where final evaluation threshold \< early stopping threshold for success and vice versa for futility. Early stopping criteria should not be less strict than final criteria.

Again the example graphs shown are based on only 100 simulations with weeks files, so any threshold derived will only very approximately yield these success/futility rates.

# Across Scenario Graphs

## Selected Arms

This graph shows a bar chart for each scenario and variant selected. Each chart shows how often each arm was ‘selected’ by the target QOI specified on the Study \> Variants tab. Each bar uses a stacked bar to show the proportion of times that arm was:

- “Successful” –the arm was correctly selected (marked as “Should succeed” on the VSR tab) and the trial was successful.

- “Should not succeed” – the arm was incorrectly selected (not marked as “Should succeed” on the VSR tab) and the trial was successful.

- Unsuccessful – the arm was selected and the trial failed.

![](../coreUGattachments/tte/media/image79.png)

Figure 25: Across scenario graph for selected arms with 4 scenarios and 3 design variants.

## QOI Box Plots

This graph shows a box and whisker plot for each scenario and variant selected. Each plot shows the distribution of the values of a selected QOI for each arm. There is a drop down control to allow the selection of the QOI to be displayed. Any Posterior probability, Predictive probability, p-value or target QOI can be selected.

![](../coreUGattachments/tte/media/image80.png)

Figure 26: Across scenario graph showing QOIs per arm with 4 scenarios and 3 design variants.

## Ppn Success

This grouped bar chart shows a bar for the proportion of successful simulations for each variant, grouped by scenario.

![](../coreUGattachments/tte/media/image81.png)

Figure 27: Across scenario graph for showing the proportion of trials that result in success with 4 scenarios and 3 design variants.

## Response

This graph shows a dose response plot for each scenario and variant selected. Each plot shows the mean estimate over the simulations and the 95%-ile interval of the mean estimates over the simulations. The graph also shoes the “true response” i.e. the mean response being simulated.

![](../coreUGattachments/tte/media/image82.png)

Figure 28: Across scenario graph for showing the average estimated dose response model vs. the true dose responses with 4 scenarios and 3 design variants.

## Allocation

This graph shows a box and whisker plot for each scenario and variant selected. Each plot shows the distribution of the number of subjects allocated to each arm over the simulations.

![](../coreUGattachments/tte/media/image83.png)

Figure 29: Across scenario graph for showing the distribution of allocation per arm with 4 scenarios and 3 design variants.

## Sample Size

This graph shows the mean sample size for each scenario at different maximum sample sizes (the different variants).

![](../coreUGattachments/tte/media/image84.png)

Figure 30: Across scenario graph for showing the mean sample size of the study with 4 scenarios and 3 design variants.

## Interim vs Final Scatter Plot

This is an interactive plot that shows the outcome of each simulation over all the scenarios of a selected variant.

The x-axis shows the value of the selected decision QOI at the specified interim, and the y-axis shows the final value of a (possibly different) selected decision QOI. As well as selecting a decision QOI for the final decision, the graph can use the actual final decision in the simulations – the value of the QOI used in the first criteria is used for the decision is used for the y-axis.

Note the graph is based just on the values at the selected interim, what might happen at other interims is ignored.

Thresholds can be set to specify early and late success/futility. Each scenario is then coloured to show its outcome – and stacked bar graphs are displayed to show the proportion of each outcome for each scenario. The thresholds can be changed and the graph re-drawn. If the selected QOI uses a p-value the checks for success are that it is “\<” than the threshold, (and vice-versa for futility), otherwise the checks for success are that it is “\>” the threshold (and vice-versa for futility).

The graph can only display the results for simulations for which cohorts files have been output, and for simulations that reached the specified interim. Thus the graph works best if interims have been specified, but the simulations run without any early stopping enabled – so each simulation has the results for every interim.

With 1,000 simulations per scenario and cohorts files for each simulation, the graph will take several seconds to re-draw. To allow several parameters to be set before redrawing (rather than having to wait for the redraw after each change) the graph is only redrawn after the ‘Redraw’ button is clicked.

![](../coreUGattachments/tte/media/image85.png)

Figure 31: Across scenario graph showing an interim QOI plotted against a final analysis QOI with the decision dictating the point color. Additionally, the bars on the right hand side represent the proportion of trials that made each decision based on the decision rules specified in the plot controls.

## Receiver Operating Characteristics

This graph plots the relative proportion of successful simulations in the scenarios compared to the proportion of successes in one specific scenario. The intended use of this is to yield a “Receiver Operating Characteristics” (ROC) curve. This is done as follows:

- Select the decision QoI that is to be used to determine final success/futility. (The graph only supports the simple case where just one is being used).

- Select a scenario that represents the ‘Null’ case – where determining success is a type-1 error.

- If multiple variants have been simulated, select which variant to plot.

FACTS computes for the null scenario, for a range of decision threshold values, what proportion of the ‘Null’ scenario sims would have been successful (giving the estimated type-1 error rate), these proportions form the x-values for each point. The corresponding threshold values are shown on the x-axis at the top of the graph, the type-1 error rate on the x-axis at the bottom of the graph.

FACTS then computes for each of the other scenarios in turn the proportion of sims that would have been successful **and selected a “should succeed” dose** so it is necessary for doses to have been specified as “should succeed” on the Virtual Subject Response profiles (giving the estimate of power for that scenario – where ‘power’ also requires correct dose selection), at each of the threshold values, these proportions are then shown on the y-axis of the graph.

Thus for each scenario we can see the estimated power of the design to determine success for a given type-1 error rate in the specified Null scenario.

![](../coreUGattachments/tte/media/image86.png)

Figure 32: Across scenario graph showing ROC curves of the power of one selected design variant under different scenarios as a function of the type I error in the selected null scenario.

# Viewing Simulation Results in the Simulations Tab

After simulations have been run, FACTS provides a table for assessing the output from the simulations. While everything that FACTS displays in the table on the SImulation tab is available in output files, FACTS does a substantial amount of “beautifying” of results in its built-in table. This entails changes like converting numeric outcome scores to readable text and using descriptive names for QOI columns.

The main table has 1 line per scenario, and shows the summary of all simulations run for each scenario. These lines show values that are stored in the summary.csv file.

More detailed simulation output, providing a row per simulation run, is accessbile by double clicking on any row of the output table. This row-per-simulation output is what is stored in the simulations.csv files.

Additional detail is available for simulations that have a weeks file by right clicking on a simulation in the simulation summary window and selecting “Weeks Results”. See the Results Output section of the Simulation tab to specify the number of weeks files to be output.

## Summary per Scenario

By default, only a subset of all columns of output are provided when simulations are complete. This set of columns is called “Highlights.” To get access to further columns, you can right click anywhere on the simulation output table and click “Summary Results: ….” for whichever set of columns you would like to see. There is a “Summary Results: All” option that will provide a pop up containing every column available for summary results.

The following sections break down which columns are available by sorting to which column subset.

### Highlights

These are the columns displayed on the simulations tab after simulations are completed, the can also be displayed in the separate “Highlights” results window.

[TABLE]

The columns that are displayed by default in the simulations tab after time-to-event simulations have been run. {.caption-top .table}

### Allocation

By right clicking and selecting the allocation columns, a pop-out will appear that provides the following columns.

[TABLE]

The allocation tabs provided in FACTS output for time-to-event trials. {.caption-top .table}

### Response

The following columns provide summaries of the estimated hazard ratios based on the bayesian model incorporating the dose response model and the predictor model, if one is specified.

[TABLE]

Response columns available in FACTS output for time-to-event simulations. {.caption-top .table}

### Observed

By right clicking and selecting the Observed columns, a pop-out will appear that provides the following columns. These columns relate to the raw data observed in the trial.

[TABLE]

Observed columns available in FACTS output for time-to-event simulations. {.caption-top .table}

### Probabilities

By right clicking and selecting the allocation columns, a pop-out will appear that provides the following columns. These columns provide summaries of the Quantities of Interest values.

[TABLE]

Probabilities columns available in FACTS output for time-to-event simulations. {.caption-top .table}

### Hierarchical Prior Parameters

By right clicking and selecting the Hierarchical Prior Parameters columns, a pop-out will appear that provides the following columns.

[TABLE]

Hierarchical Prior Parameters columns available in FACTS output for time-to-event simulations. {.caption-top .table}

### Predictor

By right clicking and selecting the Predictor columns, a pop-out will appear that provides the following columns. These colums provide summaries of the bayesian model estimates relating to the predictor endpoint.

[TABLE]

Predictor columns available in FACTS output for time-to-event simulations. {.caption-top .table}

### Model Parameters

By right clicking and selecting the Model Parameters columns, a pop-out will appear that provides the following columns.

[TABLE]

Model Parameters columns available in FACTS output for time-to-event simulations. {.caption-top .table}

## Detailed Simulation Results

After simulation has completed and simulation results have been loaded, the user may examine detailed results for any scenario with simulation data in the table by double-clicking on the row. A separate window (as in Figure 15‑1) displays the individual results for each simulation. This is the contents of the “simulations.csv” file, which is described below.

![](../coreUGattachments/tte/media/image96.png)

Figure 33: The simulation results pop out viewer showing simulation highlights.

# Output Files

Instead of viewing simulation output on the Simulation tab within FACTS, the output is all contained within easily accessible files saved the the user’s local machine. FACTS stores the results of simulations as ‘.csv’ files under a Results folder. For each row in the simulations table, there is a folder named by the profiles that make up the scenario, which contains the corresponding ‘.csv’ files.

![](../coreUGattachments/tte/media/image97.png)

Figure 34: The windows file explorer when opened to the directory containing simulation output.

These files can be opened using Microsoft Excel or any text editor. Excel, and many other apps, takes out a file lock on any file it has open, while a FACTS results file is open in another piece of software it cannot be deleted or modified by FACTS. The most common cause for an error to be reported when simulating trials in FACTS is because the user has one of the previous results files is still open in Excel.

In the scenario directory there are the following types of results file:

**Summary.csv** Contains a single row of data that summarizes the simulation results. This is the source of the shown on the simulations tab.

**Summary_freq.csv** Contains a single row of data that that summarizes the frequentist analysis of the simulations.

**Simulations.csv** Contains one row per simulation describing the final state of each simulation for every trial simulated.

**Simulations_freq.csv** Contains one row per simulation describing the frequentist analysis of the final state of each simulation for every trial simulated.

**PatientsNNNNN.csv** Contains one row per patient in simulation, where NNNNN is the number of the simulation. By default this file is written out only for the first simulation, but this can be changed via the ‘Advanced’ button on the simulations tab.

**WeeksNNNNN.csv** contains one row for each cohort during a simulation where NNN is the number of the simulation. By default this file is written only for the first 100 simulations, but this can be changed via the advanced button on the simulation tab. The values in the last row of the cohorts file will be the same as the final values for that simulation in the simulations file.

## Contents of summary.csv

The first line is a header line, starting with a ‘#’, containing the FACTS GUI version number, the name of the FACTS file, the name of the scenario, and the time stamp of the start of the simulation.

The second line is a header line, starting with a ‘#’, containing the following column headings.

[TABLE]

Contents of the summary.csv file for time-to-event simulations. {.caption-top .table}

### QOI Columns

The QOI columns depend on the QOIs that have been defined for this design. The columns in the following order:

| Statistic | Description |
|----|----|
| Posterior Probabilities | The summary file contains the mean (over the simulations) of the posterior probability for each dose. |
| Predictive Probabilities | The summary file contains the mean (over the simulations) of the predictive probability for each dose. |
| P-Values | The summary file contains the mean (over the simulations) of the p-value for each dose. |
| Target Probabilities | The summary file contains the proportion of times (over the simulations) each dose had the greatest probability of being the target. |
| Decision QOIs | The summary file contains the mean (over the simulations) of the final value of the decision QOI at the target. |

## Contents of Summary_freq.csv

The first line is a header line, starting with a ‘#’, containing the FACTS GUI version number, the name of the FACTS file, the name of the scenario, and the time stamp of the start of the simulation.

The second line is a header line, starting with a ‘#’, containing the following column headings.

[TABLE]

Contents of the summary_freq.csv file for time-to-event simulations. {.caption-top .table}

#### Continuous Predictor:

| Column Title | Number of columns | Description |
|:---|:---|:---|
| Predictor Cox Mean HR \<dose\> | D | The mean value of the cox model coefficient – the predictor as a covariate predicting final endpoint per dose. |
| Predictor Cox SE HR \<dose\> | D | The standard error of the cox model coefficient – the predictor as a covariate predicting final endpoint per dose. |
| Predictor Avg. Min \<dose\> | D | The average of the minimum value for the predictor per dose. |
| Predictor Avg. 10-percentile \<dose\> | D | The average of the 10^(th) percentile values for the predictor per dose. |
| Predictor Avg. 25-percentile \<dose\> | D | The average of the 25^(th) percentile values for the predictor per dose. |
| Predictor Avg. Median \<dose\> | D | The average of the median values for the predictor per dose. |
| Predictor Avg. 75-percentile \<dose\> | D | The average of the 75^(th) percentile values for the predictor per dose. |
| Predictor Avg. 90-percentile \<dose\> | D | The average of the 90^(th) percentile values for the predictor per dose. |
| Predictor Avg. Max \<dose\> | D | The average of the maximum value for the predictor per dose. |
| Predictor Mean \<dose\> | D | The mean of the mean value of the predictor per dose. |
| Predictor SE \<dose\> | D | The standard error of the mean value of the predictor per dose. |

Extra columns on the summary_freq.csv file when using a continuous predictor. {.caption-top .table}

#### Dichotomous Predictor:

| Column Title | Number of columns | Description |
|:---|:---|:---|
| Predictor Cox Mean HR \<dose\> | D | The mean value of the cox model coefficient – the predictor as a covariate predicting final endpoint per dose. |
| Predictor Cox SE HR \<dose\> | D | The standard error of the cox model coefficient – the predictor as a covariate predicting final endpoint per dose. |
| Predictor Mean Response Rate \<dose\> | D | The mean of the predictor response rate per dose. |
| Predictor SE Response Rate \<dose\> | D | The standard error of the predictor response rate per dose. |

Extra columns on the summary_freq.csv file when using a dichotomous predictor. {.caption-top .table}

#### Time-to-Event Predictor

| Column Title | Number of columns | Description |
|:---|:---|:---|
| Predictor Mean KM Median \<dose\> | D | The mean of the Kaplan-Meier estimate of the median time to the predictor event per dose. |
| Predictor SE KM Median \<dose\> | D | The standard error of the Kaplan-Meier estimate of the median time to the predictor event per dose. |

Extra columns on the summary_freq.csv file when using a time-to-event predictor. {.caption-top .table}

## Contents of simulations.csv and weeksNNNNN.csv

The simulations.csv file holds the FACTS summary of the final analysis for each simulation (one per row).

The weeksNNNNN.csv file holds the FACTS summary of every analysis for the NNNNN^(th) simulation. It contains a row for each interim in the trial and a row for the final analysis (Interim number 999).

The final analysis occurs after all the planned data is collected. If the trial does not stop early, the final analysis occurs after full follow-up for all subjects. If the trial does stop early at an interim analysis, then the timing of the final analysis depends on the status of the check boxes on the [Interims tab](../../../../../documentation/v81/userguides/core/design/interims.llms.md#subject-follow-up-options). If follow-up is collected after the particular interim decision, then an analysis after full follow-up of the subjects recruited up to the point of the interim when the stopping decision was taken. If there is no follow-up intended after the interim analysis decision, then the final analysis occurs immediately (at the same time as the interim analysis), and the final analysis criteria are checked.

The first line is a header line, starting with a ‘#’, containing the FACTS GUI version number, the name of the FACTS file, the name of the scenario, and the time stamp of the start of the simulation.

The second line is a header line, starting with a ‘#’, containing the following column headings.

Most of the columns are common to the two file types, but the weeks file does not contain columns for the ‘final’ values of the evaluation criteria.

[TABLE]

Simulations and weeks file column names for a time-to-event endpoint. {.caption-top .table}

### QOI Columns

The QOI columns depend on the QOIs that have been defined for this design. The columns are grouped in the following order:

| QOI Type | Description |
|----|----|
| Posterior Probabilities | The posterior probability for each QOI for each dose. |
| Predictive Probabilities | The predictive probability for each QOI for each dose. |
| P-Values | The p-value for each QOI for each dose. |
| Target Probabilities | The probability of being the target for each QOI for each dose. |
| Decision QOIs | The value of the decision QOI at the target. |
| Success Futile | A flag for each QOI decision criteria indicating if the decision QOI was evaluated and compared to a threshold at the interim (weeks file) or final analysis (simulation file). The flag value is -1 if the decision QOI was not evaluated, 0 if it did not meet the threshold and 1 if it did. |
| Success / Futile Combined | Flags indicating if the interim or final analysis determined success or futility taking all the factors defined for Success/Futility into account: 0 if the conditions were not met, 1 it the conditions were met. If success and futility conditions have been defined that are not mutually exclusive and both sets of combined conditions are met, FACTS will pick one of the outcomes as met but not the other. In order to discourage defining Success and Futility rules that can both be true FACTS does not guarantee which outcome will be selected. |

## Contents of Simulations_freq.csv

| Column Title | Number of columns | Description |
|:---|:---|:---|
| \# Trial | 1 | The number of the simulation. |
| LR Stat \<dose\> | D | Unadjusted log-rank test statistic per arm |
| LR pval \<dose\> | D | The unadjusted log-rank p-value per arm |
| LR adj_pval \<dose\> | D | The Bonferroni adjusted log-rank p-value per arm |
| Wilcoxon stat \<dose\> | D | The Wilcoxon test statistic per arm |
| Wilcoxon pval \<dose\> | D | The unadjusted Wilcoxon p-value per arm |
| Wilcoxon adj_pval \<dose\> | D | The Bonferroni adjusted Wilcoxon p-value per arm |
| HR \<dose\> | D | The Cox model hazard ratio per arm |
| HR pval \<dose\> | D | The unadjusted Cox model p-value per arm |
| HR lower CI \<dose\> | D | The unadjusted lower bound of the alpha confidence interval of the hazard ratio estimate. |
| HR upper CI \<dose\> | D | The unadjusted upper bound of the alpha confideneeeddce interval of the hazard ratio estimate. |
| HR adj pval \<dose\> | D | The Bonferroni adjusted Cox model p-value per arm |
| HR adj lower CI \<dose\> | D | The Bonferroni adjusted lower bound of the alpha confidence interval of the hazard ratio estimate. |
| HR adj upper CI \<dose\> | D | The Bonferroni adjusted upper bound of the alpha confidence interval of the hazard ratio estimate. |
| KM med \<dose\> | D | The Kaplan Meier estimates of the median survival time per arm. |
| Predictor Cols | D | The appropriate output columns for the chosen type of predictor. See next tables for specifics of columns provided for each predictor type. |

Contents of the simulations_freq.csv file for a time-to-event simulation. {.caption-top .table}

#### Continuous predictor

| Column Title | Number of columns | Description |
|:---|:---|:---|
| Predictor Cox HR \<dose\> | D | The Cox model Hazard Ratio per arm (where the model includes the predictor as a covariate predicting final endpoint). |
| Predictor Cox HR pred | 1 | The Cox model predictor coefficient |
| Predictor Cox HR pval \<dose\> | D | The Cox model p-value per arm (where the model includes the predictor as a covariate predicting final endpoint). |
| Predictor Cox HR pval pred | 1 | The Cox model predictor p-value |
| Predictor Cox HR lower CI \<dose\> | D | The Cox model lower bound of the alpha confidence interval of the hazard ratio estimate per arm (where the model includes the predictor as a covariate predicting final endpoint). |
| Predictor Cox HR lower CI pred | 1 | The Cox model lower bound of the alpha confidence interval of the predictor coefficient. |
| Predictor Cox HR upper CI \<dose\> | D | The Cox model upper bound of the alpha confidence interval of the hazard ratio estimate per arm (where the model includes the predictor as a covariate predicting final endpoint). |
| Predictor Cox HR upper CI pred | 1 | The Cox model upper bound of the alpha confidence interval of the predictor coefficient. |
| Predictor Min \<dose\> | D | The minimum value of the predictor value for each treatment arm. |
| Predictor 10-percentile \<dose\> | D | The 10^(th) percentile value of the predictor value for each treatment arm. |
| Predictor 25-percentile \<dose\> | D | The 25^(th) percentile value of the predictor value for each treatment arm. |
| Predictor Median \<dose\> | D | The mediam value of the predictor value for each treatment arm. |
| Predictor 75-percentile \<dose\> | D | The 75^(th) percentile value of the predictor value for each treatment arm. |
| Predictor 90-percentile \<dose\> | D | The 90^(th) percentile value of the predictor value for each treatment arm. |
| Predictor Max \<dose\> | D | The maximum value of the predictor value for each treatment arm. |
| Predictor Mean \<dose\> | D | The mean of the predictor value for each treatment arm |
| Predictor SD \<dose\> | D | The SD of the mean of the predictor value for each arm |

Extra columns on the simulations_freq.csv file when using a continuous predictor. {.caption-top .table}

#### Dichotomous predictor

| Column Title | Number of columns | Description |
|:---|:---|:---|
| Predictor Cox HR \<dose\> | D | The Cox model Hazard Ratio per arm (where the model includes the predictor as a covariate predicting final endpoint). |
| Predictor Cox HR pred | 1 | The Cox model predictor coefficient |
| Predictor Cox HR pval \<dose\> | D | The Cox model p-value per arm (where the model includes the predictor as a covariate predicting final endpoint). |
| Predictor Cox HR pval pred | 1 | The Cox model predictor p-value |
| Predictor Cox HR lower CI \<dose\> | D | The Cox model lower bound of the alpha confidence interval of the hazard ratio estimate per arm (where the model includes the predictor as a covariate predicting final endpoint). |
| Predictor Cox HR lower CI pred | 1 | The Cox model lower bound of the alpha confidence interval of the predictor coefficient. |
| Predictor Cox HR upper CI \<dose\> | D | The Cox model upper bound of the alpha confidence interval of the hazard ratio estimate per arm (where the model includes the predictor as a covariate predicting final endpoint). |
| Predictor Cox HR upper CI pred | 1 | The Cox model upper bound of the alpha confidence interval of the predictor coefficient. |
| Predictor Response Rate \<dose\> | D | The predictor response rate per arm. |

Extra columns on the simulations_freq.csv file when using a dichotomous predictor. {.caption-top .table}

#### Time-to-event Predictor

| Column Title | Number of columns | Description |
|:---|:---|:---|
| Predictor KM Median \<dose\> | D | The Kaplan Meier estimate of the median time to the predictor event per arm |

Extra columns on the simulations_freq.csv file when using a time-to-event predictor. {.caption-top .table}

## Contents of PatientsNNNNN.csv

| Column Title | Number of columns | Description |
|:---|:---|:---|
| \#Subject | 1 | The subject id number, starting at 1. |
| Region | 1 | Which region the subject was recruited in |
| Date | 1 | The date, in weeks from the start of the trial, of the subject’s baseline visit and randomization. |
| Dose | 1 | The index number (1, …) of the treatment arm the subject belongs to. |
| Duration | 1 | The time of observation of the subject (in weeks) |
| Outcome | 1 | Whether an event was observed (1) or not (0) |
| Predictor | 1 | The value of the observed predictor |
| Pred Outcome | 1 | A flag indicating whether the predictor was observed or not |
| Dropout | 1 | A flag indicating whether the subject dropped out (1) or not (0). |

The patients file output when simulating a time-to-event tria.. {.caption-top .table}

## Contents of MCMCNNNNN.csv

The MCMC file if requested for output by the user, contains all the MCMC samples for the fitted parameters in the design. There is one row per sample (including the burnin) and the samples from all the analyses in the simulation are included. The first two columns are the analysis index and the sample (within analysis) index. The remaining columns are the parameters whose sample values are being reported, the number and constituents of these columns are highly variable depending on design of the statistical analysis.

| Column Title | Number of columns | Description |
|:---|:---|:---|
| Analysis | 1 | The index of the analysis (interim) in the simulation |
| Sample | 1 | The index of the sample within the analysis |
| HR \<dose\> | D | The estimate of the hazard ratio for each dose, based on the dose response model fitted. |
| Lambda \<seg\> | S | The estimate of the control hazard rate in each segment of the control hazard rate model |
| A \<1-8\> / Tau | P | If the dose response model estimates parameters then the samples of these are listed next. The number and name of the parameters will vary depending on the model being fitted. Check the Design \> Dose Response tab for a listing of the parameters. |
| Predictor … |  | The parameters of the predictor endpoint, these vary by the type of endpoint that the predictor has – they will be consistent with the parameters that would be output if the predictor was the only endpoint in a single endpoint design. |

MCMC file format for a time-to-event simulation. {.caption-top .table}
