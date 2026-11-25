Ordinal-specific graphs in FACTS are under development. For your convenience, we have reproduced the continuous and dichotomous graphs materials here.

# Graphs of Simulation Results

To enable swift visualization and analysis of the simulation results, FACTS has a number of pre-defined graphs it can display. Full and detailed simulation results are available in ‘csv’ format files that can be loaded into other analysis tools allowing any aspect of the simulation to be explored. These files are described [below](#output-files).

To view the graphs, select a row of scenario results by clicking on it and then click on the ‘View Graph’ button.

Below we have often shown full screen shots of the graphs in the graph manager, but the graph display supports copying just the graph to the clipboard to facilitate pasting them into documents and presentations. Right clicking on a graph brings up a short menu that allows the image of the graph to be copied to the clipboard or saved in ‘png’ format to a file.

To view the graphs of the results of the simulations of a particular design variant in a particular scenario, select that row of scenario results by clicking on it and then click on the ‘View Graph’ button and select “Show Per Scenario Graphs”.

To view multiple graphs showing the results of the simulations of possibly all the design variants and all the scenarios click on the ‘View Graph’ button and select “Show Across Scenario Graphs”. This launches a graph display that displays multiple graphs in a trellis plot. You can select the graph type, filter the design variants and filter which scenarios displayed:

![](../coreUGattachments/continuous/media/image43.png)

Figure 1: Pop up to select scenarios and variants to display.

The graph display supports copying an image of the graph to the clipboard, to facilitate pasting them into documents and presentations. Right clicking on a graph brings up a short menu that allows the image of the graph to be copied to the clipboard or saved in ‘.png’ format to a file.

Many graphs have a number of controls to allow the graph to be tailored, standard graph controls available on most graphs are:

- Set Y axis – this displays a dialog boxing allowing the user to fix the minimum and maximum of each of the Y axes and the number of ‘tick’ marks (Not displayed if the ‘y’ value must lie in the interval 0-1.)

- Space doses evenly – if the x-value is ‘dose’, then data for each dose can either be spaced equally or proportionate to the doses’ effective dose strength.

- If using a 2D treatment arm model then it is possible to display graphs where the different doses (or “arms”) form the x-axis, then there is an option to show the row factors as separate series – otherwise the different combinations are displayed in effective strength order

## Continuous

![](../coreUGattachments/continuous/media/image41.png)

\(a\) Arms shown in effective strength order

![](../coreUGattachments/continuous/media/image42.png)

\(b\) Arms shown with row factors as a separate series

Figure 2

## Dichotomous

![](../coreUGattachments/dichotomous/media/image37.png)

\(a\) Arms shown in effective strength order

![](../coreUGattachments/dichotomous/media/image38.png)

\(b\) Arms shown with row factors as a separate series

Figure 3

#### Box and whisker plot conventions

- The mean probability is plotted as a large dot.

- The median value is plotted as a dashed line.

- The 25-75^(th) quantile range is plotted as the “box” portion of each point.

- The “whiskers” extend to the largest and smallest values within 1 ½ times the interquartile range from either end of the box.

- Points outside the whisker range are considered outliers, and are plotted as small blue dots. Note that it may be difficult to see all of these symbols if they are plotted at the same value.

## Per Scenario Graphs

### Allocation Box and Whisker Plot

## Continuous

![](../coreUGattachments/continuous/media/image44.png)

Figure 4: Distribution of Subject Allocation Across Simulations.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image40.png)

Figure 5: Distribution of Subject Allocation Across Simulations.

This graph displays a box and whisker plot of the number of subjects recruited into each arm. These plots show:

The distribution over all simulations of the number of subjects allocated to each arm shown as a box and whisker plot. If the design is not adaptive, the number allocated will be the same in every simulation and the box and whiskers collapse to a single line.

### Response and Subject Allocation

## Continuous

![](../coreUGattachments/continuous/media/image45.png)

Figure 6: Response and Subject Allocation.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image41.png)

Figure 7: Response and Subject Allocation.

This graph shows for each treatment arm, the mean subject allocation, mean response and the 2.5%-97.5% boundaries of the estimated means over the simulations.

- The blue bars show the mean number of subjects allocated.

- The black line shows the true mean response being simulated.

- The green dashed line shows the mean of the estimated response across the simulations

- The green ‘error bars’ show the spread of the central 95% of the estimated mean response across the simulations (if less than 20 simulations have been run it simply shows the full spread).

### Response and Target Selection graphs

## Continuous

![](../coreUGattachments/continuous/media/image46.png)

Figure 8: Response and QOI Selection.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image42.png)

Figure 9: Response and QOI Selection.

These plots show the true simulated mean response and the mean and 95% spread of the mean fitted mean responses along with bars showing the proportion of simulations the different treatment arms are the selected target arm for each of the “Probability of being target” QOIs, such as Pr(Max), Pr(EDq relative to ..: Quantile=q) and Pr(MED relative to…: Delta=$\delta$).

### Per Dose QOIs (Box and Whisker Plots)

This set of graphs show the distribution over the simulations of the posterior probability estimates of quantities of interest for each dose. All the Posterior Probability, Predictive Probability, P-value and Probability of being Target QOIs that have been defined are available for plotting, via a drop down list.

Note that in any one analysis the Probability of being Target will sum to 1 across the doses (and can sum to less than one in the case of the MED target if none of the doses look likely to be better than the CSD), whereas all the other QOIs are assessed individually for each dose.

## Continuous

![](../coreUGattachments/continuous/media/image47.png)

\(a\) Distribution of $P$($\theta_d$ - $\theta_{Control}$ \> 0.1) Across Simulations.

![](../coreUGattachments/continuous/media/image48.png)

\(b\) Distribution of $P$(MED relative to Control: Delta}=0.1) Across Simulations.

Figure 10

## Dichotomous

![](../coreUGattachments/dichotomous/media/image43.png)

\(a\) Distribution of $P$($P_d$ - $P_{Control}$ \> 0.15) Across Simulations.

![](../coreUGattachments/dichotomous/media/image44.png)

\(b\) Distribution of $P$(MED relative to Control: Delta}=0.2) Across Simulations.

Figure 11

### Target Response by Sample Size Scatter Plot

## Continuous

![](../coreUGattachments/continuous/media/image49.png)

Figure 12: Posterior Mean Response at P(MED relative to Control: Delta}=0.1) vs. Total Sample Size per Simulation Run

## Dichotomous

![](../coreUGattachments/dichotomous/media/image45.png)

Figure 13: Posterior Mean Response at P(MED relative to Control: Delta}=0.2) vs. Total Sample Size per Simulation Run

This graph shows a scatter plot of trial outcomes with the selected Target QOI as the y-axis and total number of subjects recruited as the x-axis. The trials are plotted with a symbol that indicated the final outcome:

- A light blue circle indicates a trial that stopped early for success

- A dark blue circle indicates a trial that was a late success

- A dark red square indicates a trial that was a late futility

- A light red square indicates a trial that stopped early for futility

- A light pink diamond indicates a trial that stopped early for success but where the final analysis was futility

- A dark pink diamond indicates a trial that stopped early for futility but where the final analysis was success

- A yellow cross indicates a trial that completed full enrolment but was inconclusive at the end.

These graphs are not particularly useful if there is no early stopping.

If the design does allow early stopping this graph shows

- The overall distribution of sample sizes – in particular is stopping evenly distributed from the moment stopping is possible, or is there are large cohort of trials that stop as soon as it is possible?

- When and with what mean response trials stop incorrectly, in particular having trials stopping and the decision being reversed suggests stopping is allowed to soon or the criteria is not conservative enough.

### Cumulative Operating Characteristics Plot

## Continuous

![](../coreUGattachments/continuous/media/image50.png)

\(a\) Cumulative Proportion of Duration

![](../coreUGattachments/continuous/media/image51.png)

\(b\) Cumulative Proportion of Subjects

Figure 14

## Dichotomous

![](../coreUGattachments/dichotomous/media/image46.png)

\(a\) Cumulative Proportion of Duration

![](../coreUGattachments/dichotomous/media/image47.png)

\(b\) Cumulative Proportion of Subjects

Figure 15

There are two graphs, one that shows the cumulative proportion of durations across all simulations, and the other shows the cumulative proportion of subjects across all simulations.

### Time course for stopping

There are two graphs, one that shows the distribution over time for stopping for futility and one for stopping for success.

## Continuous

![](../coreUGattachments/continuous/media/image52.png)

\(a\) Cumulative Proportion of Futility Over Time

![](../coreUGattachments/continuous/media/image53.png)

\(b\) Cumulative Proportion of Success Over Time

Figure 16

## Dichotomous

![](../coreUGattachments/dichotomous/media/image48.png)

\(a\) Cumulative Proportion of Futility Over Time

![](../coreUGattachments/dichotomous/media/image49.png)

\(b\) Cumulative Proportion of Success Over Time

Figure 17

These plots show as cumulative curves the proportion of the simulations that stopped at different time points from the start of the trial. The user can select whether time or subject enrollment is used as the x-axis.

### Arm Dropping Graphs

If the design has used arm dropping, the following graphs are available.

#### Response and Ppn Arms Dropped

## Continuous

![](../coreUGattachments/continuous/media/image54.png)

Figure 18: Response and Proportion Arm Dropped

## Dichotomous

![](../coreUGattachments/dichotomous/media/image50.png)

Figure 19: Response and Proportion Arm Dropped

This graph shows the true (simulated) mean response and the average fitted response with 2.5% & 97.5% quantiles.

The histogram bars show for each arm the proportion of simulations for which the arm was dropped.

#### Time Course for Arm Dropping

This graph shows, for each arm, the cumulative proportion of simulations when it was dropped. The user can select whether time or number of subjects allocated to the arm is used as the x-axis.

## Continuous

![](../coreUGattachments/continuous/media/image55.png)

Figure 20: Cumulative Proportion of Trials where Arm Dropped

## Dichotomous

![](../coreUGattachments/dichotomous/media/image51.png)

Figure 21: Cumulative Proportion of Trials where Arm Dropped

#### Time Course for Arm retention

This graph shows, for each arm, the cumulative proportion of simulations when the arm was retained. The user can select whether time or number of subjects allocated to the arm is used as the x-axis. This is simply the inverse of the arm dropping graph above.

## Continuous

![](../coreUGattachments/continuous/media/image56.png)

Figure 22: Cumulative Proportion of Trials where Arm Retained

## Dichotomous

![](../coreUGattachments/dichotomous/media/image52.png)

Figure 23: Cumulative Proportion of Trials where Arm Retained

#### Arm Retention Proportion

This graph shows, for each arm, the simple proportion of simulations where the arm was retained.

## Continuous

![](../coreUGattachments/continuous/media/image57.png)

Figure 24: Arm Retention Proportion

## Dichotomous

![](../coreUGattachments/continuous/media/image57.png)

Figure 25: Arm Retention Proportion

### Frequentist

These graphs are available if frequentist analysis is enabled.

#### Frequentist P(significance)

These graphs are the box and whisker plots of various p-values that are automatically calculated (that is to say they do not need to be defined as QOIs for them to be calculated) for the final analysis. The user can select from Unadjusted, Bonferroni and Dunnett's and LOCF, BOCF (if baseline is simulated) or PP (per-protocol) treatment of missing values.

## Continuous

![](../coreUGattachments/continuous/media/image58.png)

Figure 26: Distribution of P(LOCF. Bonferroni Adjusted Significance) Across Simulations

## Dichotomous

![](../coreUGattachments/dichotomous/media/image53.png)

Figure 27: Distribution of P(LOCF. Unadjusted Adjusted Significance) Across Simulations

#### Frequentist: Response and Significance

This graph plots the mean and 2.5-97.5 percentile of the mean across the simulations or the simple frequentist estimate of the response for each arm, along with a histogram plot of the ppn of times the response on each arm was significant. The user can select from Unadjusted, Bonferroni or Dunnett’s and LOCF, BOCF (if baseline is simulated) or PP (per-protocol) treatment of missing values.

## Continuous

![](../coreUGattachments/continuous/media/image59.png)

Figure 28: Response and Ppn LOCF. Bonferroni Significance

## Dichotomous

![](../coreUGattachments/dichotomous/media/image54.png)

Figure 29: Response and Ppn Fail. Dunnett Significance

### Per Sim: Response and Subject Alloc

This graph shows the final analysis at the end of individual simulations.

## Continuous

![](../coreUGattachments/continuous/media/image60.png)

Figure 30: Response and Subject Allocation

## Dichotomous

![](../coreUGattachments/dichotomous/media/image55.png)

Figure 31: Response and Subject Allocation

This graph includes a control that allows the user to select which simulation to graph the results from. On each graph

- The black line shows the ‘True’ mean response being simulated.

- The brown circles and arms show the mean raw data and its 95% CI for each arm.

- The green solid line shows the estimate of the mean response.

- The green dashed lines show the boundaries of the 95% credible interval for the estimate of the response.

- Blue bars showing the number of subjects allocated on each treatment arm.

## Continuous

![](../coreUGattachments/continuous/media/image61.png)

Figure 32: Response and Subject Allocation with the side panel that allows for specification of the simulation for which the plot should be shown.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image56.png)

Figure 33: Response and Subject Allocation with the side panel that allows for specification of the simulation for which the plot should be shown.

Control to select the individual simulation results to be displayed

### Per Sim: Posterior Quantities

This graph shows the final analysis at the end of individual simulations, including the distribution of a selected QOI.

## Continuous

![](../coreUGattachments/continuous/media/image62.png)

Figure 34: Response and Pr(MED relative to Control: Delta = 0.1) Probability

## Dichotomous

![](../coreUGattachments/dichotomous/media/image57.png)

Figure 35: Response and Pr(EDq relative to Control: Quantile = 0.9) Probability

This graph includes a control that allows the user to select which simulation to graph the results from. On each graph

- The black line shows the ‘True’ mean response being simulated.

- The brown circles and arms show the mean raw data and its 95% CI for each arm.

- The green solid line shows the estimate of the mean response.

- The green dashed lines show the boundaries of the 95% credible interval for the estimate of the response.

- Brown bars showing the final value of the selected QOI for each arm at the end of the simulated trial.

The user can select from any of the Posterior Probability, Predictive Probability or Target Probability QOIs to plot.

### Per Interim Response Graphs

This is an identical set of graphs to the Per Sim graphs, the difference is that in addition to a control to select which simulation to graph the results from, there is a control to select which interim within the simulation to graph the results from. These graphs are only available for those simulations for which ‘weeks’ files have been output (by default the first 100).

Note the ‘Interim’ control in both of the below figures.

## Continuous

![](../coreUGattachments/continuous/media/image63.png)

Figure 36: Response and Pr(MED relative to Control: Delta = 0.1) Probability with side panel allowing for selection of simulation, interim, and quantity of interest.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image58.png)

Figure 37: Response and Subject Allocation with side panel allowing for selection of simulation and interim number.

## Post Simulation Boundary Finding Graphs

### Explore Success/Futility Criteria

## Continuous

![](../coreUGattachments/continuous/media/image64.png)

\(a\) Proportion of Simulations Satisfying Final Futility Criteria by Threshold

![](../coreUGattachments/continuous/media/image65.png)

\(b\) Proportion of Simulations Satisfying Final Success Criteria by Threshold

Figure 38

## Dichotomous

![](../coreUGattachments/dichotomous/media/image59.png)

\(a\) Proportion of Simulations Satisfying Final Futility Criteria by Threshold

![](../coreUGattachments/dichotomous/media/image60.png)

\(b\) Proportion of Simulations Satisfying Final Success Criteria by Threshold

Figure 39

These graphs can be used to explore what proportion of simulated trials of a particular scenario would have been a success/failure at final evaluation. Using the two drop down controls the user can select the target dose to use: e.g. Pr(Max), and set and lower/upper limits to explore for the threshold (setting the range used on the x-axis). The graphs show for all the decision QOIs defined for the selected Target Dose the proportion of simulations that would have met the threshold values.

As in the examples above, the plots will be somewhat jagged if only a small number of simulations have been run. These graphs can be used to select thresholds that can be expected to yield a certain level of type-1 or power, but the user must remember these will only be approximate (depending on the number of simulations) but can be useful to understand the designs sensitivity to the thresholds and to set initial thresholds early on in the design / simulation process that will get close to the desired type-1 error and power from the outset.

### Explore Early Success/Futility Criteria

## Continuous

![](../coreUGattachments/continuous/media/image66.png)

\(a\) Cumulative Proportion of Simulations Satisfying Futility Criterion by Interim

![](../coreUGattachments/continuous/media/image67.png)

\(b\) Cumulative Proportion of Simulations Satisfying Success Criterion by Interim

Figure 40

## Dichotomous

![](../coreUGattachments/dichotomous/media/image61.png)

\(a\) Cumulative Proportion of Simulations Satisfying Futility Criterion by Interim

![](../coreUGattachments/dichotomous/media/image62.png)

\(b\) Cumulative Proportion of Simulations Satisfying Success Criterion by Interim

Figure 41

These graphs can be used to explore what proportion of simulated trials of a particular scenario would have stopped early for success/futility. NOTE these graphs require weeks files to have been output, they are also most use where the design has been simulated with interims but no early stopping (unlike the examples above where the shape of the “existing stopping rules” line indicates that early stopping occurred in these simulations).

Using the two drop down controls, the user can select which decision QOI is evaluated and from which interim onwards stopping will be permitted. Lines are then displayed for the proportion of simulations that would have stopped by each interim for a fixed set of thresholds.

Typically these can be used to see at what threshold (and starting at what interim) stopping for success or futility introduces an unacceptably level of ‘incorrect’ early stopping – stopping for futility in successful scenarios and stopping for success in futile scenarios and whether at below/above these levels there may be a useful probability of correct stopping.

### Explore Arm Dropping Criteria

## Continuous

![](../coreUGattachments/continuous/media/image68.png)

Figure 42: Cumulative Proportion of Simulations Where Selected Arm Satisfies Criterion by Interim

## Dichotomous

![](../coreUGattachments/dichotomous/media/image63.png)

Figure 43: Cumulative Proportion of Simulations Where Selected Arm Satisfies Criterion by Interim

This graph is similar to the Explore Early Success/Futility Criteria graph but for arm dropping criteria. The user has an additional control to select which dose the proportion of trials in which the arm would be dropped is displayed.

### Success/Futility Stopping Contours

## Continuous

![](../coreUGattachments/continuous/media/image69.png)

\(a\) Proportion of Simulations Reaching Futility for Early/Late Criterion Thresholds

![](../coreUGattachments/continuous/media/image70.png)

\(b\) Proportion of Simulations Reaching Success for Early/Late Criterion Thresholds

Figure 44

## Dichotomous

![](../coreUGattachments/dichotomous/media/image64.png)

\(a\) Proportion of Simulations Reaching Futility for Early/Late Criterion Thresholds

![](../coreUGattachments/dichotomous/media/image65.png)

\(b\) Proportion of Simulations Reaching Success for Early/Late Criterion Thresholds

Figure 45

These graphs allow early stopping and final evaluation criteria to be considered jointly. Like the explore early stopping criteria plots, these graphs require weeks files to have been output and will work best if interims were evaluated but no trials actually stopped early.

The user selects the decision QOI to plot, the first interim at which early stopping is allowed, and the upper/lower limit of the threshold to consider.

FACTS will then plot contours where (final evaluation threshold, early stopping threshold) yield the same proportion of trials that are successful/futile. Contours are only plotted where final evaluation threshold \< early stopping threshold for success and vice versa for futility. Early stopping criteria should not be less strict than final criteria.

Again the example graphs shown are based on only 100 simulations with weeks files, so any threshold derived will only very approximately yield these success/futility rates.

## MCMC Trace plots

## Dichotomous

![](../coreUGattachments/dichotomous/media/image66.png)

Figure 46: Trace plot of an MCMC parameter.

If an MCMC sample file has been output for one or more simulations (the default is to not output MCMC sample files due to their size), then for each of those simulations it is possible to view the MCMC trace of the sampled values for each of the parameters sampled in the MCMC.

If the design is adaptive, the user can select which interim (“update”) the samples are from, as well as which parameter’s samples to plot.

## Across Scenario Graphs

### Selected Arms

This graph shows a bar chart for each scenario and variant selected. Each chart shows how often each arm was ‘selected’ by the target QOI specified on the Study \> Variants tab. Each bar uses a stacked bar to show the proportion of times that arm was:

- “Successful” –the arm was correctly selected (marked as “Should succeed” on the VSR tab) and the trial was successful.

- “Should not succeed” – the arm was incorrectly selected (not marked as “Should succeed” on the VSR tab) and the trial was successful.

- Unsuccessful – the arm was selected and the trial failed.

## Continuous

![](../coreUGattachments/continuous/media/image71.png)

Figure 47: Across scenario graph for selected arms with 5 scenarios and 5 design variants.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image67.png)

Figure 48: Across scenario graph for selected arms with 3 scenarios and 3 design variants.

### QOI Box Plots

This graph shows a box and whisker plot for each scenario and variant selected. Each plot shows the distribution of the values of a selected QOI for each arm. There is a drop down control to allow the selection of the QOI to be displayed. Any Posterior probability, Predictive probability, p-value or target QOI can be selected.

## Continuous

![](../coreUGattachments/continuous/media/image72.png)

Figure 49: Across scenario graph for showing QOIs per arm with 5 scenarios and 5 design variants.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image68.png)

Figure 50: Across scenario graph for showing QOIs per arm with 3 scenarios and 3 design variants.

### Ppn Success

This grouped bar chart shows a bar for the proportion of successful simulations for each variant, grouped by scenario.

## Continuous

![](../coreUGattachments/continuous/media/image73.png)

Figure 51: Across scenario graph for showing the proportion of trials that result in success with 5 scenarios and 5 design variants.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image69.png)

Figure 52: Across scenario graph for showing the proportion of trials that result in success with 3 scenarios and 3 design variants.

### Response

This graph shows a dose response plot for each scenario and variant selected. Each plot shows the mean estimate over the simulations and the 95%-ile interval of the mean estimates over the simulations. The graph also shoes the “true response” i.e. the mean response being simulated.

## Continuous

![](../coreUGattachments/continuous/media/image74.png)

Figure 53: Across scenario graph for showing the average estimated dose response model vs. the true dose responses with 5 scenarios and 5 design variants.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image70.png)

Figure 54: Across scenario graph for showing the average estimated dose response model vs. the true dose responses with 5 scenarios and 5 design variants.

### Allocation

This graph shows a box and whisker plot for each scenario and variant selected. Each plot shows the distribution of the number of subjects allocated to each arm over the simulations.

## Continuous

![](../coreUGattachments/continuous/media/image75.png)

Figure 55: Across scenario graph for showing the distribution of allocation per arm with 5 scenarios and 5 design variants.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image71.png)

Figure 56: Across scenario graph for showing the distribution of allocation per arm with 3 scenarios and 3 design variants.

### Sample Size

This graph shows the mean sample size for each scenario at different maximum sample sizes (the different variants).

## Continuous

![](../coreUGattachments/continuous/media/image76.png)

Figure 57: Across scenario graph for showing the mean sample size of the study with 5 scenarios and 5 design variants.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image72.png)

Figure 58: Across scenario graph for showing the mean sample size of the study with 3 scenarios and 3 design variants.

### Interim vs Final Scatter Plot

This is an interactive plot that shows the outcome of each simulation over all the scenarios of a selected variant.

The x-axis shows the value of the selected decision QOI at the specified interim, and the y-axis shows the final value of a (possibly different) selected decision QOI. As well as selecting a decision QOI for the final decision, the graph can use the actual final decision in the simulations – the value of the QOI used in the first criteria is used for the decision is used for the y-axis.

Note the graph is based just on the values at the selected interim, what might happen at other interims is ignored.

Thresholds can be set to specify early and late success/futility. Each scenario is then coloured to show its outcome – and stacked bar graphs are displayed to show the proportion of each outcome for each scenario. The thresholds can be changed and the graph re-drawn. If the selected QOI uses a p-value the checks for success are that it is “\<” than the threshold, (and vice-versa for futility), otherwise the checks for success are that it is “\>” the threshold (and vice-versa for futility).

The graph can only display the results for simulations for which cohorts files have been output, and for simulations that reached the specified interim. Thus the graph works best if interims have been specified, but the simulations run without any early stopping enabled – so each simulation has the results for every interim.

With 1,000 simulations per scenario and cohorts files for each simulation, the graph will take several seconds to re-draw. To allow several parameters to be set before redrawing (rather than having to wait for the redraw after each change) the graph is only redrawn after the ‘Redraw’ button is clicked.

## Continuous

![](../coreUGattachments/continuous/media/image77.png)

Figure 59: Across scenario graph showing an interim QOI plotted against a final analysis QOI with the decision dictating the point color. Additionally, the bars on the right hand side represent the proportion of trials that made each decision based on the decision rules specified in the plot controls.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image73.png)

Figure 60: Across scenario graph showing an interim QOI plotted against a final analysis QOI with the decision dictating the point color. Additionally, the bars on the right hand side represent the proportion of trials that made each decision based on the decision rules specified in the plot controls.

### Receiver Operating Characteristics

This graph plots the relative proportion of successful simulations in the scenarios compared to the proportion of successes in one specific scenario. The intended use of this is to yield a “Receiver Operating Characteristics” (ROC) curve. This is done as follows:

- Select the decision QoI that is to be used to determine final success/futility. (The graph only supports the simple case where just one is being used).

- Select a scenario that represents the ‘Null’ case – where determining success is a type-1 error.

- If multiple variants have been simulated, select which variant to plot.

FACTS computes for the null scenario, for a range of decision threshold values, what proportion of the ‘Null’ scenario sims would have been successful (giving the estimated type-1 error rate), these proportions form the x-values for each point. The corresponding threshold values are shown on the x-axis at the top of the graph, the type-1 error rate on the x-axis at the bottom of the graph.

FACTS then computes for each of the other scenarios in turn the proportion of sims that would have been successful **and selected a “should succeed” dose** so it is necessary for doses to have been specified as “should succeed” on the Virtual Subject Response profiles (giving the estimate of power for that scenario – where ‘power’ also requires correct dose selection), at each of the threshold values, these proportions are then shown on the y-axis of the graph.

Thus for each scenario we can see the estimated power of the design to determine success for a given type-1 error rate in the specified Null scenario.

## Continuous

![](../coreUGattachments/continuous/media/image78.png)

Figure 61: Across scenario graph showing ROC curves of the power of one selected design variant under different scenarios as a function of the type I error in the selected null scenario.

## Dichotomous

![](../coreUGattachments/dichotomous/media/image74.png)

Figure 62: Across scenario graph showing ROC curves of the power of one selected design variant under different scenarios as a function of the type I error in the selected null scenario.

# Viewing Simulation Results in the Simulations Tab

After simulations have been run, FACTS provides a table for assessing the output from the simulations. While everything that FACTS displays in the table on the Simulation tab is available in output files, FACTS does a substantial amount of “beautifying” of results in its built-in table. This entails changes like converting numeric outcome scores to readable text and using descriptive names for QOI columns.

The main table has 1 line per scenario, and shows the summary of all simulations run for each scenario. These lines show values that are stored in the summary.csv file.

More detailed simulation output, providing a row per simulation run, is accessibile by double clicking on any row of the output table. This row-per-simulation output is what is stored in the simulations.csv files.

Additional detail is available for simulations that have a weeks file by right clicking on a simulation in the simulation summary window and selecting “Weeks Results”. See the Results Output section of the Simulation tab to specify the number of weeks files to be output.

## Summary per Scenario

By default, only a subset of all columns of output are provided when simulations are complete. This set of columns is called “Highlights.” To get access to further columns, you can right click anywhere on the simulation output table and click “Summary Results: ….” for whichever set of columns you would like to see. There is a “Summary Results: All” option that will provide a pop up containing every column available for summary results.

The following sections break down which columns are available by sorting to which column subset.

### Highlights

These are the columns displayed on the simulations tab after simulations are completed, the can also be displayed in the separate “Highlights” results window.

| Column Title | Number of columns | Description |
|----|----|----|
| Select | 1 | Not an output column, this column contains check box to allow the user to select which scenario to simulate. The ‘Select All’ button causes them all to be checked. |
| Status | 1 | This column reports on the current status of simulations: Completed, Running, No Results, Out of date, Error. It is updated automatically. |
| Scenario | 1 | This column gives the name of the scenario, concatenating together the profile names from the following tabs: · ‘Execution \> Accrual’, · ‘Execution \> Dropout Rate’, · ‘Virtual Subject Response \> Explicitly Defined \> Dose Response’, · This is the same name as used for the results directory |
| Num Sims | 1 | The number of simulations that were run to produce the displayed results. |
| Mean Subj. | 1 | This is the mean (over the simulations) of the number of subjects recruited in this scenario. |
| Ppn Overall Success | 1 | This is the proportion of simulations that stopped for success, either early success or late success (as defined below). |
| Ppn Overall Futility | 1 | This is the proportion of simulations that stopped for futility, either early futility or late futility (as defined below). |
| Ppn Early Success | 1 | This is the proportion of simulations that stopped early for success (and did not regress to futility in the final analysis – though they might have regressed to ‘inconclusive’). |
| Ppn Late Success | 1 | This is the proportion of simulations that did not stop early but were successful in the final analysis. |
| Ppn Late Futility | 1 | This is the proportion of simulations that did not stop early but were futile in the final analysis. |
| Ppn Early Futility | 1 | This is the proportion of simulations that stopped early for futility (and did not regress to success in the final analysis – though they might have regressed to ‘inconclusive’). |
| Ppn · Suc-\>Fut Flipflop | 1 | This is the proportion of simulations that stopped early for success but regressed to futility in the final analysis. |
| Ppn · Fut-\>Suc Flipflop | 1 | This is the proportion of simulations that stopped early for futility but regressed to success in the final analysis. |
| Ppn Incon- clusive | 1 | This is the proportion of simulations that did not stop early and were neither successful nor futile in the final analysis. |
| Mean Duration | 1 | The mean over the simulations of the total duration from the start of enrolment to end of follow up. |
| Ppn Correct Arm | 1 | The proportion of simulations that met the success criteria and selected (by the target QOI specified on the Study \> Variant tab) one of the arms marked as “should succeed” on the Virtual Subject Response \> Explicitly Defined \> Dose Response tab |
| Ppn · Incorrect Arm | 1 | The proportion of simulations that met the success criteria and selected (by the target QOI specified on the Study \> Variant tab) one of the arms **not** marked as “should succeed” on the Virtual Subject Response \> Explicitly Defined \> Dose Response tab |
| Version | 1 | The FACTS version number at the time the simulations were run. |

The columns that are displayed by default in the simulations tab after ordinal simulations have been run. {.caption-top .table}

### Allocation

By right clicking and selecting the allocation columns, a pop-out will appear that provides the following columns.

| Column Title | Number of columns | Description |
|----|----|----|
| Status | 1 | This column reports on the current status of simulations: Completed, Running, No Results, Out of date, Error. It is updated automatically. |
| Scenario | 1 | This column gives the name of the scenario, concatenating together the profile names from the following tabs: · ‘Execution \> Accrual’, · ‘Execution \> Dropout Rate’, · ‘Virtual Subject Response \> Explicitly Defined \> Dose Response’, · This is the same name as used for the results directory |
| Mean Subj. | 1 | This is the mean (over the simulations) of the number of subjects recruited in this scenario. |
| SD Mean Subj. | 1 | This is the standard deviation across the simulations of the number of subjects recruited. |
| Num Subj. 80% | 1 | This is the eightieth percentile across the simulations of the number of subjects recruited into the trial. |
| Mean Alloc.: \<Dose\> | One per arm | This is the mean (over the simulations) of the number of subjects recruited into each arm in this scenario. |
| SD Alloc.: \<Dose\> | One per arm | This is the SD (over the simulations) of the number of subjects allocated to each treatment arm. |
| PPn Arms Drop: \< Dose\> | One per arm | This is the number of times (over the simulations) that each arm was dropped. |

The allocation columns provided in FACTS output for ordinal trials. {.caption-top .table}

### Response

## Ordinal

In ordinal trials, some elements of the Response results depend on whether the ordinal endpoint has been modeled using the cumulative logistic framework or using independent Dirichlet models.

| Column Title | Number of columns | Description |
|----|----|----|
| Status | 1 | This column reports on the current status of simulations: Completed, Running, No Results, Out of date, Error. It is updated automatically. |
| Scenario | 1 | This column gives the name of the scenario, concatenating together the profile names from the following tabs: · ‘Execution \> Accrual’, · ‘Execution \> Dropout Rate’, · ’Virtual Subject Response \> Composite, · This is the same name as used for the results directory |
| Mean Resp: \<Dose\> | One per arm | This is the mean (over the simulations) of the estimate of a quantity summarizing this arm. · For cumulative logistic modeling, this is the log odds ratio (zero for control). · For Dirichlet modeling, this is the expected utility. |
| SD Resp: \<Dose\> | One per arm | This is the standard deviation (over the simulations) of the estimate of the response on this arm, either the log odds ratio or the expected utility as above. |
| True Mean Resp: \<Dose\> | One per arm | This is the true value of the expected utility for each under the distribution from which the simulation data was sampled. This is true regardless of how the ordinal endpoint is being modeled. (The “true log odds ratio” is not necessarily a well-defined quantity.) |

Response columns available in FACTS output for ordinal simulations. {.caption-top .table}

### Observed

| Column Title | Number of columns | Description |
|----|----|----|
| Status | 1 | This column reports on the current status of simulations: Completed, Running, No Results, Out of date, Error. It is updated automatically. |
| Scenario | 1 | This column gives the name of the scenario, concatenating together the profile names from the following tabs: · ‘Execution \> Accrual’, · ‘Execution \> Dropout Rate’, · ’Virtual Subject Response \> Composite, · This is the same name as used for the results directory |
| Mean Complete \<Dose\> | One per arm | This is the mean (over the simulations) of the number of subjects recruited per arm which have had their endpoint observed in this scenario. |
| Mean Complete Info \<Dose\> | One per arm | This is the mean (over the simulations) of information observed per arm as defined on the Interims tab (Subjects enrolled, Complete Data at Specified Visit, Opportunity to Complete at Specified Visit) in this scenario. |
| Mean Dropouts \<Dose\> \<Visit\> | One per arm per visit | This is the mean (over the simulations) of dropouts per arm per visit in this scenario. |

Observed columns available in FACTS output for ordinal simulations. {.caption-top .table}

### Probabilities

| Column Title | Number of columns | Description |
|----|----|----|
| Status | 1 | This column reports on the current status of simulations: Completed, Running, No Results, Out of date, Error. It is updated automatically. |
| Scenario | 1 | This column gives the name of the scenario, concatenating together the profile names from the following tabs: · ‘Execution \> Accrual’, · ‘Execution \> Dropout Rate’, · ’Virtual Subject Response \> Composite, · This is the same name as used for the results directory |
| \<QOI\> \<Dose\> | One per arm per QOI | For each Posterior Probability and Predictive Probability QOI defined on this endpoint, this is the mean over the simulations of the estimate of the probability of the QOI for each dose. (P-value QOIs are reported in the frequentist results table). · For each Target QOI this is the proportion of simulations where this dose was selected at the end of the trial as the dose with the greatest probability of meeting the target condition. · The probability that each dose is the target at the end of a simulated trial is its marginal probability (the number of times it was the dose closest to the target in the MCMC sampling of the analysis at the end of the trial). The target of having the Max response on this endpoint, or some fraction of it (EDq) is always identifiable, so the Ppn(target) for these QOIs will sum to 1 across the doses. · An MED target is not guaranteed to be identifiable, if no dose meets the CSD criteria in any MCMC sample so all doses have a 0 probability of having a response greater than Control (or AC) by the CSD then no dose is the MED. So the sum of each Ppn(target) QOI across the doses should sum to between 0 and 1 inclusive. · Decision QOI’s are not reported here separately, but their component QOIs – the vector of values and target QOI used to select from them are. |

Probabilities columns available in FACTS output for ordinal simulations. {.caption-top .table}

### Timing

| Column Title | Number of columns | Description |
|----|----|----|
| Status | 1 | This column reports on the current status of simulations: Completed, Running, No Results, Out of date, Error. It is updated automatically. |
| Scenario | 1 | This column gives the name of the scenario, concatenating together the profile names from the following tabs: · ‘Execution \> Accrual’, · ‘Execution \> Dropout Rate’, · ’Virtual Subject Response \> Composite, · This is the same name as used for the results directory |
| Early Success Time | 1 | This is the average time to an early decision to stop for success (i.e. the time excluding final follow up) over those simulations that did decide early to stop for success. |
| Mean LP Enrolled | 1 | This is the mean, in weeks (over the simulations) of the duration of the trial from first patient first visit to Last Patient First Visit (i.e. the duration of accrual). |

Timing columns available in FACTS output for ordinal simulations. {.caption-top .table}

### Model Parameters

## Ordinal

NOTE: we do not yet have a “Model Parameters” option for ordinal available under the “Show More Columns” button.

| Column Title | Number of columns | Description |
|----|----|----|
| Scenario | 1 | This column gives the name of the scenario, concatenating together the profile names from the following tabs: · ‘Execution \> Accrual’, · ‘Execution \> Dropout Rate’, · ’Virtual Subject Response \> Composite, · This is the same name as used for the results directory |
|  |  |  |

Model Parameters columns available in FACTS output for ordinal simulations. {.caption-top .table}

## Detailed Per Simulation Results

After simulation has completed and simulation results have been loaded, the user may examine detailed results for any scenario with simulations output in the table by double-clicking on the appropriate row. A separate window pops up and displays the results for each individual simulation. This is a “prettier” view of the contents of the “simulations.csv” file, which is described below. The simulations results are partitioned into the same results groupings as the summary results. These can be accessed from the “right click” menu, along with opening the results folder, opening the weeks file for a particular simulation and opening the simulated patients file for a particular simulation (where a “weeks” file or a “patients” file has been output).

![](../coreUGattachments/continuous/media/image88.png)

Figure 63: The simulation results pop out viewer showing simulation highlights.

# Output Files

Instead of viewing simulation output on the Simulation tab within FACTS, the output is all contained within easily accessible files saved on the user’s local machine. FACTS stores the results of simulations as ‘.csv’ files under a Results folder. For each row in the simulations table, there is a folder named by the profiles that make up the scenario, which contains the corresponding ‘.csv’ files.

![](../coreUGattachments/continuous/media/image89.png)

Figure 64: The windows file explorer when opened to the directory containing simulation output.

These files can be opened using Microsoft Excel or any text editor. Excel, and many other apps, takes out a file lock on any file it has open, while a FACTS results file is open in another piece of software it cannot be deleted or modified by FACTS. The most common cause for an error to be reported when simulating trials in FACTS is because the user has one of the previous results files is still open in Excel.

In the scenario directory there are the following types of results file:

**summary.csv** Contains a single row of data that summarizes the simulation results. This is the source of the shown on the simulations tab.

**simulations.csv** Contains one row per simulation describing the final state of each simulation for every trial simulated.

**patientsNNNNN.csv** Contains one row per patient in a simulation, where NNNNN is the number of the simulation. By default this file is written out only for the first simulation, but this can be changed on the simulations tab.

**weeksNNNNN.csv** Contains one row for each cohort during a simulation where NNN is the number of the simulation. By default this file is written only for the first 100 simulations, but this can be changed via the simulation tab. The values in the last row of the cohorts file will be the same as the final values for that simulation in the simulations file.

## Contents of summary.csv

The first line is a header line, starting with a ‘#’, containing the FACTS GUI version number, the name of the FACTS file, the name of the scenario, and the time stamp of the start of the simulation.

The second line is a header line, starting with a ‘#’, containing the following column headings.

## Ordinal

| Column Title | Number of columns | Description |
|----|----|----|
| Project | 1 | The name of the facts file |
| Scenario | 1 | The name of the scenario |
| Timestamp | 1 | The time the simulations were run |
| Version | 1 | The version of FACTS that was used to run the simulations |
| NSim | 1 | The number of simulation run. |
| Random Number Seed | 1 | The random number seed |
| No. Subj | 1 | The mean, over the simulations, of the total number of subjects recruited in the trial. |
| SE Subj. | 1 | The standard error of the total number of subjects recruited into the trials. |
| No.Subj 80%ile | 1 | The 80^(th) percentile, over the simulations, of the total number of subjects recruited in the trial. |
| Ppn Overall Success | 1 | The proportion of simulations that were successful (P(ES) + P(LS)) |
| Ppn Overall Futility | 1 | The proportion of simulations that were futile (P(EF) + P(LF)) |
| P(ES) | 1 | The proportion of simulations that stopped early for success. |
| P(LS) | 1 | The proportion of simulations that did not stop early, and were successful on final evaluation. |
| P(LF) | 1 | The proportion of simulations that did not stop early, and were futile on final evaluation. |
| P(EF) | 1 | The proportion of simulations that stopped early for futility. |
| SFFF | 1 | The proportion of simulations that stopped early for success, but at final analysis were futile (Success-Futility ‘flip-flops’). |
| FSFF | 1 | The proportion of simulations that stopped early for futility, but at final analysis were successful (Futility- Success ‘flip-flops’). |
| Undec. | 1 | The proportion of simulations that did not stop early, and met neither the success or futility final evaluation criteria and hence are counted as ‘undecided’. |
| Early Success Time | 1 | This is the mean early decision time (over the simulations stopped early for success), it is the time from the start of the trial to the interim where early success was declared. It does not include the subsequent follow up time. |
| Mean Alloc \<Dose\> | D | The mean (over the simulations) of the number of subjects allocated to each treatment arm. |
| SE Alloc \<Dose\> | D | The standard error (over the simulations) of the number of subjects allocated to each treatment arm. |
| Mean Resp · \<Dose\> | D | The mean, over the simulations, of the estimates of a quantity summarizing this arm. For cumulative logistic modeling, this is the log odds ratio (zero for control). For Dirichlet modeling, this is the expected utility. |
| SE Resp \<Dose\> | D | The standard error, over the simulations, of the estimate of response on each arm arm, either the log odds ratio or the expected utility as above. |
| log OR (lower CI) \<Dose\> | D | The lower limit of a 95% credible interval for the log odds ratio (zero for control). Available only when using cumulative logistic modeling. |
| log OR (upper CI) \<Dose\> | D | The upper limit of a 95% credible interval for the log odds ratio (zero for control). Available only when using cumulative logistic modeling. |
| True Mean resp \<Dose\> | D | The true expected utility for each treatment arm used when sampling the simulated subject responses. Note that in general the true log-odds ratio is undefined. |
| True SD resp \<Dose\> | D | The true SD of utility for each treatment arm used when sampling the simulated subject responses. |
| Mean Raw Response \<Dose\> | D | The average, over simulations, of the mean observed utility for each arm. |
| SE Mean Raw Response \<\<Dose\> | D | The standard error, over simulations, of the mean observed utility for each arm. |
| Mean Complete \<Dose\> | D | The mean number of completers for each treatment arm |
| Mean Complete Info \<Dose\> | D | The mean number of complete information (as specified by the interim information type on the Interims tab) for each treatment arm |
| No .Dropouts \<Dose\> \<Visit\> | D\*V | The mean number of dropouts per treatment arm per visit |
| Mean Duration | 1 | The mean, over the simulations, of the overall duration of the trials, from first person first visit, to last person last visit. |
| Ppn Arms Dropped \<Dose\> | D | The proportion of the simulations in which each arm was dropped. These will be zero if arm dropping is not selected as the means of adaptive allocation. |
| Mean LPFV time | 1 | The mean, over the simulations, of the accrual period of the trials, from first person first visit, to last person first visit (LPFV). |
| Ppn Correct Arm | 1 | The proportion of the simulations where the correct arm has been selected, where an arm is deemed “correct” based on whether it was marked as “Should Succeed” for the relevant scenario on the VSR tab. |
| Ppn Incorrect Arm | 1 | The proportion of the simulations where the incorrect correct arm has been selected, where an arm is deemed “incorrect” based on whether it wasn’t marked as “Should Succeed” for the relevant scenario on the VSR tab. |
| QOI Columns |  | See next section. |

Contents of the summary.csv file for ordinal simulations. {.caption-top .table}

### QOI Columns

The QOI columns depend on the QOIs that have been defined for this design. The columns in the following order:

| Statistic | Description |
|----|----|
| Posterior Probabilities | The summary file contains the mean (over the simulations) of the posterior probability for each dose. |
| Predictive Probabilities | The summary file contains the mean (over the simulations) of the predictive probability for each dose. |
| P-Values | The summary file contains the mean (over the simulations) of the p-value for each dose. |
| Target Probabilities | The summary file contains the proportion of times (over the simulations) each dose had the greatest probability of being the target. |
| Decision QOIs | The summary file contains the mean (over the simulations) of the final value of the decision QOI at the target. |

## Contents of simulations.csv and weeksNNNNN.csv

The simulations.csv file holds the FACTS summary of the final analysis for each simulation (one per row).

The weeksNNNNN.csv file holds the FACTS summary of every analysis for the NNNNN^(th) simulation. It contains a row for each interim in the trial and a row for the final analysis (Interim number 999).

The final analysis occurs after all the planned data is collected. If the trial does not stop early, the final analysis occurs after full follow-up for all subjects. If the trial does stop early at an interim analysis, then the timing of the final analysis depends on the status of the check boxes on the [Interims tab](../../../../../documentation/v80/userguides/core/design/interims.llms.md#subject-follow-up-options). If follow-up is collected after the particular interim decision, then an analysis after full follow-up of the subjects recruited up to the point of the interim when the stopping decision was taken. If there is no follow-up intended after the interim analysis decision, then the final analysis occurs immediately (at the same time as the interim analysis), and the final analysis criteria are checked.

The first line is a header line, starting with a ‘#’, containing the FACTS GUI version number, the name of the FACTS file, the name of the scenario, and the time stamp of the start of the simulation.

The second line is a header line, starting with a ‘#’, containing the following column headings.

Most of the columns are common to the two file types, but the weeks file does not contain columns for the ‘final’ values of the evaluation criteria.

## Ordinal

| Column Title | Number of columns | In simulations file | In weeks file | Description |
|----|----|----|----|----|
| \# Weeks (Duration) | 1 |  | ✔ | The week of the analysis |
| \#Sim | 1 | ✔ |  | The number of the simulation. |
| Random Number Seed | 1 | ✔ | ✔ | Base random number seed. |
| LastInterim | 1 | ✔ |  | The index of the last interim performed the index of the interim immediately before the final interim (index ‘999’). Note this is not necessarily the interim when the trial stopped if the design includes follow-up after stopping. |
| \#Subjects | 1 | ✔ | ✔ | The number of subjects recruited in the simulation. |
| Outcome | 1 | ✔ | ✔ | A flag categorizing final study outcome: 1 = Early success 2 = Late success 3 = Late futility 4 = Early futility 5 = Success to futility flip-flop 6 = Futility to success flip-flop 7 = Inconclusive |
| Early Success Time | 1 | ✔ | ✔ | This is the time to the early success decision (-9999 if there was no early success decision). The from the start of the trial to the interim where the early success conditions were first met. It does not include the subsequent follow up time. |
| Alloc \<Dose\> | D | ✔ | ✔ | The number of subjects allocated to each arm. |
| Pr(Alloc) \<Dose\> | D | ✔ | ✔ | The probability of allocation to the different arms following the interim. |
| Mean resp \<Dose\> | D | ✔ | ✔ | The posterior mean of a quantity summarizing each treatment arm. If cumulative logistic modeling is used, this is the log odds ratio. If Dirichlet modeling is used, this is the expected utility. |
| SD Mean resp \<Dose\> | D | ✔ | ✔ | The standard deviation of the estimate of either the log odds ratio or the expected utility (see above) for each treatment arm. |
| log OR (lower CI) \<Dose\> | D | ✔ | ✔ | The lower bound of a 95% credible interval for the log odds ratio, only when cumulative logistic modeling is used. |
| log OR (upper CI) \<Dose\> | D | ✔ | ✔ | The upper bound of a 95% credible interval for the log odds ratio, only when cumulative logistic modeling is used. |
| True Mean resp \<Dose\> | D | ✔ | ✔ | The true expected utility of each treatment arm for this simulation. |
| True SD resp \<Dose\> | D | ✔ | ✔ | The true SD of the utility of each treatment arm for this simulation |
| Mean Raw Response \<Dose\> | D | ✔ | ✔ | The observed mean utility on each treatment arm (unadjusted by any modeling). |
| SE Mean Raw Response \<Dose\> | D | ✔ | ✔ | The SE of the estimate of the expected utility on each dose. |
| Complete \<Dose\> | D | ✔ | ✔ | The number of completed subjects on each treatment arm at the time of the analysis. |
| Complete Information \<Dose\> | D | ✔ | ✔ | The number of subject who count as complete for the purposes of timing interims – whether complete or opportunity to complete and at which visit is as defined on the Interims tab. If interims are by enrolment then CompleteInformation is the number of subjects that have completed – final endpoint data is available. |
| \#Dropouts \<Dose\> \<Visit\> | D \* V | ✔ | ✔ | The number of subjects dropped out on each treatment arm and each visit. |
| Mean P \<Dose\> \<Outcome\> | D \* K | ✔ | ✔ | Posterior mean of the probability of each ordinal outcome for each arm |
| SD P \<Dose\> \<Outcome\> | D \* K | ✔ | ✔ | Posterior SD of the probability of each ordinal outcome for each arm |
| N \<Dose\> \<Outcome\> | D \* K | ✔ | ✔ | Number of subjects for each treatment arm and outcome. |
| DR Param \<Param\> | 10 | ✔ | ✔ | The estimate of the mean value of each of the response model parameters. The response models require up to 10 parameters, most less. The model parameters are labeled $\alpha_1$, $\alpha_2$, .. the subscripts corresponding to the index “\<Param\>”. |
| Sd DR Param \<Param\> | 4 | ✔ | ✔ | The standard deviation of the estimate of the mean value of the corresponding response model parameter. |
| Duration | 1 | ✔ | ✔ | The time in weeks to the analysis shown. |
| Arm Drop Time \<Dose\> | D | ✔ | ✔ | For each treatment arm, the time in weeks when it was decided to drop the arm. |
| LPFV | 1 | ✔ | ✔ | The time in weeks to the last patient first visit. |
| QOI Columns |  | ✔ | ✔ | See next section. |

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

## Contents of PatientsNNNNN.csv

## Ordinal

| Column | Number of columns | Description |
|----|----|----|
| Subject | 1 | The subject id number, starting at 1. |
| Region | 1 | Which region the subject was recruited in. |
| Date | 1 | The date, in weeks from the start of the trial, of the subject’s baseline visit and randomization. |
| Dose | 1 | The index number (1, …) of the treatment arm the subject belongs to. |
| LastVisit# | 1 | The index of the last visit for which the subjects data was collected, if the subject did not complete all of its visits it could be because they dropped-out, the subject’s treatment arm was dropped (and there was no follow-up), or the study stopped early (and there was no follow-up). |
| Dropout | 1 | Whether the subject dropped out of the trial (1) or not (0) before reaching their final visit. |
| Baseline | 1 | Subject baseline response, if simulated. If not simulated, then fixed at -9999. |
| Visit \<Visit\> | V | Subject’s response at each visit; possible values are 1 through the number of ordinal values. |

At present, the ordinal engine does not feature longitudinal modeling so there is only one visit and only the final endpoint is given.

## Contents of MCMCNNNNN.csv

The MCMC file if requested for output by the user, contains all the MCMC samples for the fitted parameters in the design. There is one row per sample (including the burnin) and the samples from all the analyses in the simulation are included. The first two columns are the analysis index and the sample (within analysis) index. The remaining columns are the parameters whose sample values are being reported, the number and constituents of these columns depend in particular on whether cumulative logistic modeling or Dirichlet modeling has been selected for the ordinal outcome.

## Ordinal

| Column | Number of columns | Description |
|----|----|----|
| Analysis | 1 | The index of the analysis (interim) in the simulation. |
| Sample | 1 | The index of the sample within the analysis. |
| Theta\[Arm\] | D | (Cumulative logistic modeling only). The estimate of the log odds ratio for each arm, based on the dose response model fitted. The control arm has Theta1 = 0. |
| P0\[Outcome\] | K | (Cumulative logistic modeling). Estimates of the probability of each ordinal outcome for the control arm. The outcome probabilities for the other arms are derived from these control arm probabilities and the Theta parameters above. |
| P\[Arm\]\[Outcome\] | D \* K | (Dirichlet modeling). Estimates of the probability of each ordinal outcome for each arm. |
