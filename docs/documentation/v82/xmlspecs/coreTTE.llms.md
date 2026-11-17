# 1 Introduction

This document describes the FACTS XML file (“.facts”) file format FACTS Core for a Time-to-Event endpoint.

It is intended for expert FACTS users who wish to create modified versions of “.facts” files using scripts. A full and deep understanding of FATCS Core is assumed.

## 1.1 IDs

There are number of parameters that IDs, such as “guid”, “SetId”, “qoi_id” … these are Microsoft “unique” identifiers, there should be no need to change these. It does not matter if the same Ids are used in successive “.facts” files, for instance if creating a range of sub-versions of a single original file.

## 1.2 Defaults

FACTS stores a super-set of the FACTS parameters in the “.facts” file - even storing parameters for a predictor endpoint even if using a predictor has been disabled. There are so many parameters in the FACTS file that we cannot check the behavior of FACTS if some of these are missing, so our advice is to include \*all\* the parameters even if not required. The only parameters that it is clear can be omitted are those in the optional parameter set: PredictorParameterSet that are not required oif the feature is not enabled.

## 1.3 Parameter Order

FACTS processes the parameters in the “.facts” file in the order in which the appear in the file. Some parameters depend on other parameters to have been already defined, so it is recommended that the order given here (the order FACTS itself uses when writing out the parameters) is retained. For many parameters changing their order should not be a problem, but we are not going to define, or guarantee, which those are.

# 2 The values in a “.facts” file

## 2.1 Numbers of items

ACS: The number of studies used for the hierarchical prior for the active comparator

ADC: The dose in a 2D dose model design which have been included in the study

CF: The number of column factors in a 2D dose model design

CS: The number of studies used for the hierarchical prior for the control

D: The number of doses (or treatment arms) in the design

DC: The number of possible dose combinations in a 2D dose model design

DQ: The number of Decision QOIs

I: The number of interims (when defined by information

LM: The number of longitudinal models

PostQ: The number of Posterior Probability QOIs

PredQ: The number of Predictive Probability QOIs

PvQ: The number of p-value QOIs

R: The number of regions in an accrual profile

RF: The number of row factors in a 2D dose model design

SchrVSR: the Number of Segments in the control hazard rate VSR profile

ShrMod: the number of segments in the control hazard rate model

ShrHR: the number of segments in the control hazard ratio model

SphrMod: the number of segments in the predictor hazard rate model (for a TTE predictor)

SphrVSR: the number of segments in the predictor hazard rate VSR profile (for a TTE predictor)

TQ: The number of target QOIs

V: The number of patient visits

Var: The number of variants

LM\*V: This is the value used when a different prior is used at different visits. The values are stored as lists of lists (nested \<item\>s). The outer dimension is the number of LM and the inner dimension the number of visits. If the priors are common across all models, just the first lists of values for visits is used, the others are present but ignored.

## 2.2 Common structures

### 2.2.1 Gamma prior

The parameters are stored in a \<gamma\> value as \<mu\> – the central value of the prior, and \<n\> - the weight of the prior in equivalent number of observations. E.g.:

``` xml
<gamma>
<mu>10</mu>
<n>1</n>
</gamma>
```

### 2.2.2 Normal prior

The parameters are stored in a \<menasdpair\> value as \<mean\> - the mean of the prior and \<sd\> - the standard deviation. E.g.:

``` xml
<meansdpair>
<mean>0</mean>
<sd>10</sd>
</meansdpair>
```

### 2.2.3 Beta prior

The parameters are stored in a \<betadist\> value as \<alpha\> - the alpha parameter of the beta distribution prior and \<beta\> - the beta parameter of the beta distribution prior. E.g.:

``` xml
<betadist>
<alpha>2</alpha>
<beta>2</beta>
</betadist>
```

### 2.2.4 Segment values

The FACTS Core TTE GUI writes out values that are in lists of values by dose by visit or segment using a \<segment\> structure and the innermost element. \[It’s not clear why this was used rather than a “D” long list of items as is done everywhere else\]

The \<segment\> structure has the following parameters stored in it:

``` xml
<segment>
<dose>Control</dose>
<hazardrate>0.02</hazardrate>
</segment>
```

Rather than simply indexing through a list of values per dose, the dose is identified using the dose name.

### 2.2.5 Legacy values

The FACTS Core TTE GUI writes out some legacy parameters that other versions of the FACTS Core GUI have dropped. Do not remove or change these parameters in the “.facts” file, just leave them be.

Some of them use an old \<SelectedValue\> structure, the parameters stored in this structure are:

``` xml
<selectedvalue>
<selected>0</selected>
<value>0.9</value>
</selectedvalue>
```

The \<selected\> parameter was a flag 0 or 1 indicating whether the value had been enabled, and the \<value\> parameter was the threshold that the condition was to be tested against.

### 2.2.6 Posterior probability QOI

The parameters stored in a \<post_prob_qoi\> structure are:

- condition: 3=greater than, 4=less than

- tte_qoi_type: leave at 1 for the main endpoint, in FACTS Core TTE, this takes the value ‘2’ for predictor based endpoints.

- relation

  - is_absolute: “True” or “False”

  - comparison arm: id is_absolute=False, this is the name of the comparison arm, the name must appear as a name in the \<dose_names\> property.

  - delta: the value of the delta in the comparison

- qoi_parametrization_type: 0=standard parametrization, 1=log-odds parametrization (dichotomous endpoint only), 2=hazard rate parametrization (TTE endpoint only).

- alt_name: the short name for the QOI, the default QOIs have specific names, the user added QOIs have names of the form QOI_1_N for the n^(th) posterior probability QOI

- is_default_qoi: “True” or “False”, if the QOI is a default QOI its values should not be changed.

- qoi_id: this is a Microsoft unique id, so that the QOI can be referenced from other parameters where the QOI is used.

Example:

``` xml
<post_prob_qoi>
<condition>3<\condition>
<tte_qoi_type>1<tte_qoi_type>
<relation>
<is_absolute>False</is_absolute>
<comparison_arm>Control</comparison_arm>
<delta>1</delta>
</relation>
<qoi_parametrization_type>0</qoi_parametrization_type>
<alt_name>Pr(CSD)</alt_name>
<is_default_qoi>True</is_default_qoi>
<qoi_id>ac13283c-dffc-47d6-9ba1-a371a633e626</qoi_id>
<\post_prob_qoi>
```

### 2.2.7 Predictive probability QOI

The parameters stored in a \<pred_prob_qoi\> structure are:

- pred_prob_type: 1=Bayesian

- phase: 1=Current trial, 2=Future trial

- test_type: 1=superiority, 2=non-inferioirty (Future Trial only)

- subjects_per_arm: number of subjects arm (Future Trial only)

- margin: Superiority/Non-inferiority margin (Future Trial only)

- dropout_type: 0=LOCF, 1=BOCF, 2=Ignore, 3=Ignore (Current Trial only)

- freq_test_type: 1=Unadjusted, 2=Bonferroni, 3=Dunnett (Current Trial only),

  - TTE specific test types are: 5=Log Rank, 6=Wilcoxon, 7=Cox Proportional Hazard, 8=Bonferroni adj Log Rank, 9=Bonferroni adj Wilcoxon, 10=Bonferroni adj Cox Proportional Hazard

- sample_size: 1=Current Enrollment, 2=Trial Maximum Enrollment (Current Trial only) (note: Trial Maximm cannot be used with adaptive allocation)

- alpha: one-sided alpha

- alt_name: the short name for the QOI, the default QOIs have specific names, the user added QOIs have names of the form QOI_2_N for the n^(th) predictive probability QOI

- is_default_qoi: “True” or “False”, if the QOI is a default QOI its values should not be changed.

- qoi_id: this is a Microsoft unique id, so that the QOI can be referenced from other parameters where the QOI is used.

Example:

``` xml
<pred_prob_qoi>
<pred_prob_type>1</pred_prob_type>
<phase>2</phase>
<test_type>1</test_type>
<subjects_per_arm>250</subjects_per_arm>
<margin>0</margin>
<alpha>0.025</alpha>
<alt_name>Pr(SS)</alt_name>
<is_default_qoi>True</is_default_qoi>
<qoi_id>2556a9c6-7277-4437-865c-5e5b7d451634</qoi_id>
</pred_prob_qoi>
<pred_prob_qoi>
<pred_prob_type>1</pred_prob_type>
<phase>1</phase>
<dropouts_type>0</dropouts_type>
<freq_test_type>1</freq_test_type>
<sample_size>2</sample_size>
<alpha>0.027</alpha>
<alt_name>QOI_2_3</alt_name>
<is_default_qoi>False</is_default_qoi>
<qoi_id>39c533a1-0cfd-476f-9561-aea191730f91</qoi_id>
</pred_prob_qoi>
```

### 2.2.8 P-value QOI

The parameters stored in a \<pvalue\_ qoi\> structure are:

- pvalue_dropout_type: 0=LOCF, 1=BOCF, 2=Ignore, 3=Ignore (Current Trial only)

- test_type: 1=Unadjusted, 2=Bonferroni, 3=Dunnett, 4=Trend Test

  - TTE specific test types are: 5=Log Rank, 6=Wilcoxon, 7=Cox Proportional Hazard, 8=Bonferroni adj Log Rank, 9=Bonferroni adj Wilcoxon, 10=Bonferroni adj Cox Proportional Hazard

- alt_name: the short name for the QOI, the default QOIs have specific names, the user added QOIs have names of the form QOI_3_N for the n^(th) pvalue QOI

- is_default_qoi: “True” or “False”, if the QOI is a default QOI its values should not be changed.

- qoi_id: this is a Microsoft unique id, so that the QOI can be referenced from other parameters where the QOI is used.

Example:

``` xml
<pvalue_qoi>
<pvalue_dropouts_type>0</pvalue_dropouts_type>
<test_type>2</test_type>
<alt_name>QOI_3_2</alt_name>
<is_default_qoi>False</is_default_qoi>
<qoi_id>44ba7e29-da1f-4de8-913b-24aa0ed6b3c9</qoi_id>
</pvalue_qoi>
```

### 2.2.9 Target Dose QOI

The parameters stored in a \<target_dose_qoi\> structure are:

- target_type: 1=Max, 2=MED, 3=EDq

- tte_qoi_type: leave at 1 for non-tte endpoints

- dose_combo_range_type: when using the 2D dose model, this represents the chosen 2D dose grid range type for which to evaluate the target dose QOIs. 0=Full Range, 1=Only columns, 2=Only rows.

- dose_combo_range_arm: when using the 2D dose model, this represents the specific column or row (dependent on whether dose_combo_range_type is equal to 1 or 2, respectively) of the 2D dose grid for which to evaluate the

- relation (MED & EDq target types only, present but empty for Max target type)

  - is_absolute: “True” or “False”

  - comparison arm: id is_absolute=False, this is the name of the comparison arm, the name must appear as a name in the \<dose_names\> property.

  - delta: the value of the delta in the comparison for MED, or the quantile for the EDq

- alt_name: the short name for the QOI, the default QOIs have specific names, the user added QOIs have names of the form QOI_4_N for the n^(th) target dose QOI

- is_default_qoi: “True” or “False”, if the QOI is a default QOI its values should not be changed.

- qoi_id: this is a Microsoft unique id, so that the QOI can be referenced from other parameters where the QOI is used.

Example:

``` xml
<target_dose_qoi>
<target_type>2</target_type>
<tte_qoi_type>1</tte_qoi_type>
<dose_combo_range_type>1</dose_combo_range_type>
<dose_combo_range_arm>Dose 2</dose_combo_range_arm>
<relation>
<is_absolute>True</is_absolute>
<comparison_arm />
<delta>2</delta>
</relation>
<alt_name>QOI_4_1</alt_name>
<is_default_qoi>False</is_default_qoi>
<qoi_id>276464ff-0bfd-4509-b010-564a06e16d85</qoi_id>
</target_dose_qoi>
```

### 2.2.10 Decision QOI

Note the decision qoi XML tags use a perhap confusing naming scheme the list tag is \<decisions_qoi\> and each individual entry is \<decision_qois\>.

The parameters stored in a \<decision_qois\> structure are:

- ref_qoi_id: the id of the post_prob, pred_prob or pvalue QOI to be used

- target_dose_qoi: the id of the target dose QOI to use to pick a specific value out of the ref_qoi. Instead of a QOI id, it is also possible to use an “ActualDose”. This has the form “ActualDose- - -” followed by the Dose Name or “Max/Min probability over all doses” or “Max/Min pvalue over all doses”.

- alt_name: the short name for the QOI, the default QOIs have specific names, the user added QOIs have names of the form \<ref QOI’s short name\>\_at\_\<target dose QOI short name or Dose Name\>

- is_default_qoi: “True” or “False”, if the QOI is a default QOI its values should not be changed.

- qoi_id: this is a Microsoft unique id, so that the QOI can be referenced from other parameters where the QOI is used.

Example:

``` xml
<decision_qois>
<ref_qoi_id>83e1a1cc-dc7b-4ab0-a4f0-f16d1fd20d02</ref_qoi_id>
<target_dose_qoi>975e51d6-8399-4dca-b76c-4bef85fa7b5b</target_dose_qoi>
<alt_name>QOI_1_2_at_Pr(Max)</alt_name>
<is_default_qoi>False</is_default_qoi>
<qoi_id>f8831384-dfc1-4b68-800f-005cf1c1fea2</qoi_id>
</decision_qois>
<decision_qois>
<ref_qoi_id>06288ef6-3979-4eeb-a1b2-2a8e9fc58891</ref_qoi_id>
<target_dose_qoi>ActualDose---Max probability over all doses</target_dose_qoi>
<alt_name>QOI_1_3_at_Max_probability_over_all_doses</alt_name>
<is_default_qoi>False</is_default_qoi>
<qoi_id>ac597934-f638-41df-b770-86b42f8f4bb8</qoi_id>
</decision_qois>
```

### 2.2.11 Interim Criteria

The parameters stored in an \<interim_criteria\> are

- interim_number: the number of the interim the criteria apply to, the number 32767 indicates the final success/futility criteria

- \<success_criteria\> see below for details

- \<futility_criteria\> see below for details

- \<graduation_criteria\> see below for details

### 2.2.12 Success / Futility / Graduation Criteria

The parameters stored in a \<success_criteria\>, \<futility_criteria\> or \<graduation_criteria\> are:

- is_enabled: True or False

- list of one or more \<stopping_criterion\>, see below for details

- is_and_condition: True if conditions are AND-ed together, False if conditions are OR-ed together

- min_info_reqd: N – the minimum total amount of information (as defined for interims) required

- min_dose_info_reqd:

  - target dose: The id of a target QOI or “Actual Dose- - -” followed by the Dose name

  - min_info: N – the minimum information (as defined for interims) on that dose required

### 2.2.13 Stopping Criterion

The parameters stored in a \<stopping_criterion\> structure are:

- qoi_id: the id of the decision QOI being tested

- condition: 3=greater than, 4=less than

- threshold: the threshold the QOI is to be compared to

### 2.2.14 Dose Combo

The parameters stored in a \<doseCombo\> structure are:

- iddoseY: the row factor strength of the dose combo

- iddoseX: the column factor strength of the dose combo

- isAllowed: flag indicating whether the dose combo is used in the study

- name: the name of the dose combo

- level: the effective strength of the dose combo

Example:

\<doseCombo\>

\<iddoseY\>0\</iddoseY\>

\<iddoseX\>1\</iddoseX\>

\<isAllowed\>1\</isAllowed\>

\<name\>Dose 1, Default\</name\>

\<level\>1\</level\>

\</doseCombo\>

- 

## 2.3 The values

[TABLE]
