# 1 Introduction

This document describes the FACTS XML file (“.facts”) file format FACTS Platform Trial for Continuous & Dichotomous endpoints.

It is intended for expert FACTS users who wish to create modified versions of “.facts” files using scripts. A full and deep understanding of the FACTS Platform Trial simulator is assumed.

## 1.1 IDs

There are number of parameters that IDs, such as “guid”, “SetId”, “qoi_id” … these are Microsoft “unique” identifiers, there should be no need to change these. It does not matter if the same Ids are used in successive “.facts” files, for instance if creating a range of sub-versions of a single original file.

## 1.2 Defaults

FACTS stores a super-set of the FACTS parameters in the “.facts” file - even storing parameters for the Dichotomous endpoint in a file for a Continuous design. There are so many parameters in the FACTS file that we cannot check the behavior of FACTS if some of these are missing, so our advice is to include \*all\* the parameters even if not required. The only parameters that it is clear can be omitted are those in the optional parameter sets: BaselineParameterSet and LongitudinalParameterSet that are not required if the respective features are not enabled.

## 1.3 Parameter Order

FACTS processes the parameters in the “.facts” file in the order in which the appear in the file. Some parameters depend on other parameters to have been already defined, so it is recommended that the order given here (the order FACTS itself uses when writing out the parameters) is retained. For many parameters changing their order should not be a problem, but we are not going to define, or guarantee, which those are.

# 2 The values in a “.facts” file

## 2.1 Numbers of items

D: The number of arms in the design

I: The number of milestones

LM: The number of longitudinal models

MT: The Maximum number of concurrent Treatments allowed in the trial.

PostQ: The number of Posterior Probability QOIs

PredQ: The number of Predictive Probability QOIs

PvQ: The number of p-value QOIs

R: The number of regions in an accrual profile

S: The number of scenarios

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

The parameters are stored in a \<meansdpair\> value as \<mean\> - the mean of the prior and \<sd\> - the standard deviation. E.g.:

``` xml
<meansdpair>
<mean>0</mean>
<sd>10</sd>
</meansdpair>
```

### 2.2.3 Posterior probability QOI

The parameters stored in a \<post_prob_qoi\> structure are:

- analysis_population: 0=all controls, 1=concurrent controls

- condition: 3=greater than, 4=less than

- tte_qoi_type: leave at 1 for non-tte endpoints

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
<analysis_population>0<\analysis_population>
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

### 2.2.4 Predictive probability QOI

The parameters stored in a \<pred_prob_qoi\> structure are:

- analysis_population: 0=all controls, 1=concurrent controls

- pred_prob_type: 1=Bayesian, 2=Conditional Power

- phase: 2=Future trial

- test_type: 1=superiority, 2=non-inferiority

- subjects_per_arm: number of participants per arm

- margin: Superiority/Non-inferiority margin

- alpha: one-sided alpha

- alt_name: the short name for the QOI, the default QOIs have specific names, the user added QOIs have names of the form QOI_2_N for the n^(th) predictive probability QOI

- is_default_qoi: “True” or “False”, if the QOI is a default QOI its values should not be changed.

- qoi_id: this is a Microsoft unique id, so that the QOI can be referenced from other parameters where the QOI is used.

Example:

``` xml
<pred_prob_qoi>
<analysis_population>0<\analysis_population>
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
```

### 2.2.5 P-value QOI

The parameters stored in a \<pvalue\_ qoi\> structure are:

- analysis_population: 0=all controls, 1=concurrent controls

- pvalue_dropout_type: 0=LOCF, 2=Ignore

- test_type: 1=Unadjusted

- alt_name: the short name for the QOI, the default QOIs have specific names, the user added QOIs have names of the form QOI_3_N for the n^(th) pvalue QOI

- is_default_qoi: “True” or “False”, if the QOI is a default QOI its values should not be changed.

- qoi_id: this is a Microsoft unique id, so that the QOI can be referenced from other parameters where the QOI is used.

Example:

``` xml
<pvalue_qoi>
<analysis_population>1<\analysis_population>
<pvalue_dropouts_type>0</pvalue_dropouts_type>
<test_type>1</test_type>
<alt_name>QOI_3_2</alt_name>
<is_default_qoi>False</is_default_qoi>
<qoi_id>44ba7e29-da1f-4de8-913b-24aa0ed6b3c9</qoi_id>
</pvalue_qoi>
```

### 2.2.6 Target Dose QOI

The parameters stored in a \<target_dose_qoi\> structure are:

- target_type: 1=Max

- tte_qoi_type: leave at 1 for non-tte endpoints

- dose_combo_range_type: 0=All arms, 3=Active arms, 4=Randomizing arms

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
<target_type>1</target_type>
<tte_qoi_type>1</tte_qoi_type>
<dose_combo_range_type>0</dose_combo_range_type>
<relation />
<alt_name>Pr(Max)</alt_name>
<is_default_qoi>True</is_default_qoi>
<qoi_id>276464ff-0bfd-4509-b010-564a06e16d85</qoi_id>
</target_dose_qoi>
```

### 2.2.7 Treatment Interim Criteria

The parameters stored in an \<trt_interim_criteria\> are

- \<trt_id\>: the treatment ID of the treatment interim criteria. -1 for the criteria which should apply to all treatments.

- \<trt_name\>: the name of the treatment pertaining to the interim criteria. “All Treatments” for the criteria which should apply to all treatments.

- \<interim_criteria\> see below for details

### 2.2.8 Interim Criteria

The parameters stored in an \<interim_criteria\> are

- interim_number: the number of the interim the criteria apply to, the number 32767 indicates the final success/futility criteria

- \<success_criteria\> see below for details

- \<futility_criteria\> see below for details

- \<graduation_criteria\> see below for details

### 2.2.9 Success / Futility / Graduation Criteria

The parameters stored in a \<success_criteria\>, \<futility_criteria\> or \<graduation_criteria\> are:

- is_enabled: True or False

- list of one or more \<stopping_criterion\>, see below for details

- is_and_condition: True if conditions are AND-ed together, False if conditions are OR-ed together

- min_info_reqd: ignore. Set to 0.

### 2.2.10 Stopping Criterion

The parameters stored in a \<stopping_criterion\> structure are:

- qoi_id: the id of the decision QOI being tested

- condition: 3=greater than, 4=less than

- threshold: the threshold the QOI is to be compared to

&nbsp;

- 

## 2.3 The values

[TABLE]
