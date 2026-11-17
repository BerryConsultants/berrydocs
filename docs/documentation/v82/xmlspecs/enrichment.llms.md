# 1 Introduction

This document describes the FACTS XML file (“.facts”) file format for FACTS Enrichment Design (ED).

It is intended for expert FACTS users who wish to create modified versions of “.facts” files using scripts. A full and deep understanding of FATCS ED is assumed.

## 1.1 IDs

There are number of parameters that IDs, such as “guid”, “SetId”, … these are Microsoft “unique” identifiers, there should be no need to change these. It does not matter if the same Ids are used in successive “.facts” files, for instance if creating a range of sub-versions of a single original file.

## 1.2 Defaults

FACTS stores a super-set of the FACTS parameters in the “.facts” file. There are so many parameters in the FACTS file that we cannot check the behavior of FACTS if some of these are missing, so our advice is to include \*all\* the parameters even if not required. The only parameters that it is clear can be omitted are those in the optional parameter sets: BaselineParameterSet and LongitudinalParameterSet that are not required if the respective features are not enabled.

## 1.3 Parameter Order

FACTS processes the parameters in the “.facts” file in the order in which the appear in the file. Some parameters depend on other parameters to have been already defined, so it is recommended that the order given here (the order FACTS itself uses when writing out the parameters) is retained. For many parameters changing their order should not be a problem, but we are not going to define, or guarantee, which those are.

# 2 The values in a “.facts” file

## 2.1 Numbers of items

ACS: The number of studies used for the hierarchical prior for the active comparator

BS: The number of Bayesian Augment Control (BAC) studies

CHRS: The number of control hazard rates segments in TTE

CS: The number of studies used for the hierarchical prior for the control

D: The number of doses (or treatment arms) in the design

DRS: The number of dropout rate segments in TTE

G: The number of groups in the design

I: The number of interims (when defined by information)

LM: The number of longitudinal models

R: The number of regions in an accrual profile

TCM: The number of intervals (in weeks) for the Hazard model control rates

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

### 2.2.3 Group

The parameters stored in a \<group\> are

- \<name\>: the name of the group

- \<cap\>: flag – 0=group is not capped, 1=group is capped

- \<size\>: the group size

E.g.:

\<group\>

\<name\>Group 1\</name\>

\<cap\>1\</cap\>

\<size\>50\</size\>

\</group\>

### 2.2.4 Total cap

The parameters stored in a \<total_cap\> is a \<selected_value\> object with the following data:

- \<selected\>: flag – 0=do not apply overall cap to study, 1=apply overall cap to study

- \<value\>: the max sample size if \<selected\> flag is 1.

E.g.:

\<selectedvalue\>

\<selected\>0\</selected\>

\<value\>300\</value\>

\</selectedvalue\>

### 2.2.5 BAC Study

The parameters stored in a \<bac\> are

- \<type\>: the FACTS endpoint type (1=continuous, 2=dichotomous, 3=time to event)

- \<name\>: the name of the BAC study

- \<subjects\>: the number of subjects in the BAC study

- \<response\>: the response value for the BAC study

- \<sd\>: the SD of the BAC study

- \<num_responses\>: the number of responses for the BAC study (Only used for TTE)

E.g.:

\<bac\>

\<type\>1\</type\>

\<name\>Study 3\</name\>

\<subjects\>300\</subjects\>

\<response\>0\</response\>

\<sd\>0.5\</sd\>

\<num_response\>0\</num_response\>

\</bac\>

### 2.2.6 Interim Criteria

The parameters stored in an \<interim_criteria\> are

- interim_number: the number of the interim the criteria apply to, the number 32767 indicates the final success/futility criteria

- \<success_criteria\>: List of G+1 \<posterior_cut_list_tag\> and G+1 \< phase_three_cut_list_tag \>, for each group and across groups. See below for details.

- \<futility_criteria\> List of G+1 \<posterior_cut_list_tag\> and G+1 \< phase_three_cut_list_tag \>, for each group and across groups. See below for details.

### 2.2.7 Success / Futility Criteria

The parameters stored in a \<success_criteria\> or \<futility_criteria\> are:

- posterior_cut_list_tag:

  - posterior_cut_tag: Threshold for the posterior probabilities of CSD.

- phase_three_cut_list_tag:

  - phase_three_cut_tag: Threshold for the phase III success probabilities.

  - 

## 2.3 The values

[TABLE]
