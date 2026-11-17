# 1 Introduction

This document describes the FACTS XML file (“.facts”) file format FACTS Dose Escalation using the N-CRM model.

It is intended for expert FACTS users who wish to create modified versions of “.facts” files using scripts. A full and deep understanding of FATCS Dose Escalation is assumed.

## 1.1 IDs

There are number of parameters that IDs, such as “guid”, “SetId”, “qoi_id” … these are Microsoft “unique” identifiers, there should be no need to change these. It does not matter if the same Ids are used in successive “.facts” files, for instance if creating a range of sub-versions of a single original file.

## 1.2 Defaults

FACTS stores a super-set of the FACTS parameters in the “.facts” file - even storing parameters for an efficacy endpoint after it has been de-selected as an option. There are so many parameters in the FACTS file that we cannot check the behavior of FACTS if some of these are missing, so our advice is to include \*all\* the parameters even if not required. The only parameters that it is clear can be omitted are those in the optional parameter sets: BaselineParameterSet and LongitudinalParameterSet that are not required if the respective features are not enabled.

## 1.3 Parameter Order

FACTS processes the parameters in the “.facts” file in the order in which the appear in the file. Some parameters depend on other parameters to have been already defined, so it is recommended that the order given here (the order FACTS itself uses when writing out the parameters) is retained. For many parameters changing their order should not be a problem, but we are not going to define, or guarantee, which those are.

# 2 The values in a “.facts” file

## 2.1 Numbers of items

D: The number of doses (or treatment arms) in the design

L: A list of variable length

PL, PL2: The number of doses for which prior pseudo-subject data has been provided (PL2: the same but for group2 subjects)

Var: The number of variants

## 2.2 Common structure

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

### 2.2.3 pieceWiseKnot values

The FACTS Dose Escalation GUI writes out values in VSR profiles that are defined as piecewise linear, by “knots” in lists of values by dose strength using a \<pieceWiseKnot\> structure.

The \<pieceWiseKnot\> structure has the following parameters stored in it:

``` xml
<pieceWiseKnot>
<dose>3</dose>
<value>0.15</value>
</pieceWiseKnot>
```

- 

## 2.3 The values

[TABLE]
