Welcome to the Documentation section — your go-to repository of in-depth user guides and practical examples that illuminate every aspect of FACTS. Whether you’re just getting started or delving into advanced functionalities, these resources are designed to help you navigate our software with confidence and ease.

## What You’ll Find Here

### User Guides

Access detailed manuals covering the full range of our engines. From foundations to advanced features, learn how to set up your desired trial design, learn about best practices and obtain focused understanding of the subject matter, and troubleshoot common issues.

- [3+3 and mTPI - Rule Based Dose Escalation](../documentation/v82/userguides/de.llms.md)
- [Phase II & III Designs](../documentation/v82/userguides/core/index.llms.md)
- [Seamless Designs](../documentation/v82/userguides/staged.llms.md)
- [Enrichment Designs](../documentation/v82/userguides/enrichment/index.llms.md)
- [CRM - Model Based Dose Escalation](../documentation/v82/userguides/crm.llms.md)
- [2D CRM - 2D Model Based Dose Escalation](../documentation/v82/userguides/2dcrm.llms.md)
- [FACTS/R - How to Call FACTS from R](../documentation/v82/userguides/FACTSfromR.llms.md)
- [FLFLL - FACTS Linux File Loader Light](../documentation/v82/userguides/flfll.llms.md)
- [Platform Trials](../documentation/v82/userguides/platform.llms.md)
- [FACTS Design Report](../documentation/v82/userguides/dr.llms.md)

### Tutorials

Step-by-step examples of designing and simulating trials with FACTS. Explore a range of constructed and real-world studies that demonstrate FACTS’s capabilities in action and discover strategies to optimize performance. Most tutorials come with the example “.facts” files they use, which you can download from the “Example files” box at the top of each tutorial.

#### Phase 2 and 3 Designs

- [Continuous Endpoint (FEV)](../documentation/v82/tutorials/core/continuousFEV.llms.md)
- [Continuous Endpoint (RAR Design - Chronic Cough)](../documentation/v82/tutorials/core/continuousRAR.llms.md)
- [Dichotomous Endpoint (HIV)](../documentation/v82/tutorials/core/dichotomousHIV.llms.md)
- [Dichotomous Endpoint (Phase 3 Goldilocks)](../documentation/v82/tutorials/core/dichotomousGoldilocks.llms.md)
- [Dichotomous Endpoint (Shared Control)](../documentation/v82/tutorials/core/dichotomousSharedControl.llms.md)
- [Dichotomous Endpoint (Single Arm Phase 3 with OPC)](../documentation/v82/tutorials/core/dichotomousSingleArmOPC.llms.md)
- [Two-Dimensional Dosing (Dichotomous)](../documentation/v82/tutorials/core/twoDimensionalDosing.llms.md)
- [Simple Dose Response Models (Dichotomous and Time-to-Event)](../documentation/v82/tutorials/core/simpleDRModels.llms.md)
- [Multiple Endpoint (Pain + Drug Tolerability)](../documentation/v82/tutorials/core/multendptPain.llms.md)
- [Time-to-Event Endpoint (Lung Cancer)](../documentation/v82/tutorials/core/tteLungCancer.llms.md)
- [Time-to-Event Endpoint (with Predictor)](../documentation/v82/tutorials/core/ttePredictor.llms.md)
- [Ordinal Endpoint](../documentation/v82/tutorials/core/ordinal.llms.md)

#### Enrichment and Basket Trial Designs

- [Continuous Endpoint (Alzheimer’s)](../documentation/v82/tutorials/enrichment/continuousAlzheimers.llms.md)
- [Dichotomous Endpoint (Cancer)](../documentation/v82/tutorials/enrichment/dichotomousCancer.llms.md)
- [Dichotomous Endpoint (Dominant Group)](../documentation/v82/tutorials/enrichment/dichotomousDominantGroup.llms.md)
- [Time-to-Event Endpoint (Genomic Biomarker)](../documentation/v82/tutorials/enrichment/tteGenomicBiomarker.llms.md)

#### Seamless Phase 2/3 Designs

- [Continuous Endpoint](../documentation/v82/tutorials/staged/continuous.llms.md)

#### Platform Trial Designs

- [Continuous Endpoint (PTSD)](../documentation/v82/tutorials/platform/continuousPTSD.llms.md)
- [Dichotomous Endpoint](../documentation/v82/tutorials/platform/dichotomous.llms.md)

#### Phase I Dose Escalation Designs

- [CRM](../documentation/v82/tutorials/de/ncrm.llms.md)
- [CRM Open Enrollment](../documentation/v82/tutorials/de/ncrmOpenEnrollment.llms.md)
- [CRM 2 Samples and Open Enrollment](../documentation/v82/tutorials/de/ncrm2GroupsOpenEnrollment.llms.md)
- [CRM (Toxicity)](../documentation/v82/tutorials/de/crmToxicity.llms.md)
- [CRM (Toxicity 2-Sample)](../documentation/v82/tutorials/de/crmTox2Sample.llms.md)
- [CRM (Ordinal)](../documentation/v82/tutorials/de/crmOrdinal.llms.md)
- [bCRM](../documentation/v82/tutorials/de/bcrm.llms.md)
- [CRM (Efficacy)](../documentation/v82/tutorials/de/crmEfficacy.llms.md)

### Execution Guides

Guides to running FACTS and its design engines outside of the FACTS GUI.

- [FACTS Command Line](../documentation/v82/executionguides/commandLine.llms.md): running all the simulations of one or more FACTS project files from the command line, as a batch action.
- [FACTS Trial Execution](../documentation/v82/executionguides/trialExecution.llms.md): running interim analyses of a trial with the FACTS Core, Platform Trial and Enrichment Design engines from the command line (Trial Execution Mode), and from the “Analysis” tab.
- [Dose Escalation Design Trial Execution](../documentation/v82/executionguides/deTrialExecution.llms.md): running the Dose Escalation design engine on the current trial data to obtain the recommended dose for the next cohort.
- [FACTS Forward Simulation](../documentation/v82/executionguides/forwardSimulation.llms.md): running forward simulations with the FACTS Core engines from the command line (Forward Simulation Mode).

### FACTS File XML Specifications

These specifications describe the format of the XML “.facts” files: the parameters stored in a “.facts” file for each design type. They are intended for expert FACTS users who wish to create or modify “.facts” files using scripts.

#### Phase 2 and 3 Designs

- [Continuous and Dichotomous](../documentation/v82/xmlspecs/core.llms.md)
- [Time-to-Event](../documentation/v82/xmlspecs/coreTTE.llms.md)
- [Multiple Endpoint](../documentation/v82/xmlspecs/coreME.llms.md)
- [Ordinal](../documentation/v82/xmlspecs/coreORD.llms.md)

#### Enrichment and Basket Trial Designs

- [Enrichment and Basket Trial Design](../documentation/v82/xmlspecs/enrichment.llms.md)

#### Seamless Phase 2/3 Designs

- [Continuous and Dichotomous](../documentation/v82/xmlspecs/staged.llms.md)
- [Time-to-Event](../documentation/v82/xmlspecs/stagedTTE.llms.md)
- [Multiple Endpoint](../documentation/v82/xmlspecs/stagedME.llms.md)
- [Ordinal](../documentation/v82/xmlspecs/stagedORD.llms.md)

#### Platform Trial Designs

- [Platform Trial Design](../documentation/v82/xmlspecs/platform.llms.md)

#### Dose Escalation Designs

- [CRM](../documentation/v82/xmlspecs/ncrm.llms.md)
- [2D-CRM](../documentation/v82/xmlspecs/2dcrm.llms.md)

## How to Get the Most Out of This Section

This section contains resources for the current and past versions of FACTS, which you can choose in the sidebar. If necessary, begin by reviewing the installation guides. Later, you may consult engine-specific user manuals for deeper insights, and explore our curated examples to bring theory into practice. With these resources at your fingertips, you’ll gain a richer, more informed understanding of FACTS and maximize your potential for success.
