> **TIP:**
>
> Download the FACTS files used in this tutorial and open them in FACTS with *File \> Open*.
>
> - [Core Continuous Cough Ex1.facts](continuousRARattachments/examples/Core%20Continuous%20Cough%20Ex1.facts)

# 1 Trial Overview

Investigators are interested in finding 2 target doses: 1) Min. dose with at least 25% efficacy relative to max dose (ED₂₅), and 2) Min. dose with at least 70% efficacy relative to max dose (ED₇₀).

- Arms: Placebo + multiple doses of therapy

  - 25 – 300mg every 25mg increments

- Sample Size: 200 Subjects

- Primary Efficacy Endpoint: Ratio of cough frequency at 2 weeks relative to baseline

- Success if certain we have found ED₇₀ and ED₂₅

- Interims:

  - Burnin: Enroll 10 subjects each to control, 150mg and 300mg

  - Update randomization probabilities every 4 weeks thereafter

    - Fixed randomization of 10% to control

# 2 Entering the Design into FACTS

## 2.1 Study Info:

![](continuousRARattachments/media/image2.png)

Time to primary endpoint is specified as 2 weeks. The primary endpoint of the trial is cough ratio at 2 weeks compared to baseline and lower response corresponds to a subject improving and coughing less frequently. The total number of subjects enrolled is 200 and they will be enrolled sequentially.

## 2.2 Treatment Arms:

![](continuousRARattachments/media/image3.png)

We include a control arm as well as 12 doses of the therapy (25mg – 300mg in increments of 25mg).

**\**

## 2.3 Simulation Scenarios:

We consider three simulations scenarios (pictured below) for the dose-response curves: 1) Low targets where the ED₂₅ = 18mg and ED₇₀= 78mg, 2) Mid targets where the ED₂₅ = 75mg and ED₇₀= 170mg, and 3) High targets where the ED₂₅ = 170mg and ED₇₀= 221mg. Values of the dose-response for the 12 doses of the therapy are specified under virtual subject response for each scenario. We assume a sd = .3 for each dose.

![](continuousRARattachments/media/image4.png)

![](continuousRARattachments/media/image5.png)

## 2.4 Accrual:

![](continuousRARattachments/media/image6.png)

We assume that 4.6 subjects are accrued per week

## 2.5 Quantities of Interest:

*Target doses*: We are interested in finding two target doses: ED₂₅ and ED_(70.) The doses are specified relative to the control so that the doses represent 25% and 70% of the effect of the maximum dose relative to the control dose.

![](continuousRARattachments/media/image7.png)

*\*

*Decision Quantities*: These quantities will be used in the trial to define success. In particular, we want to define success if we have found both target doses with some certainty. The evaluation doses are the doses with the greatest probability of being the ED₂₅ or ED₇₀ and if the probability of each of these evaluation doses being the ED₂₅ or ED₇₀ exceeds some threshold we will declare success.

![](continuousRARattachments/media/image8.png)

## 2.6 Dose Response Model:

We assume a sigmoidal dose-response model.

![](continuousRARattachments/media/image9.png)

## 2.7 Allocation:

We will use Adaptive Allocation.

After 10 subjects are enrolled on the control, 150mg and 300mg, we start interims. Post the burn-in of 30 subjects enrolled we allocate a fixed 1 out of every 10 subjects to control. The remaining 9 subjects are allocated equally based on the probability that each is the ED₂₅ and the probability that each is the ED₇₀

![](continuousRARattachments/media/image10.png)

## 2.8 Interims:

**I**nterims occur every 4 weeks after the initial 30 subjects are enrolled.

![](continuousRARattachments/media/image11.png)

## 2.9 Success / Futility Criteria:

There is no early stopping for success or futility in this trial. We declare success once all 200 subjects are enrolled and have their final data if we are certain we have found our target doses. In particular we declare success if we meet the following two requirements

- Pr(Dose max_(ED25) = ED₂₅) \> .5 for the dose that is most likely the ED₂₅

- Pr(Dose max_(ED70) = ED₇₀) \> .5 for the dose that is most likely the ED₇₀

![](continuousRARattachments/media/image12.png)

# 3 Simulation and Results

Under Scenario 2 when the target doses are ED₂₅ = 75mg and ED₇₀= 170mg the probability of finding both target doses with at least 50% certainty is 78%.

![](continuousRARattachments/media/image13.png)

The following plot shows the average number of subjects allocated to each dose for Scenario 2. The 100mg and 175mg doses have the highest number of subjects allocated to them on average.

![](continuousRARattachments/media/image14.png)

The following plot shows us the probability of selecting each dose as the ED₇₀ for Scenario 2. Of the measured doses, 175mg is the dose that has at least 70% of the efficacy demonstrated on the maximum dose and is selected most often.

![](continuousRARattachments/media/image15.png)

The next plot shows us the probability of selecting each dose as the ED₂₅ for Scenario 2. We are almost equally likely to select the true ED₂₅ (75mg) as the higher dose (100mg). This is likely because the response of the 75mg dose is exactly 25% of the max dose and therefore in some simulations we are estimating this rate to be less than 25% therefore choosing 100mg as the ED₂₅.

![](continuousRARattachments/media/image16.png)
