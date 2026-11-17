In FACTS Core with an ordinal endpoint there are 2 different ways to specify the virtual subject response:

1.  Explicit specification of all outcome probabilities

2.  Explicit specification of outcome probabilities for the control arm, plus an odds ratio for each other arm that defines a proportional odds treatment effect deviation from the control probabilities.

# Explicitly Defined Ordinal Response

Explicitly defined VSRs are the most common type specified in FACTS. In ordinal endpoint designs, an explicitly defined VSR is specified by providing the outcome probabilities for each arm in the trial. If desired, a cumulative logistic proportional odds model can assist with defining these probabilities.

## Dose Response

Dose response profiles can be added and deleted. For each profile the user may use either of these specifications:

1.  The outcome probabilities for each treatment arm (if “Use proportional odds” is unchecked). FACTS allows the user to supply unnormalized probabilities (for example, counts) and will convert the input into a vector of probabilities that sum to one.

2.  The outcome probabilities for the control arm (which can be entered unnormalized), and an odds ratio for each other arm. If the normalized probabilities for the control arm are denoted by $(p^0_1,p^0_2,\ldots,p^0_K)$ and the odds ratio for the $d^{th}$ arm is $\exp(\theta_d)$, then the probabilities $(p^d_1,p^d_2,\ldots,p^d_K)$ for the $d^{th}$ arm are given by $$\mbox{logit}\left(\sum_{j=1}^k p^d_j\right)=\mbox{logit}\left(\sum_{j=1}^k p^0_j\right) - \theta_d$$ for $k=1,2,\ldots,K-1$.

In addition, FACTS provides a check box that allows the user to specify whether a specific arm “should succeed” in that scenario: so that FACTS can report on the proportion of simulations that were successful **and** a ‘good’ treatment arm selected.
