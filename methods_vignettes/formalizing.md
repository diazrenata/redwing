---
title: "Formalizing"
author: Renata Diaz
output:
      pdf_document
---


# Definitions

$S_j$: Species richness in time period $j$. 

$n_{s, j}$: Abundance of species $s$ in time period $j$.

$N_j$: Total number of individuals observed in time period $j$. $\sum_{s=1}^{s=S}n_{s, j}$

$\mu_s$: Mean mass for species $s$ (grams).

$\sigma_s$: Standard deviation of mass for species $s$.

$SBSD_s(m)$: Species body-size distribution. Probability of observing an individual with mass $m$ from species $s$. $Normal(\mu_s, \sigma_s)$.

$sbsd_{s, j}$: Sampled species body-size distribution. Vector of $n_{s, j}$ masses for individuals of species $s$ observed in time period $j$, obtained by drawing $n_{s, j}$ values from $SBSD_s$.

$isd_j$: Sampled individual size distribution. Vector of $N_j$ masses for all individuals, of all species, observed in time period $j$. Obtained by concatenating $sbsd_{s, j}$ for all $s$ in time period $j$. 

$ied_j$: Sampled individual energy use distribution. $pars(isd_j)$.

$ISD_j(m)$: Individual size distribution. Probability of observing an individual with mass $m$ in time period $j$. Obtained by fitting a Gaussian mixture model to $isd_j$, extracting the density function, and rescaling so the total probability density sums to 1. 

$risd_{N, ISD}$: Re-sampled individual size distribution. Vector of $N$ individuals drawn from $ISD$. 

$ried_{N, ISD}$: Re-sampled individual energy distribution. Obtained as for $ied$ using $risd$.

$B$, $rB$: Total biomass ($\sum isd$) or re-sampled total biomass ($\sum risd$).

$E$, $rE$: Total energy use ($\sum ied$) or re-sampled total energy use ($\sum ried$).

$risd_{N_j, ISD_k}$: Re-sampled individual size distribution using $N_j$ and $ISD_k$ for time periods $j$ and $k$. 

$ried_{N_j, ISD_k}$, $rB_{N_j, ISD_k}$, $rE_{N_j, ISD_k}$: Obtained using $N_j$ and $ISD_k$ for time periods $j$ and $k$, as for $risd_{N_j, ISD_k}$. 

# Comparing across time periods

Comparing $N_j$ to $N_k$: Change in abundance from time $j$ to $k$.

Comparing $rB_{N_j, ISD_j}$ to $rB_{N_k, ISD_k}$: Change in biomass from time $j$ to time $k$. 

*The crux of it*:

Comparing $rB_{N_j, ISD_j}$ to $rB_{N_k, ISD_j}$: Change in biomass from time $j$ to time $k$ *expected* if the size structure is held constant between the two time periods. 

