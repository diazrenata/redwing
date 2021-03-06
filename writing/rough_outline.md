---
output:
  word_document:
    reference_docx: default_gdoc.docx
  pdf_document: default
csl: ecology.csl
bibliography: refs.bib
---


# Introduction/Framing

Understanding the interrelated dynamics of size- and -abundance based dimensions of biodiversity is key to understanding biodiversity change in the Anthropocene. Total abundance - i.e. the total number of individual organisms present in a system - and size-based currencies, such as the total biomass or total metabolic flux (“energy use”) of a system, are intertwined, but nonequivalent, measures of biological function. Abundance is more closely tied to species-level population dynamics, while size-based metrics more directly reflect assemblage-level resource use and contributions to materials fluxes at the broader ecosystem scale [@morlon2009; @dornelas2011; @white2007; @connolly2005]. While these currencies are naturally linked [@henderson2010; @morlon2009], changes in size composition can decouple the dynamics of one currency from another [@dornelas2011; @white2004; @white2007; @ernest2009; @yen2017]. This can mean that intuition from one currency may be misleading about others; a trend in numerical abundance might mask something else going on with biomass [@white2004]. Changes in size composition strong enough to decouple currencies may be symptomatic of important changes in ecosystem status- e.g. abundance-biomass comparison curves [@petchey2010]; size-biased extinctions [@young2016; @smith2018]. This underscores the need to understand how these dynamics are playing out in the Anthropocene [@fisher2010]. 


At the **community scale**, changes in the relationship between size and abundance tells us about important functional shifts. This is the scale at which ecological processes (i.e. compensatory dynamics, niche tracking, functional replacement) come into play - in contrast to population or global trends [@mcgill2015; @dornelas2014; @white2007]. To the extent that size is a proxy for other functional traits, changes or consistency in the community-level size structure (individiual size distribution, ISD) over time may reflect processes related to niche structure [@white2007; @petchey2010]. Strong size shifts can decouple the relationship between abundance and biomass. In aquatic systems, such changes in the scaling between abundance and biomass often signal ecosystem degradation [@petchey2010; @kerr01; @warwick1994 and refs therein]. Compensatory shifts in the size structure can buffer community function (in terms of biomass or energy use) against changes in abundance [@terry2015; @ernest2009; @white2004]. Consistency in the size structure may maintain the relationship between size- and -abundance based currencies, even as species composition, total abundance, and total biomass/total energy use fluctuate over time, which can reflect consistency in the niche structure over time [@holling1992]. 



It is important to improve our understanding of these dynamics for terrestrial animal communities in particular. In contrast to terrestrial trees and aquatic systems [@kerr01; @white2007], how the relationship between size and abundance changes over time, and the consequences of these changes for ecosystem-level properties, remain relatively unknown for terrestrial animals (but see @white2004). Terrestrial animal communities exhibit size structure [@ernest2005; @thibault2011], and case studies have demonstrated that size shifts can either decouple N from E for terrestrial animals [@white2004; @yen2017], but not always [@hernandez2011]. Establishing generalities in these dynamics is especially pertinent in the Anthropocene, as these communities are experiencing extensive and potentially size-structured change, with implications at community, ecosystem, and global scales [@young2016; @schmitz2018].

Macroecological-scale synthesis on the interrelated dynamics of the ISD, total abundance, and community function for terrestrial animals has been constrained by 1) a lack of community-level size and abundance timeseries data for these systems [@white2007; @thibault2011], and 2) appropriate statistical methods for relating change in the size structure to changes in abundance and function [@thibault2011; @yen2017]. In contrast to aquatic and forest systems, most long-term surveys of animal communities do not collect data on individuals' *sizes* across a full community (with the exception of small mammal studies, which have made major contributions to our understanding of the dynamics of size, abundance, and function for these systems; [@ernest2005; @white2004; @hernandez2011; @kelt2015]). Global, continental, or population-wide studies capture different phenomena [@white2007; this is a nod to a few studies looking at the size structure *across britain* or something]. The ISDs for terrestrial animals, and specifically for determinate growing taxa (e.g. mammals, birds), are often complex, multimodal distributions [@holling1992; @ernest2005; @thibault2011; @yen2017], and less statistically tractable than the power-law ISDs found in aquatic and tree systems [@kerr01; @white2007; more]. Quantifying change in the size structure, and relating this to change in community-wide abundance and function, is not as straightforward as computing and comparing slopes. As a result, we do not have a general understanding of either 1) the extent to which changes in the ISD decouple the community-level dynamics of abundance, biomass, and energy use in these systems, or of 2) the underlying changes in community structure that account for these effects. 

Here, we begin to address this gap by exploring how temporal changes in species composition and the size spectrum modulate the relationship between total abundance, energy, and biomass for communities of North American breeding birds. We used allometric scaling to estimate community size and abundance data for the North American Breeding Bird Survey, and evaluated how changes in total abundance, biomass, and energy use have co-varied from 1988-2018. Specifically, we examined: 1) How often do these currencies change together vs. have decoupled dynamics?; 2) What are the dominant directions and magnitudes of the overall change and any decoupling between the currencies? We also examined how these changes differ between core species and the whole-community (i.e. including transients), which currently comes completely out of left  field in this introduction (sorry!).


# Methods

1. Bird abundance data
    1. We used data from the Breeding Bird Survey to compare community attributes along each route between the 5-year periods from 1988-1992 and 2014-2018.
        1. We used a discrete time-period comparison (as opposed to continuous-time over the full timeseries) to simplify comparisons between temporal turnover in species composition and the size structure - which are complex, multidimensional distributions - and changes in community-wide total abundance, biomass, and energy use. 
            1. We acknowledge that a continuous-time perspective may be better equipped to detect nonlinear dynamics [@macgregor2019] and account for artefacts related to the selected beginning and ending dates [@cusser2020; @balhai2021]. Developing continuous-time methods for analyzing complex, multidimensional community distributions such as the size spectrum is an important and ongoing area of methodological development (e.g. @yen2017). 
        1. We used 5-year periods so as to smooth out interannual variability and to account for sampling accumulation effects in characterizing the bird community in each time period [@white2004].
            1. We used the same begin and end dates for all routes in the analysis so as to have a consistent window. 
            1. We explored the number of routes in the dataset with complete sampling coverage for two five-year “begin” and “end” periods with start dates ranging from X to X and end dates ranging from X to X, and selected beginning and ending dates of 1988 and 2018 so as to obtain a large number of routes from diverse bird conservation regions, and span a relatively long window of time (could ref @cusser2020 there). 
        1. This yielded 528 routes. 
            1. Sometimes I subsample the 528 to get a maximum of 10 routes per bird conservation region, so that the highly-sampled BCRs don't  dominate aggregate analyses [@thibault2011]. That yields 238 (I believe).
1. Estimated size data
    1. BBS contains abundances for all species along a route in each year, but does not include measurements of individual body size. We generated body size estimates for individual birds assuming that intraspecific size distributions are normally distributed around a species’ mean body size (following @thibault2011; also recent Myers/Catano/Fristoe paper I believe).
        1. Using records of species’ mean and standard deviation body sizes from @dunning2007, we drew individuals’ body sizes from the appropriate normal distributions.
            1. For species for which there was not a standard deviation recorded in @dunning2007 (n =?), we estimated the standard deviation based on an allometric scaling relationship between mean and standard deviation in body mass (also described in @thibault2011). For species with multiple records in @dunning2007, we used the mean mean and standard deviation body sizes across all records (averaging across sexes, subspecies, and records from different locations). We performed this averaging after estimating any missing standard deviation measurements.
        1. This method does not incorporate intraspecific variation in body size across geographies or over time [@dunning2007; @gardner2011]. However, it makes it possible to conduct macroecological studies of avian size distributions at a spatial and temporal scale that would otherwise be impossible [@thibault2011].
        1. For each individual bird observed, we estimated metabolic rate as (parameters) [@fristoe2015]. 
        1. For each route in a given year, we compute total energy use, total biomass, and total abundance by summing over all individuals observed on that route in that year. 

1. Comparing ISDs over time
    1. Characterizing the ISD
        1. For a given route and time period, we draw the appropriate numbers of individuals of each species from their corresponding normal distributions.
    1. We use two approaches to test whether the ISD for 1988-1992 is significantly different from the one for 2014-2018
        1. Kolmogorov-Smirnov test on the vector of masses for the begin vs end time periods
        1. Bootstrap resampling of individuals [pretty sure this derives from @ernest2005]:
            1. From the pool of all individuals "observed" in both timeperiods, draw the appropriate number for each time period without replacement.
            1. KS test comparing "begin" and "end" for the reshuffled communities
            1. Repeat 500x and retain the test statistic (D) for all tests
            1. Compute the percentile and standardized effect size [@gotelli_SES] for the test statistic for the *actual* time periods to the distribution of test statistics for the reshuffles
    1. For an intuitive measure of the magnitude of change over time, we compute an overlap measure derived from [@read2018]:
        1. We characterize the ISD as a smooth function by fitting a Gaussian mixture model (to logarithm of mass, up to 15 Gaussians, select best using BIC, all following @thibault2011). We evaluate the density function of the GMM at points for a size range from 0-exp(15), which covers the range of sizes in this dataset with ample padding on each side. We then rescale the density function so the total area under the ISD is 1.
        1. We calculate the overlap between two ISDs as the sum of the minimum density at each evaluation point. This ranges from 0 (no overlap) to 1 (complete overlap).


1. Decoupling of dynamics in total abundance, biomass, and energy use over time
    1. To test whether change in the ISD results in decoupling of currencies, we can't just compare the slopes for total energy, total biomass, total abundance to each other. This is because the three different currencies are on radically different scales of measurement. We also can't rescale using the usual methods (e.g. scale to mean 0/sd 1, sqrt transform @dornelas2014; @gotelli2017) because these destroy information about the range of variability within a single currency.
    1. Instead, we test whether the observed change in biomass or energy use differs from the change that we would expect given observed changes in community-wide abundance, but with *no change* in the ISD from beginning to end. Enter Null Model A.
    1. Figures and functions walking through this procedure are at https://github.com/diazrenata/redwing/blob/resim-cleanup/aspirational_structure/methods_vignettes/change_over_time/02_01_change_over_time_sims.md but they're a little rough. 
    1. Simulate change in total energy and total biomass under "no change in ISD" and "observed change in ISD" scenarios
          1. We construct sampling ISDs for each time period, to characterize the probability of observing an individual of a given size in that time period. We draw individuals, fit GMMs, and characterize the probabilty density function as above. For this, because there is some sampling error, I draw 5 copies of the ISD and fit the GMM to the combined 5 draws. This doesn't affect anything in practice and is mostly inspired by a one-off comment from Allen Hurlbert about "sampling error" so I might drop it. We construct sampling ISDs for the "begin" and "end" time periods.
          1. We then *re draw* individuals for each time period and shuffle the ISDs to produce scenarios.
              1. First, we draw individuals for each year using the actual ISD for that time period (so the "begin" ISD for 1988-1992 and the "end" ISD for 2014-2018).
              1. Then we draw individuals for each year, but using the "begin" ISD for all years.
              1. We draw year-by-year, instead of the whole time period, because there is interannual, intratimeperiod variation in total abundance that we would like to capture. But we pool individuals within a time period to create the ISD to smooth out species accumulation.
              1. Draws from sampling ISDs like this diverge slightly from draws from the raw species counts and sds. So we run everything through this pipeline for comparability.
          1. We compute total energy use and total biomass for each year for each scenario. 
          1. Again because there is some sampling error, we repeat the re-drawing of individuals 5 times and compute the mean total biomass/total energy use across draws. Again I don't think this really affects stuff in aggregate and I could be talked out of it. 
    1. We use Bayesian linear models to test whether change in the "actual ISDs" vs "no change-ISDs" scenarios differs.
        1. The "no-change ISDs" scenario reflects change in total energy/biomass due simply to changes in total abundance. The "actual ISDs" scenario reflects the combined effects of change in abundance and change in the ISD.
        1. We evaluate change at the route level. I tried fitting everything within one  hierarchical model, but the hierarachical model attributes a lot of variation to getting the right intercept for each route (which we don't care about) and does a really bad job estimating within-route slopes and decoupling (which is what we do care about). It's also ridiculously hard to compute for a large number of routes and I never managed to run it on the full dataset. These are challenges you don't run into if - like @dornelas2014 - you're only interested in change in one currency per site. We care most about the *decoupling* of slopes at the route level. The use of a Bayesian framework helps us offset some of the issues around p-values if we were to do this many models in a frequentist setting. 
        1. Figures and functions here:  https://github.com/diazrenata/redwing/blob/resim-cleanup/aspirational_structure/methods_vignettes/change_over_time/02_02_testing_change.md (but rough)
        1. For each currency for each route, we fit 3 models:
            1. `total_biomass ~ timeperiod * scenario`, `total_biomass ~ timeperiod`, `total_biomass ~ 1`
            1. We used LOO-crossvalidation to select the best-fitting model as the simplest model with an ELPD within 1 SE of the best-fitting model.
            1. We used `reloo` to correct for outliers.
            1. We fit as Gaussians with default priors, run for 8000 iterations. We fit as Gaussians to avoid having to deal with back-transforming the parameter estimates (and because when I coerced to other family distributions I got tons of convergence issues). Running for 8000 iterations is extremely generous for these models.
        1. If the best-fitting model does not include the scenario term, it means that the change in the ISD does not ~significantly decouple the dynamics of total energy/biomass from that which is driven by changes in abundance. If there is no **timeperiod** term, it means that there's not a ~significant change begin-end.
        1. We extract parameter estimates from the best-fitting model to examine the magnitude and direction of change beginning-to-end and decoupling due to change in the ISD. 
    1. I ran this whole pipeline on some "quality control" sims where 1) neither abundance nor the ISD changes begin-end and 2) the ISD doesn't change begin-end for either scenario. It behaves as expected for those. 

1. Core-transient
    1. We run this pipeline on the whole dataset, and using just core species (those present in >= 2/3 of timesteps).
    1. We also explore a null model in which we restrict to the same richness as the core species richness but draw a random set of species.





<!-- Discussion points -->
<!-- Overall, a signal of ~parallel dynamics, and particularly decreases, in abundance, biomass, and energy use over time -->
<!-- About 50% of the time, not significant. But when significant, overwhelmingly 1) a decrease and 2) changing together. -->
<!-- This is consistent with concerns about declines in abundance, but not consistent with size-structured declines amplifying declines in function beyond abundance  (Dirzo et al. 2014). -->
<!-- Note that this study is not definitive for biodiversity monitoring -->
<!-- Discrete vs. continuous time -->
<!-- Geographic bias in routes -->
<!-- Continuous-time methods for ISD work, and case studies better targeted for biodiversity monitoring, are both next-steps -->
<!-- Relationship usually maintained due to low taxonomic turnover, but not detectably through functional replacement -->
<!-- Low taxonomic turnover may reflect stable niche structure over time. -->
<!-- Absent the empirical basis for parameterizing a null model of species turnover (e.g. the necessary parameters to run neutral simulations), we cannot distinguish between random and systematic dynamics of taxonomic change.  -->
<!-- But, we observe pretty low turnover. -->
<!-- There is no dominating signal of functional replacement conserving the size structure beyond what is expected given taxonomic turnover + the species pool. -->
<!-- This null model is inherently conservative, with a high type-II error rate (Ulrich et al. 2017). Not deviating does not necessarily mean there’s no size structured dynamics. There may be both size structured replacement and size shifts operating simultaneously, or simply weak/imperfect functional replacement.  -->
<!-- Or, birds might be less strongly size-structured than fish, trees, or rodents. There are more ways to be a 20g bird than a 20g pocket mouse. -->
<!-- When there is a decoupling of currencies, tends to be an increase in body size → less negative slope in biomass/energy vs abundance -->
<!-- Contrasts with concerns about size-biased extinctions -->
<!-- Consistent with other reports from BBS (Schipper et al. 2016) -->
<!-- May reflect forests in ~recovery over this time period (Schipper et al. 2016) -->
<!-- Get into some case studies here -->
<!-- These results might or might not be borne out in other taxonomic groups or other geographic regions. -->
<!-- BBS is on intact habitats in systems that may have been recovering - counter to global trends or trends in areas of particular concern -->
<!-- Next steps should include specifically exploring systems that have undergone major disturbances, and/or a large degree of taxonomic turnover.  -->
<!-- And greater taxonomic coverage - comparative work between mammals and birds, for example -->

\newpage
# References
