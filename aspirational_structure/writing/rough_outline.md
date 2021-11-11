---
output:
  word_document:
    reference_docx: default_gdoc.docx
  pdf_document: default
csl: ecology.csl
bibliography: refs.bib
---


# Introduction/Framing

1. Understanding the interrelated dynamics of size- and -abundance based dimensions of biodiversity is key to understanding biodiversity change in the Anthropocene. 
    1. Total abundance - i.e. the total number of individual organisms present in a system - and size-based currencies, such as the total biomass or total metabolic flux (“energy use”) of a system, are intertwined, but nonequivalent, measures of biological function. 
    1. Abundance is more closely tied to species-level population dynamics, while size-based metrics more directly reflect assemblage-level resource use and contributions to materials fluxes at the broader ecosystem scale [@morlon2009; @dornelas2011; @white2007; @connolly2005]
    1. While these currencies are naturally linked [@henderson2010; @morlon2009), changes in size composition can decouple the dynamics of one currency from another (@dornelas2011; @white2004; @white2007; @ernest2009; @yen2017]. 
    1. This can mean that intuition from one currency may be misleading about others; a trend in numerical abundance might mask something else going on with biomass [@white2004].
    1. Changes in size composition strong enough to decouple currencies may be symptomatic of important changes in ecosystem status
          1. E.g. abundance-biomass comparison curves [@petchey2010]; size-biased extinctions [@young2016; @smith2018]
          1. This underscores the need to understand how these dynamics are playing out in the Anthropocene [@fisher2010] 
1. Looking at the relationship between size and abundance dynamics at the **community scale** tells us about important functional dynamics
    1. This is the scale at which ecological processes (i.e. compensatory dynamics, niche tracking, functional replacement) come into play - in contrast to population or global trends [@mcgill2015; @dornelas2014; @white2007].
    1. To the extent that size is a proxy for other functional traits, changes or consistency in the community-level size structure over time may reflect processes related to niche structure [@white2007; @petchey2010]
      1. Scenarios:
          1. Strong size shifts can decouple the relationship between abundance and biomass. 
             1. This is well established in aquatic systems, where changes in the scaling between abundance and biomass often signal ecosystem degradation [@petchey2010; @kerr2001; @warwick1994 and refs therein]
          1. Or, compensatory shifts in the size structure can buffer community function (in terms of biomass or energy use) against changes in abundance [@terry2015; @ernest2009; @white2004] 
         1. Or, consistency in the size structure may maintain the relationship between size- and -abundance based currencies, even as species composition, total abundance, and total biomass/total energy use fluctuate over time. 
              1. Low turnover, or size-structured replacement - either of which could reflect consistency in the niche structure over time [@holling1992]
          1. Random dynamics/drift may also contribute to the dynamics of the size spectrum
             1. Either via neutral population dynamics, or through systematic change on axes orthogonal to size. 
    1. Different subsets of a community may respond in different ways.
       1. Core and transient species have different biology and respond to different cues.
1. It is important to improve our understanding of these dynamics for terrestrial animal communities in particular.
    1. Terrestrial animal communities are relatively unknown in terms of size spectrum work [@white2007]; but see classic bugs, classic birds, [@ernest2005; @thibault2011; @yen2017].
    1. However, they:
        1. Exhibit size structure [@ernest2005; @thibault2011]
        1. Are experiencing serious and potentially size-structured change, with implications at community, ecosystem, and global scales [@young2016; @schmitz2018]  
    1. Case studies have demonstrated that size shifts can decouple N from E for terrestrial animals [@white2004; @yen2017] while others have them moving together [@hernandez2011]
1. Establishing general commonalities regarding these dynamics has been constrained by 1) a lack of macroecological-scale timeseries data on species and size composition for terrestrial animal communities [@white2007; @thibault2011], and 2) appropriate methods for working with ISDs
    1. Data requirements: A consistent sampling protocol; many communities; long temporal extent; size *AND* abundance data (for a particular taxon, often size or abundance - but not both - is the traditional unit of measure); for the community (not population or global) scale
    1. ISD challenges: the ISDs for determinate growers (mammals, birds…) are not simple power laws [@thibault2011; @ernest2005]. Quantifying change is not as straightforward as computing a slope. (Neither is *comparing* change across currencies.)
    1. As a result, we do not have a synthetic understanding of either 1) the extent to which changes in the size structure decouple the dynamics of abundance, biomass, and energy use in these systems, or of 2) the underlying changes in community structure that account for these effects. 
1. Here, we begin to address this gap by exploring how temporal changes in species composition and the size spectrum modulate the relationship between total abundance, energy, and biomass for communities of North American breeding birds. Specifically, 
    1. We used allometric scaling to estimate community size and abundance data for the North American Breeding Bird Survey, and evaluated how changes in total abundance, biomass, and energy use have co-varied from 1988-2018:
        1. How often do these currencies change together vs. have decoupled dynamics?
        1. What are the dominant directions and magnitudes of the overall change and any decoupling between the currencies?
    1. We examined how these changes differ between core species and the whole-community (i.e. including transients). 


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
    1. BBS contains abundances for all species along a route in each year, but does not include measurements of individual body size. We generated body size estimates for individual birds assuming that intraspecific size distributions are normally distributed around a species’ mean body size (following @thibault2011; also recent Myers/Botero/Fristoe paper I believe).
        1. Using records of species’ mean and standard deviation body sizes from @dunning2007, we drew individuals’ body sizes from the appropriate normal distributions.
            1. For species for which there was not a standard deviation recorded in @dunning2007 (n =?), we estimated the standard deviation based on an allometric scaling relationship between mean and standard deviation in body mass (also described in @thibault2011). For species with multiple records in @dunning2007, we used the mean mean and standard deviation body sizes across all records (averaging across sexes, subspecies, and records from different locations). We performed this averaging after estimating any missing standard deviation measurements.
        1. This method does not incorporate intraspecific variation in body size across geographies or over time [@dunning2007; @gardner2011].. However, it makes it possible to conduct macroecological studies of avian size distributions at a spatial and temporal scale that would otherwise be impossible [@thibault2011].
        1. For each individual bird observed, we estimated metabolic rate as (parameters) [@fristoe2015]. 
        
1. Change in total abundance, biomass, and energy use over time





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
