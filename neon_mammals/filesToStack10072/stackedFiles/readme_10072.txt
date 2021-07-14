###################################
########### Disclaimer ############
This is the most recent readme publication based on all site-date combinations used during stackByTable.
Information specific to the query, including sites and dates, has been removed. The remaining content reflects general metadata for the data product.
All files used during stacking are listed at the bottom of this document, which includes the data publication dates.
##################################

This data package been produced by and downloaded from the National Ecological Observatory Network (NEON). NEON is funded by the National Science Foundation (Awards 0653461, 0752017, 1029808, 1138160, 1246537, 1638695, 1638696, 1724433) and managed cooperatively by Battelle. These data are provided under the terms of the NEON data policy at https://www.neonscience.org/data-policy.

DATA PRODUCT INFORMATION
------------------------

ID: NEON.DOM.SITE.DP1.10072.001

Name: Small mammal box trapping

Description: Individual- and trap-level data collected using box traps designed to capture small mammals

NEON Science Team Supplier: Terrestrial Observation System

Abstract: This data product contains the quality-controlled, native sampling resolution data from NEON's small mammal sampling protocol. Small mammal abundance and diversity are sampled at regular intervals by NEON field technicians at core and relocatable sites. Here small mammals are defined based on a combination of behavioral, dietary, and size constraints, as the NEON design is limited to species sampled by box traps. This definition includes any mammal that is (1) nonvolant; (2) nocturnally active; (3) forages predominantly aboveground; and (4) is greater than 5 grams but less than approximately 500-600 g. In North America, this includes cricetids, heteromyids, small sciurids, and introduced murids. It does not include shrews, large squirrels, rabbits, or weasels, despite the fact that individuals of these species may be incidentally captured. Products resulting from this sampling include the species identification and unique identifier for each individual captured, as well as a suite of standard size measurements and reproductive condition data. Sample identifiers for any blood, ear, hair, whisker, fecal, and/or voucher samples collected are also provided. For additional details, see the user guide, protocols, and science design listed in the Documentation section in this data product's details webpage. For spatial data (text and shapefiles), download [`NEON_TOS_Plots`](https://data.neonscience.org/api/v0/documents/All_NEON_TOS_Plots_V7).

Latency:
The expected time from data and/or sample collection in the field to data publication is as follows, for each of the data tables (in days) in the downloaded data package. See the Data Product User Guide for more information.

mam_perplotnight:  45

mam_pertrapnight:  45

mam_voucher:  45

Brief Design Description: Small mammal sampling is based on the lunar calendar, with timing of sampling constrained to occur within 10 days before or after the new moon. Typically, core sites are sampled 6 times per year, and relocatable sites 4 times per year. Small mammals are sampled using box traps (models LFA, XLK, H.B. Sherman Traps, Inc., Tallahassee, FL, USA). Box traps are arrayed in three to eight (depending on the size of the site) 10 x 10 grids with 10m spacing between traps at all sites. Small mammal trapping bouts are comprised of one or three nights of trapping, depending on whether a grid is designated for pathogen sample collection (3 nights) or not (1 night).

Brief Study Area Description: These data are collected at all NEON terrestrial sites, except the sites in Hawaii and Puerto Rico.

Keywords: Chordata, diversity, rodents, vector-borne, demography, Rodentia, community composition, cricetids, Mammalia, mark-recapture, traps, archived samples, mouse, sciurids, density, animals, Muridae, DNA barcoding, population, archive, mammals, Dipodidae, species abundance, mice, species diversity, species composition, small mammals, heteromyids, murids, DNA sequences, voles, vertebrates, taxonomy, pathogens, vectors, material samples, specimens, Animalia, vouchers, rats, Cricetidae


DATA PACKAGE CONTENTS
---------------------

This data product contains up to 3 data tables:

mam_voucher - Voucher specimens
mam_pertrapnight - Small mammal trapping data per trap per night
mam_perplotnight - Small mammal trapping summary data per plot per night
If data are unavailable for the particular sites and dates queried, some tables may be absent.
Basic download package definition: The basic data package includes all measurements, except for voucher specimens collected off of the long-term trapping grids.

Expanded download package definition: The expanded package includes an additional table listing any voucher specimens collected off of the long-term trapping grids.

FILE NAMING CONVENTIONS
-----------------------

NEON data files are named using a series of component abbreviations separated by periods. File naming conventions for NEON data files differ between NEON science teams. A file will have the same name whether it is accessed via NEON's data portal or API. Please visit https://www.neonscience.org/data-formats-conventions for a full description of the naming conventions.

ISSUE LOG
---------

This log provides a list of issues that were identified during data collection or processing, prior to publication of this data package. For a more recent log, please visit this data product's detail page at https://data.neonscience.org/data-products/DP1.10072.001.

Issue Date: 2021-01-06
Issue: Safety measures to protect personnel during the COVID-19 pandemic resulted in reduced or eliminated sampling activities for extended periods at NEON sites. Data availability may be reduced during this time.
       Date Range: 2020-03-23 to 2021-06-01
       Location(s) Affected: All
Resolution Date: 
Resolution: 

Issue Date: 2020-11-01
Issue: Small mammal communities in Puerto Rico are exclusively invasive species of Rattus and Mus. The larger Tomahawk traps required to capture the Rattus spp. risk capturing invasive mongoose (Herpestes auropunctatus). The site host would not permit release of this invasive species once captured, and the IACUC would not permit euthanasia by NEON staff due to risks to staff safety.
       Date Range: 2016-03-01 to 2017-05-01
       Location(s) Affected: GUAN, LAJA
Resolution Date: 2020-10-14
Resolution: Small mammal trapping activities in Puerto Rico have been suspended to prevent unintended capture of invasive mongoose (Herpestes auropunctatus).

Issue Date: 2020-11-01
Issue: Bouts of missed sampling are not being recorded in the data.
       Date Range: 2013-01-01 to 2020-02-11
       Location(s) Affected: All
Resolution Date: 2020-02-11
Resolution: In 2020, NEON added the quality flag "samplingImpractical" to the small mammal `per_plotnight` table to assist users in determining the cause of missing samples.

Issue Date: 2020-11-01
Issue: Small mammal hair sample weight is below the 5 mg minimum required for stable isotope analyses.
       Date Range: 2013-01-01 to 2019-03-18
       Location(s) Affected: All
Resolution Date: 2019-03-18
Resolution: The protocol was updated to require collection of hair samples weighing 5-8 mg instead of < 1 mg.

Issue Date: 2020-11-01
Issue: Ticks are not being recorded on small mammals.
       Date Range: 2013-01-01 to 2016-01-29
       Location(s) Affected: All
Resolution Date: 2019-01-29
Resolution: Tick monitoring (presence/absence by lifestage) was added to the data product for the 2016 field season.  Beginning in the 2020 field season, the binned number of total ticks attached to a small mammal was also added to the data product with groupings that include: 1-5, 6-20, >20.

Issue Date: 2017-08-08
Issue: As part of NEON's ongoing construction tasks, the processing pipeline for most data products for NEON's observational systems (terrestrial [TOS] and aquatic [AOS]) has been simplified to use a generic codebase.
       Date Range: 2017-08-08 to 2017-08-08
       Location(s) Affected: All
Resolution Date: 2017-08-08
Resolution: Data that were published previously have been reprocessed and republished using the generic code -- replacing data that had been processed using the original algorithms described in Algorithm Theoretical Basis Documents (ATBDs). Consequently, data are in a new format including, in many cases, different data fields. Details of the new processing pipeline can be found in http://data.neonscience.org/api/v0/documents/OS_generic_transitions_vA  and http://data.neonscience.org/api/v0/documents/Nicl_Language_DRAFT.

Issue Date: 2020-11-01
Issue: On days when large numbers of small mammals are captured, collecting and processing samples from all captures requires too much time.
       Date Range: 2013-01-01 to 2015-03-23
       Location(s) Affected: All
Resolution Date: 2015-03-23
Resolution: Only ten samples of a given type (e.g., hair, fecal, ear) are collected from priority species when capture rates are high.

Issue Date: 2020-11-01
Issue: Trapping small mammals for three nights on every grid is highly resource-intensive.
       Date Range: 2013-01-01 to 2015-03-23
       Location(s) Affected: All
Resolution Date: 2015-03-23
Resolution: In consultation with the Technical Working Group, sample bout duration was decreased from three nights to one at diversity grids. By maintaining three sampling nights on the remaining three pathogen grids at each site, this design change balanced the sampling of a diversity of land cover classes with the recapture data required for density estimation.

ADDITIONAL INFORMATION
----------------------

Queries for this data product will return data collected during the date range specified. Per trapping grid data are provided in the table, `mam_perplotnight`, with associated per trap per night data provided in the `mam_pertrapnight` table. The tables can be joined by the nightuid field. Duplicates may exist where protocol and/or data entry aberrations have occurred; users should check data carefully for anomalies before joining tables. Taxonomic IDs of species of concern have been 'fuzzed'; see data package readme files for more information.

Protection of species of concern: At most sites, taxonomic IDs of species of concern have been 'fuzzed', i.e., reported at a higher taxonomic rank than the raw data, to avoid publishing locations of sensitive species. For a few sites with stricter regulations (e.g., Great Smoky Mountains National Park (GRSM)), records for species of concern are not published. 

NEON DATA POLICY AND CITATION GUIDELINES
----------------------------------------

A citation statement is available in this data product's detail page at https://data.neonscience.org/data-products/DP1.10072.001. Please visit https://www.neonscience.org/data-policy for more information about NEON's data policy and citation guidelines.

DATA QUALITY AND VERSIONING
---------------------------

NEON data are initially published with a status of Provisional, in which updates to data and/or processing algorithms will occur on an as-needed basis, and query reproducibility cannot be guaranteed. Once data are published as part of a Data Release, they are no longer provisional, and are associated with a stable DOI.

To learn more about provisional versus released data, please visit https://www.neonscience.org/data-revisions-releases.

POST STACKING README DOCUMENTATION
----------------------------------

Each row contains the readme filename used during stackByTable

NEON.D01.BART.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D01.HARV.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D02.BLAN.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D02.SCBI.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D02.SERC.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D03.DSNY.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D03.JERC.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D03.OSBS.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D04.GUAN.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D04.LAJA.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D05.STEI.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D05.TREE.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D05.UNDE.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D06.KONA.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D06.KONZ.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D06.UKFS.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D07.GRSM.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D07.MLBS.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D07.ORNL.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D08.DELA.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D08.LENO.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D08.TALL.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D09.DCFS.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D09.NOGP.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D09.WOOD.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D10.CPER.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D10.RMNP.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D10.STER.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D11.CLBJ.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D11.OAES.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D12.YELL.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D13.MOAB.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D13.NIWO.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D14.JORN.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D14.SRER.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D15.ONAQ.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D16.ABBY.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D16.WREF.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D17.SJER.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D17.SOAP.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D17.TEAK.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D18.BARR.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D18.TOOL.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D19.BONA.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D19.DEJU.DP1.10072.001.readme.20210123T023002Z.txt
NEON.D19.HEAL.DP1.10072.001.readme.20210123T023002Z.txt
