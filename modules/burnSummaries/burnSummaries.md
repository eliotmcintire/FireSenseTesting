---
title: "burnSummaries Manual"
subtitle: "v.1.0.0"
date: "Last updated: 2025-04-07"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    toc_depth: 4
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
  markdown: 
    wrap: 80
bibliography: citations/references_burnSummaries.bib
citation-style: citations/ecology-letters.csl
link-citations: true
always_allow_html: true
---

# burnSummaries Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:burnSummaries) *burnSummaries*

(ref:percent) %





[![module-version-Badge](/Users/achubaty/Documents/GitHub/BC_HRV/modules/burnSummaries/figures/moduleVersionBadge.png)](ssh://git@github.com/PredictiveEcology/burnSummariesd0617cf79ffa92b19bf10118da9c5a864daf3638)

[![Issues-badge](/Users/achubaty/Documents/GitHub/BC_HRV/modules/burnSummaries/figures/issuesBadge.png)](https://github.com/PredictiveEcology/burnSummaries/issues)

#### Authors:

Alex M Chubaty <achubaty@for-cast.ca> [aut, cre] (<https://orcid.org/0000-0001-7146-8135>)
<!-- ideally separate authors with new lines, '\n' not working -->

:::{.rmdwarning}
This documentation is work in progress.
Please report any discrepancies or omissions at <https://github.com/PredictiveEcology/BC_HRV_preamble/issues>.
:::

## Module Overview

### Module summary

(TODO)

- LandMine [@Andison:1996] - based on [@Clarke:1994];
- scfm [@Cumming:1998; @Armstrong:2003];
- fireSense [@Marchal:2017a; @Marchal:2017b; @Marchal:2019]

### Module inputs and parameters

Table \@ref(tab:moduleInputs-burnSummaries) shows the full list of module inputs.

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-burnSummaries)(\#tab:moduleInputs-burnSummaries)List of (ref:burnSummaries) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> speciesLayers </td>
   <td style="text-align:left;"> SpatRaster </td>
   <td style="text-align:left;"> initial percent cover raster layers used for simulation. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Provide a summary of user-visible parameters (Table \@ref(tab:moduleParams-burnSummaries))


<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-burnSummaries)(\#tab:moduleParams-burnSummaries)List of (ref:burnSummaries) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> reps </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> 1, 2, 3,.... </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> number of replicates/runs per study area. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> simOutPrefix </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> mySimOut </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> saved simList file prefix </td>
  </tr>
  <tr>
   <td style="text-align:left;"> simOutputPath </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> /var/fol.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Directory specifying the location of the simulation outputs. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> simTimes </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA, NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Simulation start and end times when running in 'multi' mode. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> upload </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> if TRUE, uses the `googledrive` package to upload figures. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> uploadTo </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> if `upload = TRUE`, a Google Drive folder id corresponding to `.studyAreaName`. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plots </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> screen </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Used by Plots function, which can be optionally used here </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first plot event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .plotInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time interval between plot events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Describes the simulation time at which the first save event should occur. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This describes the simulation time interval between save events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .studyAreaName </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Human-readable name for the study area used - e.g., a hash of the studyarea obtained using `reproducible::studyAreaName()` </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .seed </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Named list of seeds to use for each event (names). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should caching of events or module be used? </td>
  </tr>
</tbody>
</table>

### Events

Describe what happens for each event type.

### Plotting

Write what is plotted.

### Saving

Write what is saved.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-burnSummaries)).

<table class="table" style="color: black; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-burnSummaries)(\#tab:moduleOutputs-burnSummaries)List of (ref:burnSummaries) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> fireSizes </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> summary fire sizes table </td>
  </tr>
</tbody>
</table>

### Links to other modules

Describe any anticipated linkages to other modules, such as modules that supply input data or do post-hoc analysis.

### Getting help

- <https://github.com/PredictiveEcology/burnSummaries/issues>

### References

<!-- autogenerated -->
