# Network Motif Analysis

## Project Description
This project aims to use network motifs (small but recurring patterns in networks) to distinguish biased citation networks from unbiased ones. 

## File and Folder Description
**diesel_lung_cancer**: Motif analysis of the diesel lung cancer citation network, which is a biased citation network (Duyx et al., 2020).
- network_from_data.R: generate the diesel lung cancer citation network object from the raw data.
- network_sim_functions.R: customized functions for network motif analysis, in particular, the function that generates the network null model for statistical testing.
- diesel_lung_cancer_motif_analysis.R: carry out the motif analysis on the diesel lung cancer citation network. 


**hygiene_hypothesis**: Motif analysis of the hygiene hypothesis citation network which is an unbiased citation network (Duyx et al., 2019)
- network_from_data.R: generate the hygiene hypothesis citation network object from the raw data.
- network_sim_functions.R: customized functions for network motif analysis, in particular, the function that generates the network null model for statistical testing.
- diesel_hygiene_hypothesis_analysis.R: carry out the motif analysis on the hygiene hypothesis citation network. 


**presentations**: presentation slides about the project.
- Spring22_motif_analysis.pptx: presentation given in Spring 22 to provide an overview of the project and report preliminary findings.


## Acknowledgment and Credits
**We tested our methods on two datasets. One dataset include a biased citation network (the hygiene hypothesis network), and the other include an unbiased network (the diesel and lung cancer network). Below are the dataset ciations**

Duyx, Bram; Urlings, Miriam; Swaen, Gerard; Bouter, Lex; Zeegers, Maurice, 2017, "Citation analysis of the published literature on the hygiene hypothesis", https://doi.org/10.34894/G0X6CS, DataverseNL, V3 

Duyx, Bram; Urlings, Miriam; Swaen, Gerard; Bouter, Lex; Zeegers, Maurice, 2018, "Citation analysis of the Diesel Exhaust Exposure - Lung Cancer hypothesis", https://doi.org/10.34894/8NTFCX, DataverseNL, V4 

**We used motifr package to compute the numbers of multi-level network motifs in the networks. Below is the software citation.**

Mario Angst and Tim Seppelt (2020). motifr: Motif Analysis in Multi-Level Networks. R package version 1.0.0. https://marioangst.github.io/motifr/

## Bibliography

Duyx B, Urlings MJE, Swaen GMH, et al. Selective citation in the literature on the hygiene hypothesis: a citation analysis on the association between infections and rhinitis. BMJ Open 2019;9:e026518. doi: 10.1136/bmjopen-2018-026518

Duyx B, Urlings MJE, Swaen GMH, et al., Determinants of citation in the literature on diesel exhaust exposure and lung cancer: a citation analysis. BMJ Open 2020;10:e033967. doi: 10.1136/bmjopen-2019-033967

## History
June 20, 2022: Create the repository. Initial commit. 

## Contact
Yuanxi Fu, GitHub: @yuanxiesa
