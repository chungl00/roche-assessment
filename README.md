# roche-assessment

This repository includes all deliverables specified in the ADS Programmer Coding Assessment including the following:
1) Question 1: SDTM DS Domain Creation using {sdtm.oak}
2) Question 2: ADaM ADSL Dataset Creation
3) Question 3: TLG - Adverse Events Reporting

Due to time constraints, I did not attempt Question 4. However, my general impression is that the LLM capabilities can be very useful for helping structured clinical data become more accessible for clinical professionals.

The folder "question_1" contains the following files:
- question_1.R: R Script for creating the SDTM Disposition (DS) domain dataset from raw clinical trial data using the {sdtm.oak}
- metadata/sdtm_ct: study_ct file containing controlled terminology
- ds.csv: Resulting SDTM dataset in CSV format
- run_log.txt: Text file as evidence for code running error-free

The folder "question_2_adam" contains the following files:
- create_adsl.R: R Script for creating the ADSL dataset using SDTM source data and deriving the specified variables
- adsl.csv: Resulting ADSL dataset in CSV format
- run_log: Text file as evidence for code running error-free

The folder "question_3_tlg" contains the following files:
- 01_create_ae_summary_table.R: R Script for creating the AE summary table using the ADAE dataset
- ae_summary_table: HTML file output of AE summary table
- run_log_ae_summary_table.txt: Text file as evidence for code running error-free
- 02_create_visualizations.R: R Script for creating the 2 visualizations (stacked bar chart and scatter plot) corresponding to the ADAE dataset
- plot1.png: PNG file output of AE severity distribution by treatment (Plot 1)
- plot2.png: PNG file output of Top 10 most frequent AEs (with 95%CI for incidence rates) (Plot 2)
- run_log_visualizations.txt: Text file as evidence for code running error-free
