A cross-cultural EEG study of how obedience and conformity influence reconciliation intentions.

This repository contains the data, experiment, and analysis scripts for this manuscript.

Contents

Analysis/*: Scripts to preprocess and extract the EEG data using Python. Script to extract behavioral data using R. Scripts to model, plot, and make inference using R.
	prep_beh_individual_cambodia_SIV2.r : code to extract, preprocess and aggregate behavioral data of the individuals
	prep_beh_group_cambodia_SIV2.r : code to combine the EEG and Behavioral data 
	eeg_prep_cambodia_SIV2.ipynb : code to preprocess the EEG data
	eeg_post_cambodia_SIV2.ipynb :  code to process and extract EEG data from epochs in the time-frequency domain. 


Task/*: Script and materials to run the experiment
	task_influence.py : script to run the tasks
	materials/* : all the materials to run the experiment

Requirements

Python, R, Jasp, various packages.