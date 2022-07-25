---
title: "Script to clean and tidy data generated for course final activity. Course Name: Getting and Cleaning Data"
version: 1.0
autor: Pedro Osma
---

Introduction
============

This script files uses the data generated by the "Human Activity Recognition Using Smartphones Dataset" version 1.0 by "Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto." Smartlab - Non Linear Complex Systems Laboratory

The provided script reads the data generated by the dataset and generates two new data sets:
- The first data set, named measurements_df, contains all the measurements generated by the subjects of the experiment, algong the subject identified and a human readeable activity description.
- The second data set, named measurement_gr, summaries the previous dataset computing the average for different variables grouped by subject and activity.

Explanation of the script:
=========================

This script reads the data generated by the HAR "Human Activity Recognition" Dataset, both the test and the train data, and merges them in a new data set. The script reads the activities used by the HAR, the names of the variables being measured, and the subjects and activities being measured.

With that data, it generated a more human readable data set which contains, in a single data frame, al the information being extracted from the HDR data set.

Two data sets are generated, the first one contains the bulk information being measured and the second one contains a summary of the information being measured.

