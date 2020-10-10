# Welcome

This repository contains the analytical pipeline for the paper: "Intonational processing is incremental and holistic" by Timo B. Roettger, Dan Turner, & Jennifer Cole. Additional Materials and a preprint can be found here: https://osf.io/2wecs/

## Contents

### `data` contains 
- `acoustic_landmarks.csv` extracted landmarks from the acoustic signal (see manuscript for details)
- `/pilot/` containing raw data for five participants ran as a pilot

### `models` contains model output generated from `model_for_gaze.R`
- `Bayesian_models.RData` contains model objects.
- `posteriors.Rdata` contains extracted posteriors

### `plots` contains derived plots ()
- `TimePlot.pdf` R plot which was manually manipulated in Affinity Designer to produce
- `TimePlot_edited.afdesign` and `TimePlot_edited.png` corresponding to Fig. 4 in manuscript.
- `Fix_agg_simple.pdf` corresponds to Fig. 5 in manuscript

### `processed` contains derived data frames from the raw data
- `ret_processed.R` derived by `ret.preprocess.R`.
- `ret_processed_stage_2.R` derived by `ret.process.R`.

### `raw_beh` contains raw `.beh` files

### `raw_edf` contains raw `.edf` files

### `raw_OSfiles` contains raw OpenSesame files in `.csv`

### `scripts` contains R scripts
- `ret.preprocess.R`: This file imports eyetracking data (.edf from `raw_edf/`) and the experimental file from OpenSesame (.csv from `raw_OSfiles/`), merges them, preprocesses the data by specifying regions of interest (ROI) and counting hits in them. Stores `ret_processed.R` into `processed/`.
 - `ret.process.R`: This file imports `ret_processed.csv` and enriches the data by calculating fixation durations, proportions of duration per ROI, ROI image and role (i.e. target), and adds other columns to facilitate analysis. Stores `ret_processed_stage_2.R` into `processed/`.
 - `model_for_gaze.R`: This file runs the statistical analysis according to preregistered protocol, extracts relevant posteriors and store the results in `models/`.
 - `plot_for_gaze.R`: plots `Fix_agg_simple.pdf` corresponding to Figure 5 in manuscript.
 - `makeTimePlot.R`: plots `TimePlot.pdf`, serving as the departure point for Figure 4 in manuscript.
 
 ### `mousetracking scripts` contains R scripts dedicated for an exploratory analysis of mouse movements
 - `preprocess_for_MT.R` preprocesses the experimental file from OpenSesame (.csv from `raw_OSfiles/`) to further analyse mouse movements.
 - `plot_for_MT.R` plots mousetracking trajectories across conditions.
 
 
