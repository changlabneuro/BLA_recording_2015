# chang_et_al_2015
Models for Chang et al., **Neural mechanisms of social decision-making in the primate amygdala** (2015) (under review)

## Organization

- Data are stored in the `data` folder. `countdata.csv` contains spike data from the units recorded in the study.

- Model outputs are stored in the `outputs` directory. These are read by analysis scripts and are expected to be in this location.

## Dependencies

Analysis code is written in R and the modeling language [Stan](http://mc-stan.org). In addition, we make use of the following R libraries:

- rstan
- ggplot2
- tidyr
- dplyr
- GGally
- grid

## Analysis

- `data_cleaning.R` transforms firing rate data from the experiment to the versions used in the model. It outputs `data/countdata.csv`. Those wishing to reproduce analysis can simply use this file directly.

- `preliminary_analysis.R` performs conventional generalized linear model (GLM) analysis of the spiking data.