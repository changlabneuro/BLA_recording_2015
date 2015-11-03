# chang_et_al_2015
Models for Chang et al., **Neural mechanisms of social decision-making in the primate amygdala** (2015) (under review)

## Organization

- Data are stored in the `data` folder. `countdata.csv` contains spike data from the units recorded in the study.

- Model outputs are stored in the `outputs` directory. These are read by analysis scripts and are expected to be in this location.

- Models themselves are defined by `.stan` files in the `models` directory.

## Dependencies

Analysis code is written in R and the modeling language [Stan](http://mc-stan.org). In addition, we make use of the following R libraries:

- rstan
- ggplot2
- tidyr
- dplyr
- GGally
- grid
- glmnet

## Analysis

- `data_cleaning.R` transforms firing rate data from the experiment to the versions used in the model. It outputs `data/countdata.csv`. Those wishing to reproduce analysis can simply use this file directly.

- `preliminary_analysis.R` performs conventional generalized linear model (GLM) analysis of the spiking data.

- `runmodel.R` runs inference (using Stan) for the multivariate t-distribution model (models/multi_t.stan). This script can easily be edited to run other models (listed as `model1` - `model7`) corresponding to different assumptions. Model outputs are saved to disk in `outputs` as compressed R binaries.

- `model_analysis.R` loads model sampling outputs and generates plots.

- `helpers.R` contains useful plotting functions used in constructing supplementary figures.

- `predict_choices.R` uses a cross-validated model-fitting approach to predict prosocial choices from single-unit firing rates.

- `modeling_supplement.Rmd` is an R markdown file that generates plots and text describing the model and results.