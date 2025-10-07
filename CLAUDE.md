# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a personal R package for analyzing 10+ years of running data, with accumulated technical debt from various experimental approaches. The repo contains ODE-based heart rate modeling using optimization techniques.

**Critical**: Dependencies between files are complex and not always obvious. Always trace what depends on what before making changes.

## Core Architecture

### Heart Rate Modeling System

The core workflow uses differential equations to model heart rate responses:

1. **`model.R`**: ODE system defining heart rate dynamics based on speed/equilibrium HR
   - Uses autoregressive (AR) and moving average (MA) parameters
   - Takes time-series HR equilibrium values as input

2. **`custom_curve.R`**: Generates custom spline curves for HR equilibrium prediction
   - Uses I-splines with custom coefficients
   - Maps speed to equilibrium heart rate

3. **`return_the_predictions.R`**: Main prediction pipeline
   - Converts parameter vector `x` into model parameters
   - Interpolates missing time points (adds every second)
   - Calls `run_bare_simulation()` with constructed data

4. **`run_bare_simulation.R`**: Wraps `deSolve::ode()` to solve the ODE system
   - Uses Adams method for integration
   - Returns predicted HR time series

5. **`objective.R`**: Optimization objective function
   - Calculates sum of squared errors between predicted and actual HR
   - Writes training history to parent environment via `<<-`

### Optimization Workflow

**`inst/calculate_n_instance.r`**: Main parallel optimization script
- Sources all R files and loads data via `inst/make_data_all.R`
- Uses `nloptr` with MLSL global optimizer + BOBYQA local optimizer
- Runs parallel optimization across running sessions using `doParallel`
- Saves individual session results to `inst/training history/`

**`inst/compile_list_results_v2.r`**: Post-processing
- Reads all training history RDS files
- Extracts best parameters and deviance for each session
- Compiles into summary dataframes

### Data Pipeline

**`inst/make_data_all.R`**: Data preparation
- Loads `data/d_as_in_data.rdata` (raw session data)
- Filters to actual running sessions
- Creates time-scaled features
- Sets `favorite_session` variable

### Alternative Approaches in `inst/`

The `inst/` directory contains various experimental scripts:
- **LASSO variants**: `lasso*.r` files for elastic net regression approaches
- **Resampling**: `fun_resampling*.r` for bootstrap/resampling methods
- **Exploration**: `xplore*.r` for analyzing training history and results
- **Visualization**: `*_visual.r` for plotting and diagnostics

Many of these are standalone experiments and may not integrate with the main workflow.

## Data Structure

- **`data/d_as_in_data.rdata`**: Main dataset (object name: `e`)
  - Contains: `hr`, `hr_lead_18`, `speed`, `dist.90`, `Time`, `start_time_fac`, etc.
  - `start_time_fac`: Session identifier (Unix timestamp as factor)

- **`inst/training history/`**: Individual optimization results
  - Format: `{session_id}_{iterations}.rds`
  - Contains: optimization object, training_history matrix

## Running Optimizations

To run optimization for multiple sessions:
```r
source(here::here("inst", "calculate_n_instance.r"))
```

To compile results:
```r
source(here::here("inst", "compile_list_results_v2.r"))
```

## Key Dependencies

- **deSolve**: ODE solving
- **nloptr**: Nonlinear optimization
- **doParallel**: Parallel processing
- **dplyr**: Data manipulation
- **splines2**: Spline basis functions (used in `custom_curve.R`)

## Important Notes

- The optimization uses `<<-` to write `training_history` to parent environment (objective.R:26-30)
- Time values must be converted to numeric (from `hms` type) before ODE solving
- Missing time points are interpolated via `approxfun()` with `rule = 2`
- Parameter vector `x` is scaled (divided by 100) in `return_the_predictions.R`
- The package is named "MartysCookbook" but the repo is "Martyruns" - these refer to the same project
