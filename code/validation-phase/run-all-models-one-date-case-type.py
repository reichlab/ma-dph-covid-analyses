import sys
sys.path.insert(0, '.')

import argparse

import os
from pathlib import Path

import numpy as np
import pandas as pd

from datetime import date

import covidcast
import itertools

from sarix import sarix


def expand_grid(data_dict):
    """Create a dataframe from every combination of given values."""
    rows = itertools.product(*data_dict.values())
    return pd.DataFrame.from_records(rows, columns=data_dict.keys())


def load_data(as_of = None, end_day = "2021-07-01", case_type = 'report', case_timing = 'final'):
    """
    Load data for MA cases and hosps from covidcast
    Parameters
    ----------
    as_of: string of date in YYYY-MM-DD format. 
        Default to None.
    end_day: string of date in YYYY-MM-DD format. 
        Default to "2021-07-01"
    Returns
    -------
    df: data frame
        It has columns location, date, inc_hosp, population and rate. 
        It is sorted by location and date columns in ascending order.
    """
    # load hospitalizations
    hosp_df = covidcast.signal(data_source="hhs",
                               signal="confirmed_admissions_covid_1d",
                               start_day=date.fromisoformat("2020-10-01"),
                               end_day=date.fromisoformat(end_day),
                               geo_type="state",
                               geo_values="ma",
                               as_of=date.fromisoformat(as_of))
    hosp_df = hosp_df[["geo_value", "time_value", "value"]]
    hosp_df.columns = ["location", "date", "hosp"]
    
    # load cases
    if case_type == 'report':
        if case_timing == 'final':
            case_as_of = '2022-03-20'
        else:
            case_as_of = as_of
        
        case_df = covidcast.signal(data_source="jhu-csse",
                                   signal="confirmed_incidence_num",
                                   start_day=date.fromisoformat("2020-10-01"),
                                   end_day=date.fromisoformat(end_day),
                                   geo_type="state",
                                   geo_values="ma",
                                   as_of=date.fromisoformat(case_as_of))
        case_df = case_df[["geo_value", "time_value", "value"]]
        case_df.columns = ["location", "date", "case"]
        
        # quick fix to zero values; mean of adjacent
        # there's probably a better way
        zero_inds = np.where(case_df.case == 0)
        prev_values = case_df['case'].iloc[zero_inds[0] - 1].values
        next_values = case_df['case'].iloc[zero_inds[0] + 1].values
        case_df['case'].iloc[zero_inds] = 0.5 * (prev_values + next_values)
    else:
        raise ValueError("case_type other than 'report' not supported yet")
    
    # merge
    df = case_df.merge(hosp_df, on=["location", "date"], how = "left")
    
    # drop missing values; assumed to be trailing (dangerous?)
    df = df.dropna()
    
    return df


if __name__ == "__main__":
    # parse arguments
    parser = argparse.ArgumentParser(description="hierarchicalGP")
    parser.add_argument("--forecast_date", nargs="?", default='2020-12-07', type = str)
    parser.add_argument("--case_type", nargs="?", default='report', type=str)
    parser.add_argument("--case_timing", nargs="?", default='final', type=str)
    args = parser.parse_args()
    forecast_date = args.forecast_date
    case_type = args.case_type
    case_timing = args.case_timing

    # load data
    # should end_day be forecast_date - 1?
    data = load_data(as_of=forecast_date, end_day=forecast_date,
                     case_type=case_type, case_timing=case_timing)

    # figure out horizons
    # last date with observed data
    last_obs_date = pd.to_datetime(data.iloc[-1].date)
    # how far out to forecast to get to 28 days after due date
    due_date = pd.to_datetime(forecast_date)
    extra_horizons_rel_obs = (due_date - last_obs_date).days
    effective_horizon_rel_obs = 28 + extra_horizons_rel_obs
    # how many forecasts to keep relative to forecast_date
    extra_horizons_rel_forecast_date = (due_date - pd.to_datetime(forecast_date)).days
    effective_horizon_rel_forecast_date = int(28 + extra_horizons_rel_forecast_date)
    print('effective_horizon_rel_forecast_date')
    print(effective_horizon_rel_forecast_date)

    # fit models
    sari_variations =  expand_grid({
        'p': [p for p in range(3)],
        'P': [P for P in range(3)],
        'd': [d for d in range(1)], # update later
        'D': [D for D in range(1)] # update later
    })
    # drop first row, all zeros is not an interesting model
    sari_variations = sari_variations.iloc[1:, ]

    for i in range(sari_variations.shape[0]):
        p = sari_variations.p.values[i]
        P = sari_variations.P.values[i]
        d = sari_variations.d.values[i] # ignored
        D = sari_variations.D.values[i] # ignored

        sarix_fit = sarix.SARIX(
            xy = data[["case", "hosp"]].values,
            p = p,
            P = P,
            season_period = 7,
            transform = "quarter",
            forecast_horizon = effective_horizon_rel_obs,
            num_warmup = 1000,
            num_samples = 1000,
            num_chains = 1)

        # extract predictive quantiles for response variable
        q_levels = np.array([0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35,
                             0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80,
                             0.85, 0.90, 0.95, 0.975, 0.99])
        # q_levels = np.array([0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975])
        pred_samples = sarix_fit.predictions_orig[:, extra_horizons_rel_obs:, 1]
        pred_qs = np.percentile(pred_samples, q_levels * 100.0, axis = 0)

        # format predictions as a data frame with required columns
        horizons_str = [str(i + 1) for i in range(28)]
        preds_df = pd.DataFrame(pred_qs, columns = horizons_str)
        preds_df['forecast_date'] = forecast_date
        preds_df['location'] = '25'
        preds_df['quantile'] = q_levels
        preds_df = pd.melt(preds_df,
                            id_vars=['forecast_date', 'location', 'quantile'],
                            var_name='horizon')
        preds_df['target_end_date'] = pd.to_datetime(preds_df['forecast_date']).values + \
            pd.to_timedelta(preds_df['horizon'].astype(int), 'days')
        preds_df['base_target'] = ' day ahead inc hosp'
        preds_df['target'] = preds_df['horizon'] + preds_df['base_target']
        preds_df['type'] = 'quantile'
        preds_df = preds_df[['location', 'forecast_date', 'target', 'target_end_date', 'type', 'quantile', 'value']]

        # save predictions
        model_name = f"{case_type}_" + \
          f"{case_timing}_" +\
          f"p_{p}_" +\
          f"d_{d}_" +\
          f"P_{P}_" +\
          f"D_{D}"
        model_dir = Path("validation_forecasts") / model_name
        model_dir.mkdir(mode=0o775, parents=True, exist_ok=True)
        file_path = model_dir / f"{forecast_date}-{model_name}.csv"
        preds_df.to_csv(file_path, index = False)

