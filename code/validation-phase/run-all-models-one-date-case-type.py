import sys
sys.path.insert(0, '.')

import argparse

import os
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import norm

from datetime import date

import covidcast
import itertools

from sarix import sarix
from statsmodels.tsa.api import VAR


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
        
        # quick fix to zero values; replace with nan and then interpolate missing
        zero_inds = np.where(case_df.case == 0)
        case_df['case'].iloc[zero_inds] = np.nan
        case_df.interpolate(inplace=True)
    else:
        csv_files = os.listdir('csv-data')
        as_ofs = [f[21:-4] for f in csv_files]
        if case_timing == 'final':
            case_as_of = max(as_ofs)
        else:
            subset_as_ofs = [ao for ao in as_ofs if ao <= as_of]
            case_as_of = max(subset_as_ofs)
        
        case_df = pd.read_csv('csv-data/MA-DPH-csvdata-covid-' + case_as_of + '.csv')
        case_df['location'] = 'ma'
        case_df = case_df[['location', 'test_date', 'new_positive']]
        case_df.columns = ['location', 'date', 'case']
        
        case_df = case_df[(case_df.date >= '2020-10-01') & (case_df.date <= end_day)]
        case_df.date = pd.to_datetime(case_df.date)
    
    # merge
    df = case_df.merge(hosp_df, on=["location", "date"], how = "left")
    
    # drop missing values; assumed to be trailing (dangerous?)
    df = df.dropna()
    
    return df


def diff_df(df, vars, d, D, season_period):
    df = df.copy()
    for var in vars:
        for d_val in range(d):
            df[var] = df[var].diff()
        
        for D_varl in range(D):
            df[var] = df[var].diff(season_period)
    
    return df


def inv_diff_df(df_orig, df_diffed, vars, d, D, season_period):
    df_orig = df_orig.copy()
    df_diffed = df_diffed.copy()
    for d_val in range(1, d + 1):
        df_dm1 = diff_df(df_orig, vars, d - d_val, D, season_period)
        for var in vars:
            df_diffed[var] = df_dm1[var].shift(1) + df_diffed[var]
    
    for D_val in range(1, D + 1):
        df_Dm1 = diff_df(df_orig, vars, 0, D - D_val, season_period)
        for var in vars:
            df_diffed[var] = df_Dm1[var].shift(season_period) + df_diffed[var]
    
    return df_diffed


def inv_diff_vec(dy, y, d, D, season_period):
    '''
    Invert first-order and seasonal differencing (go from seasonally differenced
    time series to original time series).

    Inputs
    ------
    dy a first-order and/or seasonally differenced univariate time series
      with values like y_{t} - y_{t - ts_frequency}
    y a univariate time series or numeric vector with values like
      y_{t - ts_frequency}.
    d order of first differencing
    D order of seasonal differencing
    frequency frequency of time series.  Must be provided if y is not
      of class "ts" and D > 0.  See the help for stats::ts for more.

    Details
    -------
    y may have longer length than dy.  It is assumed that dy "starts"
      one time index after y "ends": that is, if y is of length T, d = 0, and
      D = 1 then dy[1] = y[T + 1] - y[T + 1 - ts_frequency]

    Returns
    -------
    a vector of the same length as dy with reconstructed values
    '''
    for i in range(1, d + 1):
        y_dm1 = diff_df(df=pd.DataFrame({'temp': y}),
                         vars=['temp'],
                         d=d-i, D=D, season_period=season_period)
        dy_full = np.concatenate([y_dm1.temp.values, dy], axis=0)
        for t in range(len(dy)):
            dy_full[len(y_dm1) + t] = dy_full[len(y_dm1) + t - 1] + dy_full[len(y_dm1) + t]
        
        dy = dy_full[-len(dy):]
    
    for i in range(1, D + 1):
        y_dm1 = diff_df(df=pd.DataFrame({'temp': y}),
                         vars=['temp'],
                         d=0, D=D-i, season_period=season_period)
        dy_full = np.concatenate([y_dm1.temp.values, dy], axis=0)
        for t in range(len(dy)):
            dy_full[len(y_dm1) + t] = dy_full[len(y_dm1) + t - season_period] + dy_full[len(y_dm1) + t]
        
        dy = dy_full[-len(dy):]
    
    return dy


def inv_diff_pred_q(df_orig, pred_q_diffed, var, d, D, season_period):
    horizon = pred_q_diffed.shape[1]
    # df_orig = pd.concat([df_orig[[var]], pd.DataFrame({var: np.full((horizon,), np.nan)})], axis=0)
    # df_orig = pd.concat([df_orig[[var]], pd.DataFrame({var: np.full((horizon,), 0.0)})], axis=0)
    for q_ind in range(pred_q_diffed.shape[0]):
        pred_q_diffed[q_ind, :] = inv_diff_vec(dy=pred_q_diffed[q_ind, :],
                                               y=df_orig[var].values,
                                               d=d, D=D,
                                               season_period=season_period)
    
    return pred_q_diffed


def save_forecast_file(location, forecast_date, pred_qs, q_levels, model_name):
    # format predictions as a data frame with required columns
    horizons_str = [str(i + 1) for i in range(28)]
    preds_df = pd.DataFrame(pred_qs, columns = horizons_str)
    preds_df['forecast_date'] = forecast_date
    preds_df['location'] = location
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
    model_dir = Path("validation_forecasts") / model_name
    model_dir.mkdir(mode=0o775, parents=True, exist_ok=True)
    file_path = model_dir / f"{forecast_date}-{model_name}.csv"
    preds_df.to_csv(file_path, index = False)


if __name__ == "__main__":
    # parse arguments
    parser = argparse.ArgumentParser(description="hierarchicalGP")
    parser.add_argument("--forecast_date", nargs="?", default='2020-12-07', type = str)
    parser.add_argument("--case_type", nargs="?", default='test', type=str)
    parser.add_argument("--case_timing", nargs="?", default='final', type=str)
    parser.add_argument("--transform", nargs="?", default='fourth_rt', type=str)
    parser.add_argument("--model_group", nargs="?", default='VAR', type=str)
    args = parser.parse_args()
    forecast_date = args.forecast_date
    case_type = args.case_type
    case_timing = args.case_timing
    transform = args.transform
    model_group = args.model_group
    
    # load data
    # should end_day be forecast_date - 1?
    data = load_data(as_of=forecast_date, end_day=forecast_date,
                     case_type=case_type, case_timing=case_timing)
    
    # data transform
    if transform == "sqrt":
        data.case[data.case <= 0] = 1.0
        data.case = np.sqrt(data.case)
        data.hosp[data.hosp <= 0] = 1.0
        data.hosp = np.sqrt(data.hosp)
    elif transform == "fourth_rt":
        data.case[data.case <= 0] = 1.0
        data.case = np.power(data.case, 0.25)
        data.hosp[data.hosp <= 0] = 1.0
        data.hosp = np.power(data.hosp, 0.25)
    elif transform == "log":
        data.case[data.case <= 0] = 1.0
        data.case = np.log(data.case)
        data.hosp[data.hosp <= 0] = 1.0
        data.hosp = np.log(data.hosp)
        
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

    # quantile levels at which to generate predictions
    q_levels = np.array([0.01, 0.025, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35,
                          0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80,
                          0.85, 0.90, 0.95, 0.975, 0.99])
    # q_levels = np.array([0.025, 0.10, 0.25, 0.50, 0.75, 0.90, 0.975])
    
    # fit models
    if model_group == 'SARIX':
        sari_variations =  expand_grid({
            'p': [p for p in range(3)],
            'P': [P for P in range(3)],
            'd': [d for d in range(2)],
            'D': [D for D in range(2)]
        })
        # keep only variations with some kind of lag
        sari_variations = sari_variations[(sari_variations.p != 0) | (sari_variations.P != 0)]
        
        for i in range(sari_variations.shape[0]):
            p = sari_variations.p.values[i]
            P = sari_variations.P.values[i]
            d = sari_variations.d.values[i]
            D = sari_variations.D.values[i]

            diffed_data = diff_df(data, ['case', 'hosp'], d, D, 7)

            sarix_fit = sarix.SARIX(
                xy = diffed_data[["case", "hosp"]].dropna().values,
                p = p,
                P = P,
                season_period = 7,
                transform = "none",
                forecast_horizon = effective_horizon_rel_obs,
                num_warmup = 1000,
                num_samples = 1000,
                num_chains = 1)

            pred_samples = sarix_fit.predictions_orig[:, extra_horizons_rel_obs:, 1]

            # extract predictive quantiles for response variable
            pred_qs = np.percentile(pred_samples, q_levels * 100.0, axis = 0)

            # invert differencing operation
            pred_qs = inv_diff_pred_q(data, pred_qs, 'hosp', d, D, 7)

            # invert data transform
            if transform == "log":
                pred_qs = np.exp(pred_qs)
            elif transform == "fourth_rt":
                pred_qs = np.maximum(0.0, pred_qs)**4
            elif transform == "sqrt":
                pred_qs = np.maximum(0.0, pred_qs)**2

            model_name = f"{case_type}_" + \
                f"{case_timing}_" +\
                "SARIX_" +\
                f"p_{p}_" +\
                f"d_{d}_" +\
                f"P_{P}_" +\
                f"D_{D}"
            save_forecast_file(location='25',
                                forecast_date=forecast_date,
                                pred_qs=pred_qs,
                                q_levels=q_levels,
                                model_name=model_name)
    elif model_group == 'VAR':
        # convert data to format suitable for statsmodels
        sm_data = data.set_index('date')[['case', 'hosp']]

        # declare VAR model
        model = VAR(sm_data)

        # identify AR lag using AIC
        model_fit = model.fit(maxlags=16, ic='aic')
        lag_order = model_fit.k_ar

        # generate predictive quantiles
        preds = model_fit.forecast_interval(sm_data.values[-lag_order:], steps=effective_horizon_rel_obs)
        pred_means = preds[0]
        pred_ses = preds[2] - preds[0]
        pred_qs = np.concatenate(
          [pred_means[:, 1:2] + norm.ppf(q) * pred_ses[:, 1:2] for q in q_levels],
          axis = 1
        )
        
        # invert data transform
        if transform == "log":
            pred_qs = np.exp(pred_qs)
        elif transform == "fourth_rt":
            pred_qs = np.maximum(0.0, pred_qs)**4
        elif transform == "sqrt":
            pred_qs = np.maximum(0.0, pred_qs)**2
        
        model_name = f"{case_type}_" + \
            f"{case_timing}_" +\
            "VAR"
        save_forecast_file(location='25',
                           forecast_date=forecast_date,
                           pred_qs=np.transpose(pred_qs[extra_horizons_rel_obs:, :]),
                           q_levels=q_levels,
                           model_name=model_name)





