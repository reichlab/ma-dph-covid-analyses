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


def expand_grid(data_dict):
	"""Create a dataframe from every combination of given values."""
	rows = itertools.product(*data_dict.values())
	return pd.DataFrame.from_records(rows, columns=data_dict.keys())


def load_data(as_of = None, end_day = "2021-07-01", case_type = 'report', case_timing = 'final', state = 'ma'):
	"""
	Load data for MA cases and hosps from covidcast
	Parameters
	----------
	as_of: string of date in YYYY-MM-DD format. 
		Default to None.
	end_day: string of date in YYYY-MM-DD format. 
		Default to "2021-07-01"
	case_type: string of recording method for Covid-19 cases
		Default to 'report'
	case_timing: string to indiciate versioning.
		Default to 'final'
	state: string of a 2-letter state abbreviation.
		Default to 'ma'
	Returns
	-------
	df: data frame
		It has columns location, date, inc_hosp, population and rate. 
		It is sorted by location and date columns in ascending order.
	"""
	# override as_of = None to use the same as_of as is used for cases
	if as_of is None:
		hosp_as_of = '2022-04-29'
	else:
		hosp_as_of = as_of
	
	# load hospitalizations
	hosp_df = covidcast.signal(data_source="hhs",
							   signal="confirmed_admissions_covid_1d",
							   start_day=date.fromisoformat("2020-10-01"),
							   end_day=date.fromisoformat(end_day),
							   geo_type="state",
							   geo_values=state,
							   as_of=date.fromisoformat(hosp_as_of))
	hosp_df = hosp_df[["geo_value", "time_value", "value"]]
	hosp_df.columns = ["location", "date", "hosp"]
	
	# load cases
	if case_type == 'report':
		if case_timing == 'final':
			case_as_of = '2022-04-29'
		else:
			case_as_of = as_of
		
		case_df = covidcast.signal(data_source="jhu-csse",
								   signal="confirmed_incidence_num",
								   start_day=date.fromisoformat("2020-10-01"),
								   end_day=date.fromisoformat(end_day),
								   geo_type="state",
								   geo_values=state,
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
			case_as_of = '2022-04-29'
		else:
			subset_as_ofs = [ao for ao in as_ofs if ao <= as_of]
			case_as_of = max(subset_as_ofs)
		
		if state == 'ma':
			case_df_path = 'csv-data/MA-DPH-csvdata-covid-' + case_as_of + '.csv'
		elif state == 'ca':
			case_df_path = 'csv-data/CA-DPH-testdate-covid-' + case_as_of + '.csv'
			
		case_df = pd.read_csv(case_df_path)
		case_df['location'] = state
		case_df = case_df[['location', 'test_date', 'new_positive']]
		case_df.columns = ['location', 'date', 'case']
		
		case_df = case_df[(case_df.date >= '2020-10-01') & (case_df.date <= end_day)]
		case_df.date = pd.to_datetime(case_df.date)
	
	# merge
	df = case_df.merge(hosp_df, on=["location", "date"], how = "left")
	
	# ensure float data type
	df[['case', 'hosp']] = df[['case', 'hosp']].astype('float64')
	
	# drop missing values; assumed to be trailing (dangerous?)
	df = df.dropna()
	
	return df


def construct_forecast_df(location, forecast_date, pred_qs, q_levels, base_target):
	# format predictions for one target variable as a data frame with required columns
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
	preds_df['base_target'] = base_target
	preds_df['target'] = preds_df['horizon'] + preds_df['base_target']
	preds_df['type'] = 'quantile'
	preds_df = preds_df[['location', 'forecast_date', 'target', 'target_end_date', 'type', 'quantile', 'value']]
	return preds_df


def save_forecast_file(location, forecast_date, hosp_pred_qs, case_pred_qs, q_levels, model_name):
	hosp_pred_df = construct_forecast_df(location,
																			 forecast_date,
																			 hosp_pred_qs,
																			 q_levels,
																			 ' day ahead inc hosp')
	if case_pred_qs is None:
		preds_df = hosp_pred_df
	else:
		case_pred_df = construct_forecast_df(location,
																				forecast_date,
																				case_pred_qs,
																				q_levels,
																				' day ahead inc case')
		preds_df = pd.concat([hosp_pred_df, case_pred_df], axis = 0)

	# save predictions
	model_dir = Path("forecasts") / model_name
	model_dir.mkdir(mode=0o775, parents=True, exist_ok=True)
	file_path = model_dir / f"{forecast_date}-{model_name}.csv"
	preds_df.to_csv(file_path, index = False)


def save_fit_samples(forecast_date, param_samples, pred_samples, model_name):
	model_dir = Path("fit_samples") / model_name
	model_dir.mkdir(mode=0o775, parents=True, exist_ok=True)
	file_path = model_dir / f"{forecast_date}-{model_name}.npz"
	np.savez_compressed(file_path,
											param_samples = param_samples,
											pred_samples = pred_samples)


def build_model_name(case_type, case_timing, smooth_case, p, d, P, D):
	return f"{case_type}_" + \
		f"{case_timing}_" +\
		f"smooth_case_{smooth_case}_" +\
		"SARIX_" +\
		f"p_{p}_" +\
		f"d_{d}_" +\
		f"P_{P}_" +\
		f"D_{D}"


if __name__ == "__main__":
	# parse arguments
	parser = argparse.ArgumentParser(description="hierarchicalGP")
	parser.add_argument("--forecast_date", nargs="?", default='2020-12-07', type = str)
	parser.add_argument("--case_type", nargs="?", default='test', type=str)
	parser.add_argument("--case_timing", nargs="?", default='final', type=str)
	parser.add_argument("--transform", nargs="?", default='fourth_rt', type=str)
	args = parser.parse_args()
	forecast_date = args.forecast_date
	case_type = args.case_type
	case_timing = args.case_timing
	transform = args.transform
	# forecast_date = '2020-12-07'
	# case_type = 'test'
	# case_timing = 'final'
	# transform = 'fourth_rt'
	
	# define model variations to fit
	if forecast_date <= '2021-06-07':
		# validation phase
		if case_type == 'none':
			smooth_case_options = [False]
		else:
			smooth_case_options = [True, False]
		
		sari_variations =  expand_grid({
			'smooth_case': smooth_case_options,
			'p': [p for p in range(5)],
			'P': [P for P in range(3)],
			'd': [d for d in range(2)],
			'D': [D for D in range(2)]
		})
		
		# keep only variations with some kind of lag
		sari_variations = sari_variations[(sari_variations.p != 0) | (sari_variations.P != 0)]
	else:
		# prospective test set evaluation phase
		# settings were chosen based on validation set performance; see eval_forecasts.R
		if case_type == 'none':
			sari_variations = pd.DataFrame({
				'smooth_case': [False],
				'p': [2],
				'd': [1],
				'P': [2],
				'D': [0]
			})
		elif case_type == 'report':
			sari_variations = pd.DataFrame({
				'smooth_case': [False, True],
				'p': [2, 2],
				'd': [0, 0],
				'P': [1, 0],
				'D': [1, 1]
			})
		else:
			sari_variations = pd.DataFrame({
				'smooth_case': [False, True],
				'p': [2, 2],
				'd': [0, 0],
				'P': [1, 0],
				'D': [1, 1]
			})
	
	# keep only variations without a model fit file
	model_names = [build_model_name(case_type,
																	case_timing,
																	sari_variations.smooth_case.values[i],
																	sari_variations.p.values[i],
																	sari_variations.d.values[i],
																	sari_variations.P.values[i],
																	sari_variations.D.values[i]) \
									for i in range(sari_variations.shape[0])]
	file_paths = [
		Path("forecasts") / model_name / f"{forecast_date}-{model_name}.csv" \
			for model_name in model_names]
	file_doesnt_exist = [not file_path.exists() for file_path in file_paths]
	sari_variations = sari_variations.loc[file_doesnt_exist]
	
	# only proceed if there are models to fit
	if sari_variations.shape[0] > 0:
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
		
		# add 7 day rolling mean of cases
		data['case_rm'] = data.rolling(7)[['case']].mean()
		
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
		for i in range(sari_variations.shape[0]):
			smooth_case = sari_variations.smooth_case.values[i]
			p = sari_variations.p.values[i]
			P = sari_variations.P.values[i]
			d = sari_variations.d.values[i]
			D = sari_variations.D.values[i]
			
			if case_type == 'none':
				modeled_vars = ['hosp']
			elif smooth_case:
				modeled_vars = ['case_rm', 'hosp']
			else:
				modeled_vars = ['case', 'hosp']
			
			sarix_fit = sarix.SARIX(
				xy = data[modeled_vars].dropna().values,
				p = p,
				d = d,
				P = P,
				D = D,
				season_period = 7,
				transform = "none",
				forecast_horizon = effective_horizon_rel_obs,
				num_warmup = 1000,
				num_samples = 1000,
				num_chains = 1)

			pred_samples = sarix_fit.predictions_orig

			# extract predictive quantiles for response variable
			hosp_pred_qs = np.percentile(pred_samples[:, :, -1], q_levels * 100.0, axis = 0)

			# subset to those we want to keep
			hosp_pred_qs = hosp_pred_qs[:, extra_horizons_rel_obs:]

			# invert data transform
			if transform == "log":
				hosp_pred_qs = np.exp(hosp_pred_qs)
			elif transform == "fourth_rt":
				hosp_pred_qs = np.maximum(0.0, hosp_pred_qs)**4
			elif transform == "sqrt":
				hosp_pred_qs = np.maximum(0.0, hosp_pred_qs)**2
			
			if case_type == 'none':
				case_pred_qs = None
			else:
				# extract predictive quantiles for cases
				case_pred_qs = np.percentile(pred_samples[:, :, -2], q_levels * 100.0, axis = 0)

				# subset to those we want to keep
				case_pred_qs = case_pred_qs[:, extra_horizons_rel_obs:]

				# invert data transform
				if transform == "log":
					case_pred_qs = np.exp(case_pred_qs)
				elif transform == "fourth_rt":
					case_pred_qs = np.maximum(0.0, case_pred_qs)**4
				elif transform == "sqrt":
					case_pred_qs = np.maximum(0.0, case_pred_qs)**2

			model_name = build_model_name(case_type, case_timing, smooth_case, p, d, P, D)
			save_forecast_file(location='25',
								forecast_date=forecast_date,
								hosp_pred_qs=hosp_pred_qs,
								case_pred_qs=case_pred_qs,
								q_levels=q_levels,
								model_name=model_name)
			param_samples = {k:v for k,v in sarix_fit.samples.items() \
				if k in ['betas_update_var', 'theta']}
			save_fit_samples(forecast_date=forecast_date,
											param_samples=param_samples,
											pred_samples=pred_samples,
											model_name=model_name)





