import sys
sys.path.insert(0, '.')

import argparse

import os
from pathlib import Path

from multiprocessing import set_start_method
#set_start_method("spawn")
from multiprocessing import get_context
from multiprocessing import Pool
import itertools
import pandas as pd
from datetime import date, timedelta


def expand_grid(data_dict):
    """Create a dataframe from every combination of given values."""
    rows = itertools.product(*data_dict.values())
    return pd.DataFrame.from_records(rows, columns=data_dict.keys())


def run_command(command):
    """Run system command"""
    os.system(command)


if __name__ == "__main__":
    # Take user inputs
    def boolean_string(s):
      if s not in {'False', 'True'}:
          raise ValueError('Not a valid boolean string')
      return s == 'True'

    parser = argparse.ArgumentParser(description="validation phase")
    parser.add_argument("--run_setting", choices = ['local','mghpcc'], nargs="?", default='local', type = str)
    parser.add_argument("--cores", nargs="?", default=1, type=int)
    parser.add_argument("--mem", nargs="?", default="1000", type=str)
    parser.add_argument("--time", nargs="?", default="2:00", type=str)
    parser.add_argument("--queue", nargs="?", default="long", type=str)
    parser.add_argument("--sh_path", nargs="?", default="sh", type=str)
    parser.add_argument("--log_path", nargs="?", default="log", type=str)

    args = parser.parse_args()

    # forecast dates for analysis: Mondays from 2020-12-07 to 2021-06-07
    first_forecast_date = date.fromisoformat("2020-12-07")
    last_forecast_date = date.fromisoformat("2021-06-07")
    # last_forecast_date = date.fromisoformat("2020-12-21")
    num_forecast_dates = (last_forecast_date - first_forecast_date).days // 7 + 1
    forecast_dates = [str(first_forecast_date + i * timedelta(days=7)) \
        for i in range(num_forecast_dates)]

    # all combinations of forecast date and case type
    variations = expand_grid({
      'forecast_date': forecast_dates,
      # 'case_type': ['report'],
      # 'case_type': ['test'],
      'case_timing': ['final'],
      'case_type': ['report', 'test'],
      # 'case_timing': ['final', 'realtime']
      'model_group': ['VAR']
    })

    # list of python commands for each variation
    commands = [
      ' '.join(
        ['python3 code/validation-phase/run-all-models-one-date-case-type.py'] + \
          ['--' + arg_name + ' ' + str(variations[arg_name][i]) \
            for arg_name in variations.columns]) \
        for i in range(variations.shape[0])
    ]

    # run in parallel on local computer or submit cluster jobs
    if args.run_setting == 'local':
      with Pool(processes=27) as pool:
        pool.map(run_command, commands)
    elif args.run_setting == 'cluster':
      # remove old sh scripts
      os.system('rm ' + args.sh_path + '*.sh')
      # create new sh scripts
      for i in range(len(commands)):
        case_str = '_'.join([str(variations[arg_name][i]) for arg_name in variations.columns])
        sh_filename = args.sh_path + case_str + '.sh'
        
        cluster_logfile = args.log_path + 'lsf_logfile.out'
        job_logfile = args.log_path + case_str + '_logfile.txt'
        
        run_cmd = commands[i] + ' > ' + job_logfile
        
        request_cmds = \
          '#!/bin/bash\n' + \
          '#BSUB -n ' + args.cores_req + '\n' + \
          '#BSUB -R span[hosts=1]\n' + \
          '#BSUB -R rusage[mem=' + args.mem_req + ']\n' + \
          '#BSUB -o ' + cluster_logfile + '\n' + \
          '#BSUB -W ' + args.time_req + '\n' + \
          '#BSUB -q ' + args.queue_req + '\n' + \
          '\n' + \
          'module load singularity/singularity-3.6.2\n' + \
          'singularity exec analysis_scripts/singularity/singularity_tfp_cpu.sif ' + \
          run_cmd + '\n'
        
        with open(sh_filename, "w") as sh_file:
          sh_file.write(request_cmds)
