"""plots.py

Usage:
  plots.py

"""
import sys

import pandas as pd
from experimentator import Experiment
import matplotlib.pyplot as plt
import seaborn as sns
from docopt import docopt


def transition_plot(df):
    df['relative_height'] = df['height'] - df['lowest_height_not_afforded']
    proportions = df.groupby()


def load_data(filename, stack=True):
    df = Experiment.load(filename).dataframe.dropna()
    for column, type_ in [
        ('height', 'int'),
        ('hip_height', 'float'),
        ('knee_height', 'float'),
        ('lowest_height_not_afforded', 'int'),
    ]:
        df[column] = df[column].astype(type_)

    if stack:
        df = pd.concat((df, df), keys=['outbound', 'return'], names=['phase'])
        df = df.reset_index().set_index(['participant', 'trial', 'phase']).sort()


def main():
    args = docopt(__doc__)


if __name__ == '__main__':
    main()
    sys.exit(0)
