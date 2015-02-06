"""plots.py

Usage:
  plots.py export-data <exp-file> <csv-file>
  plots.py export-proportions <exp-file> <csv-file>

"""
import sys
from operator import methodcaller

from toolz import compose, curry
import pandas as pd
from experimentator import Experiment
import matplotlib.pyplot as plt
import seaborn as sns
from docopt import docopt

_phases = ('outbound', 'return')


def proportion(df, columns, phase='action'):
    props = df.groupby(columns)[phase].apply(
        compose(methodcaller('get', 'around', 0), methodcaller('value_counts', normalize=True))
    )
    props.name = 'p(around)'
    return props


def plot_proportions(df, by='relative_height'):
    props = proportion(df, [by, 'width'])
    with sns.color_palette(sns.cubehelix_palette()):
        sns.FacetGrid(props.reset_index(), hue='width').map(plt.plot, by, 'p(around)', marker='o')

    plt.legend(loc='lower right')
    plt.xlabel('obstacle height {} lowest height not afforded'.format('-' if by == 'relative_height' else '/'))
    plt.ylabel('P(around)')


def load_data(filename, stack=True, collapse_unsuccesful=True):
    df = Experiment.load(filename).dataframe.dropna()
    for column, type_ in [
        ('height', 'int'),
        ('hip_height', 'float'),
        ('knee_height', 'float'),
        ('lowest_height_not_afforded', 'int'),
    ]:
        df[column] = df[column].astype(type_)

    df['relative_height'] = df['height'] - df['lowest_height_not_afforded']
    df['scaled_height'] = df['height'] / df['lowest_height_not_afforded']

    if collapse_unsuccesful:
        for phase in _phases:
            df.loc[df[phase] == 'unsuccessful', phase] = 'over'

    if stack:
        df = pd.concat((df, df), keys=_phases, names=['phase'])
        df = df.reset_index().set_index(['participant', 'trial', 'phase']).sort()
        df['action'] = [row[phase] for (p, t, phase), row in df.iterrows()]
        for phase in _phases:
            del df[phase]

    return df

def main():
    args = docopt(__doc__)

    if args['export-proportions']:
        proportion(
            load_data(args['<exp-file>']), ['relative_height', 'width']
        ).reset_index().to_csv(args['<csv-file>'], index=False)

    elif args['export-data']:
        load_data(args['<exp-file>']).to_csv(args['<csv-file>'], index=True)


if __name__ == '__main__':
    main()
    sys.exit(0)
