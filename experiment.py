from contextlib import contextmanager
from datetime import datetime
from operator import contains
from decimal import Decimal
import sys
from subprocess import call
import random

from getch import getch
from toolz import curry
from blessings import Terminal
from pandas import Series
from experimentator import Experiment

contains = curry(contains)

RESULTS_BY_CODE = {
    'o': 'over',
    'a': 'around',
    'u': 'unsuccessful',
}
MEASUREMENTS = [
    'knee_height',
    'hip_height',
    'lowest_height_not_afforded',
]
UNITS_BY_FIELD = {
    'height': 'in.',
    'width': 'ft.',
}


def big_print(*args):
    call(['toilet', '-w', '140', ' '.join(map(str, args))])


def horizontal_rule():
    big_print(17 * '=')


def trial_prompt(phase, max_phase_len=len('outbound')):
    return '{0:{1}}: \
{terminal.underline}u{terminal.normal}nsuccessful, \
{terminal.underline}o{terminal.normal}ver or \
{terminal.underline}a{terminal.normal}round? '.format(phase, max_phase_len, terminal=Terminal())


def get_input(prompt, is_allowed, single_key=False):
    result = None
    while not is_allowed(result):
        if single_key:
            print(prompt, end=' ')
            sys.stdout.flush()
            result = getch()
            print(result)
            sys.stdout.flush()
        else:
            result = input(prompt)
    return result


def show_data(data, order=None, print_func=big_print):
    if order is None:
        order = list(data.keys())

    info = Series(data)
    lines = str(info).split('\n')
    lines_by_field = {line.split()[0]: line for line in lines}

    for field in order:
        if field in info:
            to_print = [lines_by_field[field]]
            if field in UNITS_BY_FIELD:
                to_print.append(UNITS_BY_FIELD[field])
            print_func(*to_print)


def show_trial(data):
    horizontal_rule()

    possible_fields = ('trial', 'width', 'height')
    info = {'trial': '{block}.{trial}'.format(**data) if 'block' in data else data['trial']}
    info.update({field: data[field] for field in possible_fields[1:] if field in data.maps[0]})

    show_data(info, order=possible_fields)


def run_trial(experiment, trial):
    phases = experiment.experiment_data.get('phases', ('outbound', 'return'))
    trial.data['height'] += trial.data['lowest_height_not_afforded']
    show_trial(trial.data)
    results = {'time': datetime.now()}
    for phase in phases:
        results[phase] = RESULTS_BY_CODE[get_input(
            trial_prompt(phase,  max(len(phase_) for phase_ in phases)),
            contains(set(RESULTS_BY_CODE.keys())),
            single_key=True
        )]
    return results


@contextmanager
def setup_participant(experiment, participant):
    for measurement in MEASUREMENTS:
        participant.data[measurement] = Decimal(get_input(
            '{}? '.format(measurement.replace('_', ' ')),
            castable_to(Decimal)
        ))
    yield
    gender_prompt = """\
gender? ({t.underline}m{t.normal}ale, {t.underline}f{t.normal}emale, {t.underline}o{t.normal}ther) """.format(
        t=Terminal()
    )
    participant.data['gender'] = get_input(gender_prompt, contains({'m', 'f', 'o', 'M', 'F', 'O'})).lower()
    participant.data['age'] = int(get_input('age? ', castable_to(int)))


@contextmanager
def setup_block(experiment, block):
    block.data['side'] = random.choice('AB')
    fields = ('block', 'width', 'side')
    show_data({field: block.data[field] for field in fields}, order=fields)
    horizontal_rule()

    input('press ENTER to continue')
    yield
    horizontal_rule()


@curry
def castable_to(type_, value):
    try:
        type_(value)
    except:
        return False
    return True


def main():
    experiment = Experiment.from_yaml_file(sys.argv[1])
    experiment.set_context_manager('participant', setup_participant)
    if 'block' in experiment.levels:
        experiment.set_context_manager('block', setup_block)
    experiment.set_run_callback(run_trial)
    experiment.save()
    return 0


if __name__ == '__main__':
    sys.exit(main())
