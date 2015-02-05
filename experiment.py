from contextlib import contextmanager
from datetime import datetime
from operator import contains
from decimal import Decimal
from collections import defaultdict
import sys
from subprocess import call

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
UNITS_BY_FIELD = defaultdict(str)
UNITS_BY_FIELD['height'] = 'in'
UNITS_BY_FIELD['width'] = 'ft'


def big_print(*args):
    call(['toilet', '-w', '120', ' '.join(args)])


def horizontal_rule():
    big_print(17 * '=')


def trial_prompt(phase):
    return '{0:{1}}: \
{terminal.underline}u{terminal.normal}nsuccessful, \
{terminal.underline}o{terminal.normal}ver or \
{terminal.underline}a{terminal.normal}round? '.format(phase, len('outbound'), terminal=Terminal())


def get_input(prompt, is_allowed):
    result = None
    while not is_allowed(result):
        result = input(prompt)
    return result


def show_trial(data):
    horizontal_rule()

    fields = ('trial', 'width', 'height')
    info = Series({field: data[field] for field in fields})

    # To force the lines to print in a certain order.
    lines = str(info).split('\n')
    lines_by_field = {line.split()[0]: line for line in lines}
    for field in fields:
        big_print(lines_by_field[field], UNITS_BY_FIELD[field])


def run_trial(experiment, trial):
    trial.data['height'] += trial.data['lowest_height_not_afforded']
    show_trial(trial.data)
    results = {'time': datetime.now()}
    for phase in ('outbound', 'return'):
        results[phase] = RESULTS_BY_CODE[get_input(
            trial_prompt(phase),
            contains(set(RESULTS_BY_CODE.keys()))
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
    experiment.set_run_callback(run_trial)
    experiment.save()
    return 0


if __name__ == '__main__':
    sys.exit(main())
