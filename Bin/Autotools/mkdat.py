#!/usr/bin/env python3
import argparse, os, sys
from pathlib import Path
from string import Template


def parameters():
    # initialise command line parser
    parser = argparse.ArgumentParser()

    # parse and return values
    parser.add_argument(
        "--min",
        help="sets the .dat file's 'm3456min' variable (default: 0)",
        type=int,
        default=0,
    )
    parser.add_argument(
        "--max",
        help="sets the .dat file's 'm3456max' variable (default: 14000)",
        type=int,
        default=14000,
    )
    parser.add_argument(
        "template_filepath",
        help="specifies the filepath of the .dat template",
        type=str,
    )
    parser.add_argument(
        "runstring",
        help="sets the .dat file's 'runstring' variable, a label appended to the name of each .gnu file",
        type=str,
    )
    parser.add_argument(
        "seed_limit",
        help="specifies the number of unique seeds to generate, a corresponding .dat file is created for each",
        type=int,
    )
    parser.add_argument(
        "--cut",
        help="sets the .dat file's 'makecuts' flag to true (default: false)",
        action="store_true",
        default=True,
    )

    return vars(parser.parse_args()).values()


# parse parameters provided via command line
minimum, maximum, template_filepath, runstring, seed_limit, cut = parameters()

# string representing cut parameter
cut_string = "cut" if cut else "nocut"

# construct .dat directory name
dirname = f"dat_{minimum}-{maximum}_{cut_string}_{runstring}"

try:
    # construct dat template object from file
    with open(Path(template_filepath).resolve()) as template_file:
        template = Template(template_file.read())
except FileNotFoundError:
    # inform user that template file is missing
    print(Path(template_filepath).resolve())
    sys.stderr.write("Could not find a template.DAT file in the current directory")
    sys.exit(0)

# create output directory
if not os.path.exists(dirname):
    os.makedirs(dirname)

# create .dat files
for seed in range(1, seed_limit + 1):
    # 'fill in' template string
    dat_data = template.substitute(
        {
            "min": minimum,
            "max": maximum,
            "runstring": f"{runstring}_{seed}",
            "seed": seed,
            "cut": str(cut).lower(),
        }
    )

    # construct .dat filename
    filename = f"{minimum}-{maximum}_{cut_string}_{runstring}_{seed}.DAT"

    # write .dat file
    with open(f"{dirname}/{filename}", "w") as dat_file:
        dat_file.write(dat_data)
