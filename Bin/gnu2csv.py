#!/usr/bin/env python3
import sys, re
from collections import defaultdict
from argparse import ArgumentParser
from pathlib import Path

TABLE_TITLE_PATTERN = r'  set title "(.*)" font "Helvetica, 20"'
TABLE_RANGE_PATTERN = r"  set xrange \[\s+(\d+\.\d+):\s+(\d+\.\d+)\]"
START_OF_TABLE_DATA_PATTERN = r'  plot "-" with histeps'
TABLE_ROW_PATTERN = (
    r"\s+((?:\d|\.|E|\+|-)+)\s+((?:\d|\.|E|\+|-)+)\s+((?:\d|\.|E|\+|-)+)"
)
END_OF_TABLE_DATA_PATTERN = r" e"
OUTPUT_VALUE_FORMAT = "{:0<8}"


def args():
    parser = ArgumentParser()

    parser.add_argument(
        "input directory path",
        help="specifies location of .gnu file(s)",
        type=str,
    )

    return vars(parser.parse_args())


def min_diff(lst):
    sorted_lst = sorted(lst)
    return min([abs(j - i) for i, j in zip(sorted_lst[:-1], sorted_lst[1:])])


def parse_gnu_file(gnu_filepath):
    tables = dict()

    with open(gnu_filepath) as gnu_file:
        # read until EOF marker
        for line in gnu_file:
            # check for table title
            if match := re.match(TABLE_TITLE_PATTERN, line):
                # extract title, initialise occurence number
                title, occurence_number = match.group(1), 1
                # determine number of occurences
                while (title, occurence_number) in tables:
                    occurence_number += 1

            # check for table range
            elif match := re.match(TABLE_RANGE_PATTERN, line):
                # extract range
                range_min, range_max = match.group(1, 2)

            # check for 'start of table' marker
            elif re.match(START_OF_TABLE_DATA_PATTERN, line):
                rows = []

            # check for table row
            elif match := re.match(TABLE_ROW_PATTERN, line):
                # extract three numerical values
                rows.append(match.group(1, 2, 3))

            elif re.match(END_OF_TABLE_DATA_PATTERN, line):
                # store table in dictionary
                tables[(title, occurence_number)] = (
                    float(range_min),
                    float(range_max),
                    rows,
                )

    return tables


def make_output_directory(input_directory_path):
    output_directory_path = Path(
        input_directory_path.parent / input_directory_path.name.replace("gnu", "csv")
    )
    try:
        output_directory_path.mkdir()
    except FileExistsError:
        try:
            sys.stderr.write(
                f"Output Directory Already Exists:\n{output_directory_path.resolve()}\n\n"
                f"Press Enter to overwrite or ^C to cancel... "
            )
            input()
        except KeyboardInterrupt:
            sys.exit(0)
        else:
            for entry in output_directory_path.iterdir():
                entry.unlink()

    return Path(output_directory_path)


def output_filepath(title, occurence, is_duplicate_title):
    return Path(f"{title}{f' ({occurence})' if is_duplicate_title else ''}.csv")


def float_range(start, stop, step):
    while start < stop:
        yield start
        start += step


def distribution_range(
    distribution_range_start, distribution_range_stop, distribution_range_step
):
    return float_range(
        float(distribution_range_start) + (distribution_range_step / 2),
        float(distribution_range_stop),
        float(distribution_range_step),
    )


def min_binsize(title, range_min, range_max, rows):
    distribution_values = [float(row[0]) for row in rows]

    if len(rows) > 1:
        return min_diff(distribution_values)
    else:
        if title == "xsec distribution":
            return float(range_max) - float(range_min)
        else:
            return distribution_values[0] - float(range_min)


def main():
    # get input path from command line
    input_directory_path = Path(args()["input directory path"])

    # check input path is directory
    if not input_directory_path.is_dir():
        raise ValueError("Provided path must be a directory")

    # map input directory name to output directory name, e.g. 'gnu_foo' -> 'csv_foo'
    output_directory_path = make_output_directory(input_directory_path)

    # check every path in input directory is a gnu file
    # TODO update name.endswith to .suffix
    if not all(
        [entry.name.endswith(".gnu") for entry in input_directory_path.iterdir()]
    ):
        raise ValueError(f"Provided directory must only contain .gnu files")

    runs = [
        parse_gnu_file(gnu_file_entry.resolve())
        for gnu_file_entry in input_directory_path.iterdir()
    ]
    print(len(runs))

    csv_tables = {}

    for (title, occurence) in runs[0]:
        csv_tables[(title, occurence)] = defaultdict(list)

        for run in runs:
            (range_min, range_max, gnu_table_rows) = run[(title, occurence)]
            range_step = min_binsize(title, range_min, range_max, gnu_table_rows)

            gnu_table_row_index = 0
            for csv_distribution_value in distribution_range(
                range_min, range_max, range_step
            ):
                try:
                    gnu_table_row = gnu_table_rows[gnu_table_row_index]
                except IndexError:
                    csv_tables[(title, occurence)][csv_distribution_value] += [
                        OUTPUT_VALUE_FORMAT.format(0.0) for _ in range(2)
                    ]
                    continue

                if float(gnu_table_row[0]) == csv_distribution_value:
                    csv_tables[(title, occurence)][csv_distribution_value] += [
                        gnu_table_row[1],
                        gnu_table_row[2],
                    ]
                    gnu_table_row_index += 1
                else:
                    csv_tables[(title, occurence)][csv_distribution_value] += [
                        OUTPUT_VALUE_FORMAT.format(0.0) for _ in range(2)
                    ]

    for (title, occurence), csv_table in csv_tables.items():
        is_duplicate_title = True if (title, 2) in csv_tables else False
        with open(
            Path(
                output_directory_path
                / Path(f"{title}{f' ({occurence})' if is_duplicate_title else ''}.csv")
            ),
            "w",
        ) as csv_file:
            for distribution_value, data in csv_table.items():
                csv_file.write(
                    f"{OUTPUT_VALUE_FORMAT.format(distribution_value)},{','.join(data)}\n"
                )


if __name__ == "__main__":
    main()
