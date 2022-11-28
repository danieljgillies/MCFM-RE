#!/usr/bin/env python3
import sys, subprocess
from argparse import ArgumentParser
from pathlib import Path


def args():
    parser = ArgumentParser()

    parser.add_argument(
        "mcfm filepath",
        help="specifies location of mcfm script",
        type=str,
    )

    parser.add_argument(
        "input directory path",
        help="specifies location of .dat file(s)",
        type=str,
    )

    parser.add_argument(
        "prefix",
        help="Prefix that will MCFM generates",
        type=str,
    )

    return vars(parser.parse_args())


def make_output_directory(input_directory_path):
    output_directory_path = Path(input_directory_path.name.replace("dat", "gnu"))
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


def main():
    # get mcfm filepath from command line
    mcfm_filepath = Path(args()["mcfm filepath"])

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
        [entry.name.endswith(".DAT") for entry in input_directory_path.iterdir()]
    ):
        raise ValueError(f"Provided directory must only contain .DAT files")

    # TODO currently working HERE

    for dat_file_entry in input_directory_path.iterdir():
        subprocess.call(
            f"./{mcfm_filepath} {dat_file_entry.parent} {dat_file_entry.name}",
            shell=True,
        )
        runstring = args()["prefix"]
        runstring += "_".join(dat_file_entry.name.split("_")[-2:]).replace(".DAT", "")

        gnu_filepath = Path(
            input_directory_path / f"{runstring}.gnu"
        )

        gnu_filepath.rename(
            Path(output_directory_path / dat_file_entry.name.replace("DAT", "gnu"))
        )

        for grid_file_entry in input_directory_path.glob("*.grid"):
            grid_file_entry.unlink()


if __name__ == "__main__":
    main()

# # for each .dat file in input directory (./dat)
# for FILEPATH in *_$1/*.DAT; do
#     # get filename (strip path and extension)
#     FILENAME=$(basename $FILEPATH .DAT)

#     # get seed from filename
#     SEED=${FILENAME:6:1}

#     # run experiment
#     ./mcfm dat/$FILENAME.DAT

#     # append seed to gnu filename
#     mv "WWqqbr_nlo_PDF4LHC_1.00_1.00_${2}.gnu" "${basename $GNU_FILENAME .gnu}_${SEED}.gnu"

#     # create output directory if it doesn't exist
#     mkdir -p gnu

#     # move new gnu file into output directory (./gnu)
#     mv $GNU_FILENAME gnu/
# done
