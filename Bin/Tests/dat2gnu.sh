# for each .dat file in input directory (./dat)
for FILEPATH in $1/*.DAT; do
    # get filename (strip path and extension)
    FILENAME=$(basename $FILEPATH .DAT)

    # get seed from filename
    SEED=${FILENAME:6:1}

    # run experiment
    ./mcfm dat/$FILENAME.DAT

    # append seed to gnu filename
    mv "WWqqbr_nlo_PDF4LHC_1.00_1.00_${2}.gnu" "${basename $GNU_FILENAME .gnu}_${SEED}.gnu"

    # create output directory if it doesn't exist
    mkdir -p gnu

    # move new gnu file into output directory (./gnu)
    mv $GNU_FILENAME gnu/
done
