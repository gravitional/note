#!/bin/sh
# [ConvertFilesIntoArchive页面]: https://wiki.lyx.org/Tips/ConvertFilesIntoArchive
# This script creates archive with lyx file and all included files (graphics
# and so on). It can make plain tar archive or compress it with bz2 or gz
#
# Author: Marcin Bukat
# email : wodz@netlandia.pl
# Ths script is a FREE software
# Use at Your own risk

function usage
#Prints usage instructions
{
    echo "Usage: lyx_pak.sh [OPTONS] [FULL PATH TO LYX FILE]"
    echo " OPTIONS:"
    echo "-j        compress with bzip2"
    echo "-z        compress with gzip"
    exit
}

function archive
#Creates archive
{
    ARCHNAME=$(basename $LYXFILENAME lyx)tar
    case "$1" in
    -j)
        COMPRESS="bzip2 $ARCHNAME"
        ;;
    -z)
        COMPRESS="gzip $ARCHNAME"
        ;;
    *)
        COMPRESS=""
        ;;
    esac

    cd $WORKDIR

    tar cvf $ARCHNAME $LYXFILENAME $(cat $LYXFILENAME | grep filename | cut -d " " -f 2) "$(cat $LYXFILENAME | grep BibTeX | cut -d { -f2 | tr -d '}').bib"
    $COMPRESS

    if [ $? -eq 0 ]; then
        echo "Lyx archive created successfully"
    else
        exit 1
    fi
}
#Main part starts here

#Check if there is any parameter
if [ $# -gt 0 ]; then
    #Ok we got two parameters so first is a commpress switch
    if [ $# -eq 2 ]; then
        if [ "$1" = "-j" ] || [ "$1" = "-z" ]; then
            PARAM=$1
            LYXFILENAME=$(basename $2)
            WORKDIR=$(dirname $2)
            if [ -f $2 ] && [ -d $(dirname $2) ]; then
                #parameters are correct so make archive
                archive $PARAM
            else
                usage
            fi
        else
            usage
        fi
    else
        #One parameter - only path to the file
        PARAM="-"
        LYXFILENAME=$(basename $1)
        WORKDIR=$(dirname $1)
        if [ -f "$1" ] && [ -d "$(dirname $1)" ]; then
            #parameters are correct so make archive
            archive $PARAM
        else
            usage
        fi
    fi
else
    usage
fi
