#!/usr/bin/Rscript

# Compute bootstrap confidence intervals for the mean of some numbers.
# Copyright (C) 2016  Ben J. Ward
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


# Just a function that makes sure the needed R packages are installed.
# Gets called before any package loading commands.
ensurePkgs <- function(){
    r = getOption("repos") # hard code the UK repo for CRAN
    r["CRAN"] = "http://cran.uk.r-project.org"
    options(repos = r)
    rm(r)

    # Installed package list.
    inst <- installed.packages()[, 1]

    # List of required packages.
    reqs <- c("boot", "argparse", "parallel", "foreach", "iterators", "doParallel")

    # Check each required package is installed.
    for(req in reqs){
        if(!(req %in% inst)){
            warning(paste0("Detected that ", req, " is not installed. - INSTALLING"))
            install.packages(req)
        }
    }
}

# A function which is called to process arguments and flags fed to the script on
# the command line.
processArgs <- function(){
    parser <- ArgumentParser(description = "Compute summary stats and boot.ci's")
    parser$add_argument("--file",
                        dest = "file",
                        type = "character",
                        nargs = 1,
                        default = "stdin",
                        help = "File with data.")
    parser$add_argument("--cols",
                        dest = "cols",
                        type = "integer",
                        nargs = '*',
                        default = 1,
                        help = "Columns of the table to use.")
    parser$add_argument("--delim",
                        dest = "delim",
                        type = "character",
                        nargs = 1,
                        default = "\t",
                        help = "Field delimiter of table.")
    parser$add_argument("--head",
                        dest = "head",
                        action = "store_true",
                        help = "Whether or not the input table has a head.")
    parser$add_argument("--reps",
                        dest = "reps",
                        type = "integer",
                        default = 1000L,
                        help = "The number of reps in bootstrap.")
    parser$add_argument("--ssize",
                        dest = "ssize",
                        type = "integer",
                        nargs = 1,
                        default = 0,
                        help = "How many samples to take each repetition.")
    arguments <- parser$parse_args()
    return(arguments)
}

sampleGenerator <- function(data, reps, size){
    rep <- 1L

    nextEl <- function(){
        if(rep > reps){
            stop('StopIteration', call.=FALSE)
        }
        rep <<- rep + 1L
        return(sample(data, size, replace = TRUE))
    }

    it <- list(nextElem = nextEl)
    class(it) <- c("samplegenerator", "abstractiter", "iter")
    return(it)
}

# End of functions and libraries.
# Begin script.

ensurePkgs()

suppressMessages(library(argparse))
suppressMessages(library(iterators))
suppressMessages(library(parallel))
suppressMessages(library(foreach))

cores <- detectCores()

if(cores > 1){
    suppressMessages(library(doParallel))
    cl <- makeCluster(cores)
    registerDoParallel(cl)
}

args <- processArgs()

f <- file(args$file)
open(f, blocking = TRUE)

data <- read.table(f, header = args$head, sep = args$delim)

for(col in args$cols){
    colData <- data[, col]
    if(args$ssize <= 0){
        ssize <- length(colData)
    } else {
        ssize <- args$ssize
    }
    if(length(colData) < 100L){
        writeLines("Data to be summarized:")
        write(colData, stdout(), ncolumns = 50L)
    }
    s <- summary(colData)
    writeLines("\n\nSummary of data:\n")
    writeLines(paste0("Min: ", s[1], "\n1st Q: ", s[2], "\nMedian: ", s[3], "\nMean: ", s[4], "\n3rd Q: ", s[5], "\nMax: ", s[6]))
    writeLines("\n\nResampling statistic info:")

    itx <- sampleGenerator(colData, args$reps, ssize)
    res <- foreach(i = itx, .combine = c) %dopar% mean(i)
    ci <- quantile(res, c(0.025, 0.50, 0.975))

    writeLines("\n\nBootstrapped distribution for statistic:")
    print(res)

    writeLines("\n\nBootstrapped interval for statistic:")
    print(ci)
}

if(cores > 1){
    stopCluster(cl)
}

q("no")
