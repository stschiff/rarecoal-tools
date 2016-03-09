# The Rarecoal-tools package
This software implements tools to process genome sequencing data in preparation to perform rare variant analyses and in particular to generate histogram files needed by the rarecoal package.

## Installation instructions
* Install the Haskell tool stack, available [here](https://github.com/commercialhaskell/stack).
* Download this repository and unpack it.
* Change directory into this repository
* (Optional) run `stack setup` to install the correct compiler. If you are not sure whether you have the compiler already, you can directly go to the next step. It will tell you to run `stack setup` if you need to.
* Run `stack build`. This will download all the dependencies and may take a while. You may get an error about missing software such as pkg-config, or gsl, which then need to be installed using your standard package manager, such as `apt-get` on Linux or `brew` on a Mac. As soon as you have installed those, just run `stack build` again to continue where you left before the error.
* Run `stack install` to copy the executables into "~/.local/bin". You should then add this directory to your path.


All tools described here have an online help, as shown by typing the command name and then `-h`.

### vcf2FreqSum
This tool converts a multi-sample VCF file, read from stdin, to a simpler file which I call "freqSum". The first lines of an example are:

    1	10539	C	A	0	0	0	0	0	0	0	0	0	0	-1	0	0	0
    1	11008	C	G	0	0	0	0	1	0	1	0	0	0	0	0	0	0
    1	11012	C	G	0	0	0	0	1	0	1	0	0	0	0	0	0	0
    1	13110	G	A	1	1	0	0	0	0	0	0	0	0	-1	0	0	0
    1	13116	T	G	0	0	0	0	0	1	0	1	0	0	0	1	0	0


Here, the first four columns are chromosome, position, reference allele, alternative allele. The following columns give the alternative allele count in each individual. A `-1` denotes missing data. Note that this tool fails if you pass indels or multi-allelic SNPs. You should therefore combine this script with a filtering step, for example using bcftools (v1.2):

    bcftools view -m2 -M2 -c1 -v snps -S <sample_list.txt> <vcf_file> | vcf2FreqSum > output.txt

### groupFreqSum
This tools groups freqSum columns (read from stdin) into sample groups, giving the total allele count of all samples in that group. Here is the help file (type `groupFreqSum -h`):

	Usage: groupFreqSum (-n|--nVec nVec) [-m|--removeMissing]
	  This tool merges samples or groups into larger groups, adding up the allele
	  counts

	Available options:
	  -h,--help                Show this help text
	  -n,--nVec nVec           comma-separated list of numbers that specify how to
	                           join sample or groups, surrounded by square brackets.
	                           Example: -n [20,20,1] specifies that you want to
	                           merge the first twenty samples/groups into one, and
	                           sample 21 through 40, and then have the last group
	                           separate. See README for instructions.
	  -m,--removeMissing       if one individual/group has missing data (-1) declare
	                           the whole group as missing data.

It expects a comma-separated list, surrounded by square-brackets, of how to merge samples. For example, if you have a freqSum file for 500 individuals, as generated from vcf2freqSum, and you would like to merge them into 5 groups of 100 samples each, you would use 

    groupFreqSum -n [100,100,100,100,100] < freqSumFile.txt > groupedFreqSumFile.txt

The output from this command line will still be a freqSum file, but with groups instead of individuals given in the columns. The advantage of this data format is that it is very general with respect to individuals vs. groups. In the first example above (at vcf2FreqSum), the output contained a single column for each individual, with allele counts not exceeding 2 (for homozygous non-ref genotype), naturally. However, once you used `groupFreqSum`, you end up with columns describing the allele counts in a whole group. The format is still the same. By default, this command interprets missing data as a call of the reference allele. An optional flag `--removeMissing` changes this behaviour and declares an entire group as missing if one individual from that group has missing data.

### mergeFreqSum
This tools merges two freqSum files. It takes four arguments, as shown by typing `mergeFreqSum -h`:

    Usage: mergeFreqSum freqSumFile1 freqSumFile2 <n1> <n2>
    Available options:
      -h,--help                Show this help text
      freqSumFile1             file 1, put - for stdin
      freqSumFile2             file 2
      <n1>                     number of populations in file 1
      <n2>                     number of populations in file 2

Here, n1 and n2 are the number of sample/group columns (which can also denote populations or groups if `groupFreqSum` has been used).
If a site is present in one file but not the other, all missing genotypes are assumed to be homozygous reference.

### downSampleFreqSum
Typing `downSampleFreqSum -h` gives:

	Usage: downSampleFreqSum <POSITION> <N_BEFORE> <N_AFTER>
	  Tool for downsampling a freqSum file. If a column is -1, the downsampled
	  column will also have -1.
	  
	Available options:
	  -h,--help                Show this help text
	  <POSITION>               the 0-based index of the population to sample from
	  <N_BEFORE>               the number of haplotypes in the group before sampling
	  <N_AFTER>                the new number of haplotypes to downsample to

This can be used to downsample the number of samples in a particular sample or group in the input file, read from stdin. The three arguments are the 0-based index of the column to downsample, the number of haplotypes in that column, and the (lower) new number of haplotypes in that column.

### freqSum2Histogram
This is the key tool to convert a freqSumFile to an allele sharing histogram, as used by rarecoal. Type `-h` for getting help:

	Usage: freqSum2Histogram (-n|--nVec nVec) [-m|--maxM INT] [-i|--popIndices LIST]
	                         (-N|--nrCalledSites ARG)
	  Tool to convert a freqSum file to to a histogram file as needed for rarecoal.
	  Any pattern containing -1 in any selected column is excluded.
	  
	Available options:
	  -h,--help                Show this help text
	  -n,--nVec nVec           comma-separated list of the number chromosomes (twice
	                           the number of diploid samples) for each population,
	                           surrounded by square brackets. Example: -n
	                           [100,100,100] for three population each with 100
	                           haplotypes.
	  -m,--maxM INT            Specify the maximum allele count per
	                           population (default: 10)
	  -i,--popIndices LIST     If given, select only the populations at the 0-based
	                           indices from the input freqSum file. Example: -i
	                           [0,2,3]: select only the first, the third and the
	                           fourth population. (default: [])
	  -N,--nrCalledSites ARG   set the total nr of called sites. This sets the
	                           number of non-variant sites (via the pattern
	                           consisting of zeros only) such that the total number
	                           of sites matches the number given. This number is
	                           important for estimating population sizes correctly,
	                           see the README for instructions.

One key ingredient in this tool is the total number of sites, specified via `-N`. This is an important input, as it will set the number of non-variant counts in your histogram, specified by the pattern consisting of zeros only, e.g. 0,0,0,0,0 in five populations. This number is important for estimating population sizes, which relies on knowing the ratio of variants and non-variants. If you are processing modern sequencing data (say from the 1000 Genomes Project), you can more or less assume that the entire mappable and callable genome is covered in all individuals. For humans, the number in all autosomes is close to 2,500,000,000, but the details depend on your specific data set and processing. For the 1000 Genomes Project, you can have a look at the `/vol1/ftp/release/20130502/supporting/accessible_genome_masks` directory on the FTP site and count all accessible sites if you don't apply further filtering.

### extractInFreqSum
Typing `extractInFreqSum -h` gives:

	Usage: extractInFreqSum (-i|--position <POSITION>)
	  Takes a column from a freqSum file and moves it to the end
	  
	Available options:
	  -h,--help                Show this help text
	  -i,--position <POSITION> 0-based index of the sample/group to move

This tool can be used to move a column in a freqSumFile behind the last column. Useful for extracting individual samples before grouping multiple samples with `groupFreqSum`.

### ms2hist
Typing `ms2hist` gives:

	Usage: ms2hist (-n|--nVec <LIST>) (-m|--maxM <INT>) (-N|--nrCalledSites INT)
	  converts ms-format output from simulations into a histogram as used for
	  Rarecoal.

This converts the output from the `ms` and `scrm` simulation tools to a histogram. The input is read from stdin and should only contain the line starting with "POSITIONS:" and the following lines specifying the genotypes. You should use the `tail` command in UNIX to select the respective lines of the `ms` or `scrm` outputs. It cannot be used to process multiple chromosomes simultaneously.
This tools expects the total number of sites simulated as a parameter `-N`. Note that this parameter should give the entire length of the chromosome that you simulated, not just the number of segregating sites!

### sampleHist

	Usage: sampleHist (-q|--queryBranch <INT>) (-n|--howMany <INT>)
	                  (-i|--hist <path-to-histogram>)
	  sample a number of haplotypes (independently at each site) from a population
	  in a histogram
	  
	Available options:
	  -h,--help                Show this help text
	  -q,--queryBranch <INT>   the population index (0-based) from which to sample
	  -n,--howMany <INT>       how many samples should be drawn at each site
	  -i,--hist <path-to-histogram>
	                           the input histogram file, set - for stdin

This extracts samples from a subpopulation in the histogram, by sampling without replacement independently at every site underlying the histogram. The extracted samples therefore do not represent real individuals, but "average" individuals with genotypes sampled independently at every site from a full population. This can be useful if you need to extract individuals from histograms which were generated from data for which only allele frequencies but not genotypes are given. 

### combineHistograms

	Usage: combineHistograms histogram_file
	  Tool to combine multiple histogram files, for help add option -h
	  
	Available options:
	  -h,--help                Show this help text
	  histogram_file           histogram file, put as many as you want to add up

This simply adds up multiple histograms.