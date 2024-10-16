# Script to count number of references that cited a paper that was also cited by another article
import math
import collections
this_script_n = 4

# Function to read the config file
def read_configs(config_path = "config.ini"):
    lines = list()
    with open(config_path, 'r') as file:
        lines = file.readlines()
    configs = dict()
    for line in lines:
        name = line.split('=')[0]
        cf = '='.join(line.split('=')[1:]).strip(' \t\r\n')
        configs[name] = cf
    return(configs)

# Function to clean numeric values
def floatNaN(f):
    try:
        return(float(f))
    except (ValueError, TypeError):
        return(float('nan'))

# Function to clean missing string values
def strEmpty(s):
    try:
        if s is None:
            return('')
        else:
            return(str(s))
    except:
        return('')

# Function to clean string values
def cleanString(s):
    try:
        if s is None:
            return('')
        # Get rid of zero-width spaces
        else:
            s = str(s)
            s2 = ''.join(['' if x == '\u200b' else x for x in s])
            s = ''.join([c if ord(c) > 31 and ord(c) != 127 else ' ' for c in s2])
            #s2 = ''.join([' ' if x == '\u00A0' else x for x in s])
            return(s)
    except:
        return('')

# Main script run on shuffled citations, was also run on all citations using a different path
def main():
    citesPath = "./citations_shuffled.tsv"

    cites = collections.defaultdict(list)

    print("Loading citation data")
    i = 0
    with open(citesPath, 'r') as file:
        for line in file:
            try:
                i += 1
                if i == 1:
                    continue
                lineData = line.split("\t")
                cites_pmid = int(float(cleanString(lineData[0].strip("\"\'\r\n\t "))))
                citedStr = lineData[1].strip("\"\'\r\n\t ")
                citedPmids = [floatNaN(cleanString(x.strip("\"\'\r\n\t "))) for x in citedStr.split(' ')]
                citedPmids = [x for x in citedPmids if not math.isnan(x)]
                if len(citedPmids) == 0:
                    continue
                citedPmids = [int(x) for x in citedPmids]
                cites[cites_pmid].extend(citedPmids)
                if i >= 1000:
                    pass
                if i % 1000000 == 0:
                    print("Loading citation data " + str(i))
            except ValueError:
                print(str(i) + " " + str(cites_pmid) + "," + citedStr)
        del i

    print("Got citation data")

    print("Deduplicating citations")
    # Deduplicate citation lists
    for k in list(cites.keys()):
        cites[k] = set(cites[k])

    print("Deduplication complete")

    outpath = "./citations_shuffled_incites.tsv"
    i=0
    ks = list(cites.keys())
    ks.reverse()
    with open(outpath, 'w') as file:
        for k in ks:
            i += 1
            if i % 100000 == 0:
                print("Counting incites " + str(i))
            for v in cites[k]:
                others = cites[k].copy()
                others.discard(v)
                count = 0
                for o in others:
                    if v in cites[o]:
                        count += 1
                if count > 0:
                    mystr = str(k) + "\t" + str(v) + "\t" + str(count) + "\n"
                    silent = file.write(mystr)

    print("Done")


if __name__=='__main__':
    main()

