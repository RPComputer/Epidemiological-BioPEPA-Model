#Imports
import sys, getopt
import json, numpy

#Loading data
def usage():
    output = "The script needs:\n\t specific model file 'sfile' \n\t parameters file 'pfile';\n\t contact matrix file 'mfile';\n\t output filename 'ofile'\n"
    ustring = 'model-writer.py -m <sfile> -p <pfile> -i <mfile> -o <ofile>'
    print(output + ustring)

def inputs(argv):
    try:
        opts, args = getopt.getopt(argv,"hm:p:i:o:")
    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            usage()
            sys.exit()
        elif opt == "-m":
            exec(open(arg).read())
        elif opt == "-p":
            global parametersfile
            parametersfile = arg
        elif opt == "-i":
            global matrixfile
            matrixfile = arg
        elif opt == "-o":
            global outputfilename
            outputfilename = arg

def error(message):
    print(message)
    sys.exit()

def checkparameters(param):
    #Base checks
    if param["state_number"] != len(param["states"]):
        error("States not match declared states number")
    if param["model_calsses_number"] != len(param["model_classes"]):
        error("Classes not match declared classes number")
    #Model checks
    if "starting_state" not in param or len(param["starting_state"]) == 0:
        error("Starting state missing")
    if param["starting_state"] not in param["states"]:
        error("States declaration error")
    #need to check all the basic rates: rate for each class for each action except contact
    seq = param["states_description"]
    rates = param["disease_rates_by_class"]
    to_check = []
    for k,v in seq:
        for e in v:
            to_check.append(e[0])
    for a in to_check:
        for v in rates.values():
            if a not in v.keys():
                error("States and action rates binding error")
    #Initial state check
    sum = 0
    for c,s in param["initial_state"]:
        if len(s) != param["model_calsses_number"]:
            error("Error in initial state definition")
        for p in s:
            sum += p
    if sum != param["total_population"]:
        error("Error in initial state definition")

def parseparameters():
    global parameters
    parameters = json.load(parametersfile)
    checkparameters(parameters)

def parsematrix():
    file = open(matrixfile, "r")
    classes = file.readline().split()
    for c in classes:
        if c not in parameters["model_classes"]:
            file.close()
            error("Model classes and matrix classes mismatch error")
    global matrix
    matrix = numpy.loadtxt(file, delimiter=",", skiprows=1)
    file.close()

#Model computation and writing
def compute_functionalrates():
    #must use imported specifi model functions
    pass

def model_builder():
    #must use imported specifi model functions
    pass

#Main
def main(argv):
    inputs(argv)
    parseparameters()
    parsematrix()
    compute_functionalrates()
    model_builder()


if __name__ == "__main__":
    main(sys.argv[1:])