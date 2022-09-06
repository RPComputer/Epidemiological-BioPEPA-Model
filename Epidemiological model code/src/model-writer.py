#Imports
import sys, getopt, importlib
import json, numpy
from modelImplementation import ModelImplementation

#Substitutions auxiliary class
class SubstitutionsData:
    def __init__(self):
        self.biopepa_file_name = ""
        self.substitutions = []
    def toJSON(self):
        return json.dumps(self, default=lambda o: o.__dict__, 
            sort_keys=True, indent=4)

#WRITER: ModelImplementation

global OUTFILECONTENT, OUTVALUESFILECONTENT
OUTFILECONTENT = []
OUTVALUESFILECONTENT = []
global headerline, valuesline
headerline = ""
valuesline = ""

#Loading data
def usage():
    output = "The script needs:\n\t specific model file 'sfile' \n\t specific model class name 'cname' \n\t parameters file 'pfile';\n\t contact matrix file 'mfile';\n\t output filename 'ofile'\n"
    ustring = 'model-writer.py -m <sfile> -c <cname> -p <pfile> -i <mfile> -o <ofile>'
    print(output + ustring)

def inputs(argv):
    try:
        opts, args = getopt.getopt(argv,"hm:c:p:i:o:")
        for opt, arg in opts:
            if opt == '-h':
                usage()
                sys.exit()
            elif opt == "-m":
                global modelScript
                modelScript = importlib.import_module(arg)
            elif opt == "-c":
                global modelclassname
                modelclassname = arg
            elif opt == "-p":
                global parametersfile
                parametersfile = arg
            elif opt == "-i":
                global matrixfile
                matrixfile = arg
            elif opt == "-o":
                global outputfilename
                outputfilename = arg
    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit(2)
    

def error(message):
    print("An error has occurred, aborting. Details:\n")
    print(message)
    sys.exit()

def checkparameters(param):
    #Base checks
    if param["state_number"] != len(param["states"]):
        error("States not match declared states number")
    if param["model_classes_number"] != len(param["model_classes"]):
        error("Classes not match declared classes number")
    #Model checks
    if "starting_state" not in param or len(param["starting_state"]) == 0:
        error("Starting state missing")
    if param["starting_state"] not in param["states"]:
        error("States declaration error")
    if "transmission_action" not in param or param["transmission_action"] == "":
        error("Transmission action not declared")
    if len(param["transmission_states"]) + len(param["internal_classed_states"]) + len(param["classless_states"]) != param["state_number"]:
        error("States subdivision error, check transmission/internal/classless states")
    #need to check all the basic rates: rate for each class for each action except transmission action
    seq = param["states_description"]
    rates = param["disease_rates_by_class"]
    taction = param["transmission_action"]
    to_check = []
    for k,v in seq.items():
        for e in v:
            to_check.append(e[0])
    for a in to_check:
        for v in rates.values():
            if a not in v.keys() and taction not in a:
                error("States and action rates binding error")
    #Initial state check
    sum = 0
    for s in param["initial_state"].values():
        if len(param["initial_state"].values()) != param["model_classes_number"]:
            error("Error in initial state definition")
        for p in s:
            sum += p
    if sum != param["total_population"]:
        error("Error in initial state definition")

def parseparameters():
    global parameters
    pfile = open(parametersfile, 'r')
    parameters = json.load(pfile)
    checkparameters(parameters)
    global WRITER
    WRITER = getattr(modelScript, modelclassname)
    WRITER = WRITER(parameters)
    if not issubclass(type(WRITER), ModelImplementation):
        error("Module provided do not respect ModelImplementation inteface")

def parsematrix():
    file = open(matrixfile, "r")
    global classes
    classes = file.readline().strip('\n').split(sep=',')
    global coordinates
    coordinates = dict()
    for i in range(len(classes)):
        coordinates[classes[i]] = i
    for c in classes:
        if c not in parameters["model_classes"]:
            file.close()
            error("Model classes and matrix classes mismatch error")
    numpy.set_printoptions(suppress=True)
    global matrix
    matrix = numpy.loadtxt(file, delimiter=",", skiprows=0)
    file.close()

#Model computation and writing
def compute_functionalrates():
    #must use imported specifi model functions
    result = WRITER.computeTransmissionRates(coordinates,matrix)
    if type(result) is not tuple:
        error("Model implementation - computeTransmissionRates method result is not compatible")
    elif type(result[0]) is not list or type(result[1]) is not dict:
        error("Model implementation - computeTransmissionRates method result is not compatible")
    #add to OUTFILECONTENT
    OUTFILECONTENT.append("//-Functional rates")
    OUTFILECONTENT.extend(result[0])
    global headerline
    global valuesline
    for k,v in result[1].items():
        headerline += str('"'+k+'"') + ','
        valuesline += str(v) + ','

def model_builder():
    csvcontent = dict()
    #prepare initial state lines, competence of this script, add to OUTFILECONTENT
    statesToClass = set(parameters["states"]) - set(parameters["classless_states"])
    for s in statesToClass:
        for c in parameters["model_classes"]:
            initalNum = parameters["initial_state"][c][parameters["states"].index(s)]
            csvcontent[s + c] = str(initalNum)
    for s in parameters["classless_states"]:
        stateNum = 0
        for c in parameters["model_classes"]:
            stateNum += parameters["initial_state"][c][parameters["states"].index(s)]
        csvcontent[s] = str(stateNum)
    global headerline
    global valuesline
    for k,v in csvcontent.items():
        headerline += str('"'+k+'"') + ','
        valuesline += str(v) + ','
    headerline.strip(',')
    valuesline.strip(',')
    OUTVALUESFILECONTENT.append(headerline)
    OUTVALUESFILECONTENT.append(valuesline)
    #must use imported specifi model functions
    modelDefinition = WRITER.computeModelDefinitions()
    #compute system equation
    systemEquation = WRITER.computeSystemEquation()
    #add to OUTFILECONTENT everything
    OUTFILECONTENT.append("//Model")
    OUTFILECONTENT.extend(modelDefinition)
    OUTFILECONTENT.append("//System equation")
    OUTFILECONTENT.extend(systemEquation)


def writefile():
    filename = outputfilename.split('.')[0]
    print("Writing output file: ", outputfilename, "...")
    with open(outputfilename, 'w') as file:
        for l in OUTFILECONTENT:
            file.write(l)
            file.write("\n")

    outputvaluesfilename = filename + '.csv'
    print("Writing output file: ", outputvaluesfilename, "...")
    with open(outputvaluesfilename, 'w') as file:
        for l in OUTVALUESFILECONTENT:
            file.write(l)
            file.write("\n")

    filecontent = SubstitutionsData()
    filecontent.biopepa_file_name = filename
    filecontent.substitutions = WRITER.getSubstitutions()
    print("Writing output file: ", "custom_functions_config.json", "...")
    with open("custom_functions_config.json", 'w') as file:
        file.write(filecontent.toJSON())



#Main
def main(argv):
    inputs(argv)
    parseparameters()
    parsematrix()
    compute_functionalrates()
    model_builder()
    writefile()


if __name__ == "__main__":
    main(sys.argv[1:])