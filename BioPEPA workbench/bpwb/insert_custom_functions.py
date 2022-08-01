#Imports
from distutils.command.config import config
import sys, getopt
import json


global OUTFILECONTENT
OUTFILECONTENT = []

#Loading data
def usage():
    output = "The script needs:\n\t configuration file 'cfile' \n"
    ustring = 'imput_custom_functions.py -c <cfile> '
    print(output + ustring)

def inputs(argv):
    try:
        opts, args = getopt.getopt(argv,"hc:")
        for opt, arg in opts:
            if opt == '-h':
                usage()
                sys.exit()
            elif opt == "-c":
                global configfilename
                configfilename = arg
    except getopt.GetoptError as err:
        print(err)
        usage()
        sys.exit(2)
    

def error(message):
    print("An error has occurred, aborting. Details:\n")
    print(message)
    sys.exit()

def checkconfiguration(config):
    if "biopepa_file_name" not in config or len(config["biopepa_file_name"]) == 0:
        error("biopepa_file_name missing")
    if "placeholder_variable" not in config or len(config["placeholder_variable"]) == 0:
        error("placeholder_variable missing")
    if "custom_function_c++_name" not in config or len(config["custom_function_c++_name"]) == 0:
        error("custom_function_c++_name missing")
    if "custom_function_c_name" not in config or len(config["custom_function_c_name"]) == 0:
        error("custom_function_c_name missing")
    if "input_time_variable" not in config:
        error("input_time_variable missing")
    if "input_parameters" not in config or len(config["input_parameters"]) == 0:
        error("input_parameters missing")
    

def parseconfiguration():
    global configuration
    pfile = open(configfilename, 'r')
    configuration = json.load(pfile)
    checkconfiguration(configuration)
    

def writefiles():
    #Stochkit modification
    with open('/stochkit/ProblemDefinition.cpp', 'r') as file :
        filedata = file.read()
    #Compute function call
    functionCall = configuration["custom_function_c++_name"] + "("
    first = True
    if configuration["input_time_variable"] == True:
        functionCall += "t"
        first = False
    for v in configuration["input_parameters"].values():
        if first:
            functionCall += v
            first = False
        else:
            functionCall += ", " + v
    functionCall += ")"
    # Replace the target string
    filedata = filedata.replace(configuration["placeholder_variable"], functionCall)

    # Write the file out again
    with open('/stochkit/ProblemDefinition.cpp', 'w') as file:
        file.write(filedata)
    
    #Sundials modification
    filepath = '/sundials/' + configuration["biopepa_file_name"] + '001_cv.c'
    with open(filepath, 'r') as file :
        filedata = file.read()
    #Add custom function include
    index = filedata.find('#include')
    sundialsResult = filedata[:index] + '#include "CustomFunction.c"\n' + filedata[index:]
    #Compute function call
    functionCall = configuration["custom_function_c_name"] + "("
    first = True
    if configuration["input_time_variable"] == True:
        functionCall += "t"
        first = False
    for v in configuration["input_parameters"].values():
        if first:
            functionCall += v
            first = False
        else:
            functionCall += ", " + v
    functionCall += ")"
    # Replace the target string
    placeholder = configuration["placeholder_variable"] + "_"
    sundialsResult = sundialsResult.replace(placeholder, functionCall)

    # Write the file out again
    with open(filepath, 'w') as file:
        file.write(sundialsResult)

#Main
def main(argv):
    inputs(argv)
    parseconfiguration()
    writefiles()


if __name__ == "__main__":
    main(sys.argv[1:])