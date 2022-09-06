#Imports
from distutils.command.config import config
from os import getcwd
import sys, getopt
import json

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
    if "substitutions" not in config or len(config["substitutions"]) == 0:
        error("substitutions missing")
    for substituion in config["substitutions"]:
        if "placeholder_variable" not in substituion or len(substituion["placeholder_variable"]) == 0:
            error("placeholder_variable missing")
        if "custom_function_c++_name" not in substituion or len(substituion["custom_function_c++_name"]) == 0:
            error("custom_function_c++_name missing")
        if "custom_function_c_name" not in substituion or len(substituion["custom_function_c_name"]) == 0:
            error("custom_function_c_name missing")
        if "input_time_variable" not in substituion:
            error("input_time_variable missing")
        if "input_parameters" not in substituion or len(substituion["input_parameters"]) == 0:
            error("input_parameters missing")
    

def parseconfiguration():
    global configuration
    pfile = open(configfilename, 'r')
    configuration = json.load(pfile)
    checkconfiguration(configuration)
    
#Needed for stochkit replace
def rreplace(s, old, new, occurrence):
    li = s.rsplit(old, occurrence)
    return new.join(li)

def writefiles():
    #STOCHKIT modification
    for substituion in configuration["substitutions"]:
        with open('./stochkit/ProblemDefinition.cpp', 'r') as file :
            filedata = file.read()
        #Compute function call
        functionCall = substituion["custom_function_c++_name"] + "("
        first = True
        if substituion["input_time_variable"] == True:
            functionCall += "t"
            first = False
        for v in substituion["input_parameters"]:
            if first:
                functionCall += v
                first = False
            else:
                functionCall += ", " + v
        functionCall += ")"
        
        # Replace the target string
        #filedata = filedata.replace(substituion["placeholder_variable"], functionCall)
        filedata = rreplace(filedata, substituion["placeholder_variable"], functionCall, filedata.count(substituion["placeholder_variable"]) - 2) #skip first 2 occurences for rates declaration
        
    # Write the file out again
    with open('./stochkit/ProblemDefinition.cpp', 'w') as file:
        file.write(filedata)
    include = True
    #SUNDIALS modification
    filepath = './sundials/' + configuration["biopepa_file_name"] + '001_cv.c'
    for substituion in configuration["substitutions"]:
        with open(filepath, 'r') as file :
            filedata = file.readlines()
        #Add custom function include
        if include:
            index = filedata.index('#include <math.h>\n')
            filedata.insert(index, '#include "CustomFunctions.c"\n')
            include = False
        #Compute function call
        functionCall = substituion["custom_function_c_name"] + "("
        first = True
        if substituion["input_time_variable"] == True:
            functionCall += "t"
            first = False
        for v in substituion["input_parameters"]:
            if '"' not in v:
                v = v + '_'
            if first:
                functionCall += v
                first = False
            else:
                functionCall += ", " + v
        functionCall += ")"
        # Replace the target string
        placeholder = substituion["placeholder_variable"] + "_"
        for i in range(len(filedata)):
            if placeholder in filedata[i]:
                if filedata[i].find('=') < filedata[i].find(placeholder) and 'realtype' not in filedata[i]:
                    filedata[i] = filedata[i].replace(placeholder, functionCall)
        filedata = ''.join(filedata)
    # Write the file out again
    with open(filepath, 'w') as file:
        file.write(filedata)

#Main
def main(argv):
    inputs(argv)
    parseconfiguration()
    writefiles()


if __name__ == "__main__":
    main(sys.argv[1:])