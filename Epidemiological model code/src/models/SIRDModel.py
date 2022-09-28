#Imports
from modelImplementation import ModelImplementation
from transition import Transition
from substituionsInfo import SubstitutionInfo
from itertools import product
import numpy as np

class SIRDmodel (ModelImplementation):

    def __init__(self, parameters):
        super().__init__(parameters)
        self.classes = self.parameters["model_classes"]
        self.transmissionStatesToClass = set(self.parameters["transmission_states"])
        self.taction = parameters["transmission_action"]
        self.substitutions.append(SubstitutionInfo(placeholderVariable="placeholder", custom_function_c_name="readRt", custom_function_cpp_name="readRt", inputParameters=["\"Rt.csv\""]))

    def contactToTransmissionMatrix(self, contactMatrix, diseaseRates):
        diseaseRates = list(diseaseRates)
        #build transition matrix
        rows = []
        c = 0
        for i in range(len(contactMatrix)):
            row = []
            for j in range(len(contactMatrix[0])):
                if i == j:
                    alpha = diseaseRates[c]["recovery"]
                    gamma = diseaseRates[c]["death"]
                    row.append(1/(alpha + gamma))
                else:
                    row.append(0)
            rows.append(row)
        inverseSigma = np.matrix(rows)
        #compute rho of contact matrix times transition matrix
        matrixProduct = np.matmul(contactMatrix, inverseSigma)
        #obtain beta coefficient to obtain transmission matrix
        rho = max(abs(np.linalg.eigvals(matrixProduct)))
        beta = 3/rho
        print("Rho:", rho)
        print("Beta:", beta)
        #compute transmission matrix
        Tmatrix = beta * contactMatrix
        return Tmatrix

    def defineTransmissionFormula(self, Tmatrix, coord, c1, c2, ratename):
        coefficients = dict()
        coefficient = Tmatrix[coord[c1],coord[c2]]
        coefficientName = ratename + "rate"
        coefficients[coefficientName] = coefficient
        infected = 'I' + c2 + '*'
        notInfected = ""
        totSum = ""
        classedStates = set(self.parameters["transmission_states"]).union(set(self.parameters["internal_classed_states"]))
        for s in classedStates:
            totSum += s+c2 + " + "
            if s != 'I':
                notInfected += s+c2 + " + "
        totSum = totSum.strip('+ ')
        notInfected = notInfected.strip('+ ')
        elements = 'recovery'+c1+'rate + death'+c1+'rate'
        rate = "((" + infected + str(coefficientName) + '*(' + notInfected + ')' + '*(' + elements + ')*placeholder)/(' + totSum + "))"
        action = ratename + " = [" + rate + "];"
        return (action, coefficients)

    def computeTransmissionRates(self, coord, matrix):
        Tmatrix = self.contactToTransmissionMatrix(matrix, self.parameters["disease_rates_by_class"].values())
        diseaseRates = self.parameters["disease_rates_by_class"]
        result = ([],dict())
        result[1]['placeholder'] = 1
        #compute actions linked to classes that have to be rated
        classedActionsToRate = [self.taction]
        for e,act in self.parameters["states_description"].items():
            for s in self.parameters["internal_classed_states"]:
                for a in act:
                    if s == e or s == a[1]:
                        classedActionsToRate.append(a[0])
               
        #compute rates for classed actions
        for a in classedActionsToRate:
            if a == self.taction:
                for c in product(self.classes, repeat=2):
                    ratename = a + c[0] + c[1]
                    transmissionInfo = self.defineTransmissionFormula(Tmatrix, coord, c[0], c[1], ratename)
                    result[0].append(transmissionInfo[0])
                    result[1].update(transmissionInfo[1])
                    for s in self.transmissionStatesToClass:
                        for t in self.parameters["states_description"][s]:
                            infectiveState = self.parameters["transmission_states"][1]
                            if t[0] == a:
                                trans = Transition(start_state=s,end_state=t[1],cat_state=infectiveState,start_category=c[0],end_category=c[0],cat_category=c[1],action=a,ratename=ratename)
                                self.transitions.append(trans)
            else:
                for k,v in diseaseRates.items():
                    for k1,v1 in v.items():
                        if k1 == a:
                            ratename = a+k
                            coefficient = v1
                            coefficientName = a+k+"rate"
                            elements = '*I' + k
                            action = ratename + " = [" + str(coefficientName) + elements + "];"
                            result[0].append(action)
                            result[1][coefficientName] = coefficient
                            for s in self.parameters["internal_classed_states"]:
                                for s1,a1 in self.parameters["states_description"].items():
                                    for t in a1:
                                        if t[0] == k1 and t[1] == s:
                                            trans = Transition(start_state=s1,end_state=s,start_category=k,end_category=k,action=a,ratename=ratename)
                                            self.transitions.append(trans)

        #compute rates for classless actions
        for k,v in diseaseRates.items():
            for k1,v1 in v.items():
                if k1 not in classedActionsToRate:
                    ratename = k1+k
                    coefficient = v1
                    coefficientName = k1+k+"rate"
                    elements = '*I' + k
                    action = ratename + " = [" + str(coefficientName) + elements + "];"
                    result[0].append(action)
                    result[1][coefficientName] = coefficient
                    for s in self.parameters["classless_states"]:
                        for s1,a1 in self.parameters["states_description"].items():
                            for t in a1:
                                if t[0] == k1 and t[1] == s:
                                    trans = Transition(start_state=s1,end_state=s,start_category=k,action=t[0],ratename=ratename)
                                    self.transitions.append(trans)
        self.ratesComputed = True
        return result


    def transition_info(self,s,c,t):
        res = []
        if s == t.start_state and c == t.start_category:
            res.append(t.ratename)
            res.append("<<"+s+c)
            return res
        elif s == t.end_state and c == t.end_category:
            res.append(t.ratename)
            res.append(">>"+s+c)
            return res
        elif s == t.cat_state and c == t.cat_category:
            res.append(t.ratename)
            res.append("(+)")
            return res
        else: return None

    def computeModelDefinitions(self):
        super().computeModelDefinitions()
        model = []
        for s in self.transmissionStatesToClass:
            for c in self.classes:
                line = s+c+" = "
                for tr in self.transitions:
                        tinfo = self.transition_info(s,c,tr)
                        if tinfo is not None:
                            transition = "(" + tinfo[0] + ",1)" + tinfo[1]
                            line += transition
                            line += " + "
                line = line.strip('+ ')
                line += ";"
                model.append(line)

        for s in self.parameters["internal_classed_states"]:
            involved_transitions = []
            for t in self.transitions:
                for x in self.parameters["states_description"].values():
                    if (t.start_state == s or t.end_state == s) and t.action in [e[0] for e in x]:
                        involved_transitions.append(t)
            for c in self.classes:
                line = s+c+" = "
                for t in involved_transitions:
                    tinfo = self.transition_info(s,c,t)
                    if tinfo is not None:
                        transition = "(" + tinfo[0] + ",1)" + tinfo[1]
                        line += transition
                        line += " + "
                line = line.strip('+ ')
                line += ";"
                model.append(line)

        for s in self.parameters["classless_states"]:
            involved_transitions = []
            for t in self.transitions:
                for x in self.parameters["states_description"].values():
                    if t.end_state == s and t.action in [e[0] for e in x]:
                        involved_transitions.append(t)
            line = s+" = "
            for t in involved_transitions:
                transition = "(" + t.ratename + ",1)" + ">>" + s
                line += transition
                line += " + "
            line = line.strip('+ ')
            line += ";"
            model.append(line)

        return model
    
    def computeSystemEquation(self):
        systemEquation = []
        line = "("
        for s in self.transmissionStatesToClass:
            for c in self.classes:
                line += s + c + " <> "
        for s in self.parameters["internal_classed_states"]:
            for c in self.classes:
                line += s + c + " <> "
        for s in self.parameters["classless_states"]:
            line += s
            if not s == self.parameters["classless_states"][-1]:
                line += " <> "
        line += ")"
        systemEquation.append(line)
        return systemEquation