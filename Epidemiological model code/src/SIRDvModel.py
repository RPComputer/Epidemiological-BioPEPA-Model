#Imports
from SIRDModel import SIRDmodel
from transition import Transition
from substituionsInfo import SubstitutionInfo
from itertools import product
import numpy as np

class SIRDvmodel (SIRDmodel):

    def computeTransmissionRates(self, coord, matrix):
        Tmatrix = self.contactToTransmissionMatrix(matrix, self.parameters["disease_rates_by_class"].values())
        diseaseRates = self.parameters["disease_rates_by_class"]
        result = ([],dict())
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
                            if t[0] == a:
                                trans = Transition(start_state=s,end_state=t[1],cat_state=t[1],start_category=c[0],end_category=c[0],cat_category=c[1],action=a,ratename=ratename)
                                self.transitions.append(trans)
            else:
                for k,v in diseaseRates.items():
                    for k1,v1 in v.items():
                        #update with vaccination rate: if v1 is string then...
                        if k1 == a and type(v1) is not str:
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
                        elif k1 == a and type(v1) is str:
                            ratename = a+k
                            coefficientName = a+k+"rate"
                            elements = '*S' + k
                            action = ratename + " = [" + str(coefficientName) + elements + "];"
                            result[0].append(action)
                            result[1][coefficientName] = 1
                            functionCallInfo = SubstitutionInfo(placeholderVariable=coefficientName,inputParameters=["vaccinations.csv",k])
                            self.substitutions.append(functionCallInfo)
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
