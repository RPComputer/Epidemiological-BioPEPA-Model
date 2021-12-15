#Imports
from modelImplementation import ModelImplementation
from itertools import combinations_with_replacement as comb

class ExampleModel (ModelImplementation):

    def computeContacts(self, coord, matrix):
        result = []
        classes = self.parameters["model_classes"]
        statesToClass = set(self.parameters["states"]) - set(self.parameters["classless_states"])
        classedActionsToRate = []
        for s in statesToClass:
            for c in self.parameters["states_description"].keys():
                if s == c:
                    for i in len(self.parameters["states_description"][s]):
                        classedActionsToRate.append(self.parameters["states_description"][s][i][0])
        for c in comb(classes,2):
            for a in classedActionsToRate:
                ratename = a + c[0] + c[1]
                coefficient = matrix[coord[c[0]],coord[c[1]]]
                elements = '*S' + c[0] + '*I' + c[1]
                result.append(ratename + " = [" + coefficient + elements + "];")
        diseaseRates = self.parameters["disease_rates_by_class"]
        for k,v in diseaseRates.values():
            for k1,v1 in v:
                ratename = k1+k
                coefficient = v1
                elements = '*I' + k
                result.append(ratename + " = [" + coefficient + elements + "];")
        return result

    def computeModelDefinitions(self, parameters):
        pass
    
    def computeSystemEquation(self):
        return super().computeSystemEquation()