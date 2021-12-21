#Imports
from modelImplementation import ModelImplementation
from transition import Transition
from itertools import combinations_with_replacement as comb

class ExampleModel (ModelImplementation):

    def __init__(self, parameters):
        super().__init__(parameters)
        self.classes = self.parameters["model_classes"]
        self.statesToClass = set(self.parameters["states"]) - set(self.parameters["classless_states"])
        self.transitions = []


    def computeContacts(self, coord, matrix):
        result = []
        classedActionsToRate = []
        for s in self.statesToClass:
            for c in self.parameters["states_description"].keys():
                if s == c:
                    for i in range(len(self.parameters["states_description"][s])):
                        classedActionsToRate.append(self.parameters["states_description"][s][i][0])
        for c in comb(self.classes,2):
            for a in classedActionsToRate:
                ratename = a + c[0] + c[1]
                coefficient = matrix[coord[c[0]],coord[c[1]]]
                elements = '*S' + c[0] + '*I' + c[1]
                result.append(ratename + " = [" + str(coefficient) + elements + "];")
                for s in self.statesToClass:
                    for t in self.parameters["states_description"][s]:
                        if t[0] == a:
                            trans = Transition(start_state=s,end_state=t[1],cat_state=t[1],start_category=c[0],end_category=c[0],cat_category=c[1],action=a,ratename=ratename)
                            self.transitions.append(trans)
        
        diseaseRates = self.parameters["disease_rates_by_class"]
        for k,v in diseaseRates.items():
            for k1,v1 in v.items():
                ratename = k1+k
                coefficient = v1
                elements = '*I' + k
                result.append(ratename + " = [" + str(coefficient) + elements + "];")
                #self.transitions.append(ratename)
                for s in self.parameters["classless_states"]:
                    for s1,a1 in self.parameters["states_description"].items():
                        for t in a1:
                            if t[1] == s:
                                trans = Transition(start_state=s1,end_state=s,start_category=k,action=t[0],ratename=ratename)
                                self.transitions.append(trans)
        self.ratesComputed = True
        return result


    def transition_info(self,s,c,t):
        res = []
        #if c is not None:
        for tr in self.transitions:
            if s == tr.start_state and c == tr.start_category and t[0] == tr.action:
                res.append(tr.ratename)
                res.append("<<"+s)
                return res
            elif s == tr.end_state and c == tr.end_category and t[0] == tr.action:
                res.append(tr.ratename)
                res.append(">>"+s)
                return res
            elif s == tr.cat_state and c == tr.cat_category and t[0] == tr.action:
                res.append(tr.ratename)
                res.append("(+)")
                return res
        '''
        else:
            for tr in self.transitions:
                if s == tr.start_state and c == tr.start_category and t[0] == tr.action:
                    res.append(tr.ratename)
                    res.append("<<"+s)
                    return res
                elif s == tr.end_state and c == tr.end_category and t[0] == tr.action:
                    res.append(tr.ratename)
                    res.append(">>"+s)
                    return res
                elif s == tr.cat_state and c == tr.cat_category and t[0] == tr.action:
                    res.append(tr.ratename)
                    res.append("(+)")
                    return res
        '''

    def computeModelDefinitions(self):
        super().computeModelDefinitions()
        model = []
        for s in self.statesToClass:
            for c in self.classes:
                line = s+c+" = "
                first = True
                for t in self.parameters["states_description"][s]:
                    tinfo = self.transition_info(s,c,t)
                    transition = "(" + tinfo[0] + ",1)" + tinfo[1]
                    line += transition
                    if not first and not t == self.parameters["states_description"][s][-1]:
                        line += " + "
                line += ";"
                model.append(line)

        for s in self.parameters["classless_states"]:
            involved_transitions = []
            for t in self.transitions:
                if t.end_state == s:
                    involved_transitions.append(t)
            line = s+" = "
            first = True
            for t in involved_transitions:
                transition = "(" + t.ratename + ",1)" + ">>" + s
                line += transition
                if not first and not t == involved_transitions[-1]:
                    line += " + "
            line += ";"
            model.append(line)

        return model
    
    def computeSystemEquation(self):
        systemEquation = []
        line = ""
        for s in self.statesToClass:
            for c in self.parameters["model_classes"]:
                line += s + c + "[" + s + c +"0] <*> "
        for s in self.parameters["classless_states"]:
            line += s + "[" + s +"0]"
            if not s == self.parameters["classless_states"][-1]:
                line += " <*> "
        systemEquation.append(line)
        return systemEquation