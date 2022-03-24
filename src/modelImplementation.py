import sys

class ModelImplementation:

    def __init__(self, parameters):
        self.parameters = parameters
        self.ratesComputed = False

    def computeTransmissionRates(self,matrix):
        pass
    
    def computeModelDefinitions(self):
        if not self.ratesComputed:
            print("An error has occurred, aborting. Details:\n")
            print("Impossible to compute model without computing functional rates first")
            sys.exit()
    
    def computeSystemEquation(self):
        pass
