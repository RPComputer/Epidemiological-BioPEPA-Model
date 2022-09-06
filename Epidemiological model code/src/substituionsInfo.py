class SubstitutionInfo:
    def __init__(self, placeholderVariable = "", input_time_variable = True, custom_function_cpp_name = "readDatatable", custom_function_c_name = "readDatatable", inputParameters = []):
        self.placeholderVariable = placeholderVariable
        self.custom_function_cpp_name = custom_function_cpp_name
        self.custom_function_c_name = custom_function_c_name
        self.input_time_variable = input_time_variable
        self.input_parameters = inputParameters
