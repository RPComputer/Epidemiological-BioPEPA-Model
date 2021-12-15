class Transition:
    def __init__(self, start_state, end_state, action, ratename, start_category = None, cat_state = None, cat_category = None, end_category = None) -> None:
        self.start_state = start_state
        self.start_category = start_category
        self.cat_state = cat_state
        self.cat_category = cat_category
        self.end_state = end_state
        self.end_category = end_category
        self.action = action
        self.ratename = ratename