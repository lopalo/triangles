from kivy.uix.widget import Widget


class _World(object):

    def __init__(self):
        self.widget = Widget()

    def move_vector(self, length, angle):
        #TODO
        print "Move: ", length, angle

    def fire(self, state):
        #TODO
        print "Fire: ", state


World = _World()
