from kivy.lang import Builder
from kivy.properties import ListProperty, NumericProperty
from kivy.uix.widget import Widget
from kivy.clock import Clock
from kivy.vector import Vector
from kivy.core.window import Window

from controller import Controller


class _World(object):

    def __init__(self):
        Builder.load_file('world.kv', rulesonly=True)
        self._widget = Widget()
        self._user_commands = None
        self._server_tick = None
        self._uid = None
        self._objects = {}

    def activate(self, parent, name):
        Controller.add_handler('world', self)
        Controller.send(cmd="world.start", args=dict(name=name))
        parent.add_widget(self._widget, 0)

    def deactivate(self):
        Controller.remove_handler('world')
        if self._user_commands is not None:
            self._user_commands.deactivate()
        self._widget.parent.remove_widget(self._widget)

    def move_vector(self, length, angle):
        self._user_commands.move_vector(length, angle)

    def fire(self, state):
        self._user_commands.fire(state)

    def add_object(self, ident, obj, index=0):
        self._objects[ident] = obj
        self._widget.add_widget(obj, index)
        #TODO: set positions and angles

    def handle_init(self, uid, server_tick, level_size):
        self._uid = uid
        self._server_tick = server_tick
        self._user_commands = UserCommands(uid, server_tick)
        self._user_commands.activate()
        self.add_object('background', Background(size=level_size))
        self.add_object(uid, Triangle())

    def handle_objects_info(self, objects):
        for ident, data in objects.items():
            self.add_object(ident, Triangle())

    def handle_tick(self, tick_data):
        objects = self._objects
        window_center = Vector(Window.size) / 2
        user_data = tick_data[self._uid]
        world_pos = window_center - Vector(user_data['pos'])
        objects['background'].pos = world_pos
        user_obj = objects[self._uid]
        user_obj.center = window_center
        user_obj.angle = user_data['angle']
        unknown_objects = set()
        for ident, value in tick_data.items():
            if ident == self._uid:
                continue
            if ident not in objects:
                unknown_objects.add(ident)
                continue
            pos = Vector(value['pos'])
            pos += world_pos
            obj = objects[ident]
            obj.center = pos
            obj.angle = value['angle']
        if unknown_objects:
            Controller.send(cmd="world.get_objects_info",
                            args=dict(idents=unknown_objects))
        #TODO: remove objects that have not been updated for a long time


class Background(Widget):
    pass


class Triangle(Widget):
    nose = ListProperty([0, 0])
    angle = NumericProperty(0)


class UserCommands(object):

    def __init__(self, uid, server_tick):
        self._uid = uid
        self._server_tick = server_tick
        self._fire = False
        self._move_vector = (0, 0) # length, angle

    def move_vector(self, length, angle):
        if not length and not angle:
            self._move_vector = (length, self._move_vector[1])
        else:
            self._move_vector = (length, angle)

    def fire(self, state):
        self._fire = state

    def activate(self):
        Clock.schedule_interval(self.send, self._server_tick / 1000.)

    def deactivate(self):
        Clock.unschedule(self.send)

    def send(self, dt):
        data = dict(cmd='world.user_commands',
                    args=dict(move_vector=self._move_vector,
                              fire=self._fire))
        Controller.send(**data)



World = _World()

