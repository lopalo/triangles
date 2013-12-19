from kivy.lang import Builder
from kivy.clock import Clock
from kivy.properties import ListProperty, NumericProperty
from kivy.uix.widget import Widget
from kivy.vector import Vector
from kivy.core.window import Window

from network import Connection, UserCommands


UPDATE_PERIOD = 1. / 40.


class _World(object):

    def __init__(self):
        Builder.load_file('world.kv', rulesonly=True)
        self._widget = Widget()
        self.connection = None
        self._user_commands = None
        self._server_tick = None
        self._uid = None
        self._objects = {}

    def activate(self, parent):
        parent.add_widget(self._widget, 0)
        Clock.schedule_interval(self.update, UPDATE_PERIOD)

    def deactivate(self):
        self._widget.parent.remove_widget(self._widget)
        Clock.unschedule(self.update)
        if self._user_commands is not None:
            self._user_commands.deactivate()

    def do_connect(self, address, name):
        self.connection = conn = Connection(address)
        conn.connect()
        conn.send(cmd="start_game", args=dict(name=name))

    def move_vector(self, length, angle):
        self._user_commands.move_vector(length, angle)

    def fire(self, state):
        self._user_commands.fire(state)

    def add_object(self, ident, obj, index=0):
        self._objects[ident] = obj
        self._widget.add_widget(obj, index)

    def update(self, dt):
        data = self.connection.receive()
        while data is not None:
            cmd, args = data['cmd'], data['args']
            getattr(self, 'handle_' + cmd)(**args)
            data = self.connection.receive()

    def handle_init(self, uid, server_tick, level_size):
        self._uid = uid
        self._server_tick = server_tick
        self._user_commands = UserCommands(uid, server_tick,
                                           self.connection.send)
        self._user_commands.activate()
        self.add_object('background', Background(size=level_size))
        self.add_object(uid, Triangle())

    def handle_tick(self, tick_data):
        objects = self._objects
        window_center = Vector(Window.size) / 2
        user_data = tick_data[self._uid]
        world_pos = window_center - Vector(user_data['pos'])
        objects['background'].pos = world_pos
        user_obj = objects[self._uid]
        user_obj.center = window_center
        user_obj.angle = user_data['angle']
        for ident, value in tick_data.items():
            if ident == self._uid:
                continue
            if ident not in objects:
                #TODO: request object info
                self.add_object(ident, Triangle())
                continue
            pos = Vector(value['pos'])
            pos += world_pos
            obj = objects[ident]
            obj.center = pos
            obj.angle = value['angle']


class Background(Widget):
    pass


class Triangle(Widget):
    nose = ListProperty([0, 0])
    angle = NumericProperty(0)


World = _World()
