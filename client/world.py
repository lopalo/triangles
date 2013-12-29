from kivy.lang import Builder
from kivy.properties import ListProperty, NumericProperty
from kivy.uix.widget import Widget
from kivy.vector import Vector
from kivy.animation import Animation
from kivy.core.window import Window

from controller import Controller, UserCommands


INIT_OBJECT_TTL = 10


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

    def _add_object(self, ident, obj, data=None, index=0):
        self._objects[ident] = obj
        self._widget.add_widget(obj, index)
        if not isinstance(obj, MovableObject) or data is None:
            return
        world_pos = Vector(self._objects['background'].pos)
        pos = Vector(data['pos'])
        pos += world_pos
        obj.center = pos
        obj.angle = data['angle']

    def _remove_object(self, ident):
        obj = self._objects.pop(ident)
        self._widget.remove_widget(obj)

    def handle_init(self, uid, server_tick, level_size):
        self._uid = uid
        self._server_tick = server_tick
        self._user_commands = UserCommands(uid, server_tick)
        self._user_commands.activate()
        self._add_object('background', Background(size=level_size))
        self._add_object(uid, Triangle())

    def handle_objects_info(self, objects):
        for ident, data in objects.items():
            if ident in self._objects:
                continue
            self._add_object(ident, Triangle(), data)

    def handle_tick(self, tick_data):
        objects = self._objects
        duration = self._server_tick / 1000.
        window_center = Vector(Window.size) / 2
        user_data = tick_data[self._uid]
        world_pos = window_center - Vector(user_data['pos'])
        #NOTE: strange bug when using pos instead x, y
        bg = objects['background']
        Animation.cancel_all(bg, 'x', 'y')
        Animation(x=world_pos[0], y=world_pos[1], duration=duration).start(bg)
        user_obj = objects[self._uid]
        Animation.cancel_all(user_obj, 'center', 'angle')
        Animation(center=window_center,
                  angle=user_data['angle'],
                  duration=duration).start(user_obj)
        unknown_objects = set()
        for ident, value in tick_data.items():
            if ident == self._uid:
                continue
            if ident not in objects:
                unknown_objects.add(ident)
                continue

            obj = objects[ident]
            if isinstance(obj, TemporaryObject):
                obj.ttl = INIT_OBJECT_TTL
            pos = Vector(value['pos'])
            pos += world_pos
            Animation.cancel_all(obj, 'center', 'angle')
            Animation(center=pos,
                      angle=value['angle'],
                      duration=duration).start(obj)
        if unknown_objects:
            Controller.send(cmd="world.get_objects_info",
                            args=dict(idents=unknown_objects))
        for ident in set(objects) - set(tick_data):
            obj = objects[ident]
            if not isinstance(obj, TemporaryObject):
                continue
            obj.ttl -= 1
            if obj.ttl <= 0:
                self.remove_object(ident)


class Background(Widget):
    pass


class TemporaryObject(Widget):

    ttl = NumericProperty(INIT_OBJECT_TTL)


class MovableObject(TemporaryObject):
    pass


class Triangle(MovableObject):
    nose = ListProperty([0, 0])
    angle = NumericProperty(0)


World = _World()

