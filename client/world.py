from time import time
from kivy.lang import Builder
from kivy.properties import ListProperty, NumericProperty, StringProperty
from kivy.uix.widget import Widget
from kivy.vector import Vector
from kivy.animation import Animation
from kivy.core.window import Window
from kivy.logger import Logger as log

from controller import Controller, UserCommands


OBJECT_TTL = 10

class WorldError(Exception):
    pass


class _World(object):
    """ 'background' is a reserved object id """

    def __init__(self):
        Builder.load_file('world.kv', rulesonly=True)
        self._widget = Widget()
        self._user_commands = None
        self._server_tick_duration = None
        self._uid = None
        self._objects = {}
        self._last_tick = None
        self._tick = time()

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
        if self._user_commands is None:
            log.warning("Cannot accept user's command")
            return
        self._user_commands.move_vector(length, angle)

    def fire(self, state):
        self._user_commands.fire(state)

    @property
    def world_pos(self):
        return Vector(self._objects['background'].pos)

    @world_pos.setter
    def world_pos(self, value):
        self._objects['background'].pos = value

    @property
    def window_center(self):
        return Vector(Window.size) / 2

    @property
    def duration(self):
        last_tick = self._last_tick
        server_tick = self._server_tick_duration
        if last_tick is None:
            return server_tick
        else:
            delay = self._tick - last_tick - server_tick
            if 0 < delay < server_tick:
                return server_tick - delay
            elif delay < 0:
                return server_tick
            else:
                return 0

    def _add_object(self, ident, data=None, index=0):
        type = data['type']
        if type == 'background':
            obj = Background(size=data['size'])
        elif type == 'triangle':
            obj = Triangle()
        elif type == 'bullet':
            obj = Bullet()
        else:
            raise WorldError("Unknown type '{}'".format(type))
        self._objects[ident] = obj
        self._widget.add_widget(obj, index)
        if type in ('triangle', 'bullet'):
            pos = Vector(data['pos'])
            pos += self.world_pos
            obj.center = pos
        if type == 'triangle':
            obj.angle = data['angle']
            obj.name = data['name']
        return obj

    def _remove_object(self, ident):
        obj = self._objects.pop(ident)
        self._widget.remove_widget(obj)

    def handle_init(self, uid, server_tick, level_size, objects):
        self._uid = uid
        server_tick = server_tick / 1000.
        self._server_tick_duration = server_tick
        self._user_commands = UserCommands(uid, server_tick)
        self._user_commands.activate()
        self._add_object('background', dict(size=level_size,
                                            type='background'))
        user_data = objects[uid]
        world_pos = self.window_center - Vector(user_data['pos'])
        self.world_pos = world_pos
        self.handle_objects_info(objects)

    def handle_objects_info(self, objects):
        for ident, data in objects.items():
            if ident in self._objects:
                continue
            self._add_object(ident, data)

    def handle_tick(self, objects, bullets):
        self._tick = time()
        user_data = objects[self._uid]
        world_pos = self.window_center - Vector(user_data['pos'])
        #NOTE: strange bug when using pos instead x, y
        bg = self._objects['background']
        animation(bg, self.duration, x=world_pos[0], y=world_pos[1])
        user_obj = self._objects[self._uid]
        animation(user_obj, self.duration,
                  center=self.window_center,
                  angle=user_data['angle'])
        self._tick_objects(objects, world_pos)
        self._tick_bullets(bullets, world_pos)
        self._last_tick = self._tick

    def _tick_objects(self, tick_objects, new_world_pos):
        objects = self._objects
        unknown_objects = set()
        for ident, value in tick_objects.items():
            if ident == self._uid:
                continue
            if ident not in objects:
                unknown_objects.add(ident)
                continue

            obj = objects[ident]
            if isinstance(obj, TemporaryObject):
                obj.ttl = OBJECT_TTL
            pos = Vector(value['pos'])
            pos += new_world_pos
            animation(obj, self.duration, center=pos, angle=value['angle'])
        if unknown_objects:
            Controller.send(cmd="world.get_objects_info",
                            args=dict(idents=list(unknown_objects)))

        for ident in set(objects) - set(tick_objects):
            obj = self._objects[ident]
            if not isinstance(obj, TemporaryObject):
                continue
            obj.ttl -= 1
            if obj.ttl <= 0:
                self._remove_object(ident)

    def _tick_bullets(self, tick_bullets, new_world_pos):
        if not tick_bullets: #NOTE: can be an empty list
            tick_bullets = {}
        objects = self._objects
        for ident, pos in tick_bullets.items():
            if ident in objects:
                bullet = objects[ident]
                pos = Vector(pos) + new_world_pos
                animation(bullet, self.duration, center=pos)
                if bullet.hidden:
                    bullet.show()
            else:
                bullet = self._add_object(ident, dict(type='bullet', pos=pos))

        bullets = set(k for k, v in objects.items() if isinstance(v, Bullet))
        for ident in bullets - set(tick_bullets):
            self._remove_object(ident)



class Background(Widget):
    pass


class TemporaryObject(Widget):

    ttl = NumericProperty(OBJECT_TTL)


class Tri(Widget):
    """Drawing"""
    nose = ListProperty([0, 0])
    angle = NumericProperty(0)


class Triangle(TemporaryObject):
    name = StringProperty()
    angle = NumericProperty(0)


class Bullet(Widget):
    radius = NumericProperty(0)

    def show(self):
        self.opacity = 1

    def hidden(self):
        return not self.opacity

def animation(obj, duration, **kwargs):
    Animation.cancel_all(obj, *kwargs) # unpack keys
    Animation(duration=duration, **kwargs).start(obj)

World = _World()

