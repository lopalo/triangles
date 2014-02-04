from kivy.lang import Builder
from kivy.properties import ListProperty, NumericProperty, StringProperty
from kivy.uix.widget import Widget
from kivy.vector import Vector
from kivy.animation import Animation
from kivy.core.window import Window
from kivy.logger import Logger as log

from controller import Controller, UserCommands


OBJECT_TTL = 10
BULLET_TTL = 2

class WorldError(Exception):
    pass


class _World(object):
    """ 'background' is a reserved object id """

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
        self._server_tick = server_tick
        self._user_commands = UserCommands(uid, server_tick)
        self._user_commands.activate()
        self._add_object('background', dict(size=level_size,
                                            type='background'))
        user_data = objects[uid]
        window_center = Vector(Window.size) / 2
        world_pos = window_center - Vector(user_data['pos'])
        self.world_pos = world_pos
        self.handle_objects_info(objects)

    def handle_objects_info(self, objects):
        for ident, data in objects.items():
            if ident in self._objects:
                continue
            self._add_object(ident, data)

    def handle_tick(self, objects, bullets):
        #TODO: divide this method
        objs = self._objects
        duration = self._server_tick / 1000.
        window_center = Vector(Window.size) / 2
        user_data = objects[self._uid]
        world_pos = window_center - Vector(user_data['pos'])
        #NOTE: strange bug when using pos instead x, y
        bg = objs['background']
        Animation.cancel_all(bg, 'x', 'y')
        Animation(x=world_pos[0], y=world_pos[1], duration=duration).start(bg)
        user_obj = objs[self._uid]
        Animation.cancel_all(user_obj, 'center', 'angle')
        Animation(center=window_center,
                  angle=user_data['angle'],
                  duration=duration).start(user_obj)
        unknown_objects = set()
        for ident, value in objects.items():
            if ident == self._uid:
                continue
            if ident not in objs:
                unknown_objects.add(ident)
                continue

            obj = objs[ident]
            if isinstance(obj, TemporaryObject):
                obj.ttl = OBJECT_TTL
            pos = Vector(value['pos'])
            pos += world_pos
            Animation.cancel_all(obj, 'center', 'angle')
            Animation(center=pos,
                      angle=value['angle'],
                      duration=duration).start(obj)
        if unknown_objects:
            Controller.send(cmd="world.get_objects_info",
                            args=dict(idents=list(unknown_objects)))
        for ident, pos in bullets.items():
            if ident in objs:
                bullet = objs[ident]
                #TODO: animation
                bullet.center = Vector(pos) + world_pos
                if bullet.hidden:
                    bullet.show()
                bullet.ttl = BULLET_TTL
            else:
                bullet = self._add_object(ident, dict(type='bullet', pos=pos))

        for ident in set(objs) - set(objects) - set(bullets):
            obj = objs[ident]
            if not isinstance(obj, TemporaryObject):
                continue
            obj.ttl -= 1
            if obj.ttl <= 0:
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


class Bullet(TemporaryObject):
    ttl = NumericProperty(BULLET_TTL)

    def show(self):
        self.opacity = 1

    def hidden(self):
        return not self.opacity


World = _World()

