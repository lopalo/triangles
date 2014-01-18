import os
import json
import logging

from kivy.app import App
from kivy.uix.widget import Widget
from kivy.uix.boxlayout import BoxLayout
from kivy.properties import (
    ObjectProperty, NumericProperty,
    ReferenceListProperty)
from kivy.vector import Vector
from kivy.logger import Logger

from world import World
from controller import Controller


class StartMenu(BoxLayout):

    def connect(self, *args, **kwargs):
        self.parent.connect(*args, **kwargs)


class LeftControl(Widget):
    outer_rad = NumericProperty(0)
    inner_x = NumericProperty(0)
    inner_y = NumericProperty(0)
    inner_pos = ReferenceListProperty(inner_x, inner_y)
    inner_rad = NumericProperty(0)
    pressed = False

    def on_touch_down(self, touch):
        center_vect = Vector(self.center)
        vect = Vector(touch.pos) - center_vect
        if vect.length() > self.outer_rad:
            return
        self.pressed = True
        self.inner_pos = vect + center_vect
        self.move_vector(vect)

    def on_touch_move(self, touch):
        if not self.pressed:
            return
        center_vect = Vector(self.center)
        vect = Vector(touch.pos) - center_vect
        if vect.length() > self.outer_rad:
            vect /= vect.length() / self.outer_rad
        self.inner_pos = vect + center_vect
        self.move_vector(vect)

    def on_touch_up(self, touch):
        if not self.pressed:
            return
        self.pressed = False
        self.inner_pos = self.center
        self.move_vector(Vector(0, 0))

    def move_vector(self, vect):
        length = vect.length() / self.outer_rad
        World.move_vector(length, vect.angle(Vector(1, 0)))


class RightControl(Widget):
    pressed = False

    def on_touch_down(self, touch):
        vect = Vector(touch.pos) - Vector(self.center)
        if vect.length() > self.radius:
            return
        self.pressed = True
        World.fire(True)

    def on_touch_up(self, touch):
        if not self.pressed:
            return
        self.pressed = False
        World.fire(False)


class UI(Widget):
    pass


class GameWidget(Widget):

    ui = ObjectProperty(None)

    def activate_world(self, *args, **kwargs):
        World.activate(self, *args, **kwargs)


class MainWidget(Widget):

    start_menu = ObjectProperty(None)
    game_widget = ObjectProperty(None)

    def connect(self, address, name):
        self.remove_widget(self.start_menu)
        Controller.connect(address, wait=True)
        self.game_widget = GameWidget()
        self.game_widget.activate_world(name)
        self.add_widget(self.game_widget)


class GameApp(App):

    def __init__(self, start_args=None, **kwargs):
        self.start_args = start_args
        super(GameApp, self).__init__(**kwargs)

    def build(self):
        Controller.activate()
        start_args = self.start_args
        if start_args is not None:
            self.root.connect(start_args['address'], start_args['name'])
        return self.root



if __name__ == '__main__':
    Logger.setLevel(logging.INFO)
    start_args = None
    if 'TRI_START_ARGS' in os.environ:
        with open(os.environ['TRI_START_ARGS']) as f:
            start_args = json.load(f)
    GameApp(start_args=start_args).run()


